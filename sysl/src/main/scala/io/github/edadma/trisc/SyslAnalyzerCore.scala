package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerCore:
  self: SyslAnalyzer =>

  def analyze(programIn: ProgramAST): TProgram =
    // Pre-pass 0: extract module name, then lower extension blocks. Module
    // extraction has to come first so each ExtensionEntry gets the right
    // `definingModule` for visibility checks. Lowering replaces every
    // ExtensionDeclAST with one synthesized free FunDeclAST per method, so
    // the rest of the pipeline can register/analyze them as if the user had
    // written `__ext_<recvType>__<method>(recv: T, ...) -> R = body` by hand.
    for decl <- programIn.decls do
      decl match
        case ModuleDeclAST(path) => currentModule = Some(path.mkString("_"))
        case _ =>
    // Own module's extensions are always visible. Imported modules are added
    // by `registerImport`; the empty sentinel handles unmoduled compilation
    // units where same-unit extensions still need to dispatch.
    currentModule.foreach(visibleExtensionModules.add)
    val (loweredDecls, extEntries) = lowerExtensions(programIn.decls)
    for entry <- extEntries do
      val bucket = extensionsByMethod.getOrElseUpdate(entry.methodName, mutable.ListBuffer.empty)
      // Dedup against entries already mirrored from same-module siblings
      // (driver passes meta.extensions through to siblings now). Without this,
      // a sibling-file extension would already be in the bucket and the local
      // lowering would re-add the same (definingModule, mangledFnName) pair,
      // making tryExtensionDispatch report it as ambiguous.
      if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
        bucket += entry
    val program = ProgramAST(loweredDecls)

    // Pass 0: forward-declare all type names so recursive references resolve.
    // Struct and enum names are registered as placeholder types; fields are
    // resolved in the next pass once all names are visible.
    //
    // Same-file duplicate struct/enum declarations are still rejected
    // (`pass0*` set tracks own-file decls). Cross-sibling pre-registration
    // may have already placed a placeholder in `structTypes`/`dataEnumTypes`;
    // that's allowed — the own file's declaration wins and pass 0.5 fills
    // the real fields. Without this tolerance, two-file modules where each
    // sibling pre-registers the other's types throw spurious duplicates.
    val pass0Structs = mutable.HashSet[String]()
    val pass0Enums = mutable.HashSet[String]()
    for decl <- program.decls do
      decl match
        case sd @ StructDeclAST(name, _, typeParams, attrs, _, _, _) if typeParams.isEmpty =>
          if pass0Structs.contains(name) then
            throw AnalysisError(s"duplicate struct: '$name'", decl)
          pass0Structs += name
          structTypes(name) = SyslType.StructType(name, Nil) // placeholder — fields filled below
          // δ.5: track ghost-marked struct types so the discipline check (in
          // validateGhostDiscipline) can reject real-code construction.
          if attrs.exists(_.name == "ghost") then ghostTypes += name
        case ed @ DataEnumDeclAST(name, _, typeParams, attrs, _, _) if typeParams.isEmpty =>
          if pass0Enums.contains(name) then
            throw AnalysisError(s"duplicate enum: '$name'", decl)
          pass0Enums += name
          dataEnumTypes(name) = SyslType.EnumType(name, Nil) // placeholder — variants filled below
          if attrs.exists(_.name == "ghost") then ghostTypes += name
        case ee @ EnumDeclAST(name, _, attrs) =>
          if attrs.exists(_.name == "ghost") then ghostTypes += name
        case _ => ()

    // Extract module path for name mangling
    for decl <- program.decls do
      decl match
        case ModuleDeclAST(path) =>
          currentModule = Some(path.mkString("_"))
        case _ =>

    // Stage F.5 — record locally-defined types for the orphan rule. Done eagerly so the
    // impl-registration pass can consult these maps without ordering surprises.
    val curMod = currentModule.getOrElse("")
    if curMod.nonEmpty then
      for decl <- program.decls do
        decl match
          case StructDeclAST(name, _, _, _, _, _, _)        => typeDefiningModule(name) = curMod
          case DataEnumDeclAST(name, _, _, _, _, _)         => typeDefiningModule(name) = curMod
          case EnumDeclAST(name, _, _)                => typeDefiningModule(name) = curMod
          case InterfaceDeclAST(name, _, _, _)        => typeDefiningModule(name) = curMod
          case TypeAliasDeclAST(name, _, _, _, _, _, _, _, _) => typeDefiningModule(name) = curMod
          case TraitDeclAST(name, _, _, _, _, _, _)      => traitDefiningModule(name) = curMod
          case _ => ()

    // Pass 0.5: resolve struct and data-enum FIELDS before any function signature.
    // Functions declared before their referenced structs/enums would otherwise capture
    // a placeholder with empty fields (sizeOf = 0), breaking backends that read typ.sizeOf
    // on a TCall's return-typed temp. Two iterations: first pass resolves each declaration's
    // fields in source order (forward struct-to-struct refs still see placeholders); second
    // pass re-resolves so captured references inside struct fields point to fully-filled types.
    // Pre-register simple (no-payload) enums so data-enum variant fields and
    // struct fields can reference them during the resolveStructsAndEnums passes
    // below. The main declaration pass (~line 782) repopulates simpleEnumTypes
    // with the same value plus the variant→enum bindings; this just gets the
    // type known to `resolveType` before any field resolution runs.
    //
    // Same logic for generic type aliases: a struct field whose declared type
    // is `Alias[T]` would otherwise hit "'Alias' is not a generic type"
    // because the alias is only registered in `genericTypeAliases` during the
    // main first pass. The main pass re-registers idempotently; this just
    // gets the alias visible to `resolveType` during struct/enum field
    // resolution. The duplicate-check in the main pass tolerates the
    // pre-seeded entry (see ~line 2197).
    for decl <- program.decls do
      decl match
        case EnumDeclAST(name, members, _) =>
          if !simpleEnumTypes.contains(name) then
            val variants = members.map((vname, _) => (vname, Nil: List[(String, SyslType)]))
            simpleEnumTypes(name) = SyslType.EnumType(name, variants)
        case TypeAliasDeclAST(name, target, tparams, _, isNew, _, _, defs, bounds) if tparams.nonEmpty =>
          if !genericTypeAliases.contains(name) && !typeAliases.contains(name) then
            genericTypeAliases(name) = (tparams, target, isNew)
            if defs.nonEmpty then genericTypeAliasDefaults(name) = defs
            if bounds.nonEmpty then genericTypeAliasBounds(name) = bounds
        // Pre-seed interface placeholders so a struct field declared as
        // `field: SomeInterface` resolves during `resolveStructsAndEnums`
        // below. The main declaration pass repopulates with the real method
        // list; the duplicate-check there tolerates a placeholder entry.
        case InterfaceDeclAST(name, _, _, _) =>
          if !interfaceTypes.contains(name) then
            interfaceTypes(name) = SyslType.InterfaceType(name, Nil)
        case _ => ()

    def resolveStructsAndEnums(): Unit =
      for decl <- program.decls do
        decl match
          case StructDeclAST(name, fields, typeParams, _, invariants, _, _) if typeParams.isEmpty =>
            if !genericStructs.contains(name) then
              val resolvedFields = fields.map((n, t, _) => (n, resolveType(t)))
              val volSet = fields.zipWithIndex.collect { case ((_, _, true), i) => i }.toSet
              structTypes(name) = SyslType.StructType(name, resolvedFields, volSet)
              if invariants.nonEmpty then structInvariants(name) = invariants
          case DataEnumDeclAST(name, variants, typeParams, _, _, _) if typeParams.isEmpty =>
            val resolvedVariants = variants.map { case EnumVariantAST(vname, fields) =>
              val resolvedFields = fields.map { (fname, ftype) =>
                val resolved = resolveType(ftype)
                // Auto-wrap directly-recursive variant fields as &Self. Without
                // this the enum's layout is recursively-infinite and backends
                // miscompile any walk past depth 1. Idempotent: an explicit
                // `&Self` is already RefType and is left alone. Containers of
                // Self (slice, raw pointer) and mutual recursion are out of
                // scope — only the direct `field: Self` shape is rewritten.
                val autoWrapped = resolved match
                  case SyslType.EnumType(en, _) if en == name =>
                    SyslType.RefType(resolved)
                  case _ => resolved
                (fname, autoWrapped)
              }
              (vname, resolvedFields)
            }
            val et: SyslType.EnumType = SyslType.EnumType(name, resolvedVariants)
            dataEnumTypes(name) = et
            for ((vname, _), idx) <- resolvedVariants.zipWithIndex do
              variantToEnum(vname) = (et, idx)
          case _ => ()
    resolveStructsAndEnums()
    resolveStructsAndEnums() // second pass: fix forward struct-to-struct refs in field types

    // First pass: register all functions and globals
    for decl <- program.decls do
      decl match
        case _: ModuleDeclAST => // metadata only
        case _: ImportDeclAST => // handled later
        case ExternFuncDeclAST(name, params, returnType, _) =>
          if !functions.contains(name) && !builtinFunctions.contains(name) then
            val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
            val retType = returnType.map(resolveType).getOrElse(UnitType)
            functions(name) = FunInfo(name, paramTypes, retType)
            externalSymbols += name
          // else: already registered from same-module sibling or import — skip
        case ExternVarDeclAST(name, typ, _) =>
          if !globalScope.contains(name) then
            val resolved = resolveType(typ)
            globalScope(name) = SymInfo(name, resolved, mutable = false)
            externalSymbols += name
          // else: already registered from same-module sibling or import — skip
        case sd @ StructDeclAST(name, fields, typeParams, _, invariants, _, _) =>
          if typeParams.nonEmpty then
            // Generic struct: store as template, don't resolve fields yet
            if genericStructs.contains(name) then
              throw AnalysisError(s"duplicate struct: '$name'", decl)
            genericStructs(name) = sd
          else
            if genericStructs.contains(name) then throw AnalysisError(s"duplicate struct: '$name'", decl)
            // Fields already resolved by pass 0.5 (resolveStructsAndEnums).
            // Do not re-assign structTypes here — that would invalidate references captured
            // by function signatures processed later in this same source-order loop.
        case fd @ FunDeclAST(name, params, returnType, _, _, typeParams, _, _, isDef, _, _) =>
          // Duplicate-parameter-name check.
          val seenParams = mutable.HashSet[String]()
          for p <- params do
            if !seenParams.add(p.name) then
              val friendly = if p.name == "__self__"
                then "method '$name' already has an implicit 'self' parameter — remove the explicit 'self: *Type' declaration"
                else s"duplicate parameter name '${p.name}' in function '$name'"
              throw AnalysisError(friendly, decl)
          // Validate that any parameters with defaults come at the end (contiguous trailing).
          val firstDefaultIdx = params.indexWhere(_.default.isDefined)
          if firstDefaultIdx >= 0 then
            for i <- (firstDefaultIdx + 1) until params.length do
              if params(i).default.isEmpty then
                throw AnalysisError(
                  s"parameter '${params(i).name}' of '$name' must have a default value (all parameters after a defaulted parameter must also have defaults)",
                  decl,
                )
          // Validate mode constraints: no defaults on out/inout; no out/inout on self.
          for p <- params do
            if p.mode != ParamMode.In && p.default.isDefined then
              throw AnalysisError(s"parameter '${p.name}' of '$name' is ${p.mode.toString.toLowerCase} and cannot have a default value", decl)
            if p.mode != ParamMode.In && p.name == "__self__" then
              throw AnalysisError(s"method receiver 'self' of '$name' cannot be '${p.mode.toString.toLowerCase}'", decl)
          if typeParams.nonEmpty then
            // Generic function: store as template, don't resolve types yet
            if genericTemplates.contains(name) || functions.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            if params.exists(_.default.isDefined) then
              throw AnalysisError(s"generic function '$name' cannot have default parameter values (not yet supported)", decl)
            if params.exists(_.mode != ParamMode.In) then
              throw AnalysisError(s"generic function '$name' cannot have 'out'/'inout' parameters (not yet supported)", decl)
            genericTemplates(name) = fd
          else
            // For Out/Inout params, the call-side signature carries `*T` (hidden pointer);
            // the body sees the inner `T` with autoIndirect via SymInfo. Store modes parallel
            // to params so checkArgs and SMETA can consult them.
            // For `=> T` (call-by-name) params, the storage type is `() -> T`. Mode
            // must be `In` — by-name is incompatible with out/inout (the thunk has
            // no lvalue to write back to). Track by-name positions in `byNameFlags`
            // so the call-site auto-wrap and body auto-eval paths can find them.
            val byNameFlags = params.map(_.typ.isInstanceOf[ByNameTypeAST])
            val anyByName = byNameFlags.exists(identity)
            for (p <- params) do
              if p.typ.isInstanceOf[ByNameTypeAST] then
                if p.mode != ParamMode.In then
                  throw AnalysisError(s"parameter '${p.name}' of '$name' is by-name (`=> T`) and cannot also be '${p.mode.toString.toLowerCase}'", decl)
                if p.default.isDefined then
                  throw AnalysisError(s"parameter '${p.name}' of '$name' is by-name (`=> T`) and cannot have a default value", decl)
            val paramTypes = params.map { p =>
              val (effectiveAst, isByName) = p.typ match
                case ByNameTypeAST(inner) => (FuncTypeAST(Nil, inner), true)
                case other => (other, false)
              val inner = resolveType(effectiveAst)
              val sigType = p.mode match
                case ParamMode.In => inner
                case ParamMode.Out | ParamMode.Inout => PtrType(inner)
              (p.name, sigType)
            }
            val paramModes = params.map(_.mode)
            val retType = returnType.map(resolveType).getOrElse(UnitType)
            if functions.contains(name) || genericTemplates.contains(name) then
              throw AnalysisError(s"duplicate function: '$name'", decl)
            val mangledName = if shouldMangle(name) then mangleName(name) else name
            val isPureAttr = fd.attributes.exists(_.name == "pure")
            val isRealtimeAttr = fd.attributes.exists(_.name == "realtime")
            val isGhost = fd.attributes.exists(_.name == "ghost")
            // Extract `#reads(a, b)` / `#writes(c)` raw identifier lists. Validation that
            // each name resolves to a module-level mutable var is deferred to validateEffects
            // (run after the body is analyzed, so all relevant globals are in scope).
            // Each attribute may appear multiple times; results are unioned. `#pure` cannot
            // be combined with explicit `#reads`/`#writes` (it already implies both empty).
            def extractIdentList(attrName: String): Option[Set[String]] =
              val matching = fd.attributes.filter(_.name == attrName)
              if matching.isEmpty then None
              else Some(matching.flatMap { attr =>
                attr.args.map {
                  case AttrPositional(AttrLitIdent(n)) => n
                  case other => throw AnalysisError(s"#$attrName on '$name' expects identifier arguments, got $other", fd)
                }
              }.toSet)
            val readsSet = extractIdentList("reads")
            val writesSet = extractIdentList("writes")
            // Expression functions (`def ...`) are implicitly `#pure` — they exist to serve
            // as proof-friendly abstraction predicates, so side effects and global mutation
            // are never appropriate. `#ghost def` stays on the ghost track (ghost is already
            // restricted and is stripped before codegen).
            if isDef && (readsSet.isDefined || writesSet.isDefined) then
              throw AnalysisError(s"'def $name' cannot carry #reads/#writes — def functions are implicitly pure", fd)
            val isPure = isPureAttr || (isDef && !isGhost)
            if isPureAttr && (readsSet.isDefined || writesSet.isDefined) then
              throw AnalysisError(s"#pure on '$name' cannot be combined with #reads/#writes (it already implies both empty)", fd)
            if isGhost && isPureAttr then
              throw AnalysisError(s"#ghost on '$name' is incompatible with #pure (ghost code is removed before codegen, so #pure is meaningless)", fd)
            if isGhost && (readsSet.isDefined || writesSet.isDefined) then
              throw AnalysisError(s"#ghost on '$name' is incompatible with #reads/#writes (ghost code is removed before codegen)", fd)
            if isGhost && isRealtimeAttr then
              throw AnalysisError(s"#ghost on '$name' is incompatible with #realtime (ghost code is removed before codegen, so #realtime is meaningless)", fd)
            // Collision check: a parameterless decl and a zero-arg decl with the
            // same name are ambiguous at the call site (`foo` could mean either),
            // so reject. (Two zero-arg or two parameterless decls with the same
            // name are caught by the regular duplicate-function check above.)
            if fd.isParameterless && fd.typeParams.nonEmpty then
              throw AnalysisError(s"parameterless function '$name' cannot be generic", decl)
            functions(name) = FunInfo(mangledName, paramTypes, retType, isDef && params.isEmpty, isPure, paramModes, readsSet, writesSet, isGhost, if anyByName then byNameFlags else Nil, fd.isParameterless, isRealtimeAttr)
            // Record #deprecated info. The lexer's StringLit carrier is
            // byte-form (each Char is one UTF-8 byte); decode for the host
            // diagnostic so Unicode reasons render correctly when printed.
            for attr <- fd.attributes if attr.name == "deprecated" do
              val reason = attr.args.collectFirst { case AttrPositional(AttrLitString(s)) =>
                new String(s.getBytes("ISO-8859-1"), "UTF-8")
              }
              deprecations(name) = reason
            // Register as method if name matches StructName_methodName pattern
            val underscoreIdx = name.indexOf('_')
            if underscoreIdx > 0 && params.nonEmpty && params.head.name == "__self__" then
              val structName = name.substring(0, underscoreIdx)
              val methodName = name.substring(underscoreIdx + 1)
              if structTypes.contains(structName) then
                methods.getOrElseUpdate(structName, mutable.Set.empty) += methodName
        case EnumDeclAST(name, members, _) =>
          if enumTypes.contains(name) then throw AnalysisError(s"duplicate enum: '$name'", decl)
          var nextValue = 0L
          val resolved = members.map { (memberName, explicitValue) =>
            val value = explicitValue.getOrElse(nextValue)
            nextValue = value + 1
            (memberName, value)
          }
          enumTypes(name) = resolved.toMap
          // Also register as an EnumType so it can appear in type positions
          // (e.g. `Result[int, TestError]`). Variants carry no payload.
          val variants = resolved.map((vname, _) => (vname, Nil: List[(String, SyslType)]))
          val et: SyslType.EnumType = SyslType.EnumType(name, variants)
          simpleEnumTypes(name) = et
          // Register bare variant names as constructors, so `NotFound` produces
          // a TEnumConstruct value usable where `TestError` is expected.
          // `TestError.NotFound` (qualified access) still yields the integer
          // constant via the enumTypes path for backward compatibility.
          for ((vname, _), idx) <- variants.zipWithIndex do
            if variantToEnum.contains(vname) || genericVariantToEnum.contains(vname) then
              throw AnalysisError(s"duplicate variant name: '$vname'")
            variantToEnum(vname) = (et, idx)
        case de @ DataEnumDeclAST(name, variants, typeParams, _, _, _) =>
          if typeParams.nonEmpty then
            // Generic enum: store template, don't resolve fields
            if genericEnums.contains(name) || dataEnumTypes.contains(name) || enumTypes.contains(name) then
              throw AnalysisError(s"duplicate enum: '$name'", decl)
            genericEnums(name) = de
            // Register bare variant names for inference at construction sites
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              genericVariantToEnum.get(vname) match
                case Some((n, i)) if n == name && i == idx => () // already from registerGenericTemplatesFrom(sibling)
                case Some((n, i)) =>
                  throw AnalysisError(s"duplicate variant name: '$vname' ($n#$i vs '$name'#$idx)", decl)
                case None =>
                  if variantToEnum.contains(vname) then variantToEnum.remove(vname)
                  genericVariantToEnum(vname) = (name, idx)
          else
            if enumTypes.contains(name) || genericEnums.contains(name) then
              throw AnalysisError(s"duplicate enum: '$name'", decl)
            // Variants already resolved by pass 0.5 (resolveStructsAndEnums).
            // Do not re-assign dataEnumTypes here — same reason as StructDeclAST above.
        case TypeAliasDeclAST(name, target, tparams, _, isNew, range, predicate, defaults, bounds) =>
          // The struct-field pre-pass (~line 1976) seeds generic type aliases
          // into `genericTypeAliases` so struct fields can name them. When the
          // main pass revisits the SAME decl, skip the duplicate diagnostic so
          // the pre-seeded entry doesn't trip it.
          val preSeededGeneric = tparams.nonEmpty && genericTypeAliases.get(name)
            .exists(_ == ((tparams, target, isNew)))
          if !preSeededGeneric && (typeAliases.contains(name) || genericTypeAliases.contains(name)) then
            throw AnalysisError(s"duplicate type alias: '$name'", decl)
          if tparams.nonEmpty then
            // `within` and `where` need scalar ordering / operations on T; both are
            // unavailable on a bare type parameter without trait bounds. `new` does NOT
            // need either — it's just nominal identity per instantiation, fully supported
            // by the existing monomorphization machinery.
            if range.nonEmpty || predicate.nonEmpty then
              throw AnalysisError(s"generic type aliases cannot use 'within' or 'where': '$name'", decl)
            genericTypeAliases(name) = (tparams, target, isNew)
            if defaults.nonEmpty then genericTypeAliasDefaults(name) = defaults
            if bounds.nonEmpty then genericTypeAliasBounds(name) = bounds
          else
            typeAliases(name) = (target, isNew, range, predicate)
        case TraitDeclAST(name, tparams, methods, _, assocs, _, tBounds) =>
          // Tolerate sibling-pre-registered traits (importedTraitNames) — those
          // came from this same trait decl via cross-sibling forward-decl pass
          // and are structurally identical. Without this, two-file modules
          // where each sibling pre-registers the other's traits throw a
          // spurious duplicate when the defining file's main pass reaches the
          // decl. A real same-file duplicate (`importedTraitNames` doesn't
          // contain it) is still rejected.
          if traits.contains(name) && !importedTraitNames.contains(name) then
            throw AnalysisError(s"duplicate trait: '$name'", decl)
          if !traits.contains(name) then
            // Check no duplicate method names within the trait
            val methodNames = methods.map(_.name)
            if methodNames.distinct.length != methodNames.length then
              throw AnalysisError(s"duplicate method names in trait '$name'")
            if tparams.distinct.length != tparams.length then
              throw AnalysisError(s"duplicate type parameter names in trait '$name'")
            // Validate associated-type declarations: unique names, no clash with
            // type parameters, and no clash with the reserved `T::Attr` names used
            // for enum / within-int introspection (First, Last, Range, Image,
            // Value, Valid, Pos, Val, Succ, Pred). Until Phase A3 wires up
            // projection resolution, bounds are accepted but not enforced.
            val assocNames = assocs.map(_.name)
            if assocNames.distinct.length != assocNames.length then
              throw AnalysisError(s"duplicate associated-type names in trait '$name'")
            for a <- assocs do
              if tparams.contains(a.name) then
                throw AnalysisError(s"associated type '${a.name}' shadows trait type parameter in '$name'", a)
              if methodNames.contains(a.name) then
                throw AnalysisError(s"associated type '${a.name}' shadows method name in trait '$name'", a)
              if SyslAnalyzer.ReservedTypeAttrNames.contains(a.name) then
                throw AnalysisError(s"associated type '${a.name}' uses a reserved attribute name in trait '$name'; rename to avoid clashing with the built-in T::${a.name} introspection attribute", a)
            // Validate that every name on the LHS of typeBounds is one of the
            // declared type parameters; flag-but-don't-fail if a bound names an
            // unknown trait (resolution happens at impl registration to avoid
            // ordering pitfalls between trait + impl decls).
            for (bn, _) <- tBounds do
              if !tparams.contains(bn) then
                throw AnalysisError(s"trait '$name' declares bound on unknown type parameter '$bn'", decl)
            traits(name) = TraitInfo(name, tparams, methods, assocs, tBounds)
            registerTraitOperatorEntries(name, methods, decl)
          else
            // Already registered (sibling pre-collect); the trait now owns
            // this unit (this is the defining unit's main pass) so clear the
            // imported flag so getTraitDecls includes it in this unit's
            // perFileMeta.
            importedTraitNames -= name
        case InterfaceDeclAST(name, methodASTs, embeddedNames, _) =>
          // Tolerate a placeholder pre-seeded by pass 0.5 (empty methods list)
          // — pass 0.5 pre-registers interface names so struct fields typed by
          // them resolve before this pass runs. A real duplicate (non-empty
          // methods list) still throws.
          val preSeededInterface = interfaceTypes.get(name).exists(_.methods.isEmpty)
          if interfaceTypes.contains(name) && !preSeededInterface then throw AnalysisError(s"duplicate interface: '$name'", decl)
          // Resolve embedded interfaces and flatten methods
          val embeddedMethods = embeddedNames.flatMap { en =>
            interfaceTypes.getOrElse(en, throw AnalysisError(s"embedded interface '$en' not found", decl)).methods
          }
          val ownMethods = methodASTs.map { m =>
            val paramTypes = m.params.map(p => resolveType(p.typ))
            val retType = resolveType(m.returnType)
            // Resolve raw names in #reads/#writes through globalScope so subset checks at
            // boxing/dispatch sites compare mangled names directly.
            val resolvedEff = if m.effects.isPure || (m.effects.reads.isEmpty && m.effects.writes.isEmpty) then m.effects
              else
                def resolveOne(n: String, kind: String): String =
                  globalScope.get(n) match
                    case Some(sym) if sym.mutable && !sym.isConst => sym.name
                    case Some(_) => throw AnalysisError(s"#$kind on interface '$name' method '${m.name}' references '$n' which is not mutable")
                    case None    => throw AnalysisError(s"#$kind on interface '$name' method '${m.name}' references unknown global '$n'")
                FuncEffects(m.effects.isPure, m.effects.reads.map(_.map(resolveOne(_, "reads"))), m.effects.writes.map(_.map(resolveOne(_, "writes"))), m.effects.isRealtime)
            (m.name, paramTypes, retType, resolvedEff)
          }
          val allMethods = embeddedMethods ++ ownMethods
          // Check for duplicate method names
          val names = allMethods.map(_._1)
          if names.distinct.length != names.length then
            throw AnalysisError(s"duplicate method names in interface '$name'")
          interfaceTypes(name) = SyslType.InterfaceType(name, allMethods)
        case _: ImplDeclAST =>
          // Deferred to registerImpls after all traits are known
          ()
        case VarDeclAST(name, _, _, _, isMutable, attrs, _, isConst) =>
          if globalScope.contains(name) then
            throw AnalysisError(s"duplicate global: '$name'", decl)
          // Pre-register the name so effect signatures on same-unit interface methods /
          // function types / callbacks can resolve it during Pass 1. The real SymInfo
          // (with correct type) is written by `analyzeRegularVarDecl` in Pass 2 and
          // overwrites this placeholder. Fields used by effect-name resolution (`mutable`
          // and `isConst`) are set correctly from the AST here.
          val mangled = if shouldMangle(name) then mangleName(name) else name
          val isGhost = attrs.exists(_.name == "ghost")
          globalScope(name) = SymInfo(mangled, SyslType.UnitType, isMutable, isConst = isConst, isGhost = isGhost)
        case _: StaticAssertDeclAST => // evaluated in pass 2
        case _ => // other decls (e.g. CondDeclAST) handled elsewhere

    // Pre-pass: evaluate module-level `const` initializers eagerly so they are available
    // to `within` range bounds and other contexts that resolve types before function bodies.
    for decl <- program.decls do
      decl match
        case VarDeclAST(name, typOpt, init, _, _, _, _, true) =>
          val declType = typOpt.map(resolveType).getOrElse(I32)
          if !declType.isIntegral then
            throw AnalysisError(s"const '$name' must have an integer type (found $declType); float/string/aggregate const is not yet supported", decl)
          evalConstExprAST(init) match
            case Some(v) =>
              val masked = maskToType(v, declType)
              compileTimeConstants(name) = masked
              val mangled = if shouldMangle(name) then mangleName(name) else name
              compileTimeConstants(mangled) = masked
              // Also publish the const in globalScope so name-based lookups during this pass
              // (e.g. struct invariant validation) can find it.
              if !globalScope.contains(name) then
                globalScope(name) = SymInfo(mangled, declType, mutable = false, isConst = true)
            case None =>
              throw AnalysisError(s"const '$name' initializer is not compile-time evaluable", decl)
        case _ =>

    // Validate all struct invariants now that constants are registered — catches wrong field
    // names, unknown identifiers, and non-bool result types before any mutation site sees them.
    for (structName, invariants) <- structInvariants do
      val st = structTypes(structName)
      validateStructInvariants(structName, st.fields, invariants)

    // Re-register sibling forward-decls now that own types/aliases are in
    // scope. Sibling concrete impls that reference own generic aliases
    // (e.g. operators.lsysl declares `impl Peek[string, Parser[unit]]`
    // where Parser is in parsyl.lsysl) failed to resolve in the driver's
    // pre-pass; with own pass 1 done, they can now register. Idempotent —
    // skip if already registered.
    // Drop ONLY the stale impl entries that were registered by the sibling
    // pre-register against type-placeholders (tracked via the stub-functions
    // set). The post-hook re-registers them with real types now that own
    // pass 1 has filled in struct fields. Cross-module imports (which use
    // the proper symbol path) leave importedConcreteImplStubFunctions empty
    // for their entries — they're untouched here.
    val staleStubs = importedConcreteImplStubFunctions.toList
    if staleStubs.nonEmpty then
      // Locate impl entries whose mangled methods are stale stubs and drop them.
      for (traitName, buf) <- implTemplates do
        buf.filterInPlace(t =>
          t.typeParams.nonEmpty || !t.methods.values.exists(staleStubs.contains))
      // Also drop the stale FunInfo stubs and their imported-key entries.
      for fn <- staleStubs do
        functions.remove(fn)
        externalSymbols -= fn
      importedConcreteImplStubFunctions.clear()
      // Drop concrete impl keys whose mangled name was a stale stub (others
      // — proper cross-module imports — keep their entries).
      val stubSet = staleStubs.toSet
      importedConcreteImplKeys.filterInPlace { case (traitName, _) =>
        // Conservative: drop a key only if the corresponding impl is gone.
        // Re-registration adds a fresh key.
        implTemplates.getOrElse(traitName, Nil).exists(_.methods.values.exists(stubSet.contains))
      }
    // Same cleanup for free-fn stubs: drop so the re-pre re-registers with
    // real types in scope (sibling pre-register may have latched onto
    // placeholder struct types in the first pass).
    val staleFreeFnStubs = importedSiblingFreeFnStubKeys.toList
    if staleFreeFnStubs.nonEmpty then
      for k <- staleFreeFnStubs do
        functions.remove(k)
        externalSymbols -= k
      importedSiblingFreeFnStubKeys.clear()
    for sib <- siblingForwardDecls do
      registerSiblingForwardDeclsFrom(sib)

    // Intermediate pass: register impl blocks (traits now known; signatures may reference traits)
    for decl <- program.decls do
      decl match
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, assocBindings, _, implTypeBounds) =>
          val trait_ = traits.getOrElse(traitName,
            throw AnalysisError(s"impl references unknown trait '$traitName'", decl))
          if targetTypes.length != trait_.typeParams.length then
            throw AnalysisError(s"impl of '$traitName' has ${targetTypes.length} target type(s) but the trait has ${trait_.typeParams.length} type parameter(s)", decl)
          // Check required methods are all provided
          val providedNames = methods.map(_.name).toSet
          val missing = trait_.methods.filter(m => m.body.isEmpty && !providedNames.contains(m.name))
          if missing.nonEmpty then
            throw AnalysisError(s"impl ${traitName}[${targetTypes.mkString(", ")}] missing required method(s): ${missing.map(_.name).mkString(", ")}")
          // Check each impl method exists in trait
          for m <- methods do
            if !trait_.methods.exists(_.name == m.name) then
              throw AnalysisError(s"impl method '${m.name}' is not declared in trait '$traitName'")
          // Validate associated-type bindings (Phase A1):
          //   - every assoc type the trait declared must be bound;
          //   - every binding must name a declared assoc;
          //   - no duplicate binding names.
          val bindingNames = assocBindings.map(_.name)
          if bindingNames.distinct.length != bindingNames.length then
            throw AnalysisError(s"duplicate associated-type binding(s) in impl of '$traitName'", decl)
          val declaredAssocs = trait_.assocTypes.map(_.name).toSet
          for b <- assocBindings do
            if !declaredAssocs.contains(b.name) then
              throw AnalysisError(s"impl of '$traitName' binds undeclared associated type 'type ${b.name}'", b)
          val missingAssocs = trait_.assocTypes.map(_.name).filterNot(bindingNames.contains)
          if missingAssocs.nonEmpty then
            throw AnalysisError(s"impl of '$traitName' missing associated-type binding(s): ${missingAssocs.map("type " + _).mkString(", ")}", decl)
          // Phase C — enforce trait bounds declared on associated types.
          // For each assoc binding, look up the trait's declaration of that
          // assoc; for each declared bound trait, validate that the binding
          // target satisfies the bound (an impl exists). For now we enforce
          // this only on concrete impls; generic impls would need
          // instantiation-time deferral (their binding targets may reference
          // impl tvars) and are tracked as Phase C.2.
          if implTypeParams.isEmpty then
            val assocBoundsByName = trait_.assocTypes.map(a => (a.name, a.bounds)).toMap
            for b <- assocBindings do
              val bounds = assocBoundsByName.getOrElse(b.name, Nil)
              if bounds.nonEmpty then
                val resolvedBindingTarget = resolveType(b.target)
                for boundTrait <- bounds do
                  if !traits.contains(boundTrait) then
                    throw AnalysisError(s"associated type 'type ${b.name}' on trait '$traitName' references unknown trait '$boundTrait'", b)
                  val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                    if t.typeParams.isEmpty then
                      t.resolvedConcrete.flatMap(_.headOption).contains(resolvedBindingTarget)
                    else
                      tryUnifyAll(t.targetPatterns.headOption.toList, List(resolvedBindingTarget), t.typeParams.toSet).isDefined
                  }
                  if !matched then
                    throw AnalysisError(s"impl of '$traitName' binds 'type ${b.name} = $resolvedBindingTarget', which does not satisfy bound '$boundTrait' declared on the associated type", b)
          // Phase C follow-up — enforce trait bounds declared on the trait's
          // own type parameters (e.g. `trait Container[T: Ord]`). For each
          // bounded trait tparam, the corresponding impl target must satisfy
          // each bound trait. Concrete impls check their resolved targets
          // directly; generic impls defer to `instantiateImpl` because a
          // target pattern may reference impl tvars whose substitution isn't
          // known until dispatch.
          //
          // Validate that each bound names a known trait up-front, regardless
          // of impl-tparam-ness, so a typo on `trait Container[T: Ord]` fails
          // at the first impl rather than at every dispatch site.
          for (tp, tBounds) <- trait_.typeBounds do
            for boundTrait <- tBounds do
              if !traits.contains(boundTrait) then
                throw AnalysisError(s"bound '$boundTrait' on trait '$traitName' type parameter '$tp' refers to unknown trait", decl)
          if implTypeParams.isEmpty then
            val resolvedTargetsForBoundCheck = targetTypes.map(resolveType)
            for ((tp, tgt) <- trait_.typeParams.zip(resolvedTargetsForBoundCheck)) do
              val tBounds = trait_.typeBounds.getOrElse(tp, Nil)
              for boundTrait <- tBounds do
                val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                  if t.typeParams.isEmpty then
                    t.resolvedConcrete.flatMap(_.headOption).contains(tgt)
                  else
                    tryUnifyAll(t.targetPatterns.headOption.toList, List(tgt), t.typeParams.toSet).isDefined
                }
                if !matched then
                  throw AnalysisError(s"impl of '$traitName' for $tgt does not satisfy bound '$boundTrait' declared on type parameter '$tp'", decl)
          // Validate that any impl-level type-bound LHS names a declared impl
          // tparam, and that each bound names a known trait. Defer the
          // dispatch-time check itself to `instantiateImpl`.
          for (bn, bs) <- implTypeBounds do
            if !implTypeParams.contains(bn) then
              throw AnalysisError(s"impl declares bound on unknown type parameter '$bn'", decl)
            for bt <- bs do
              if !traits.contains(bt) then
                throw AnalysisError(s"bound '$bt' on impl type parameter '$bn' refers to unknown trait", decl)
          // Built-in prefix-sigil collision: when a trait method carries
          // `#operator(<sigil>)` for one of `-`, `!`, `~`, `*`, `&`, the impl's
          // first target pattern must NOT cover the sigil's natural built-in
          // domain (e.g. `impl Neg[bool]` for `#operator("!")` would steal
          // `!true`). Generic patterns (`Parser[A]`) and nominal aliases
          // (`type Meters = new int`) read as user-defined and pass freely;
          // only direct references to built-in scalar / pointer shapes fail.
          if targetTypes.nonEmpty then
            for sigilTraitMethod <- trait_.methods do
              val opAttrs = sigilTraitMethod.attributes.filter(a => a.name == "operator" || a.name == "op")
              for attr <- opAttrs.headOption do
                val sigil = extractOperatorSymbol(attr, sigilTraitMethod)
                if sigilTraitMethod.params.length == 1 && implOperandConflictsWithBuiltinPrefix(sigil, targetTypes.head) then
                  throw AnalysisError(
                    s"impl of '$traitName' for ${targetTypes.head} conflicts with built-in prefix '$sigil' on its natural type; pick a different operand type",
                    decl,
                  )
          if implTypeParams.nonEmpty then
            // Generic impl: defer signature checking + mangling to specialization time.
            // Every declared impl tvar must appear in at least one target pattern,
            // otherwise it's never bindable at dispatch time and the impl can't match
            // anything — fail fast with a clear diagnostic.
            val patternTvars = targetTypes.flatMap(collectNamedTypeNames).toSet
            val unused = implTypeParams.filterNot(patternTvars.contains)
            if unused.nonEmpty then
              throw AnalysisError(s"impl of '$traitName' declares unused type parameter(s) ${unused.mkString(", ")} (must appear in target patterns)", decl)
            // Coherence check (Stage F.5) walks here before we add to implTemplates.
            val newTemplate = ImplTemplate(
              typeParams = implTypeParams,
              targetPatterns = targetTypes,
              resolvedConcrete = None,
              methods = mutable.LinkedHashMap.empty,
              methodInfos = Nil,
              methodASTs = methods,
              definingModule = currentModule.getOrElse(""),
              implDecl = Some(impl),
              assocBindings = assocBindings,
              typeBounds = implTypeBounds,
            )
            checkOrphanRule(traitName, targetTypes, currentModule.getOrElse(""), decl)
            checkCoherence(traitName, newTemplate, decl)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += newTemplate
          else
            // Concrete impl: resolve targets, register one mangled function per trait method.
            val resolvedTargets = targetTypes.map(resolveType)
            if findConcreteImpl(traitName, resolvedTargets.head).exists(t =>
                t.resolvedConcrete.contains(resolvedTargets)) then
              throw AnalysisError(s"duplicate impl: trait '$traitName' already implemented for ${resolvedTargets.mkString(", ")}", decl)
            // Mangle suffix: join all resolved targets with `_` so multi-param impls don't collide
            val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
            val methodMap = mutable.LinkedHashMap.empty[String, String]
            val infos = mutable.ListBuffer.empty[ImplMethodInfo]
            val savedEnv = typeEnv
            val savedAssocs = assocBindingsEnv
            typeEnv = trait_.typeParams.zip(resolvedTargets).toMap
            // Resolve impl's assoc bindings using the trait-param-substituted
            // typeEnv so a binding like `type Item = T` (referring to a generic
            // impl tvar) lands as the substituted concrete type. For non-generic
            // impls, this is just direct resolution.
            assocBindingsEnv = assocBindings.map(b => (b.name, resolveType(b.target))).toMap
            try
              for traitMethod <- trait_.methods do
                val rawMangled = s"${traitName}_${traitMethod.name}_${typeMangled}"
                val mangled = if shouldMangle(rawMangled) then mangleName(rawMangled) else rawMangled
                if functions.contains(mangled) then
                  throw AnalysisError(s"impl method collides with existing function '$mangled'", decl)
                val expectedParams = traitMethod.params.map(p => (p.name, resolveType(p.typ)))
                val expectedRet = resolveType(traitMethod.returnType)
                val providedOpt = methods.find(_.name == traitMethod.name)
                val (paramTypes, retType, body, synthesized) = providedOpt match
                  case Some(implMethod) =>
                    val pTypes = implMethod.params.map(p => (p.name, resolveType(p.typ)))
                    val r = implMethod.returnType.map(resolveType).getOrElse(UnitType)
                    if pTypes.map(_._2) != expectedParams.map(_._2) then
                      throw AnalysisError(s"impl method '${implMethod.name}' parameter types don't match trait: expected ${expectedParams.map(_._2).mkString("(", ", ", ")")}, got ${pTypes.map(_._2).mkString("(", ", ", ")")}", decl)
                    if r != expectedRet then
                      throw AnalysisError(s"impl method '${implMethod.name}' return type doesn't match trait: expected $expectedRet, got $r", decl)
                    (pTypes, r, implMethod.body, false)
                  case None =>
                    (expectedParams, expectedRet, traitMethod.body.get, true)
                functions(mangled) = FunInfo(mangled, paramTypes, retType)
                methodMap(traitMethod.name) = mangled
                infos += ImplMethodInfo(mangled, paramTypes, retType, body, isSynthesized = synthesized)
            finally
              typeEnv = savedEnv
              assocBindingsEnv = savedAssocs
            val newTemplate = ImplTemplate(
              typeParams = Nil,
              targetPatterns = targetTypes,
              resolvedConcrete = Some(resolvedTargets),
              methods = methodMap,
              methodInfos = infos.toList,
              methodASTs = Nil,
              definingModule = currentModule.getOrElse(""),
              implDecl = Some(impl),
              assocBindings = assocBindings,
              typeBounds = implTypeBounds,
            )
            checkOrphanRule(traitName, targetTypes, currentModule.getOrElse(""), decl)
            checkCoherence(traitName, newTemplate, decl)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += newTemplate
        case _ =>

    // Re-resolve struct/enum fields one more time so any field captured at
    // pass 0.5 against a placeholder interface (empty methods) picks up the
    // now-fully-populated InterfaceType. Without this, `struct S { f: I }`
    // would dispatch through a zero-method `I` and fail with "interface I has
    // no method 'foo'" at the first call site.
    resolveStructsAndEnums()

    // Second pass: produce typed AST (skip generic templates; they're instantiated on demand)
    val tDecls = program.decls.flatMap {
      case f: FunDeclAST if f.typeParams.nonEmpty => Nil
      case s: StructDeclAST if s.typeParams.nonEmpty => Nil
      case e: DataEnumDeclAST if e.typeParams.nonEmpty => Nil
      case _: TraitDeclAST => Nil // traits emit nothing; only impls do
      case _: ExtensionDeclAST => Nil // Phase 1 stub: parsed but not yet analyzed
      case i: InterfaceDeclAST => List(TInterfaceDecl(i.name, interfaceTypes(i.name)))
      case impl: ImplDeclAST   => analyzeImplMethods(impl)
      case sa: StaticAssertDeclAST =>
        evalStaticAssert(sa)
        Nil
      case mi: ModuleInvariantDeclAST =>
        // Spec-only: validate that the expression is a bool reference over module-level
        // state, then drop. The WhyML backend reads the original AST directly. Other
        // backends never see this decl (analyzer returns no TDecl).
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        try
          val tExpr = analyzeExpr(mi.expr)
          if tExpr.typ != BoolType then
            throw AnalysisError(s"module_invariant must be a bool expression, got ${tExpr.typ}")
        finally
          scopeStack = null
        Nil
      case d => List(analyzeDecl(d))
    }
    val allDecls = tDecls ++ specializedDecls.toList
    // Ghost strip pass: remove `#ghost var`/`#ghost fn` declarations from codegen output,
    // and inside every real function body, drop ghost-local decls, ghost-target assignments,
    // and contract checks that reference ghost state. Ghost discipline has already been
    // enforced in analyzeDecl, so here the pass is a pure rewrite with no error checks.
    val strippedDecls = stripGhostDecls(allDecls)
    TProgram(strippedDecls)
