package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerImports:
  self: SyslAnalyzer =>

  def registerImport(meta: ModuleMeta, selectors: List[ImportSelector] = List(WildcardImport), modulePath: String = ""): Unit =
    // Qualified import: import std.strings → access as strings.foo
    selectors match
      case List(QualifiedImport) =>
        val nsName = modulePath.split("/").last
        moduleNamespaces(nsName) = meta
        return
      case List(ExtensionsOnlyImport) =>
        // Predef auto-import: only register extension entries + their `__ext_*`
        // synth functions (and the generic templates that back generic
        // extensions). Skip every other public symbol so the module's regular
        // functions (e.g. `contains` in `std.string`) don't pollute the
        // importing unit's namespace and collide with same-named functions in
        // other modules. Visibility flag for the source module is still set so
        // the dispatcher's `visibleExtensionModules` check passes.
        val extFnShortNames: Set[String] = meta.extensions.map(_.mangledFnName).toSet
        val importedDefMod =
          if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_') else ""
        // 1. Register the synth `__ext_*` functions (concrete-receiver case).
        for sym <- meta.publicSymbols do
          val sn = shortName(sym.name)
          if extFnShortNames.contains(sn) && !functions.contains(sn) then
            sym.typ match
              case SymbolMeta.Kind.Func(params, returnType, isDef, isPure, modes, effects, isParameterless) =>
                val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
                val (reads, writes) = effects match
                  case e if e.isPure || e.isUnknown => (None, None)
                  case e => (e.reads, e.writes)
                functions(sn) = FunInfo(sym.name, paramPairs, returnType, isDef, isPure || effects.isPure, modes, reads, writes, isParameterless = isParameterless, isRealtime = effects.isRealtime, isConst = effects.isConst)
                if reads.isDefined || writes.isDefined then
                  resolvedEffectsCache(sym.name) = (reads.getOrElse(Set.empty), writes.getOrElse(Set.empty))
                externalSymbols += sn
              case _ => ()
        // 2. Register generic-receiver synth templates (`__ext_*` FunDeclAST in
        //    meta.genericTemplates) so cross-module generic extension dispatch
        //    sees them. Filter to ext-only — don't pull in unrelated generics.
        val extOnlyTemplates = meta.genericTemplates.collect {
          case fd: FunDeclAST if fd.name.startsWith("__ext_") => fd: DeclAST
        }
        if extOnlyTemplates.nonEmpty then
          registerGenericTemplatesFrom(ProgramAST(extOnlyTemplates), filter = None)
        // 2b. Register synth trait + generic impl decls for `__ExtOp_*` operator
        //     extensions. The trait must be registered before the impl so the
        //     impl-decl arity check passes; do trait first.
        for template <- meta.genericTemplates do
          template match
            case TraitDeclAST(name, tparams, methods, _, _, _, _) if name.startsWith("__ExtOp_") =>
              if !traits.contains(name) then
                traits(name) = TraitInfo(name, tparams, methods)
                importedTraitNames += name
                registerTraitOperatorEntries(name, methods, template)
            case _ => ()
        for template <- meta.genericTemplates do
          template match
            case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, _, _, _)
                if traitName.startsWith("__ExtOp_") && implTypeParams.nonEmpty =>
              if !implTemplates.getOrElse(traitName, Nil).exists(t =>
                  t.typeParams == implTypeParams && t.targetPatterns == targetTypes) then
                implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                  typeParams = implTypeParams,
                  targetPatterns = targetTypes,
                  resolvedConcrete = None,
                  methods = mutable.LinkedHashMap.empty,
                  methodInfos = Nil,
                  methodASTs = methods,
                  definingModule = "",
                  implDecl = Some(impl),
                )
            case _ => ()
        // 3. Register concrete extension entries from meta.extensions.
        for ext <- meta.extensions do
          val entry = ExtensionEntry(
            methodName = ext.methodName,
            receiverTypeAst = syslTypeToAST(ext.receiverType),
            mangledFnName = ext.mangledFnName,
            definingModule = ext.definingModule,
          )
          val bucket = extensionsByMethod.getOrElseUpdate(ext.methodName, mutable.ListBuffer.empty)
          if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
            bucket += entry
          importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
        // 4. Reconstruct generic-receiver extension entries from synth templates.
        for template <- extOnlyTemplates do
          template match
            case fd: FunDeclAST if fd.name.startsWith("__ext_") && fd.params.nonEmpty =>
              val sep = fd.name.indexOf("__", 6)
              if sep > 0 then
                val methodName = fd.name.substring(sep + 2)
                val recv = fd.params.head
                val entry = ExtensionEntry(
                  methodName = methodName,
                  receiverTypeAst = recv.typ,
                  mangledFnName = fd.name,
                  definingModule = importedDefMod,
                )
                val bucket = extensionsByMethod.getOrElseUpdate(methodName, mutable.ListBuffer.empty)
                if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
                  bucket += entry
                importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
            case _ => ()
        if modulePath.nonEmpty then
          visibleExtensionModules += importedDefMod
        return
      case _ =>
    // Symbol names in meta may be module-mangled (e.g. "std_strings__trim_space").
    // Strip the prefix for selector matching and local lookup keys, but keep
    // the mangled name in FunInfo.name so TCall/TFunDecl use it for codegen/linker.
    // Deduplicate by short name: if both a mangled and extern version exist, prefer non-extern.
    def dedup(syms: List[SymbolMeta]): List[SymbolMeta] =
      syms.groupBy(s => shortName(s.name)).values.map { group =>
        if group.size > 1 then group.find(!_.isExtern).getOrElse(group.head)
        else group.head
      }.toList
    // Synthesized extension functions (`__ext_<typeKey>__<method>`) must be
    // pulled in alongside their EXT entry whenever an EXT is being imported
    // — even on a named-import like `import mylib.{add_one}` where the user
    // didn't name the extension's method. Without this, `tryExtensionDispatch`
    // would find the entry but fail to find the function in `functions`.
    val extensionFnShortNames: Set[String] = meta.extensions.map(_.mangledFnName).toSet
    val selectedSymbols = selectors match
      case List(WildcardImport) => dedup(meta.publicSymbols)
      case named =>
        val nameMap = named.collect { case NamedImport(n, r) => (n, r) }.toMap
        // Match selectors against short names (without module prefix).
        // When a struct or enum is imported by name, pull in its methods
        // (StructName_method). When a *function* is imported and its
        // signature mentions a struct/enum that lives in this same module,
        // also pull in that type's methods — methods belong to the type, not
        // the import scope, so `import std.builder.{new_builder}` should let
        // the user call methods on the returned `StrBuilder` without naming
        // it in the selector list.
        val directMatch = meta.publicSymbols.filter(sym => nameMap.contains(shortName(sym.name)))
        val moduleStructAndEnumNames: Set[String] =
          meta.publicSymbols.collect {
            case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Struct] => shortName(sym.name)
            case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Enum]   => shortName(sym.name)
          }.toSet
        val explicitlyImportedTypeNames = directMatch.collect {
          case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Struct] => shortName(sym.name)
          case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Enum]   => shortName(sym.name)
        }.toSet
        val typesReachableFromImportedFuncs: Set[String] =
          directMatch.iterator.collect {
            case sym if sym.typ.isInstanceOf[SymbolMeta.Kind.Func] =>
              val SymbolMeta.Kind.Func(params, ret, _, _, _, _, _) = sym.typ: @unchecked
              (params :+ ret).iterator.flatMap(collectStructAndEnumNames).toSet
          }.flatten.toSet.intersect(moduleStructAndEnumNames)
        val importedTypeNames = explicitlyImportedTypeNames ++ typesReachableFromImportedFuncs
        val withMethods = if importedTypeNames.isEmpty then directMatch
        else directMatch ++ meta.publicSymbols.filter { sym =>
          sym.typ.isInstanceOf[SymbolMeta.Kind.Func] &&
            importedTypeNames.exists(tn => shortName(sym.name).startsWith(s"${tn}_"))
        }
        val withExtensions = withMethods ++ meta.publicSymbols.filter { sym =>
          sym.typ.isInstanceOf[SymbolMeta.Kind.Func] && extensionFnShortNames.contains(shortName(sym.name))
        }
        dedup(withExtensions)
    // Build alias map for renamed imports: alias -> original mangled name
    val aliasMap: Map[String, String] = selectors match
      case List(WildcardImport) => Map.empty
      case named =>
        named.collect { case NamedImport(n, Some(alias)) => (alias, n) }.toMap
    // Reverse: short-name -> alias
    val shortToAlias: Map[String, String] = selectors match
      case List(WildcardImport) => Map.empty
      case named =>
        named.collect { case NamedImport(n, Some(alias)) => (n, alias) }.toMap
    for sym <- selectedSymbols do
      val sn = shortName(sym.name)
      val localKey = shortToAlias.getOrElse(sn, sn) // use alias if provided
      sym.typ match
        case SymbolMeta.Kind.Func(params, returnType, isDef, isPure, modes, effects, isParameterless) =>
          val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
          // Even if the function itself is already known, walk its types to
          // discover any nested generic-struct instances (e.g. an imported
          // `eoi -> Box[unit]` surfaces `Box_unit`, which the importing unit
          // needs to recognize as `Box[unit]` for downstream unification).
          for p <- params do linkNestedGenericStructInstances(p)
          linkNestedGenericStructInstances(returnType)
          if functions.contains(localKey) then
            // Allow same-module sibling re-registration (same mangled name) and externs
            val existing = functions(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing function")
          else
            // Carry over the imported effect signature so `funInfoEffects` and
            // `validateEffects` see the same information as if the function were
            // defined locally. Pre-resolved (mangled) names round-trip directly.
            val (reads, writes) = effects match
              case e if e.isPure => (None, None)
              case e if e.isUnknown => (None, None)
              case e => (e.reads, e.writes)
            functions(localKey) = FunInfo(sym.name, paramPairs, returnType, isDef, isPure || effects.isPure, modes, reads, writes, isParameterless = isParameterless, isRealtime = effects.isRealtime, isConst = effects.isConst)
            // Pre-populate the resolved-effects cache so cross-module reads use the same
            // already-mangled names without trying to look them up in this unit's globalScope.
            if reads.isDefined || writes.isDefined then
              resolvedEffectsCache(sym.name) = (reads.getOrElse(Set.empty), writes.getOrElse(Set.empty))
            externalSymbols += localKey
        case SymbolMeta.Kind.Data(dataType, isMutable) =>
          linkNestedGenericStructInstances(dataType)
          if globalScope.contains(localKey) then
            val existing = globalScope(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported symbol '$localKey' conflicts with existing global")
          else
            globalScope(localKey) = SymInfo(sym.name, dataType, mutable = isMutable)
            externalSymbols += localKey
        case SymbolMeta.Kind.Const(constType, value) =>
          linkNestedGenericStructInstances(constType)
          // Cross-file `const`: register in globalScope (so VarRef name resolution
          // succeeds) AND in compileTimeConstants / compileTimeFloats under both the
          // local-key short name and the fully-mangled name so the analyzer's
          // constant-folding paths (VarRef → TIntLit/TFloatLit substitution) find
          // the value either way. Float consts carry the Double via the `value`
          // Long slot's bit pattern — `Double.longBitsToDouble` recovers it.
          if globalScope.contains(localKey) then
            val existing = globalScope(localKey)
            if !sym.isExtern && existing.name != sym.name then
              throw AnalysisError(s"imported const '$localKey' conflicts with existing global")
          else
            globalScope(localKey) = SymInfo(sym.name, constType, mutable = false, isConst = true)
            externalSymbols += localKey
          if constType.isFloat then
            val d = java.lang.Double.longBitsToDouble(value)
            compileTimeFloats(localKey) = d
            compileTimeFloats(sym.name) = d
          else
            compileTimeConstants(localKey) = value
            compileTimeConstants(sym.name) = value
        case SymbolMeta.Kind.Struct(st) =>
          structTypes(shortName(sym.name)) = st
          linkImportedGenericStructToTemplate(st)
        case SymbolMeta.Kind.Interface(it) =>
          interfaceTypes(shortName(sym.name)) = it
        case SymbolMeta.Kind.Enum(et) =>
          val sn = shortName(sym.name)
          if et.variants.forall(_._2.isEmpty) then
            // Simple enum (no data variants) — register as both simpleEnumTypes and enumTypes
            simpleEnumTypes(sn) = et
            val members = et.variants.zipWithIndex.map { case ((vname, _), idx) => (vname, idx.toLong) }.toMap
            enumTypes(sn) = members
          else
            // Data enum — register in dataEnumTypes and variantToEnum
            linkImportedDataEnumToTemplate(et)
            dataEnumTypes(sn) = et
            // Mangled generic instances (e.g. ParseMaybe_i32) link via enumToTemplate when the suffix
            // parses as a monotype. Suffixes like _Tuple2 or func(...) do not parse — still do not
            // register Got/Miss on variantToEnum or the last imported instance wins and breaks seq/map.
            val isMangledGenericInstance =
              genericEnums.exists { case (base, decl) =>
                decl.typeParams.length == 1 && et.name.startsWith(base + "_") && et.name != base
              }
            if !enumToTemplate.contains(et.name) && !isMangledGenericInstance then
              for ((vname, _), idx) <- et.variants.zipWithIndex do
                variantToEnum(vname) = (et, idx)
        case SymbolMeta.Kind.Impl(traitName, targetType, methods) =>
          if findConcreteImpl(traitName, targetType).isEmpty then
            val mm = mutable.LinkedHashMap.from(methods)
            implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) +=
              ImplTemplate(
                typeParams = Nil,
                targetPatterns = List(syslTypeToAST(targetType)),
                resolvedConcrete = Some(List(targetType)),
                methods = mm,
                methodInfos = Nil,
                methodASTs = Nil,
                definingModule = "",   // imported impls — orphan check skipped on import
              )
            importedConcreteImplKeys += ((traitName, List(targetType)))

    // Register generic templates from imported module (needed for cross-module generic instantiation).
    // Selective imports (`import std.option.{Option, Some, None}`) must filter generic templates
    // by selector — otherwise generic functions like `std.option.expect[T]` leak into scope and
    // shadow the testing-builtin `expect` (or whatever else the user wants to use). Wildcard
    // imports register everything as before.
    val genericTemplateFilter: Option[Set[String]] = selectors match
      case List(WildcardImport) => None
      case named =>
        Some(named.collect { case NamedImport(n, _) => n }.toSet)
    if meta.genericTemplates.nonEmpty then
      registerGenericTemplatesFrom(ProgramAST(meta.genericTemplates), genericTemplateFilter)

    // Generic struct templates may have just been registered above; the symbol
    // loop walked imported function/data/const types before genericStructs was
    // populated, so any nested generic-struct instances (e.g. `Box_unit` inside
    // `eoi -> Box[unit]`) couldn't be linked then. Re-walk the publicSymbols'
    // types now that templates are in scope so structToTemplate has the
    // mangled-instance → template mapping for downstream unifyTypes.
    for sym <- meta.publicSymbols do
      sym.typ match
        case SymbolMeta.Kind.Func(params, returnType, _, _, _, _, _) =>
          for p <- params do linkNestedGenericStructInstances(p)
          linkNestedGenericStructInstances(returnType)
        case SymbolMeta.Kind.Data(t, _) => linkNestedGenericStructInstances(t)
        case SymbolMeta.Kind.Const(t, _) => linkNestedGenericStructInstances(t)
        case SymbolMeta.Kind.Struct(st) => linkImportedGenericStructToTemplate(st)
        case _ => ()

    // Register generic enum instance mappings for cross-module type inference
    for inst <- meta.genericEnumInstances do
      if !enumToTemplate.contains(inst.mangledName) then
        enumToTemplate(inst.mangledName) = (inst.baseName, inst.typeArgs)
      importedEnumInstNames += inst.mangledName

    // Register trait declarations from imported templates
    for template <- meta.genericTemplates do
      template match
        case TraitDeclAST(name, tparams, methods, _, assocs, _, tBounds) =>
          if !traits.contains(name) then
            traits(name) = TraitInfo(name, tparams, methods, assocs, tBounds)
            importedTraitNames += name
            registerTraitOperatorEntries(name, methods, template)
        case _ =>

    // Register trait impl mappings from imported module
    for impl <- meta.traitImpls do
      if findConcreteImpl(impl.traitName, impl.targetType).isEmpty then
        val mm = mutable.LinkedHashMap.from(impl.methods)
        implTemplates.getOrElseUpdate(impl.traitName, mutable.ListBuffer.empty) +=
          ImplTemplate(
            typeParams = Nil,
            targetPatterns = List(syslTypeToAST(impl.targetType)),
            resolvedConcrete = Some(List(impl.targetType)),
            methods = mm,
            methodInfos = Nil,
            methodASTs = Nil,
            definingModule = "",
          )
        importedConcreteImplKeys += ((impl.traitName, List(impl.targetType)))

    // Register ImplDeclASTs from imported templates. The driver puts every
    // user-written generic impl + every multi-target concrete impl in the
    // genericTemplates list — single-target concrete impls go through the
    // meta.traitImpls path above, so they're not duplicated here.
    for template <- meta.genericTemplates do
      template match
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, _, _, implTBounds) =>
          val alreadyHas = implTemplates.getOrElse(traitName, Nil).exists(t =>
            t.typeParams == implTypeParams && t.targetPatterns == targetTypes)
          if !alreadyHas then
            if implTypeParams.nonEmpty then
              // Generic impl: methods are analyzed lazily at instantiation time.
              implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                typeParams = implTypeParams,
                targetPatterns = targetTypes,
                resolvedConcrete = None,
                methods = mutable.LinkedHashMap.empty,
                methodInfos = Nil,
                methodASTs = methods,
                definingModule = "",
                implDecl = Some(impl),
                typeBounds = implTBounds,
              )
              importedGenericImplKeys += ((traitName, implTypeParams, targetTypes))
            else
              // Multi-target concrete impl. Resolve targets so the dispatch
              // path (`enumerateImplCandidates` concrete branch) can compare
              // against them, and mangle method names with the *owning*
              // module's prefix (not the importing analyzer's `currentModule`)
              // so dispatch lands on the same name the impl's defining unit
              // emitted. For cross-module imports, modulePath identifies the
              // owning module; for same-module siblings (modulePath empty)
              // the driver pre-seeds `currentModule` to the shared module.
              // If targets can't resolve yet (rare — types are registered
              // earlier in registerImport), skip and rely on a later import
              // iteration to pick it up.
              scala.util.Try(targetTypes.map(resolveType)).toOption.foreach { resolvedTargets =>
                val owningModuleMangled =
                  if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_')
                  else currentModule.getOrElse("")
                val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
                val methodMap = mutable.LinkedHashMap.empty[String, String]
                for m <- methods do
                  val rawMangled = s"${traitName}_${m.name}_${typeMangled}"
                  val mangled =
                    if owningModuleMangled.nonEmpty && !neverMangle.contains(rawMangled) then
                      s"${owningModuleMangled}__${rawMangled}"
                    else rawMangled
                  methodMap(m.name) = mangled
                implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                  typeParams = Nil,
                  targetPatterns = targetTypes,
                  resolvedConcrete = Some(resolvedTargets),
                  methods = methodMap,
                  methodInfos = Nil,
                  methodASTs = Nil,
                  definingModule = "",
                  implDecl = Some(impl),
                )
                importedConcreteImplKeys += ((traitName, resolvedTargets))
              }
        case _ => ()

    // Register imported extensions: every extension flows into the side table
    // so dispatch can find it; visibility is gated at dispatch time. The
    // module's own key is also added to `visibleExtensionModules` so any
    // extension *defined in* this imported module is callable in this unit
    // (matches Scala 3's "import brings extensions" rule). Modules imported
    // by short name via `modulePath` are stored in their underscore-mangled
    // form to match the analyzer's `currentModule` representation.
    val importedDefiningModule =
      if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_') else ""
    for ext <- meta.extensions do
      val entry = ExtensionEntry(
        methodName = ext.methodName,
        receiverTypeAst = syslTypeToAST(ext.receiverType),
        mangledFnName = ext.mangledFnName,
        definingModule = ext.definingModule,
      )
      val bucket = extensionsByMethod.getOrElseUpdate(ext.methodName, mutable.ListBuffer.empty)
      // De-dup by (definingModule, mangledFnName) so siblings importing each
      // other don't double-register the same extension.
      if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
        bucket += entry
      importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
    // Phase 2d: reconstruct ExtensionEntry's for generic synth templates that
    // travelled through `meta.genericTemplates`. The synth name encodes the
    // method (`__ext_<typeKey>__<methodName>`) and the first parameter's
    // TypeAST is the original receiver pattern with its free type vars intact.
    // The `definingModule` is the module we're currently importing — it's not
    // carried in the FunDeclAST itself, so use `modulePath` (already mangled).
    for template <- meta.genericTemplates do
      template match
        case fd: FunDeclAST if fd.name.startsWith("__ext_") && fd.params.nonEmpty =>
          val sep = fd.name.indexOf("__", 6)  // skip leading "__ext_"
          if sep > 0 then
            val methodName = fd.name.substring(sep + 2)
            val recv = fd.params.head
            val entry = ExtensionEntry(
              methodName = methodName,
              receiverTypeAst = recv.typ,
              mangledFnName = fd.name,
              definingModule = importedDefiningModule,
            )
            val bucket = extensionsByMethod.getOrElseUpdate(methodName, mutable.ListBuffer.empty)
            if !bucket.exists(e => e.definingModule == entry.definingModule && e.mangledFnName == entry.mangledFnName) then
              bucket += entry
            importedExtensionKeys += ((entry.definingModule, entry.mangledFnName))
        case _ => ()
    if modulePath.nonEmpty then
      visibleExtensionModules += importedDefiningModule

    // Stash non-generic `#const fn` bodies for later re-analysis in this unit's
    // context. The owning-module prefix matches what the source unit mangled
    // with — for sibling imports `modulePath` is empty so we fall back to the
    // current unit's `currentModule` (siblings share the same module path).
    val constFnOwningModule =
      if modulePath.nonEmpty then modulePath.replace('/', '_').replace('.', '_')
      else currentModule.getOrElse("")
    for fd <- meta.constFunBodies do
      val mangled =
        if constFnOwningModule.nonEmpty && !neverMangle.contains(fd.name) then
          s"${constFnOwningModule}__${fd.name}"
        else fd.name
      if !pendingImportedConstFnBodies.contains(mangled) && !constFunDecls.contains(mangled) then
        pendingImportedConstFnBodies(mangled) = (fd, constFnOwningModule)

  def isExternal(name: String): Boolean = externalSymbols.contains(name)
  def externals: Set[String] = externalSymbols.toSet

  /** Inverse of `typeToMangled` for a single type (used in mangled generic enum names like `ParseMaybe_i32`). */
  protected def parseMangledMonotype(s: String): Option[SyslType] =
    if s.isEmpty then None
    else if s.startsWith("slice") then parseMangledMonotype(s.drop(5)).map(SyslType.SliceType.apply)
    else if s.startsWith("ptr") then parseMangledMonotype(s.drop(3)).map(SyslType.PtrType.apply)
    else if s.startsWith("ref") then parseMangledMonotype(s.drop(3)).map(SyslType.RefType.apply)
    else
      s match
        case "i8" => Some(SyslType.I8)
        case "i16" => Some(SyslType.I16)
        case "i32" => Some(SyslType.I32)
        case "i64" => Some(SyslType.I64)
        case "u8" => Some(SyslType.U8)
        case "u16" => Some(SyslType.U16)
        case "u32" => Some(SyslType.U32)
        case "u64" => Some(SyslType.U64)
        case "bool" => Some(SyslType.BoolType)
        case "string" => Some(SyslType.StringType)
        case "unit" => Some(SyslType.UnitType)
        case "f32" | "float" => Some(SyslType.F32)
        case "f64" | "double" => Some(SyslType.F64)
        case _ => None

  /** Link mangled imported enum names to generic templates for `unifyTypes` only. Do not call `instantiateGenericEnum` here — it would overwrite `variantToEnum` for shared variant names like `Got`/`Miss`. */
  protected def linkImportedDataEnumToTemplate(et: SyslType.EnumType): Unit =
    if genericEnums.isEmpty then return
    for (baseName, decl) <- genericEnums if decl.typeParams.length == 1 do
      val prefix = baseName + "_"
      if et.name.startsWith(prefix) then
        val suffix = et.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          enumToTemplate(et.name) = (baseName, List(t))
        }

  /** Cross-unit mirror of the local instantiation cache: when an imported
   *  symbol's type names a generic-struct *instance* (e.g. `Box_unit`),
   *  populate `structToTemplate` so subsequent `unifyTypes(Box[A], Box_unit)`
   *  in this unit can recover the type-arg binding. Without this, sibling /
   *  cross-module references to a value of generic-struct-instance type don't
   *  drive type inference at the use site. Mirrors the enum equivalent above.
   *  Single-target template only; multi-arg templates would need a richer
   *  inverse mangler. */
  protected def linkImportedGenericStructToTemplate(st: SyslType.StructType): Unit =
    if genericStructs.isEmpty then return
    if structToTemplate.contains(st.name) then return
    for (baseName, decl) <- genericStructs if decl.typeParams.length == 1 do
      val prefix = baseName + "_"
      if st.name.startsWith(prefix) then
        val suffix = st.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          structToTemplate(st.name) = (baseName, List(t))
        }

  /** Cross-unit mirror for generic *type aliases*: when an imported value's
   *  type is a nominal alias instance (e.g. `Box_unit` where `type Box[A] = new A`),
   *  populate `genericAliasToTemplate` so subsequent `unifyTypes(Box[A], Box_unit)`
   *  can recover the type-arg binding. Mirrors the struct equivalent, but for
   *  the `type X[A] = new ...` family. Single-target template only. */
  protected def linkImportedGenericAliasToTemplate(nt: SyslType.NamedType): Unit =
    if genericTypeAliases.isEmpty then return
    if genericAliasToTemplate.contains(nt.name) then return
    for (baseName, (tps, _, _)) <- genericTypeAliases if tps.length == 1 do
      val prefix = baseName + "_"
      if nt.name.startsWith(prefix) then
        val suffix = nt.name.drop(prefix.length)
        parseMangledMonotype(suffix).foreach { t =>
          genericAliasToTemplate(nt.name) = (baseName, List(t))
        }

  /** Walk a SyslType and link any nested generic-struct or generic-alias
   *  instance to its template. Used when registering imported function
   *  signatures whose param/return types may surface instance types
   *  (e.g. `Box_unit`) that aren't themselves imported as standalone symbols. */
  protected def linkNestedGenericStructInstances(t: SyslType): Unit = t match
    case st: SyslType.StructType =>
      linkImportedGenericStructToTemplate(st)
      for (_, ft) <- st.fields do linkNestedGenericStructInstances(ft)
    case nt @ SyslType.NamedType(_, base, true, _, _) =>
      linkImportedGenericAliasToTemplate(nt)
      linkNestedGenericStructInstances(base)
    case SyslType.NamedType(_, base, _, _, _) => linkNestedGenericStructInstances(base)
    case SyslType.PtrType(p) => linkNestedGenericStructInstances(p)
    case SyslType.RefType(p) => linkNestedGenericStructInstances(p)
    case SyslType.ArrayType(e, _) => linkNestedGenericStructInstances(e)
    case SyslType.SliceType(e) => linkNestedGenericStructInstances(e)
    case SyslType.FuncType(ps, r, _, _) =>
      for p <- ps do linkNestedGenericStructInstances(p)
      linkNestedGenericStructInstances(r)
    case SyslType.EnumType(_, variants) =>
      for (_, fs) <- variants; (_, ft) <- fs do linkNestedGenericStructInstances(ft)
    case _ => ()

  /** Generic templates are omitted from `ModuleMeta` / typed `TProgram`; same-package siblings need the raw AST templates to resolve calls like `alt(...)`. */
  /** Register generic templates (functions, structs, data enums) for the current
   *  unit or for a cross-module import. When `filter` is `None` (wildcard / own
   *  module), every template is registered.
   *
   *  When `filter` is `Some(set)`, only **functions** whose name is in the set are
   *  registered — this is what makes selective imports actually selective for
   *  generic functions, which is where shadowing bugs surface (the canonical
   *  case: `import std.option.{Option, Some, None}` should NOT pull in the
   *  generic `expect[T]`). Generic structs / data enums are still registered
   *  unconditionally because their names appear in user-written types and the
   *  analyzer needs them resolvable; the symbol-table import path already
   *  filters them through publicSymbols. */
  def registerGenericTemplatesFrom(
      program: ProgramAST,
      filter: Option[Set[String]] = None,
  ): Unit =
    def funcSelected(name: String): Boolean = filter.forall(_.contains(name))
    for decl <- program.decls do
      decl match
        case fd @ FunDeclAST(name, _, _, _, _, tps, _, _, _, _, _) if tps.nonEmpty =>
          if funcSelected(name) && !genericTemplates.contains(name) && !functions.contains(name) then
            genericTemplates(name) = fd
            importedTemplateNames += name
        case sd @ StructDeclAST(name, _, tps, _, _, _, _) if tps.nonEmpty =>
          if !genericStructs.contains(name) then
            genericStructs(name) = sd
            importedTemplateNames += name
        case de @ DataEnumDeclAST(name, variants, tps, _, _, _) if tps.nonEmpty =>
          if !genericEnums.contains(name) then
            genericEnums(name) = de
            importedTemplateNames += name
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              genericVariantToEnum.get(vname) match
                case Some((n, i)) =>
                  if n != name || i != idx then
                    throw AnalysisError(s"duplicate variant name: '$vname'", de)
                case None =>
                  // Import may have registered Got/Miss on variantToEnum when mangled linking failed;
                  // template wins so analyze(generic enum) does not see variantToEnum + empty genericVariantToEnum.
                  if variantToEnum.contains(vname) then variantToEnum.remove(vname)
                  genericVariantToEnum(vname) = (name, idx)
        case TypeAliasDeclAST(name, target, tps, _, isNew, _, _, defs, bounds) if tps.nonEmpty =>
          if !genericTypeAliases.contains(name) && !typeAliases.contains(name) then
            genericTypeAliases(name) = (tps, target, isNew)
            if defs.nonEmpty then genericTypeAliasDefaults(name) = defs
            if bounds.nonEmpty then genericTypeAliasBounds(name) = bounds
            importedTemplateNames += name
        case _ => ()

  /** Sibling forward-decl pass: registers structs/enums/aliases/traits/impls
   *  from a same-module sibling file's source AST. Distinct from
   *  registerGenericTemplatesFrom — that one is called from registerImport's
   *  cross-module path and must NOT mangle with the importing module's
   *  prefix. This method is only safe to call when the analyzer's
   *  currentModule equals the sibling's owning module (i.e. they're in the
   *  same module). The driver pre-seeds currentModule and calls this on
   *  every sibling AST during Step 4b; the analyzer re-calls it after own
   *  pass 1 so sibling concrete impls referencing own types can resolve.
   *  Idempotent — every clause guards on existence. */
  def registerSiblingForwardDeclsFrom(program: ProgramAST): Unit =
    // Pre-pass: register generic templates first so cross-sibling generic
    // types are visible to the type-checks in subsequent clauses.
    registerGenericTemplatesFrom(program)
    for decl <- program.decls do
      decl match
        case td @ TraitDeclAST(name, tparams, methods, _, assocs, _, tBounds) =>
          if !traits.contains(name) then
            traits(name) = TraitInfo(name, tparams, methods, assocs, tBounds)
            importedTraitNames += name
            registerTraitOperatorEntries(name, methods, td)
        case StructDeclAST(name, fields, typeParams, _, _, _, _) if typeParams.isEmpty =>
          // Best-effort: register the struct with resolved fields so cross-
          // sibling field accesses (e.g. atoms.lsysl reading `inp.source`
          // when Input is in parsyl.lsysl) work during body analysis.
          // Falls back to a Nil-fields placeholder if any field type can't
          // resolve yet — the post-pass-1 hook re-runs and may complete it.
          val existing = structTypes.get(name)
          val needsFill = existing.forall(_.fields.isEmpty)
          if needsFill then
            val resolvedFields = scala.util.Try(fields.map { case (fn, ft, _) =>
              (fn, resolveType(ft))
            }).getOrElse(Nil)
            structTypes(name) = SyslType.StructType(name, resolvedFields)
        case DataEnumDeclAST(name, variants, typeParams, _, _, _) if typeParams.isEmpty =>
          // Best-effort: resolve variant fields too. Same fallback as struct.
          val existing = dataEnumTypes.get(name)
          val needsFill = existing.forall(_.variants.forall(_._2.isEmpty))
          if needsFill then
            val resolvedVariants = variants.map { case EnumVariantAST(vname, vfields) =>
              val resolved = scala.util.Try(vfields.map { case (fn, ft) =>
                (fn, resolveType(ft))
              }).getOrElse(Nil)
              (vname, resolved)
            }
            dataEnumTypes(name) = SyslType.EnumType(name, resolvedVariants)
            for (EnumVariantAST(vname, _), idx) <- variants.zipWithIndex do
              if !variantToEnum.contains(vname) && !genericVariantToEnum.contains(vname) then
                variantToEnum(vname) = (dataEnumTypes(name), idx)
        case TypeAliasDeclAST(name, target, typeParams, _, isNew, range, predicate, _, _) if typeParams.isEmpty =>
          if !typeAliases.contains(name) && !genericTypeAliases.contains(name) then
            typeAliases(name) = (target, isNew, range, predicate)
        case fd @ FunDeclAST(name, params, returnType, _, _, tps, _, attrs, isDef, isParameterless, _)
            if tps.isEmpty && !name.startsWith("__") =>
          // Non-generic free functions (including struct methods, parsed as
          // `Input_at_end` etc.) — pre-register a stub FunInfo so cross-
          // sibling references like `inp.at_end()` (which looks up
          // `functions(Input_at_end)`) and direct calls (like `literal(s)`
          // from operators.lsysl into atoms.lsysl) resolve. Param/return
          // types may still be placeholder-typed at first pre-register; the
          // post-pass-1 hook re-registers with real types after own pass 1
          // populates struct fields. Skip if the name conflicts with an
          // already-known function/template, or if signature resolution
          // fails (sibling types not yet registered — retry next iteration).
          if !functions.contains(name) && !genericTemplates.contains(name) then
            scala.util.Try {
              val paramTypes = params.map(p => (p.name, resolveType(p.typ)))
              val retType = returnType.map(resolveType).getOrElse(UnitType)
              val mangled = if shouldMangle(name) then mangleName(name) else name
              val isPure = attrs.exists(_.name == "pure")
              val isRealtime = attrs.exists(_.name == "realtime")
              val isConst = attrs.exists(_.name == "const")
              functions(name) = FunInfo(
                mangled, paramTypes, retType, isDef, isPure,
                isParameterless = isParameterless, isRealtime = isRealtime, isConst = isConst,
              )
              externalSymbols += name
              importedSiblingFreeFnStubKeys += name
            }
        case impl @ ImplDeclAST(traitName, implTypeParams, targetTypes, methods, _, _, _, implTBounds) =>
          val alreadyHas = implTemplates.getOrElse(traitName, Nil).exists(t =>
            t.typeParams == implTypeParams && t.targetPatterns == targetTypes)
          if !alreadyHas then
            if implTypeParams.nonEmpty then
              implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                typeParams = implTypeParams,
                targetPatterns = targetTypes,
                resolvedConcrete = None,
                methods = mutable.LinkedHashMap.empty,
                methodInfos = Nil,
                methodASTs = methods,
                definingModule = "",
                implDecl = Some(impl),
                typeBounds = implTBounds,
              )
              importedGenericImplKeys += ((traitName, implTypeParams, targetTypes))
            else
              // Concrete: best-effort resolve. Targets that don't resolve yet
              // (sibling hasn't pre-collected its type decls) are skipped; a
              // later call retries with more state. Build methods map by
              // mangling so enumerateImplCandidates can look up paramTypes
              // via `functions(mangled)` — empty methods map breaks dispatch.
              scala.util.Try(targetTypes.map(resolveType)).toOption.foreach { resolvedTargets =>
                val owningModuleMangled = currentModule.getOrElse("")
                val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
                val methodMap = mutable.LinkedHashMap.empty[String, String]
                // Also register a stub FunInfo per impl method using the trait
                // method's signature substituted with the impl's targets — so
                // enumerateImplCandidates' concrete branch can fetch paramTypes
                // from `functions(mangled)` and the dispatch comparison works.
                // The function body is owned by the sibling's compilation unit;
                // here we only need the type shape for dispatch.
                val trait_ = traits.get(traitName)
                for m <- methods do
                  val rawMangled = s"${traitName}_${m.name}_${typeMangled}"
                  val mangled =
                    if owningModuleMangled.nonEmpty && !neverMangle.contains(rawMangled) then
                      s"${owningModuleMangled}__${rawMangled}"
                    else rawMangled
                  methodMap(m.name) = mangled
                  if !functions.contains(mangled) then
                    trait_.foreach { ti =>
                      ti.methods.find(_.name == m.name).foreach { tm =>
                        scala.util.Try {
                          val savedEnv = typeEnv
                          typeEnv = typeEnv ++ ti.typeParams.zip(resolvedTargets).toMap
                          try
                            val paramTypes = tm.params.map(p => (p.name, resolveType(p.typ)))
                            val retType = resolveType(tm.returnType)
                            functions(mangled) = FunInfo(mangled, paramTypes, retType)
                            externalSymbols += mangled
                            importedConcreteImplStubFunctions += mangled
                          finally typeEnv = savedEnv
                        }
                      }
                    }
                implTemplates.getOrElseUpdate(traitName, mutable.ListBuffer.empty) += ImplTemplate(
                  typeParams = Nil,
                  targetPatterns = targetTypes,
                  resolvedConcrete = Some(resolvedTargets),
                  methods = methodMap,
                  methodInfos = Nil,
                  methodASTs = Nil,
                  definingModule = "",
                  implDecl = Some(impl),
                  typeBounds = implTBounds,
                )
                importedConcreteImplKeys += ((traitName, resolvedTargets))
              }
        case VarDeclAST(name, typOpt, init, _, isMutable, attrs, _, isConst) =>
          // Module-level `val` / `const`: register a forward stub in globalScope
          // so cross-sibling `VarRefAST(name)` lookups don't throw during
          // pre-collection. Without this, two siblings that each define a const
          // (or `val literal`) the other references form an unbreakable mutual-
          // undefined cycle in Step 4b's fix-point loop — convergence stalls,
          // both files miss the packageMetaCache, and Step 5 surfaces a
          // misleading `undefined variable` from a third file that depends on
          // the cascade victim.
          //
          // Type is computed conservatively: explicit annotation wins; otherwise
          // we inspect the initializer and only register if we can pin the type
          // exactly (literal int → I32/I64 by the same rule as IntLitAST in
          // analyzeExpr; literal bool → BoolType; const-fold over already-known
          // forward consts). Anything else is left for the own-file pass — a
          // wrong stub type poisons type-checking far away (e.g. an int param
          // mistakenly fed an i64 forward-stub fails with "expects int, got i64").
          if !globalScope.contains(name) && !attrs.exists(_.name == "address") then
            scala.util.Try {
              val stubInfo: Option[(SyslType, Option[Long])] = typOpt match
                case Some(t) =>
                  val rt = resolveType(t)
                  Some((rt, tryConstEvalInit(init)))
                case None =>
                  initStubType(init).map(t => (t, tryConstEvalInit(init)))
              stubInfo.foreach { case (resolvedType, foldedOpt) =>
                val mangledName = if shouldMangle(name) then mangleName(name) else name
                val isGhost = attrs.exists(_.name == "ghost")
                globalScope(name) = SymInfo(mangledName, resolvedType, mutable = isMutable, isConst = isConst, isGhost = isGhost)
                externalSymbols += name
                foldedOpt.foreach { v =>
                  compileTimeConstants(name) = v
                  compileTimeConstants(mangledName) = v
                }
              }
            }
        case _ => ()

  /** Type stub for sibling forward-decl: only return a type when we can pin it
   *  exactly from the AST. Returns None for anything we'd have to guess at. */
  private def initStubType(init: ExpressionAST): Option[SyslType] =
    init match
      case IntLitAST(n) =>
        Some(if n > 0xFFFFFFFFL || n < -0x80000000L then IntType(64) else IntType(32))
      case TypedIntLitAST(_, typeName) =>
        scala.util.Try(resolveType(NamedTypeAST(typeName))).toOption
      case BoolLitAST(_) => Some(BoolType)
      case UnaryAST("-", inner) => initStubType(inner)
      case BinaryAST(l, _, r) =>
        // Conservative: only return a type when both sides agree.
        (initStubType(l), initStubType(r)) match
          case (Some(lt), Some(rt)) if lt == rt => Some(lt)
          case _ => None
      case _ => None

  /** Best-effort literal evaluation for sibling forward-decl pre-registration. */
  private def tryConstEvalInit(init: ExpressionAST): Option[Long] =
    init match
      case IntLitAST(n) => Some(n)
      case BoolLitAST(b) => Some(if b then 1L else 0L)
      case UnaryAST("-", IntLitAST(n)) => Some(-n)
      case BinaryAST(l, op, r) =>
        for li <- tryConstEvalInit(l); ri <- tryConstEvalInit(r) yield op match
          case "+" => li + ri
          case "-" => li - ri
          case "*" => li * ri
          case "/" => if ri != 0 then li / ri else 0L
          case "%" => if ri != 0 then li % ri else 0L
          case "<<" => li << ri
          case ">>" => li >> ri
          case "&" => li & ri
          case "|" => li | ri
          case "^" => li ^ ri
          case _ => 0L
      case VarRefAST(n) if compileTimeConstants.contains(n) => Some(compileTimeConstants(n))
      case _ => None
