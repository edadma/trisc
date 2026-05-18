package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerExtensions:
  self: SyslAnalyzer =>

  /** Predef modules whose extensions are auto-visible — slash-form module
   *  paths for every primitive owner, suitable for the driver's
   *  `packageMetaCache` / `smetaCache` lookup. The driver consults this to
   *  inject a synthetic wildcard import for each Predef module that's present
   *  in the source set, so e.g. `"hi".chars` works without a literal
   *  `import std.string`. Modules not present in the source set are silently
   *  skipped — no error if std isn't around. */
  def predefModulePaths: Set[String] =
    primitiveDefiningModule.values.toSet.map(_.replaceFirst("_", "/"))

  /** Stable string key for a TypeAST, used to mangle the synthesized extension
   *  function. Best-effort — collisions only matter for cross-block conflicts,
   *  which dispatch ambiguity-checks anyway. */
  protected def extensionTypeKey(t: TypeAST): String = t match
    case NamedTypeAST(name, Nil)  => name
    case NamedTypeAST(name, args) => s"${name}_${args.map(extensionTypeKey).mkString("_")}"
    case PtrTypeAST(e)            => s"ptr_${extensionTypeKey(e)}"
    case PtrNonNullTypeAST(e)     => s"ptr_${extensionTypeKey(e)}"
    case RefTypeAST(e)            => s"ref_${extensionTypeKey(e)}"
    case ArrayTypeAST(_, e)       => s"slice_${extensionTypeKey(e)}"
    case SliceTypeAST(e)          => s"slice_${extensionTypeKey(e)}"
    case _                        => "anon"

  /** Lower extension blocks to free FunDecls + side-table entries. Runs once at
   *  the top of analyze(), before pass 0, so the synthesized funcs flow through
   *  the normal registration/analysis pipeline. Methods carrying `#operator(<sigil>)`
   *  also emit a synth trait+impl pair so the existing `customBinaryOperatorTraits`
   *  / `customUnaryOperatorTraits` dispatch fires unchanged (Phase 2c). */
  protected def lowerExtensions(decls: List[DeclAST]): (List[DeclAST], List[ExtensionEntry]) =
    val out = mutable.ListBuffer[DeclAST]()
    val entries = mutable.ListBuffer[ExtensionEntry]()
    val module = currentModule.getOrElse("")
    for decl <- decls do
      decl match
        case ExtensionDeclAST(tparams, recv, methods, _, _, extTypeBounds) =>
          val key = extensionTypeKey(recv.typ)
          for m <- methods do
            val mangled = s"__ext_${key}__${m.name}"
            // Combine extension-level tparams with method-level tparams. Order
            // matters for instantiation lookup later — extension tparams come
            // first so a generic receiver's type bindings are stable.
            val mergedTparams = tparams ++ m.typeParams
            // Forward extension-level type bounds to the synth fn. The fn-level
            // bounds carrier on FunDeclAST is `typeBounds`; method-level wins
            // on collision (rare; the method-level set is typically empty here).
            val mergedTypeBounds = extTypeBounds ++ m.typeBounds
            val synth = m.copy(
              name = mangled,
              params = recv :: m.params,
              typeParams = mergedTparams,
              typeBounds = mergedTypeBounds,
            )
            out += synth
            entries += ExtensionEntry(m.name, recv.typ, mangled, module)
            // Phase 2c: an extension method with `#operator(<sigil>)` also synthesizes
            // a trait+impl pair so the existing operator-dispatch machinery picks it
            // up. The trait is generic in the receiver position only (`T`); other
            // params and return type are concretely typed exactly as the user wrote.
            // The impl provides a one-line body that delegates to the synth function.
            val opAttrs = m.attributes.filter(a => a.name == "operator" || a.name == "op")
            if opAttrs.length > 1 then
              throw AnalysisError(s"extension method '${m.name}' has multiple #operator attributes", m)
            for opAttr <- opAttrs.headOption do
              val sigil = extractOperatorSymbol(opAttr, m)
              val traitName = s"__ExtOp_${key}_${m.name}"
              val tparam = "T"
              // Trait method uses T everywhere (receiver, other params, return).
              // For the concrete-receiver case the impl substitutes T = recv.typ
              // and signature checking ensures everything matches. For the
              // generic-receiver case (Phase 2d) the impl is generic and the
              // dispatcher uses the impl's methodAST directly for unification,
              // so this stub trait signature isn't consulted at dispatch time —
              // it just needs to be syntactically valid + carry the operator
              // attribute + have the right arity for `registerTraitOperatorEntries`.
              val recvAsT = ParamAST("__ext_self__", NamedTypeAST(tparam, Nil), None, ParamMode.In)
              val traitMethodParams =
                if tparams.isEmpty then recvAsT :: m.params
                else recvAsT :: m.params.map(p => p.copy(typ = NamedTypeAST(tparam, Nil)))
              val traitRet =
                if tparams.isEmpty then m.returnType.getOrElse(NamedTypeAST("unit", Nil))
                else NamedTypeAST(tparam, Nil)
              val traitMethod = TraitMethodAST(
                m.name,
                traitMethodParams,
                traitRet,
                None,
                List(Attribute(opAttr.name, opAttr.args)),
              )
              out += TraitDeclAST(traitName, List(tparam), List(traitMethod))
              // Impl body: delegate to the synth extension function so the lowered
              // free function carries the user's body and the impl is just a thunk.
              val implRecv = recv.copy(name = "__ext_self__")
              val implParams = implRecv :: m.params
              val callArgs: List[ExpressionAST] =
                VarRefAST("__ext_self__") :: m.params.map(p => VarRefAST(p.name))
              val implBody = ExprBodyAST(CallAST(mangled, callArgs))
              val userReturn = m.returnType.getOrElse(NamedTypeAST("unit", Nil))
              val implMethod = FunDeclAST(
                m.name,
                implParams,
                Some(userReturn),
                implBody,
              )
              // Generic-receiver extensions yield a generic impl whose tparams
              // are the user's extension-level tparams; the receiver pattern
              // (using those tparams) becomes the trait's target type.
              val implDecl = ImplDeclAST(traitName, tparams, List(recv.typ), List(implMethod), Nil, Nil, Map.empty, extTypeBounds)
              out += implDecl
              if tparams.nonEmpty then extensionImplDecls += implDecl
        case other =>
          out += other
    (out.toList, entries.toList)

  /** Walk a SyslType collecting candidate "defining module" keys for the Predef-trick
   *  visibility check. Returns every named/struct/enum/interface name *plus* the
   *  underlying primitive name (so an extension on `string` defined in `std.string`
   *  is visible without import). Pointer/ref/slice/array element types contribute
   *  too — extensions on `[]u8` defined in `std.bytes` should be visible because
   *  the element is a primitive owned by `std.int`/`std.bytes`/etc. */
  protected def collectReceiverTypeNames(t: SyslType): Set[String] = t match
    case SyslType.StringType                     => Set("string")
    case SyslType.BoolType                       => Set("bool")
    case SyslType.IntType(w)                     => Set(s"i$w", "int")
    case SyslType.UIntType(w)                    => Set(s"u$w", "uint")
    case SyslType.FloatType(w)                   => Set(s"f$w", "float")
    case SyslType.StructType(name, _, _)         => Set(name)
    case SyslType.EnumType(name, _)              => Set(name)
    case SyslType.InterfaceType(name, _)         => Set(name)
    case SyslType.NamedType(name, base, _, _, _) => Set(name) ++ collectReceiverTypeNames(base)
    case SyslType.PtrType(p)                     => collectReceiverTypeNames(p)
    case SyslType.RefType(i)                     => collectReceiverTypeNames(i)
    case SyslType.ArrayType(e, _)                => collectReceiverTypeNames(e)
    case SyslType.SliceType(e)                   => collectReceiverTypeNames(e)
    case _                                       => Set.empty

  /** Phase 2b/2d dispatch: consult `extensionsByMethod` for a visible match,
   *  gated by module visibility. An entry is visible iff its `definingModule`
   *  is empty (same-unit, no module decl), or appears in
   *  `visibleExtensionModules` (own module + every imported module), or
   *  matches the Predef rule (the receiver type is owned by that module —
   *  either via `typeDefiningModule` for user types or
   *  `primitiveDefiningModule` for built-ins).
   *
   *  Generic extensions (Phase 2d) — entries whose synth function lives in
   *  `genericTemplates` — are matched via a structural unification of the
   *  receiver pattern against the call-site type, and resolved through
   *  `instantiateGeneric` so the lowered free function is specialized for
   *  the inferred type args.
   *
   *  Returns Some(call) on a unique visible hit, throws on ambiguity, returns
   *  None when no entry matches. */
  protected def tryExtensionDispatch(method: String, tObj: TExpr, tArgs: List[TExpr]): Option[TExpr] =
    val candidates = extensionsByMethod.get(method).map(_.toList).getOrElse(Nil)
    val recvNames = collectReceiverTypeNames(tObj.typ)
    def isVisible(entry: ExtensionEntry): Boolean =
      val dm = entry.definingModule
      if dm.isEmpty then true
      else if visibleExtensionModules.contains(dm) then true
      else recvNames.exists { n =>
        typeDefiningModule.get(n).contains(dm) || primitiveDefiningModule.get(n).contains(dm)
      }

    // Try to match a candidate's receiver pattern to the call's receiver type.
    // For non-generic candidates, fall through to a direct equality check; for
    // generic candidates, run a structural unification — bindings are discarded
    // here, the real instantiation happens below via `instantiateGeneric`.
    def matchesReceiver(entry: ExtensionEntry): Boolean =
      if genericTemplates.contains(entry.mangledFnName) then
        val template = genericTemplates(entry.mangledFnName)
        val tparams = template.typeParams.toSet
        val env = mutable.Map.empty[String, SyslType]
        try
          unifyTypes(entry.receiverTypeAst, tObj.typ, tparams, env)
          // The receiver pattern's structure must match the call type's outer
          // shape; unifyTypes silently no-ops on shape mismatch (e.g. a slice
          // pattern against a non-slice arg), so verify outer shape here.
          receiverShapeMatches(entry.receiverTypeAst, tObj.typ) &&
            // Every type variable that *appears* in the receiver pattern must
            // be bound — unbound ones can still resolve via remaining args
            // during instantiateGeneric, so we only require the receiver-side
            // params to be pinned at this stage.
            tparams.forall(tp => !receiverPatternUses(entry.receiverTypeAst, tp) || env.contains(tp))
        catch case _: Throwable => false
      else
        try resolveType(entry.receiverTypeAst) == tObj.typ
        catch case _: Throwable => false

    val matches = candidates.filter(isVisible).filter(matchesReceiver)
    matches match
      case Nil => None
      case List(entry) =>
        if genericTemplates.contains(entry.mangledFnName) then
          val argTypes = tObj.typ :: tArgs.map(_.typ)
          val (mangled, funInfo) = instantiateGeneric(entry.mangledFnName, argTypes)
          val checkedArgs = checkArgs(mangled, funInfo.params.tail, tArgs, funInfo.modes.drop(1))
          Some(TCall(funInfo.name, tObj :: checkedArgs, funInfo.returnType))
        else
          val funInfo = functions(entry.mangledFnName)
          val checkedArgs = checkArgs(entry.mangledFnName, funInfo.params.tail, tArgs, funInfo.modes.drop(1))
          Some(TCall(funInfo.name, tObj :: checkedArgs, funInfo.returnType))
      case multiple =>
        throw AnalysisError(
          s"extension method '$method' is ambiguous on receiver type ${tObj.typ}: " +
            s"defined in modules ${multiple.map(_.definingModule).mkString(", ")}")

  /** Outer-shape match for receiver patterns. Each constructor must agree at the
   *  top level; deeper structure is checked recursively. NamedTypeAST that names
   *  a type parameter matches anything; named types must agree by name (their
   *  args are unified separately). */
  protected def receiverShapeMatches(pat: TypeAST, t: SyslType): Boolean = pat match
    case NamedTypeAST(_, _) => true // typevar OR named type — let unifyTypes / resolveType decide
    case PtrTypeAST(inner) => t match
      case SyslType.PtrType(p) => receiverShapeMatches(inner, p)
      case _                   => false
    case PtrNonNullTypeAST(inner) => t match
      case SyslType.PtrType(p) => receiverShapeMatches(inner, p)
      case _                   => false
    case RefTypeAST(inner) => t match
      case SyslType.RefType(i) => receiverShapeMatches(inner, i)
      case _                   => false
    case ArrayTypeAST(_, inner) => t match
      case SyslType.ArrayType(e, _) => receiverShapeMatches(inner, e)
      case _                        => false
    case SliceTypeAST(inner) => t match
      case SyslType.SliceType(e) => receiverShapeMatches(inner, e)
      case _                     => false
    case _ => true

  /** Whether a receiver TypeAST pattern syntactically uses the named type variable. */
  protected def receiverPatternUses(pat: TypeAST, tv: String): Boolean = pat match
    case NamedTypeAST(name, args) => name == tv || args.exists(receiverPatternUses(_, tv))
    case PtrTypeAST(inner)        => receiverPatternUses(inner, tv)
    case PtrNonNullTypeAST(inner) => receiverPatternUses(inner, tv)
    case RefTypeAST(inner)        => receiverPatternUses(inner, tv)
    case ArrayTypeAST(_, inner)   => receiverPatternUses(inner, tv)
    case SliceTypeAST(inner)      => receiverPatternUses(inner, tv)
    case _                        => false

  // Stage F.5 bookkeeping: which module defined each trait / named type. Populated as
  // declarations are processed; consulted by the orphan rule at impl registration.
  protected val traitDefiningModule = new mutable.LinkedHashMap[String, String]
  protected val typeDefiningModule = new mutable.LinkedHashMap[String, String]

  /** Concrete-impl lookup: find a registered impl whose first resolved target is exactly
   *  `operandType` and which has no type params. Used by built-in trait method calls,
   *  concrete operator dispatch, and generic-bound checks. */
  protected def findConcreteImpl(traitName: String, operandType: SyslType): Option[ImplTemplate] =
    implTemplates.get(traitName).flatMap { ts =>
      ts.find(t => t.typeParams.isEmpty && t.resolvedConcrete.flatMap(_.headOption).contains(operandType))
    }

  /** Iterate over all registered concrete impls (legacy shape) for cross-unit serialization
   *  and similar bookkeeping that hasn't been generalized yet. */
  protected def concreteImpls: Iterator[(String, SyslType, mutable.LinkedHashMap[String, String])] =
    implTemplates.iterator.flatMap { case (traitName, ts) =>
      ts.iterator.collect {
        // Single-target concrete impls without associated-type bindings round-trip via
        // TraitImplMeta. Impls *with* assoc bindings ride through the TEMPLATES section
        // instead (since TraitImplMeta has no slot for them) — they're picked up by the
        // driver's templates filter on `assocs.nonEmpty` and the analyzer's
        // `getTraitDecls` / generic-template registration path.
        case t if t.typeParams.isEmpty
            && t.resolvedConcrete.exists(_.length == 1)
            && t.assocBindings.isEmpty =>
          (traitName, t.resolvedConcrete.get.head, t.methods)
      }
    }
