package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerGenerics:
  self: SyslAnalyzer =>

  /** Enumerate every registered impl template of `traitName` whose `methodName` parameter
   *  patterns unify with `argTypes`. Concrete impls fast-path through `==` checks;
   *  generic impls go through `tryUnifyAll` against the raw impl-method param TypeAST.
   *
   *  Returned in registration order. The dispatcher above expects: 0 → no impl, 1 → use,
   *  N>1 → ambiguous.
   */
  protected def enumerateImplCandidates(
      traitName: String,
      methodName: String,
      argTypes: List[SyslType],
  ): List[(ImplTemplate, Map[String, SyslType])] =
    implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
      if t.typeParams.isEmpty then
        // Concrete: compare argTypes against this impl method's bound parameter types.
        // Use either the recorded methodInfo (source-defined impl) or, for cross-unit
        // imports without methodInfos, fall back to `functions(mangled).params`.
        val infoOpt = t.methodInfos.find(_.mangled.endsWith("_" + methodName) || true).find(i =>
          // We index by name embedded in `mangled`; safest is to look up the mangled name first
          t.methods.get(methodName).contains(i.mangled)
        )
        val paramTypes = infoOpt.map(_.paramTypes.map(_._2))
          .orElse(t.methods.get(methodName).flatMap(m => functions.get(m).map(_.params.map(_._2))))
          .orElse(t.methods.get(methodName).flatMap(m => functions.get(shortName(m)).map(_.params.map(_._2))))
        // Compare via `latticeEqual` so a `(T) -> U #pure` actual matches a
        // `(T) -> U` impl-pattern slot — the same lattice rule the generic
        // path already uses in `tryUnifyAll`'s post-validation. Without this,
        // a closure literal (always inferred `#pure` for side-effect-free
        // bodies) would never dispatch through a concrete operator-trait impl.
        paramTypes match
          case Some(ps) if ps.length == argTypes.length &&
              ps.zip(argTypes).forall((p, a) => latticeEqual(p, a)) =>
            Some((t, Map.empty[String, SyslType]))
          case _ => None
      else
        // Generic: unify raw impl method param TypeAST against arg types.
        val implMethod = t.methodASTs.find(_.name == methodName)
        implMethod match
          case Some(im) =>
            val patternTypes = im.params.map(_.typ)
            tryUnifyAll(patternTypes, argTypes, t.typeParams.toSet).map(s => (t, s))
          case None =>
            // Generic impl with synthesized default — derive trait-method param patterns
            // by substituting impl's targetPatterns into the trait method's params, then
            // unify against arg types.
            val trait_ = traits(traitName)
            trait_.methods.find(_.name == methodName).flatMap { tm =>
              // Build a tparam→TypeAST substitution: traitParam → impl's targetPattern at that index
              val traitToImpl = trait_.typeParams.zip(t.targetPatterns).toMap
              val patternTypes = tm.params.map(p => substituteTypeAST(p.typ, traitToImpl))
              tryUnifyAll(patternTypes, argTypes, t.typeParams.toSet).map(s => (t, s))
            }
    }.toList

  // Instantiate a generic struct with concrete type arguments, returning its StructType
  /** Instantiate a `new` generic alias (`type Parser[A] = new (Input) -> ParseResult[A]`)
   *  for a specific list of type args. Each instantiation gets a unique mangled name
   *  (e.g. `Parser_i32`) and is wrapped as a nominal `NamedType` so it is distinct from
   *  both its underlying base and from other instantiations.
   *
   *  Cached so that two mentions of `Parser[i32]` produce object-equal `SyslType` values —
   *  this is what makes trait/impl dispatch see them as the same type.
   */
  /** Phase B — fill missing trailing type-arg slots from declared defaults.
   *  Defaults resolve under the partial env of already-pinned slots so a later
   *  default may reference an earlier param (e.g. `[I, O = I]`). The returned
   *  list always has length `tparams.length`. Errors if `typeArgs.length` is
   *  greater than `tparams.length`, or if a missing slot has no default.
   *  `kind` is just for the diagnostic ("struct" / "enum" / "type alias"). */
  protected def fillTypeArgsFromDefaults(
      name: String,
      kind: String,
      tparams: List[String],
      defaults: Map[String, TypeAST],
      typeArgs: List[SyslType],
  ): List[SyslType] =
    if typeArgs.length == tparams.length then return typeArgs
    if typeArgs.length > tparams.length then
      throw AnalysisError(s"generic $kind '$name' expects ${tparams.length} type arg(s), got ${typeArgs.length}")
    val pinned = mutable.Map.empty[String, SyslType]
    for (tp, ty) <- tparams.zip(typeArgs) do pinned(tp) = ty
    for tp <- tparams.drop(typeArgs.length) do
      defaults.get(tp) match
        case Some(defAst) =>
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ pinned.toMap
          try pinned(tp) = resolveType(defAst)
          finally typeEnv = savedEnv
        case None =>
          throw AnalysisError(s"generic $kind '$name' expects ${tparams.length} type arg(s), got ${typeArgs.length} (no default for type parameter '$tp')")
    tparams.map(pinned)

  /** Validate that `typeArgs` satisfy the trait bounds declared on
   *  `typeParams`, mirroring the bound check in `instantiateGeneric`. Returns
   *  Left(msg) on failure, Right(()) on success. Multi-bound disambiguation
   *  (when two bounds bind the same assoc name to different types) is detected
   *  here too. The companion `buildAssocBindingsEnv` constructs the matching
   *  assocBindingsEnv map. Both are used by struct/enum/alias instantiation. */
  protected def enforceBoundsAndBuildAssocs(
      what: String,
      typeParams: List[String],
      typeArgs: List[SyslType],
      typeBounds: Map[String, List[String]],
  ): Either[String, Unit] =
    val tpIter = typeParams.iterator
    while tpIter.hasNext do
      val tp = tpIter.next()
      val bounds = typeBounds.getOrElse(tp, Nil)
      val concreteType = typeArgs(typeParams.indexOf(tp))
      val tnIter = bounds.iterator
      while tnIter.hasNext do
        val traitName = tnIter.next()
        if !traits.contains(traitName) then
          return Left(s"bound '$traitName' on type parameter '$tp' of $what refers to unknown trait")
        val matched = implTemplates.getOrElse(traitName, Nil).exists { t =>
          if t.typeParams.isEmpty then
            t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
          else
            tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet).isDefined
        }
        if !matched then
          return Left(s"type $concreteType does not satisfy bound '$traitName' for type parameter '$tp' in $what")
    Right(())

  /** Build the assoc-bindings env for `typeParams` instantiated to `typeArgs`,
   *  by locating each bounded type parameter's matching impl and pulling its
   *  assocBindings into the env. Mirrors the same logic in `instantiateGeneric`,
   *  including multi-bound ambiguity rejection. */
  protected def buildAssocBindingsEnv(
      typeParams: List[String],
      typeArgs: List[SyslType],
      typeBounds: Map[String, List[String]],
  ): Map[String, SyslType] =
    val newAssocs = mutable.Map.empty[String, SyslType]
    val newAssocSources = mutable.Map.empty[String, String]
    for (tp, concreteType) <- typeParams.zip(typeArgs) do
      val bounds = typeBounds.getOrElse(tp, Nil)
      for traitName <- bounds do
        val matched = implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
          if t.typeParams.isEmpty then
            if t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
            then Some((t, Map.empty[String, SyslType]))
            else None
          else
            tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet)
              .map(s => (t, s))
        }.nextOption()
        for (impl, implSubst) <- matched do
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ implSubst
          try
            for binding <- impl.assocBindings do
              val resolved = resolveType(binding.target)
              newAssocs.get(binding.name) match
                case Some(prior) if prior != resolved =>
                  val priorTrait = newAssocSources.getOrElse(binding.name, "?")
                  throw AnalysisError(
                    s"projection '${tp}::${binding.name}' is ambiguous: " +
                      s"bound '$priorTrait' binds it to $prior but bound '$traitName' binds it to $resolved",
                  )
                case _ =>
                  newAssocs(binding.name) = resolved
                  newAssocSources(binding.name) = traitName
          finally typeEnv = savedEnv
    newAssocs.toMap

  protected def instantiateGenericNominalAlias(
      name: String,
      tparams: List[String],
      target: TypeAST,
      typeArgs: List[SyslType],
  ): SyslType =
    val defaults = genericTypeAliasDefaults.getOrElse(name, Map.empty)
    val bounds = genericTypeAliasBounds.getOrElse(name, Map.empty)
    val filledArgs = fillTypeArgsFromDefaults(name, "type alias", tparams, defaults, typeArgs)
    val cacheKey = (name, filledArgs)
    genericAliasInstantiations.get(cacheKey) match
      case Some(t) => t
      case None =>
        val typeArgs = filledArgs
        // Enforce trait bounds declared on the alias's type parameters (Phase C
        // follow-up) and bring projection assocs into scope while resolving the
        // alias body, so `Parser[I: Input, A] = (I) -> ParseResult[I, A]` can
        // resolve `I::Elem` references inside the target type.
        enforceBoundsAndBuildAssocs(s"type alias '$name'", tparams, typeArgs, bounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ tparams.zip(typeArgs).toMap
        assocBindingsEnv = buildAssocBindingsEnv(tparams, typeArgs, bounds)
        val base =
          try resolveType(target)
          finally
            typeEnv = savedEnv
            assocBindingsEnv = savedAssocs
        val nt = SyslType.NamedType(mangled, base, nominal = true, range = None, predicateFunc = None)
        genericAliasInstantiations(cacheKey) = nt
        genericAliasToTemplate(mangled) = (name, typeArgs)
        nt

  protected def instantiateGenericStruct(name: String, typeArgs: List[SyslType]): SyslType.StructType =
    val template0 = genericStructs.getOrElse(name,
      throw AnalysisError(s"'$name' is not a generic struct"))
    val filledArgs = fillTypeArgsFromDefaults(name, "struct", template0.typeParams, template0.typeParamDefaults, typeArgs)
    val cacheKey = (name, filledArgs)
    genericStructInstantiations.get(cacheKey) match
      case Some(st) => st
      case None =>
        val template = template0
        val typeArgs = filledArgs
        // Enforce trait bounds declared on the struct's type parameters and
        // build the assoc-bindings env so projection-typed fields like
        // `pulled: T::Item` resolve via the matching impl.
        enforceBoundsAndBuildAssocs(s"struct '$name'", template.typeParams, typeArgs, template.typeBounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        // Insert a placeholder StructType to handle recursive field types
        val placeholder: SyslType.StructType = SyslType.StructType(mangled, Nil)
        genericStructInstantiations(cacheKey) = placeholder
        structTypes(mangled) = placeholder
        structToTemplate(mangled) = (name, typeArgs)
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ template.typeParams.zip(typeArgs).toMap
        assocBindingsEnv = buildAssocBindingsEnv(template.typeParams, typeArgs, template.typeBounds)
        try
          val resolvedFields = template.fields.map((n, t, _) => (n, resolveType(t)))
          val st: SyslType.StructType = SyslType.StructType(mangled, resolvedFields)
          genericStructInstantiations(cacheKey) = st
          structTypes(mangled) = st
          specializedDecls += TStructDecl(mangled, resolvedFields)
          st
        finally
          typeEnv = savedEnv
          assocBindingsEnv = savedAssocs

  // Instantiate a generic enum with concrete type arguments, returning its EnumType
  protected def instantiateGenericEnum(name: String, typeArgs: List[SyslType]): SyslType.EnumType =
    val template0 = genericEnums(name)
    val filledArgs = fillTypeArgsFromDefaults(name, "enum", template0.typeParams, template0.typeParamDefaults, typeArgs)
    val cacheKey = (name, filledArgs)
    genericEnumInstantiations.get(cacheKey) match
      case Some(et) => et
      case None =>
        val template = template0
        val typeArgs = filledArgs
        // Enforce trait bounds declared on the enum's type parameters (Phase C
        // follow-up) and build the assoc-bindings env so projection-typed variant
        // fields like `pulled: T::Item` resolve via the matching impl.
        enforceBoundsAndBuildAssocs(s"enum '$name'", template.typeParams, typeArgs, template.typeBounds) match
          case Left(err) => throw AnalysisError(err)
          case Right(_)  => ()
        val mangled = name + "_" + typeArgs.map(typeToMangled).mkString("_")
        val savedEnv = typeEnv
        val savedAssocs = assocBindingsEnv
        typeEnv = typeEnv ++ template.typeParams.zip(typeArgs).toMap
        assocBindingsEnv = buildAssocBindingsEnv(template.typeParams, typeArgs, template.typeBounds)
        try
          val resolvedVariants = template.variants.map { case EnumVariantAST(vname, fields) =>
            val resolvedFields = fields.map((fname, ftype) => (fname, resolveType(ftype)))
            (vname, resolvedFields)
          }
          val et: SyslType.EnumType = SyslType.EnumType(mangled, resolvedVariants)
          genericEnumInstantiations(cacheKey) = et
          dataEnumTypes(mangled) = et
          enumToTemplate(mangled) = (name, typeArgs)
          specializedDecls += TDataEnumDecl(mangled, et)
          et
        finally
          typeEnv = savedEnv
          assocBindingsEnv = savedAssocs

  // Analyze each impl method (including synthesized defaults) as a mangled top-level function
  protected def analyzeImplMethods(impl: ImplDeclAST): List[TDecl] =
    // Generic impls (typeParams.nonEmpty) are NOT analyzed at registration time —
    // they're specialized on demand at each dispatch site by `instantiateImpl`.
    if impl.typeParams.nonEmpty then return Nil
    val resolvedTargets = impl.targetTypes.map(resolveType)
    val template = implTemplates.getOrElse(impl.traitName, Nil).find(t =>
      t.typeParams.isEmpty && t.resolvedConcrete.contains(resolvedTargets)
    ).getOrElse(
      throw AnalysisError(s"internal: impl of '${impl.traitName}' for ${resolvedTargets.mkString(", ")} not registered"))
    val methodMap = template.methods
    val infos = template.methodInfos
    val trait_ = traits(impl.traitName)
    val savedEnv = typeEnv
    val savedRewrite = traitCallRewrite
    val savedAssocs = assocBindingsEnv
    // Activate the impl's assoc bindings so projection types in trait method
    // bodies (`var t: Self::Item = ...`, casts, etc.) resolve correctly when
    // analyzing an inherited default body. The set is empty for impls without
    // assoc types, in which case projection use is rejected as before.
    assocBindingsEnv = template.assocBindings.map(b => (b.name, resolveType(b.target))).toMap
    try
      // For synthesized defaults, set typeEnv + traitCallRewrite so trait params resolve
      // and unqualified calls to sibling trait methods route to the impl's mangled
      // functions. Multi-param traits get a multi-entry env; the convention scales.
      infos.map { info =>
        if info.isSynthesized then
          typeEnv = trait_.typeParams.zip(resolvedTargets).toMap
          traitCallRewrite = methodMap.toMap
        else
          typeEnv = savedEnv
          traitCallRewrite = savedRewrite
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        // Determine which params are by-name from the original impl-method AST
        // (or trait method, for synthesized defaults). Body references to
        // by-name params must auto-call; that's enforced via SymInfo.isByName.
        // ImplMethodInfo has no `name` field; reverse-lookup via methodMap.
        val originalName: Option[String] =
          methodMap.collectFirst { case (n, m) if m == info.mangled => n }
        val sourceParams: List[ParamAST] =
          originalName match
            case Some(n) =>
              impl.methods.find(_.name == n).map(_.params)
                .orElse(trait_.methods.find(_.name == n).map(_.params))
                .getOrElse(Nil)
            case None => Nil
        val byNameFlags: List[Boolean] =
          if sourceParams.length == info.paramTypes.length then
            sourceParams.map(_.typ.isInstanceOf[ByNameTypeAST])
          else List.fill(info.paramTypes.length)(false)
        for (((paramName, paramType), idx) <- info.paramTypes.zipWithIndex) do
          val isByN = idx < byNameFlags.length && byNameFlags(idx)
          val visibleType = paramType match
            case FuncType(Nil, ret, _, _) if isByN => ret
            case other => other
          currentScope(paramName) = SymInfo(paramName, visibleType, true, isByName = isByN)
        // Persist by-name flags onto the impl method's FunInfo so direct CallAST
        // dispatch (Trait.method(..) routed through traitCallRewrite, or direct
        // mangled-name calls) auto-wraps args at by-name slots.
        if byNameFlags.exists(identity) then
          functions.get(info.mangled).foreach { fi =>
            functions(info.mangled) = fi.copy(byName = byNameFlags)
          }
        val tBody = info.body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
        val tParams = info.paramTypes.map((n, t) => TParam(n, t))
        scopeStack = null
        TFunDecl(info.mangled, tParams, info.retType, tBody, isPrivate = false)
      }
    finally
      typeEnv = savedEnv
      traitCallRewrite = savedRewrite
      assocBindingsEnv = savedAssocs

  /** Stage G — specialize a generic impl method at a use site, or fast-path return for a
   *  concrete impl. Cached by `(template-identity, methodName, sortedSubst)` so repeated
   *  dispatches with the same operand types reuse one mangled function.
   *
   *  For concrete impls (`subst.isEmpty`), looks up the pre-mangled function. For generic
   *  impls, builds a typeEnv from `trait.typeParams ++ impl.typeParams` (the impl's vars
   *  also need to be in scope — patterns like `Bar[X]` mention `X`), resolves param/return
   *  types, mangles by hash of the substitution, registers a TFunDecl, and emits it via
   *  `specializedDecls`.
   */
  protected def instantiateImpl(
      template: ImplTemplate,
      traitName: String,
      methodName: String,
      subst: Map[String, SyslType],
  ): (String, FunInfo) =
    if template.typeParams.isEmpty then
      // Concrete: pre-registered at impl-declaration time
      val mangled = template.methods.getOrElse(methodName,
        throw AnalysisError(s"trait method '$traitName.$methodName' not implemented in this impl"))
      val funInfo = functions.getOrElse(mangled,
        functions.getOrElse(shortName(mangled),
          throw AnalysisError(s"trait method '$traitName.$methodName' resolved to '$mangled' but function not found")))
      (mangled, funInfo)
    else
      // Generic: specialize on demand. Cache key includes the substitution applied to
      // every type variable (sorted for determinism) plus the template identity.
      val sortedSubst = template.typeParams.map(tp => tp -> subst(tp))
      val cacheKey = (System.identityHashCode(template), methodName, sortedSubst)
      genericImplInstantiations.get(cacheKey) match
        case Some((m, fi)) => (m, fi)
        case None =>
          val trait_ = traits(traitName)
          // Phase C follow-up — enforce trait bounds on the impl's type
          // parameters at the moment a concrete substitution is selected. For
          // each impl tparam with bounds, verify that subst(tparam) is implemented
          // by some impl of every bound trait. Mirrors `enforceBoundsAndBuildAssocs`
          // shape but without assoc-binding env construction — those flow through
          // the impl's own assocBindings.
          for (tp, bounds) <- template.typeBounds do
            val concrete = subst.getOrElse(tp,
              throw AnalysisError(s"impl bound check: type parameter '$tp' has no substitution"))
            for boundTrait <- bounds do
              val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                if t.typeParams.isEmpty then
                  t.resolvedConcrete.flatMap(_.headOption).contains(concrete)
                else
                  tryUnifyAll(t.targetPatterns.headOption.toList, List(concrete), t.typeParams.toSet).isDefined
              }
              if !matched then
                throw AnalysisError(s"type $concrete does not satisfy bound '$boundTrait' for impl type parameter '$tp' of trait '$traitName'")
          // Phase C follow-up — enforce trait bounds on the trait's own type
          // parameters at the moment the impl is selected. For each trait
          // tparam with bounds, the corresponding resolved target (under the
          // impl's substitution) must satisfy each bound. Concrete impls
          // checked this at registration; generic impls defer here because
          // their target patterns may reference impl tvars whose substitution
          // wasn't known until now.
          if trait_.typeBounds.nonEmpty then
            val savedEnvForTraitBounds = typeEnv
            typeEnv = typeEnv ++ subst
            try
              for ((tp, pat) <- trait_.typeParams.zip(template.targetPatterns)) do
                val tBounds = trait_.typeBounds.getOrElse(tp, Nil)
                if tBounds.nonEmpty then
                  val resolvedPat = resolveType(pat)
                  for boundTrait <- tBounds do
                    val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                      if t.typeParams.isEmpty then
                        t.resolvedConcrete.flatMap(_.headOption).contains(resolvedPat)
                      else
                        tryUnifyAll(t.targetPatterns.headOption.toList, List(resolvedPat), t.typeParams.toSet).isDefined
                    }
                    if !matched then
                      throw AnalysisError(s"impl of '$traitName' for $resolvedPat does not satisfy bound '$boundTrait' declared on type parameter '$tp'")
            finally typeEnv = savedEnvForTraitBounds
          // Phase C.2 — enforce trait bounds on associated-type bindings for
          // generic impls. The concrete-impl path checks at registration; for
          // generic impls the binding target may reference impl tvars (e.g.
          // `type Token = T`), so we defer to here where `subst` is known.
          // Resolve each binding under the substitution and validate against
          // the trait's declared assoc bounds.
          if template.assocBindings.nonEmpty && trait_.assocTypes.exists(_.bounds.nonEmpty) then
            val assocBoundsByName = trait_.assocTypes.map(a => (a.name, a.bounds)).toMap
            val savedEnvForAssocBounds = typeEnv
            typeEnv = typeEnv ++ subst
            try
              for b <- template.assocBindings do
                val bounds = assocBoundsByName.getOrElse(b.name, Nil)
                if bounds.nonEmpty then
                  val resolvedBindingTarget = resolveType(b.target)
                  for boundTrait <- bounds do
                    if !traits.contains(boundTrait) then
                      throw AnalysisError(s"associated type 'type ${b.name}' on trait '$traitName' references unknown trait '$boundTrait'")
                    val matched = implTemplates.getOrElse(boundTrait, Nil).exists { t =>
                      if t.typeParams.isEmpty then
                        t.resolvedConcrete.flatMap(_.headOption).contains(resolvedBindingTarget)
                      else
                        tryUnifyAll(t.targetPatterns.headOption.toList, List(resolvedBindingTarget), t.typeParams.toSet).isDefined
                    }
                    if !matched then
                      throw AnalysisError(s"generic impl of '$traitName' binds 'type ${b.name} = $resolvedBindingTarget' (under substitution), which does not satisfy bound '$boundTrait' declared on the associated type")
            finally typeEnv = savedEnvForAssocBounds
          // Resolve the trait's targetPatterns under the new substitution to obtain the
          // concrete trait-level types — these become the trait typeParam → concrete map.
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ subst
          val resolvedTargets =
            try template.targetPatterns.map(resolveType)
            finally typeEnv = savedEnv
          val typeMangled = resolvedTargets.map(typeToMangled).mkString("_")
          val implMethod = template.methodASTs.find(_.name == methodName).getOrElse {
            // Method not provided by impl — must be a synthesized default from the trait
            val tm = trait_.methods.find(_.name == methodName).getOrElse(
              throw AnalysisError(s"trait '$traitName' has no method '$methodName'"))
            // Synthesize a FunDeclAST from the trait method's default body
            val body = tm.body.getOrElse(
              throw AnalysisError(s"impl missing required method '$methodName' (no default in trait)"))
            FunDeclAST(tm.name, tm.params, Some(tm.returnType), body)
          }
          val rawMangled = s"${traitName}_${methodName}_${typeMangled}"
          val mangled = if shouldMangle(rawMangled) then mangleName(rawMangled) else rawMangled
          // Build the full typeEnv: trait type params bound to resolved targets, plus
          // impl type params bound to subst (so patterns like `Bar[X]` referenced inside
          // the method resolve correctly).
          val fullEnv = trait_.typeParams.zip(resolvedTargets).toMap ++ subst
          typeEnv = typeEnv ++ fullEnv
          // Activate the impl's assoc bindings — resolved under the substituted typeEnv
          // so generic-impl bindings like `type Item = T` land as the substituted type.
          val savedAssocs = assocBindingsEnv
          assocBindingsEnv = template.assocBindings.map(b => (b.name, resolveType(b.target))).toMap
          val (paramTypes, retType, body, isSynthesized) =
            try
              val pTypes = implMethod.params.map(p => (p.name, resolveType(p.typ)))
              val r = implMethod.returnType.map(resolveType).getOrElse(UnitType)
              val provided = template.methodASTs.exists(_.name == methodName)
              (pTypes, r, implMethod.body, !provided)
            finally typeEnv = savedEnv
          val byNameFlags: List[Boolean] = implMethod.params.map(_.typ.isInstanceOf[ByNameTypeAST])
          val funInfo = FunInfo(mangled, paramTypes, retType, byName = if byNameFlags.exists(identity) then byNameFlags else Nil)
          functions(mangled) = funInfo
          // Save into template.methods so cross-method dispatch (e.g. default methods that
          // call sibling methods) finds the same specialization.
          template.methods(methodName) = mangled
          genericImplInstantiations(cacheKey) = (mangled, funInfo)
          // Analyze the body in the substituted env, with traitCallRewrite set so default
          // methods route sibling trait-method calls to this impl's mangled functions.
          val savedRewrite = traitCallRewrite
          val savedScope = scopeStack
          val savedLoopDepth = loopDepth
          typeEnv = typeEnv ++ fullEnv
          if isSynthesized then traitCallRewrite = template.methods.toMap
          scopeStack = new mutable.ArrayBuffer
          loopDepth = 0
          pushScope()
          for (((paramName, paramType), idx) <- paramTypes.zipWithIndex) do
            val isByN = idx < byNameFlags.length && byNameFlags(idx)
            val visibleType = paramType match
              case FuncType(Nil, ret, _, _) if isByN => ret
              case other => other
            currentScope(paramName) = SymInfo(paramName, visibleType, true, isByName = isByN)
          val tBody =
            try body match
              case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
              case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
            finally
              typeEnv = savedEnv
              traitCallRewrite = savedRewrite
              scopeStack = savedScope
              loopDepth = savedLoopDepth
              assocBindingsEnv = savedAssocs
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, isPrivate = false)
          (mangled, funInfo)

  protected val genericImplInstantiations = new mutable.HashMap[(Int, String, List[(String, SyslType)]), (String, FunInfo)]

  // Resolve a trait method call like Ord.cmp(a, b) to the appropriate impl's mangled function.
  // Stage G: enumerates candidates by unifying each impl's method param patterns directly
  // against the call's arg types. Result-position trait params (e.g. R in Concat[A, B, R])
  // don't need to be inferable from arg types — they're determined by which impl matches.
  protected def analyzeTraitCall(traitName: String, methodName: String, tArgs: List[TExpr]): (String, FunInfo) =
    val trait_ = traits(traitName)
    val method = trait_.methods.find(_.name == methodName).getOrElse(
      throw AnalysisError(s"trait '$traitName' has no method '$methodName'"))
    if method.params.length != tArgs.length then
      throw AnalysisError(s"trait method '$traitName.$methodName' expects ${method.params.length} argument(s), got ${tArgs.length}")
    val argTypes = tArgs.map(_.typ)
    val candidates = enumerateImplCandidates(traitName, methodName, argTypes)
    candidates match
      case Nil =>
        throw AnalysisError(s"no impl of trait '$traitName.$methodName' matches arg type(s) ${argTypes.mkString(", ")}")
      case (template, subst) :: Nil =>
        instantiateImpl(template, traitName, methodName, subst)
      case multi =>
        throw AnalysisError(s"ambiguous: ${multi.length} impls of '$traitName' match $methodName(${argTypes.mkString(", ")})")

  protected def instantiateGeneric(
      name: String,
      argTypes: List[SyslType],
      explicitTypeArgs: List[SyslType] = Nil,
  ): (String, FunInfo) =
    val template = genericTemplates(name)
    val typeParams = template.typeParams
    // Infer type arguments (explicit type args from `f[T](...)` pre-seed the env)
    val env = mutable.Map.empty[String, SyslType]
    if explicitTypeArgs.nonEmpty then
      if explicitTypeArgs.length != typeParams.length then
        throw AnalysisError(
          s"generic function '$name' expects ${typeParams.length} type argument(s), got ${explicitTypeArgs.length}",
        )
      for (tp, ty) <- typeParams.zip(explicitTypeArgs) do env(tp) = ty
    if template.params.length != argTypes.length then
      throw AnalysisError(s"generic function '$name' expects ${template.params.length} argument(s), got ${argTypes.length}")
    for (p, a) <- template.params.zip(argTypes) do
      unifyTypes(p.typ, a, typeParams.toSet, env)
    // Phase B — fill any still-unbound type parameters from declared defaults.
    // Defaults resolve under the partial env so a later default may reference an
    // earlier param (e.g. `[I, O = I]`). Iterate in declaration order so
    // earlier-pinned params are visible to later defaults.
    // Phase B.2 — defaults may also reference the projections of already-pinned
    // bounded params (e.g. `[I: Reader, T = I::Token]`). Before resolving each
    // default, build a partial assocBindingsEnv from the params pinned so far.
    for tp <- typeParams if !env.contains(tp) do
      template.typeParamDefaults.get(tp) match
        case Some(defAst) =>
          val savedEnvB = typeEnv
          val savedAssocsB = assocBindingsEnv
          typeEnv = typeEnv ++ env.toMap
          val pinnedSoFar = typeParams.takeWhile(env.contains)
          assocBindingsEnv = buildAssocBindingsEnv(
            pinnedSoFar,
            pinnedSoFar.map(env(_)),
            template.typeBounds,
          )
          try env(tp) = resolveType(defAst)
          finally
            typeEnv = savedEnvB
            assocBindingsEnv = savedAssocsB
        case None => // leave unbound; the next pass produces the diagnostic
    // Require all type params to be pinned
    for tp <- typeParams if !env.contains(tp) do
      throw AnalysisError(s"cannot infer type parameter '$tp' for generic function '$name'")
    val inferredArgs = typeParams.map(env(_))
    // Check trait bounds
    for tp <- typeParams do
      val bounds = template.typeBounds.getOrElse(tp, Nil)
      val concreteType = env(tp)
      for traitName <- bounds do
        if !traits.contains(traitName) then
          throw AnalysisError(s"bound '$traitName' on type parameter '$tp' of '$name' refers to unknown trait")
        // Bound is satisfied if the trait has any registered impl whose first target
        // pattern unifies with `concreteType` (concrete fast-path or generic impl).
        val trait_ = traits(traitName)
        val matched = implTemplates.getOrElse(traitName, Nil).exists { t =>
          if t.typeParams.isEmpty then
            t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
          else
            tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet).isDefined
        }
        if !matched then
          throw AnalysisError(s"type $concreteType does not satisfy bound '$traitName' for type parameter '$tp' in call to '$name'")
    val cacheKey = (name, inferredArgs)
    instantiations.get(cacheKey) match
      case Some(mangled) => (mangled, functions(mangled))
      case None =>
        val mangled = mangleGenericName(name, inferredArgs)
        if functions.contains(mangled) && !externalSymbols.contains(mangled) && !externalSymbols.contains(shortName(mangled)) then
          // Already instantiated locally — reuse it
          instantiations(cacheKey) = mangled
          return (mangled, functions(mangled))
        // If the function exists as an imported symbol, we still need to re-instantiate
        // locally so the backend emits its body in this compilation unit.
        // Save and install typeEnv for this instantiation
        val savedEnv = typeEnv
        val savedAssocsInst = assocBindingsEnv
        typeEnv = typeParams.zip(inferredArgs).toMap
        // Phase A3 — for each bounded type parameter, locate the matching impl
        // and pull its associated-type bindings into `assocBindingsEnv` so that
        // projections like `T::Item` in the generic body resolve to the impl's
        // declared type. The bound check above already validated that a matching
        // impl exists; we re-find it here to capture both the impl template and
        // (for generic impls) the substitution that pins its type parameters.
        // Multi-bound disambiguation: if two distinct bound traits both bind the
        // same assoc name to *different* resolved types, the projection is
        // ambiguous and we reject the call. Repeated identical bindings (same
        // name, same resolved type) are fine.
        val newAssocs = mutable.Map.empty[String, SyslType]
        val newAssocSources = mutable.Map.empty[String, String]
        for tp <- typeParams do
          val bounds = template.typeBounds.getOrElse(tp, Nil)
          val concreteType = env(tp)
          for traitName <- bounds do
            val matched = implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
              if t.typeParams.isEmpty then
                if t.resolvedConcrete.flatMap(_.headOption).contains(concreteType)
                then Some((t, Map.empty[String, SyslType]))
                else None
              else
                tryUnifyAll(t.targetPatterns.headOption.toList, List(concreteType), t.typeParams.toSet)
                  .map(s => (t, s))
            }.nextOption()
            for (impl, implSubst) <- matched do
              val savedEnv2 = typeEnv
              typeEnv = typeEnv ++ implSubst
              try
                for binding <- impl.assocBindings do
                  val resolved = resolveType(binding.target)
                  newAssocs.get(binding.name) match
                    case Some(prior) if prior != resolved =>
                      val priorTrait = newAssocSources.getOrElse(binding.name, "?")
                      throw AnalysisError(
                        s"projection '${tp}::${binding.name}' is ambiguous in '$name': " +
                          s"bound '$priorTrait' binds it to $prior but bound '$traitName' binds it to $resolved",
                      )
                    case _ =>
                      newAssocs(binding.name) = resolved
                      newAssocSources(binding.name) = traitName
              finally typeEnv = savedEnv2
        assocBindingsEnv = newAssocs.toMap
        try
          // Resolve param/return types in the new env
          val paramTypes = template.params.map(p => (p.name, resolveType(p.typ)))
          val retType = template.returnType.map(resolveType).getOrElse(UnitType)
          val funInfo = FunInfo(mangled, paramTypes, retType)
          // Register before analyzing body to support recursion
          functions(mangled) = funInfo
          instantiations(cacheKey) = mangled
          // Analyze body in a fresh scope, using the mangled name
          val savedScopeStack = scopeStack
          val savedLoopDepth = loopDepth
          scopeStack = new mutable.ArrayBuffer
          loopDepth = 0
          pushScope()
          for (paramName, paramType) <- paramTypes do
            currentScope(paramName) = SymInfo(paramName, paramType, true)
            // Auto-alias `self` -> `__self__` for generic methods
            if paramName == "__self__" then
              currentScope("self") = SymInfo(paramName, paramType, true)
          val savedExpectedInst = currentExpected
          // Use only the *result* R of `func(...) -> R`, not the full function type, so nested
          // closures still treat outer parameters (e.g. alt's `a`, `b`) as captures rather than
          // mis-reading `func(string,int)->…` as the expected shape of a single-arg closure.
          currentExpected = template.returnType.map(resolveType).map {
            case ft: FuncType => ft.returnType
            case t => t
          }
          val tBody = try
            template.body match
              case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
              case BlockBodyAST(stmts, _) => TBlockBody(analyzeBlock(stmts))
          finally
            currentExpected = savedExpectedInst
          scopeStack = savedScopeStack
          loopDepth = savedLoopDepth
          val tParams = paramTypes.map((n, t) => TParam(n, t))
          specializedDecls += TFunDecl(mangled, tParams, retType, tBody, template.isPrivate)
          (mangled, funInfo)
        finally
          typeEnv = savedEnv
          assocBindingsEnv = savedAssocsInst
