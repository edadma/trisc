package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerUnification:
  self: SyslAnalyzer =>

  /** Merge two concrete-type observations of the same generic type variable,
   *  taking the lattice LUB on `FuncType` effects rather than demanding
   *  structural equality. Recurses through container types (slice / array /
   *  ptr / ref / struct / named) so an embedded `FuncType` anywhere in the
   *  shape uses the lattice. Returns `None` when the two types are
   *  structurally incompatible or carry incomparable effect annotations.
   *
   *  This is the inference-engine analogue of `latticeEqual`: that one decides
   *  *whether* a single observation flows into a slot; this one decides *what*
   *  binding to pick when several observations of the same type variable
   *  appear at different use sites. The merged binding is wide enough that
   *  every original observation still satisfies it as a slot.
   */
  protected def mergeBindings(t1: SyslType, t2: SyslType): Option[SyslType] =
    if t1 == t2 then Some(t1)
    else
      (t1, t2) match
        case (FuncType(p1, r1, esc1, eff1), FuncType(p2, r2, _, eff2)) if p1.length == p2.length =>
          val mergedParams = p1.zip(p2).map((a, b) => mergeBindings(a, b))
          if mergedParams.exists(_.isEmpty) then None
          else
            mergeBindings(r1, r2).flatMap { mr =>
              lubEffect(eff1, eff2).map { eff =>
                FuncType(mergedParams.map(_.get), mr, esc1, eff)
              }
            }
        case (SliceType(a), SliceType(b)) => mergeBindings(a, b).map(SliceType.apply)
        case (ArrayType(a, n1), ArrayType(b, n2)) if n1 == n2 =>
          mergeBindings(a, b).map(ArrayType(_, n1))
        case (PtrType(a), PtrType(b)) => mergeBindings(a, b).map(PtrType.apply)
        case (RefType(a), RefType(b)) => mergeBindings(a, b).map(RefType.apply)
        case (StructType(n1, f1, v1), StructType(n2, f2, v2))
            if n1 == n2 && v1 == v2 && f1.length == f2.length &&
              f1.zip(f2).forall { case ((fn1, _), (fn2, _)) => fn1 == fn2 } =>
          val merged = f1.zip(f2).map { case ((fn, ft1), (_, ft2)) => mergeBindings(ft1, ft2).map((fn, _)) }
          if merged.exists(_.isEmpty) then None
          else Some(StructType(n1, merged.map(_.get), v1))
        case (NamedType(n1, u1, nom1, ro1, pr1), NamedType(n2, u2, nom2, ro2, pr2))
            if n1 == n2 && nom1 == nom2 && ro1 == ro2 && pr1 == pr2 =>
          mergeBindings(u1, u2).map(NamedType(n1, _, nom1, ro1, pr1))
        case _ => None

  // Unify a parameter TypeAST (which may contain type variables) against a concrete SyslType,
  // recording type variable bindings. Returns true if unification succeeded structurally.
  protected def unifyTypes(param: TypeAST, arg: SyslType, typeParams: Set[String], env: mutable.Map[String, SyslType]): Unit =
    param match
      case NamedTypeAST(name, _) if typeParams.contains(name) =>
        env.get(name) match
          case Some(existing) if existing == arg => ()
          case Some(existing) =>
            // Lattice merge: two concrete observations of the same type variable
            // are compatible iff one is ≤ the other under the effect/structure
            // lattice. The merged binding is the GLB (more-specific). Without
            // this, any combinator-library call where the user supplies a
            // literal closure (auto-`#pure`) and the same type variable is also
            // constrained by an unannotated context is rejected.
            mergeBindings(existing, arg) match
              case Some(merged) => env(name) = merged
              case None =>
                throw AnalysisError(s"cannot infer type parameter '$name': seen both $existing and $arg")
          case None => env(name) = arg
      case PtrTypeAST(inner) => arg match
        case PtrType(a) => unifyTypes(inner, a, typeParams, env)
        case _ => () // type mismatch handled later by checkArgs
      case RefTypeAST(inner) => arg match
        case RefType(a) => unifyTypes(inner, a, typeParams, env)
        case _ => ()
      case ArrayTypeAST(_, inner) => arg match
        case ArrayType(a, _) => unifyTypes(inner, a, typeParams, env)
        case _ => ()
      case SliceTypeAST(inner) => arg match
        case SliceType(a) => unifyTypes(inner, a, typeParams, env)
        case _ => ()
      case FuncTypeAST(paramTypes, ret, _, _) => arg match
        case FuncType(argParams, argRet, _, _) =>
          if paramTypes.length == argParams.length then
            for (pt, at) <- paramTypes.zip(argParams) do unifyTypes(pt, at, typeParams, env)
          unifyTypes(ret, argRet, typeParams, env)
        case _ => ()
      case ByNameTypeAST(inner) => arg match
        // `=> T` desugars to `() -> T`. Treat it identically here so impl
        // patterns with by-name slots unify against call-site args that have
        // already been auto-wrapped to a zero-arg thunk.
        case FuncType(Nil, argRet, _, _) => unifyTypes(inner, argRet, typeParams, env)
        case _ => () // structural mismatch handled later by tryUnifyAll's post-check
      case TupleTypeAST(elems) => arg match
        case StructType(_, fields, _) if elems.length == fields.length =>
          for (e, (_, ft)) <- elems.zip(fields) do unifyTypes(e, ft, typeParams, env)
        case _ => ()
      case NamedTypeAST(name, tArgs) if tArgs.nonEmpty =>
        // If this is a generic type alias, expand it and unify the expanded type
        if genericTypeAliases.contains(name) then
          val (tparams, target, isNew) = genericTypeAliases(name)
          if tArgs.length == tparams.length then
            if isNew then
              // Nominal generic alias: gate on `genericAliasToTemplate` so the
              // actual's mangled name still has to resolve back to *this* template
              // (otherwise `Parser2[A]` would unify against a `Parser[T]`-shaped
              // underlying).
              //
              // Two binding sources are consulted, in this order:
              //   1. Cached `concreteArgs` from `genericAliasToTemplate`. Works
              //      directly even for phantom type parameters that don't appear
              //      in the alias's `target` (e.g. `type Box[T] = new int`).
              //   2. Refinement via expand-and-unify against `arg.underlying`.
              //      This catches the case where `typeToMangled` collides two
              //      distinct instantiations (e.g. `Parser[(int)->int]` and
              //      `Parser[(int)->int #pure]` both mangle to `Parser_fni32Retfni32Reti32`),
              //      so the cached `concreteArgs` may have been overwritten by a
              //      different instantiation. Pulling the per-instance bindings
              //      from the underlying recovers the correct effect annotations.
              //
              // `unifyTypes` already merges conflicting bindings via the effect
              // lattice, so doing both is safe — the underlying-unification
              // either confirms the cache or refines it.
              arg match
                case SyslType.NamedType(argName, argUnderlying, true, _, _) =>
                  genericAliasToTemplate.get(argName) match
                    case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                      for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
                      val subst = tparams.zip(tArgs).toMap
                      val expandedTarget = substituteTypeAST(target, subst)
                      unifyTypes(expandedTarget, argUnderlying, typeParams, env)
                    case _ => () // structural mismatch — caught by post-validation
                case _ => ()
            else
              // Transparent: substitute alias type params with the call's type args in
              // the target TypeAST, then unify the expanded structure against the actual.
              // e.g., type Parser[T] = (string, int) -> Result[T, string]
              //   Parser[A] → substitute T→A in target → (string, int) -> Result[A, string]
              val subst = tparams.zip(tArgs).toMap
              val expanded = substituteTypeAST(target, subst)
              unifyTypes(expanded, arg, typeParams, env)
        else arg match
          case SyslType.StructType(argName, argFields, _) =>
            // Same lossy-cache caveat as the nominal-alias branch above:
            // `typeToMangled` drops effect annotations on `FuncType`, so two
            // distinct generic-struct instantiations can collide on the same
            // mangled name and clobber each other in `structToTemplate`. The
            // cache lookup gates the template identity (so `Foo[T]` doesn't
            // match an unrelated `Bar[T]`), but the per-instance bindings are
            // refined by walking the actual struct's fields and unifying each
            // field's TypeAST (with the pattern's tArgs substituted in) against
            // the field's resolved type. `unifyTypes` merges any conflicting
            // binding via the effect lattice — so doing both is safe.
            structToTemplate.get(argName) match
              case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
                genericStructs.get(name).foreach { template =>
                  if template.typeParams.length == tArgs.length && template.fields.length == argFields.length then
                    val subst = template.typeParams.zip(tArgs).toMap
                    for ((fnTpl, ftAst, _), (fnAct, ftActual)) <- template.fields.zip(argFields) do
                      if fnTpl == fnAct then
                        val expanded = substituteTypeAST(ftAst, subst)
                        unifyTypes(expanded, ftActual, typeParams, env)
                }
              case _ => ()
          case SyslType.EnumType(argName, argVariants) =>
            // Same lossy-cache caveat as above. The variants-walk refinement
            // is what makes nested function-typed enum payloads (e.g.
            // `enum PR[A] { Ok(value: A, ...) }` with `A = (...) -> int`)
            // recover their per-instance effect annotations even when the
            // shared mangled name has clobbered the cache.
            enumToTemplate.get(argName) match
              case Some((templateName, concreteArgs)) if templateName == name && concreteArgs.length == tArgs.length =>
                for (p, a) <- tArgs.zip(concreteArgs) do unifyTypes(p, a, typeParams, env)
                genericEnums.get(name).foreach { template =>
                  if template.typeParams.length == tArgs.length && template.variants.length == argVariants.length then
                    val subst = template.typeParams.zip(tArgs).toMap
                    for (vAst, (vName, vFields)) <- template.variants.zip(argVariants) do
                      if vAst.name == vName && vAst.fields.length == vFields.length then
                        for ((_, ftAst), (_, ftActual)) <- vAst.fields.zip(vFields) do
                          val expanded = substituteTypeAST(ftAst, subst)
                          unifyTypes(expanded, ftActual, typeParams, env)
                }
              case _ => ()
          case _ => ()
      case _ => () // concrete parameter type, nothing to infer

  /** Substitute named types in a TypeAST. Used to expand generic type alias params before unification. */
  protected def substituteTypeAST(t: TypeAST, subst: Map[String, TypeAST]): TypeAST = t match
    case NamedTypeAST(name, Nil) if subst.contains(name) => subst(name)
    case NamedTypeAST(name, args) => NamedTypeAST(name, args.map(substituteTypeAST(_, subst)))
    case PtrTypeAST(inner) => PtrTypeAST(substituteTypeAST(inner, subst))
    case PtrNonNullTypeAST(inner) => PtrNonNullTypeAST(substituteTypeAST(inner, subst))
    case ArrayTypeAST(size, elem) => ArrayTypeAST(size, substituteTypeAST(elem, subst))
    case SliceTypeAST(elem) => SliceTypeAST(substituteTypeAST(elem, subst))
    case FuncTypeAST(params, ret, esc, eff) => FuncTypeAST(params.map(substituteTypeAST(_, subst)), substituteTypeAST(ret, subst), esc, eff)
    case TupleTypeAST(elems) => TupleTypeAST(elems.map(substituteTypeAST(_, subst)))
    case RefTypeAST(inner) => RefTypeAST(substituteTypeAST(inner, subst))
    case ByNameTypeAST(inner) => ByNameTypeAST(substituteTypeAST(inner, subst))
    case ProjectionTypeAST(qualifier, member) =>
      // If the qualifier is being substituted to a NamedTypeAST whose name we
      // can use as the new qualifier, rewrite. Otherwise pass through unchanged
      // — projection resolution happens at resolveType time against the
      // active impl context. (Pre-A4 substitution semantics.)
      subst.get(qualifier) match
        case Some(NamedTypeAST(n, _)) => ProjectionTypeAST(n, member)
        case _ => t

  /** Strict structural equality with effect-lattice tolerance for FuncType.
   *  Used for impl-dispatch post-validation. `slot` is the impl pattern resolved
   *  with the unifier-bound env; `actual` is the call-site type. They must be
   *  structurally identical *except* that nested FuncType effects compare by
   *  `effectsSatisfy(actualEff, slotEff)` rather than `==`. This lets a `#pure`
   *  closure (auto-inferred for any side-effect-free body) flow into an
   *  unannotated higher-order parameter — the common shape for combinator
   *  libraries that haven't yet opted into the effect discipline.
   *
   *  Containers (slice/array/ref/ptr/tuple/struct/enum) recurse component-wise
   *  so the lattice rule fires on FuncType anywhere in the tree.
   */
  protected def latticeEqual(slot: SyslType, actual: SyslType): Boolean =
    (slot, actual) match
      case (FuncType(p1, r1, _, eff1), FuncType(p2, r2, _, eff2)) =>
        // Parameters and return types match by lattice (recursive). Effects
        // checked one-way: actual must satisfy slot. Escape flag ignored — it's
        // an optimization hint, not a type distinction (mirrors `compatible`).
        p1.length == p2.length &&
          p1.zip(p2).forall((a, b) => latticeEqual(a, b)) &&
          latticeEqual(r1, r2) &&
          effectsSatisfy(eff2, eff1)
      case (SliceType(a), SliceType(b)) => latticeEqual(a, b)
      case (ArrayType(a, n1), ArrayType(b, n2)) => n1 == n2 && latticeEqual(a, b)
      case (PtrType(a), PtrType(b)) => latticeEqual(a, b)
      case (RefType(a), RefType(b)) => latticeEqual(a, b)
      case (StructType(n1, f1, v1), StructType(n2, f2, v2)) if n1 == n2 && v1 == v2 && f1.length == f2.length =>
        // Same nominal struct — recurse on fields so an embedded FuncType still uses
        // the lattice (anonymous tuple structs land here too — they share generated names).
        f1.zip(f2).forall { case ((fn1, ft1), (fn2, ft2)) => fn1 == fn2 && latticeEqual(ft1, ft2) }
      case (NamedType(n1, u1, nom1, r1, p1), NamedType(n2, u2, nom2, r2, p2)) =>
        n1 == n2 && nom1 == nom2 && r1 == r2 && p1 == p2 && latticeEqual(u1, u2)
      // Default: strict equality. Covers primitives (int, float, bool, string, unit),
      // enums, interfaces — anywhere effects don't appear in the shape.
      case (s, a) => s == a

  /** Stage F.3 entry point — try to unify a list of TypeAST patterns against a list of
   *  concrete SyslType actuals, returning the inferred binding map on success or None on
   *  any failure. Failure modes captured: arity mismatch, type-var conflict (caught as
   *  AnalysisError from the underlying `unifyTypes`), unbound type parameters, or
   *  structural mismatch that the recursive walker silently no-ops past.
   *
   *  The post-validation step substitutes the inferred env into each pattern, re-resolves
   *  it via `resolveType`, and demands `latticeEqual` against the actual. The lattice
   *  rule lets a candidate FuncType with stricter effects (e.g. `#pure`) flow into an
   *  unannotated slot — the common shape for combinator-library impls. Without this, a
   *  literal closure (always inferred `#pure` for side-effect-free bodies) would never
   *  match an `(A) -> B` impl pattern.
   */
  protected def tryUnifyAll(
      patterns: List[TypeAST],
      actuals: List[SyslType],
      tvars: Set[String],
  ): Option[Map[String, SyslType]] =
    if patterns.length != actuals.length then None
    else
      val env = mutable.Map.empty[String, SyslType]
      val unifyOk =
        try
          for (p, a) <- patterns.zip(actuals) do
            unifyTypes(p, a, tvars, env)
          true
        catch case _: AnalysisError => false
      if !unifyOk then None
      else
        val savedEnv = typeEnv
        typeEnv = typeEnv ++ env
        val structuralOk =
          try
            patterns.zip(actuals).forall { (p, a) =>
              val resolved =
                try Some(resolveType(p))
                catch case _: Throwable => None
              resolved.exists(r => latticeEqual(r, a))
            }
          finally typeEnv = savedEnv
        if !structuralOk then None
        else if !tvars.forall(env.contains) then None
        else Some(env.toMap)

  /** Stage F.5 helper — pattern-to-pattern overlap. Two impl templates overlap iff there
   *  exists a concrete substitution that satisfies both pattern lists simultaneously.
   *
   *  Approach: walk both patterns side-by-side, unifying type-var-bearing structure into
   *  a single combined env (`tvarsA ∪ tvarsB`). When both sides hit the same concrete
   *  shape they must agree structurally; when one is a tvar it binds.
   *
   *  This is symmetric, conservative, and good enough for the at-most-one coherence rule.
   *  False positives (rejecting non-overlapping templates) are preferred to false
   *  negatives (admitting actual ambiguity).
   */
  protected def patternsOverlap(
      a: List[TypeAST],
      b: List[TypeAST],
      tvars: Set[String],
  ): Boolean =
    if a.length != b.length then return false
    val env = mutable.Map.empty[String, TypeAST]
    def bindOrEqual(name: String, t: TypeAST): Boolean =
      env.get(name) match
        case Some(prev) => prev == t
        case None => env(name) = t; true
    def overlap(x: TypeAST, y: TypeAST): Boolean = (x, y) match
      case (NamedTypeAST(nx, Nil), _) if tvars.contains(nx) => bindOrEqual(nx, y)
      case (_, NamedTypeAST(ny, Nil)) if tvars.contains(ny) => bindOrEqual(ny, x)
      case (NamedTypeAST(nx, ax), NamedTypeAST(ny, ay)) =>
        nx == ny && ax.length == ay.length && ax.zip(ay).forall((p, q) => overlap(p, q))
      case (PtrTypeAST(ix), PtrTypeAST(iy)) => overlap(ix, iy)
      case (RefTypeAST(ix), RefTypeAST(iy)) => overlap(ix, iy)
      case (SliceTypeAST(ex), SliceTypeAST(ey)) => overlap(ex, ey)
      case (ArrayTypeAST(_, ex), ArrayTypeAST(_, ey)) => overlap(ex, ey)
      case (FuncTypeAST(px, rx, _, _), FuncTypeAST(py, ry, _, _)) =>
        px.length == py.length && px.zip(py).forall((p, q) => overlap(p, q)) && overlap(rx, ry)
      case (TupleTypeAST(ex), TupleTypeAST(ey)) =>
        ex.length == ey.length && ex.zip(ey).forall((p, q) => overlap(p, q))
      case _ => false
    a.zip(b).forall((p, q) => overlap(p, q))
