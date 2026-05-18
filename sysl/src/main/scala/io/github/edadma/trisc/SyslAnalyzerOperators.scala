package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

trait SyslAnalyzerOperators:
  self: SyslAnalyzer =>

  // Built-in binary operator → (trait name, method name). Extensible via #operator("sym") on trait methods.
  protected val builtinBinaryOperatorTraits: Map[String, (String, String)] = Map(
    "<"  -> ("Ord", "lt"),  "<=" -> ("Ord", "le"),
    ">"  -> ("Ord", "gt"),  ">=" -> ("Ord", "ge"),
    "==" -> ("Eq",  "eq"),  "!=" -> ("Eq",  "ne"),
    "+"  -> ("Add", "add"), "-"  -> ("Sub", "sub"),
    "*"  -> ("Mul", "mul"), "/"  -> ("Div", "div"),
  )

  protected val customBinaryOperatorTraits = new mutable.LinkedHashMap[String, (String, String)]
  protected val customUnaryOperatorTraits  = new mutable.LinkedHashMap[String, (String, String)]

  protected def lookupBinaryOperatorTrait(op: String): Option[(String, String)] =
    customBinaryOperatorTraits.get(op).orElse(builtinBinaryOperatorTraits.get(op))

  protected def lookupUnaryOperatorTrait(op: String): Option[(String, String)] =
    customUnaryOperatorTraits.get(op)

  // Lvalue-mutation prefix sigils. Always reserved — `++x` / `--x` are
  // statement-shaped and the lvalue semantics make user impls trickier than
  // the parser-combinator use cases the language wants to enable. The other
  // five built-in prefix sigils (`-`, `!`, `~`, `*`, `&`) are NOT in this
  // set: a `#operator(<sigil>)` registration is allowed for them, gated at
  // *impl* time on the operand-type vs. the sigil's natural built-in domain
  // (so users can `impl Lookahead[Parser[A]]` with `#operator("&")` while
  // a stray `impl ![bool]` is still rejected).
  protected val builtinPrefixOps: Set[String] =
    Set("++", "--")

  /** Set of named-type names a `#operator(<sigil>)` impl is NOT allowed to
   *  cover, because the sigil's built-in semantics already own that type.
   *  Used by the impl-registration conflict check; checks the *raw* AST so
   *  nominal aliases (`type Meters = new int`) remain user-overloadable.
   */
  protected def builtinPrefixDomainNames(op: String): Set[String] =
    val numeric = Set("int", "uint", "long", "ulong", "short", "ushort",
      "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64",
      "float", "f32", "double", "f64", "char", "byte")
    val integral = Set("int", "uint", "long", "ulong", "short", "ushort",
      "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "char", "byte")
    op match
      case "-" => numeric
      case "!" => Set("bool")
      case "~" => integral
      case "&" => numeric + "bool"
      case "*" => Set.empty // pointer dereference collisions caught structurally below
      case _   => Set.empty

  /** Does `pat` (an impl's first target-type pattern) conflict with the
   *  built-in semantics of `op`? Walks the AST head only — generic patterns
   *  like `Parser[A]`, struct names, enum names all read as user-defined
   *  and pass; only direct references to built-in scalar / pointer shapes
   *  fail. The check uses the raw AST so that `type Meters = new int`
   *  (a NamedTypeAST head of `Meters`) is freely overloadable even though
   *  its underlying is `int`.
   */
  protected def implOperandConflictsWithBuiltinPrefix(op: String, pat: TypeAST): Boolean =
    val names = builtinPrefixDomainNames(op)
    pat match
      case NamedTypeAST(n, _) => names.contains(n)
      case PtrTypeAST(_) | PtrNonNullTypeAST(_) | RefTypeAST(_) =>
        op == "*" || op == "&"
      case _ => false

  /** Runtime counterpart of `builtinPrefixDomainNames`, applied to a resolved
   *  `SyslType` at a use site. Used by the dispatch arms (UnaryAST, AddrOfAST,
   *  DerefAST) to decide *built-in vs. user-impl-first*. Nominal aliases keep
   *  their outer NamedType so dispatch on `type Meters = new int` doesn't
   *  accidentally route through the built-in numeric path.
   */
  protected def builtinPrefixDomainContains(op: String, t: SyslType): Boolean =
    op match
      case "-" => t.isInstanceOf[IntType] || t.isInstanceOf[UIntType] || t.isInstanceOf[FloatType]
      case "!" => t == BoolType
      case "~" => t.isInstanceOf[IntType] || t.isInstanceOf[UIntType]
      case "*" => t.isInstanceOf[PtrType] || t.isInstanceOf[RefType] || t.isInstanceOf[ArrayType]
      case "&" =>
        t.isInstanceOf[IntType] || t.isInstanceOf[UIntType] || t.isInstanceOf[FloatType] ||
          t == BoolType || t.isInstanceOf[PtrType] || t.isInstanceOf[RefType]
      case _   => false

  /** Register #operator / #op attributes from trait methods. Arity routes the
   *  registration: a single-param trait method becomes a prefix operator;
   *  two-param methods become infix. Anything else is rejected.
   */
  protected def registerTraitOperatorEntries(traitName: String, methods: List[TraitMethodAST], node: Any): Unit =
    for m <- methods do
      val opAttrs = m.attributes.filter(a => a.name == "operator" || a.name == "op")
      if opAttrs.length > 1 then
        throw AnalysisError(s"trait method '${m.name}' has multiple #operator / #op attributes", m)
      opAttrs.headOption.foreach { attr =>
        val sym = extractOperatorSymbol(attr, m)
        m.params.length match
          case 2 =>
            if builtinBinaryOperatorTraits.contains(sym) then
              throw AnalysisError(
                s"operator '$sym' is reserved for built-in trait dispatch; use the standard trait (${builtinBinaryOperatorTraits(sym)._1}) instead of #operator",
                m,
              )
            customBinaryOperatorTraits.get(sym) match
              case Some((t, meth)) if t != traitName || meth != m.name =>
                throw AnalysisError(
                  s"operator '$sym' is already bound to trait '$t' (method '$meth')",
                  m,
                )
              case _ => ()
            customBinaryOperatorTraits(sym) = (traitName, m.name)
          case 1 =>
            if builtinPrefixOps.contains(sym) then
              throw AnalysisError(
                s"prefix operator '$sym' is reserved for built-in dispatch; cannot overload via #operator",
                m,
              )
            customUnaryOperatorTraits.get(sym) match
              case Some((t, meth)) if t != traitName || meth != m.name =>
                throw AnalysisError(
                  s"prefix operator '$sym' is already bound to trait '$t' (method '$meth')",
                  m,
                )
              case _ => ()
            customUnaryOperatorTraits(sym) = (traitName, m.name)
          case n =>
            throw AnalysisError(
              s"trait method '${m.name}' with #operator(\"$sym\") must take one (prefix) or two (infix) parameters; got $n",
              m,
            )
      }

  protected def extractOperatorSymbol(attr: Attribute, at: Any): String =
    attr.args match
      case List(AttrPositional(AttrLitString(s))) if s.nonEmpty => s
      case List(AttrNamed("sym", AttrLitString(s))) if s.nonEmpty => s
      case List(AttrNamed("symbol", AttrLitString(s))) if s.nonEmpty => s
      case _ =>
        throw AnalysisError(s"#${attr.name} requires a non-empty string literal, e.g. #operator(\"~\")", at)
  // Instantiate a generic function with inferred type arguments, returning the mangled name
  // and FunInfo of the instantiated function. Reuses cached instantiations.
  // If an operator has a user-defined struct/enum operand, desugar to the corresponding trait call.
  // Returns None if no desugaring applies (use built-in dispatch).
  //
  // `strict` controls behaviour when no impl matches:
  //   - strict=true: throw "no impl of trait for operator on operands" — used when an
  //     operand is non-arithmetic (struct/enum/nominal alias of non-numeric) and built-in
  //     fallback can't possibly succeed; the better diagnostic names trait + operands.
  //   - strict=false: return None silently — used when both operands are numeric (or
  //     nominal aliases of numerics) and the built-in arithmetic path is a valid fallback,
  //     so `Meters + Meters` with no `impl Add[Meters]` still does plain int arithmetic.
  protected def tryOperatorDispatch(op: String, tLeft: TExpr, tRight: TExpr, strict: Boolean): Option[TExpr] =
    lookupBinaryOperatorTrait(op) match
      case None => None
      case Some((traitName, methodName)) =>
        val operands = List(tLeft.typ, tRight.typ)
        // For built-in operators (`+`, `-`, `==`, …), only fire user-defined
        // dispatch when at least one operand is a user type. This preserves the
        // friendly "unknown operator on i32" path and keeps `1 + 2` from going
        // through trait machinery. Custom operators (registered via #operator)
        // have no built-in fallback, so the gate is wrong for them: e.g. with
        // `impl[B] MapTo[string, B, Parser[B]]`, the dispatch
        // `"+" ^^^ (_ + _)` has operand types `(string, fn)` — both built-in —
        // but it must still go through trait dispatch to be meaningful.
        val isCustomOp = customBinaryOperatorTraits.contains(op)
        if !isCustomOp then
          val hasUserType = operands.exists {
            case _: SyslType.StructType | _: SyslType.EnumType | _: SyslType.NamedType => true
            case _ => false
          }
          if !hasUserType then return None
        if !traits.contains(traitName) then
          if strict then
            throw AnalysisError(s"operator '$op' on ${operands.mkString(", ")} requires trait '$traitName' but it is not defined")
          else return None
        val candidates = enumerateImplCandidates(traitName, methodName, operands)
        candidates match
          case Nil =>
            if strict then
              throw AnalysisError(s"no impl of '$traitName' for operator '$op' on ${operands.mkString(", ")}")
            else None
          case (template, subst) :: Nil =>
            val (mangled, funInfo) = instantiateImpl(template, traitName, methodName, subst)
            val checkedArgs = checkArgs(mangled, funInfo.params, List(tLeft, tRight))
            Some(TCall(mangled, checkedArgs, funInfo.returnType))
          case multi =>
            throw AnalysisError(s"ambiguous: ${multi.length} impls of '$traitName' match operator '$op' on ${operands.mkString(", ")}")

  /** Prefix-operator dispatch. Caller is expected to have already gated on
   *  `customUnaryOperatorTraits.contains(op)`. Throws when the bound trait
   *  is missing or no impl matches; throws on ambiguity. Use
   *  `tryUnaryOperatorDispatchOpt` instead at sites that want to fall back
   *  to a different lowering when no impl matches (e.g. the `&` arm where
   *  built-in address-of is the fallback for true lvalues).
   */
  protected def tryUnaryOperatorDispatch(op: String, tOperand: TExpr): TExpr =
    tryUnaryOperatorDispatchOpt(op, tOperand).getOrElse {
      val (traitName, _) = customUnaryOperatorTraits(op)
      if !traits.contains(traitName) then
        throw AnalysisError(
          s"prefix operator '$op' on ${tOperand.typ} requires trait '$traitName' but it is not defined")
      throw AnalysisError(s"no impl of '$traitName' for prefix operator '$op' on ${tOperand.typ}")
    }

  /** Same as `tryUnaryOperatorDispatch` but returns `None` instead of
   *  throwing when zero impls match. Ambiguity (>1 impl) still throws.
   */
  protected def tryUnaryOperatorDispatchOpt(op: String, tOperand: TExpr): Option[TExpr] =
    val (traitName, methodName) = customUnaryOperatorTraits(op)
    if !traits.contains(traitName) then return None
    val candidates = enumerateImplCandidates(traitName, methodName, List(tOperand.typ))
    candidates match
      case Nil => None
      case (template, subst) :: Nil =>
        val (mangled, funInfo) = instantiateImpl(template, traitName, methodName, subst)
        val checkedArgs = checkArgs(mangled, funInfo.params, List(tOperand))
        Some(TCall(mangled, checkedArgs, funInfo.returnType))
      case multi =>
        throw AnalysisError(
          s"ambiguous: ${multi.length} impls of '$traitName' match prefix operator '$op' on ${tOperand.typ}")

  /** Lookahead helper for binary-operator expected-type forwarding. Given the
   *  trait+method that `op` resolves to, the LHS operand's already-resolved
   *  type, and an optional outer expected type for the result, find the unique
   *  impl that matches the LHS at param 0 (and the result if expected is given),
   *  then resolve and return the type of the second formal parameter under the
   *  inferred type-param bindings. Returns `None` if zero or many impls match,
   *  or if any required type variable is still unbound after the partial
   *  unification.
   *
   *  This lets a closure-literal RHS (`lhs ^^^ (_ + _)`) typecheck with the
   *  expected type the dispatcher would use *after* dispatch — exactly the same
   *  service ordinary call sites already provide for closure-literal args.
   */
  protected def expectedTypeForBinaryOpRhs(
      traitName: String,
      methodName: String,
      leftType: SyslType,
      expectedReturnType: Option[SyslType],
  ): Option[SyslType] =
    if !traits.contains(traitName) then return None
    val results = implTemplates.getOrElse(traitName, Nil).iterator.flatMap { t =>
      // Recover the impl method's pattern types — explicit AST when present,
      // else derived from the trait method by substituting impl's targetPatterns.
      val (paramPatterns, retPattern): (List[TypeAST], TypeAST) =
        t.methodASTs.find(_.name == methodName) match
          case Some(im) =>
            (im.params.map(_.typ), im.returnType.getOrElse(NamedTypeAST("unit", Nil)))
          case None =>
            traits(traitName).methods.find(_.name == methodName) match
              case Some(tm) =>
                val traitToImpl = traits(traitName).typeParams.zip(t.targetPatterns).toMap
                (tm.params.map(p => substituteTypeAST(p.typ, traitToImpl)),
                 substituteTypeAST(tm.returnType, traitToImpl))
              case None => (Nil, NamedTypeAST("unit", Nil))
      if paramPatterns.length != 2 then None
      else
        val tvars = t.typeParams.toSet
        val env = mutable.Map.empty[String, SyslType]
        try
          unifyTypes(paramPatterns.head, leftType, tvars, env)
          expectedReturnType.foreach(rt => unifyTypes(retPattern, rt, tvars, env))
          // `unifyTypes` silently no-ops on shape mismatches (its callers post-
          // validate via `latticeEqual`). Ordinary dispatch (`tryUnifyAll`) does
          // the post-check; this lookahead must do the same, otherwise a
          // non-matching impl (e.g. `MapTo[Parser[A], …]` against `string` LHS)
          // sneaks through and `expectedTypeForBinaryOpRhs` returns None
          // (ambiguous). The placeholder closure then has nothing to resolve
          // against. Match the dispatcher's structural check 1:1.
          val savedEnv = typeEnv
          typeEnv = typeEnv ++ env.toMap
          val structuralOk =
            try
              latticeEqual(resolveType(paramPatterns.head), leftType) &&
                expectedReturnType.forall(rt =>
                  try latticeEqual(resolveType(retPattern), rt)
                  catch case _: Throwable => false)
            catch case _: Throwable => false
          val out =
            if !structuralOk then None
            else
              try Some(resolveType(paramPatterns(1)))
              catch case _: Throwable => None
          typeEnv = savedEnv
          out
        catch case _: AnalysisError => None
    }.toList
    results match
      case single :: Nil => Some(single)
      case _ => None
