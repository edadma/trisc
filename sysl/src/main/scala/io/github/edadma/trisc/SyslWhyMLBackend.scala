package io.github.edadma.trisc

/** Phase 1 sysl → WhyML translator. Walks the parser AST (NOT the typed AST) so that
 *  `require` / `ensure` / `variant` clauses are still attached to functions as separate
 *  declarative pieces — by the time the analyzer runs they have been woven into the
 *  function body around `__result__` and a return-rewrite pass.
 *
 *  Phase 1 scope: pure functions returning int / bool, integer arithmetic, comparisons,
 *  bool ops, if-expressions, `require` / `ensure` / `variant`, `old(e)` inside `ensure`,
 *  and recursion. Anything outside that surface is rejected with a clear message naming
 *  the unsupported construct so the user knows it's a translator gap, not a sysl error. */
class SyslWhyMLBackend(moduleName: String = "M"):
  private val out = new StringBuilder
  private var indentLevel = 0
  /** Set of enum (no-payload) type names declared in this program. Used by `formatExpr` to
   *  recognize `EnumName.Variant` field access and rewrite to just `Variant` (WhyML
   *  constructors live in the module-level namespace, not under the type). */
  private var enumNames: Set[String] = Set.empty
  /** Names currently bound as WhyML `ref`s in scope. Reads of these get `!name`; assignments
   *  get `name := expr`. Populated during `formatBlockBody` when a sysl `var` is detected to
   *  be reassigned later in the same scope; popped on the way out. Phase 4a does not handle
   *  shadowing — a function with two same-named locals in disjoint scopes would conflate them. */
  private val refScope: scala.collection.mutable.Set[String] = scala.collection.mutable.Set.empty

  private def indent: String = "  " * indentLevel
  private def line(s: String): Unit =
    out.append(indent).append(s).append('\n')
  private def blank(): Unit = out.append('\n')

  private class Unsupported(val msg: String) extends RuntimeException(msg)
  private def unsupported(what: String, ctx: String = ""): Nothing =
    val suffix = if ctx.isEmpty then "" else s" ($ctx)"
    throw new Unsupported(s"WhyML translator: unsupported $what$suffix")

  /** Translate a parsed sysl program to a WhyML module string. */
  def generate(program: ProgramAST): String =
    out.clear()
    indentLevel = 0
    refScope.clear()
    val enums = program.decls.collect { case e: EnumDeclAST => e }
    enumNames = enums.map(_.name).toSet
    val constants = program.decls.collect { case v: VarDeclAST => v }
    val fns = program.decls.collect { case f: FunDeclAST => f }
    line(s"module $moduleName")
    indentLevel += 1
    line("use int.Int")
    // ComputerDivision provides truncating `div` and `mod` (C-style). int.Int does not
    // define `/` or `%` because their semantics are debatable; we pin to the C-style choice
    // since it matches sysl's interpreter and codegen behavior. Always-imported is harmless.
    line("use int.ComputerDivision")
    // ref.Ref provides the `ref` constructor, `(!)` deref, and `(:=)` assignment used by
    // Phase 4a. Always-imported is harmless: programs that don't use refs simply never
    // reference these symbols. Without this import Why3 reports "unbound symbol 'ref'".
    line("use ref.Ref")
    blank()
    var first = true
    for e <- enums do
      if !first then blank()
      first = false
      emitEnum(e)
    for c <- constants do
      if !first then blank()
      first = false
      emitConstant(c)
    for fn <- fns do
      if !first then blank()
      first = false
      emitFunction(fn)
    indentLevel -= 1
    line("end")
    out.toString

  /** Module-level `val NAME : T = expr` (or `const NAME : T = expr`) becomes a WhyML
   *  `constant`. Mutable `var` at module scope would need WhyML refs and is deferred —
   *  most proof-relevant module-level data is naturally immutable (limits, sentinels,
   *  shared math constants), so `val` covers the common case. */
  private def emitConstant(v: VarDeclAST): Unit =
    if v.isMutable then
      unsupported("module-level `var`", s"${v.name}: only `val` / `const` (immutable) module-level bindings are supported in Phase 3c")
    val t = v.typ.getOrElse(unsupported("module-level val without type annotation", v.name))
    // `let constant` (program-level) — usable from both contracts and code bodies. Without
    // `let`, the constant is logic-only and Why3 reports "logical symbol used in a non-ghost
    // context" when a `let function` body references it.
    line(s"let constant ${sanitizeName(v.name)} : ${typeOf(t)} = ${formatExpr(v.init)}")

  /** sysl `enum Color { Red, Green, Blue }` → WhyML `type color = Red | Green | Blue`.
   *  The integer values that sysl assigns (auto-incrementing or explicit) are dropped —
   *  WhyML algebraic types don't expose a numeric tag, and proofs typically reason about
   *  variant identity rather than its underlying int. WhyML type names are conventionally
   *  lowercase; constructor names stay as-written (sysl convention is also uppercase). */
  private def emitEnum(e: EnumDeclAST): Unit =
    val typeName = s"${e.name.head.toLower}${e.name.tail}"
    val ctors = e.members.map { case (name, _) => name }
    if ctors.isEmpty then unsupported("empty enum", e.name)
    line(s"type $typeName = ${ctors.mkString(" | ")}")

  /** True iff `fn` calls itself by name anywhere in its body (Phase 1 detects only direct
   *  self-recursion — mutual recursion would need a cross-decl scan and `with` syntax in WhyML). */
  private def isRecursive(fn: FunDeclAST): Boolean =
    fn.body match
      case ExprBodyAST(e)        => callsName(e, fn.name)
      case BlockBodyAST(stmts, _) => stmts.exists(stmtCallsName(_, fn.name))

  private def callsName(e: ExpressionAST, name: String): Boolean = e match
    case CallAST(n, args) if n == name => true
    case CallAST(_, args)              => args.exists(callsName(_, name))
    case BinaryAST(l, _, r)            => callsName(l, name) || callsName(r, name)
    case UnaryAST(_, x)                => callsName(x, name)
    case IfExprAST(c, tb, eb) =>
      callsName(c, name) || tb.exists(stmtCallsName(_, name)) ||
        eb.exists(_.exists(stmtCallsName(_, name)))
    case _ => false

  private def stmtCallsName(s: StmtAST, name: String): Boolean = s match
    case ReturnStmtAST(Some(e)) => callsName(e, name)
    case ReturnStmtAST(None)    => false
    case ExprStmtAST(e)         => callsName(e, name)
    case VarStmtAST(_, _, init, _, _, _, _) => callsName(init, name)
    case _                      => false

  private def emitFunction(fn: FunDeclAST): Unit =
    if fn.attributes.exists(_.name == "test") then return
    if fn.typeParams.nonEmpty then unsupported("generic function", fn.name)
    val name = sanitizeName(fn.name)
    val params =
      if fn.params.isEmpty then "()"
      else fn.params.map(p => s"(${sanitizeName(p.name)}: ${typeOf(p.typ)})").mkString(" ")
    val (contracts, bodyStmts) = splitBody(fn)
    val isGhost = fn.attributes.exists(_.name == "ghost")
    val bodyStr = formatBlockBody(bodyStmts)
    // For predicate-shape detection we need the trailing expression of a single-stmt body.
    // Multi-statement bodies can never be predicate-shaped (a let-chain isn't a prop).
    val singleBodyExpr: Option[ExpressionAST] = bodyStmts match
      case List(ExprStmtAST(e))         => Some(e)
      case List(ReturnStmtAST(Some(e))) => Some(e)
      case _                            => None

    // Lift to a logic-level `predicate` when the shape is right: `def f(...) -> bool` whose
    // body IS a quantifier and which carries no contracts. Why3's `forall`/`exists` are
    // formula-level (return `prop`), so they cannot appear in `let function`'s value-typed
    // body — only in contract positions or as the body of `predicate` / formula-valued
    // logic definitions. `predicate` is exactly the right WhyML construct here, and matches
    // sysl's runtime semantics for `def`-with-quantifier (a pure boolean-valued query).
    // `#ghost` is redundant on predicates (they're inherently logic-level), so we drop it.
    val returnsBool = fn.returnType match
      case Some(NamedTypeAST("bool", Nil)) => true
      case _                               => false
    val isPredicateShape = fn.isDef && returnsBool && contracts.isEmpty &&
      singleBodyExpr.exists(isFormula)
    if isPredicateShape then
      line(s"predicate $name $params = ${stripOuterParens(formatExpr(singleBodyExpr.get))}")
      return

    val recKw = if isRecursive(fn) then "rec " else ""
    // WhyML's `ghost` qualifier marks a function as proof-only — it is checked but erased
    // before extraction. Maps 1:1 from sysl's `#ghost`. The required keyword order is
    // `let [rec] [ghost] function f ...` — ghost must follow rec, not precede it.
    val ghostKw = if isGhost then "ghost " else ""
    // `let function` is reflected to the logic and must be a pure expression — it cannot use
    // mutable refs, sequenced assignments, or loops. When the body uses any of those (Phase 4),
    // drop the `function` keyword and emit a plain `let`. The function is still verified
    // against its contracts; it just isn't usable inside contract expressions of OTHER
    // functions. Pure functions (Phase 1–3) keep `function` so they can be called from
    // contracts as well.
    val funKw = if isImpure(bodyStmts) then "" else "function "
    val ret = fn.returnType match
      case None    => unsupported("function without explicit return type", fn.name)
      case Some(t) => typeOf(t)
    line(s"let $recKw$ghostKw" + funKw + s"$name $params : $ret")
    indentLevel += 1
    for c <- contracts do emitContract(c)
    line(s"= $bodyStr")
    indentLevel -= 1

  /** True if `e` is a formula-shaped expression — currently just a top-level quantifier.
   *  Extended in later phases to recognize boolean connectives joining quantifiers. */
  private def isFormula(e: ExpressionAST): Boolean = e match
    case _: QuantifierAST => true
    case _                => false

  private def splitBody(fn: FunDeclAST): (List[ContractClauseAST], List[StmtAST]) =
    fn.body match
      case ExprBodyAST(e) => (Nil, List(ExprStmtAST(e)))
      case BlockBodyAST(stmts, contracts) => (contracts, stmts)

  /** Lower a sequence of body statements to a single WhyML expression string.
   *
   *  Supported mid-body forms (Phase 3 + 4a/b):
   *    - `val name = expr`                  → `let name = expr in <rest>`
   *    - `var name = expr` (immutable use)  → `let name = expr in <rest>`
   *    - `var name = expr` (later reassigned) → `let name = ref expr in <rest>` + add to refScope
   *    - `name = expr`                      → `name := expr; <rest>` (must be in refScope)
   *    - `name op= expr`                    → `name := (!name op expr); <rest>`
   *    - `if cond then return e` (no else)  → `(if cond then e else <rest>)`
   *    - `while cond do body done`          → `(while ... done); <rest>` (with invariant/variant)
   */
  private def formatBlockBody(stmts: List[StmtAST]): String =
    stmts match
      case Nil => unsupported("empty function body", "must have a trailing expression or return")
      case List(s) => stmtAsTrailingExpr(s)
      case head :: rest =>
        head match
          case VarStmtAST(name, _, init, isMutable, _, _, isGhost) =>
            val sname = sanitizeName(name)
            val ghostKw = if isGhost then "ghost " else ""
            // Detect ref-shape: declared mutable AND actually reassigned later in scope.
            // A `var` that's never reassigned can stay as an immutable `let` — it's pure
            // and Why3 prefers pure where possible.
            val mutableUse = isMutable && isReassigned(name, rest)
            if mutableUse then
              refScope += name
              try
                s"let $ghostKw$sname = ref ${formatExpr(init)} in ${formatBlockBody(rest)}"
              finally refScope -= name
            else
              s"let $ghostKw$sname = ${formatExpr(init)} in ${formatBlockBody(rest)}"

          case AssignStmtAST(target, value) =>
            if !refScope(target) then
              unsupported("assignment to non-mutable binding", target)
            s"${sanitizeName(target)} := ${formatExpr(value)}; ${formatBlockBody(rest)}"

          case CompoundAssignStmtAST(target, op, value) =>
            if !refScope(target) then
              unsupported("compound assignment to non-mutable binding", target)
            s"${formatCompoundAssign(target, op, value)}; ${formatBlockBody(rest)}"

          case WhileStmtAST(cond, body, _) =>
            s"${formatWhile(cond, body)}; ${formatBlockBody(rest)}"

          // Sysl `for i in lo..<hi <body>` parses to a C-style ForStmtAST(init, cond, update, body).
          // When the shape is canonical (immutable counter, increment-by-1, `<`/`<=` cond, body
          // doesn't reassign the counter), emit a native WhyML `for i = lo to hi do ... done`,
          // whose termination is by construction (no `variant` needed). Otherwise lower to a
          // while-equivalent and recurse — the init's VarStmt naturally goes through ref-form.
          case fs: ForStmtAST =>
            canonicalForLoop(fs) match
              case Some((counterName, lo, hi, body)) =>
                val (annotations, realBody) = body.span {
                  // WhyML `for` accepts only `invariant`; user `variant` is redundant for native
                  // for-loops (loop bound is finite by construction) and silently dropped.
                  case _: InvariantStmtAST | _: VariantStmtAST => true
                  case _                                        => false
                }
                val invariants = annotations.collect {
                  case InvariantStmtAST(e, _) => s"invariant { ${stripOuterParens(formatExpr(e))} }"
                }.mkString(" ")
                val bodyStr = formatLoopBody(realBody)
                val sep = if invariants.isEmpty || bodyStr.isEmpty then "" else " "
                val sname = sanitizeName(counterName)
                s"(for $sname = ${formatExpr(lo)} to ${formatExpr(hi)} do $invariants$sep$bodyStr done); ${formatBlockBody(rest)}"
              case None =>
                val whileEquivalent = fs.init :: WhileStmtAST(fs.cond, fs.body :+ fs.update, None) :: rest
                formatBlockBody(whileEquivalent)

          // Bare expression-statements that we know how to lower to a unit-typed step:
          // `i++` / `i--` are sugar for the corresponding compound assignment.
          case ExprStmtAST(PostIncAST(name)) =>
            if !refScope(name) then unsupported("post-increment of non-mutable binding", name)
            s"${sanitizeName(name)} := (!${sanitizeName(name)} + 1); ${formatBlockBody(rest)}"
          case ExprStmtAST(PostDecAST(name)) =>
            if !refScope(name) then unsupported("post-decrement of non-mutable binding", name)
            s"${sanitizeName(name)} := (!${sanitizeName(name)} - 1); ${formatBlockBody(rest)}"

          // Mid-body early-exit: `if cond then return e` (no else) followed by more stmts
          // lowers to `if cond then <e> else <rest>`. The then-branch must end in a return
          // (otherwise it would fall through into the rest, which has different semantics).
          case ExprStmtAST(IfExprAST(cond, thenStmts, None)) if thenStmts.exists(isReturn) =>
            val thenExpr = stmtsAsExpr(thenStmts)
            s"(if ${formatExpr(cond)} then $thenExpr else ${formatBlockBody(rest)})"

          case other =>
            unsupported(
              "non-binding statement in function body",
              s"supported mid-body forms are val/var bindings, assignments, while loops, and `if cond then return expr`; got ${other.getClass.getSimpleName}")

  /** Format a compound-assignment `target op= value` as `target := (!target op value)`.
   *  `/` and `%` route through int.ComputerDivision's prefix `div` / `mod`; everything else is
   *  infix. The `op` string carries the trailing `=`, e.g. `+=`, which we strip before mapping. */
  private def formatCompoundAssign(target: String, op: String, value: ExpressionAST): String =
    val tname = sanitizeName(target)
    val baseOp = if op.endsWith("=") then op.dropRight(1) else op
    val rhs = baseOp match
      case "/" => s"(div !$tname ${formatExpr(value)})"
      case "%" => s"(mod !$tname ${formatExpr(value)})"
      case _   => s"(!$tname ${mapBinaryOp(baseOp)} ${formatExpr(value)})"
    s"$tname := $rhs"

  /** Format a sysl `while cond <body>` as a WhyML `while cond do invariant{} variant{} body done`.
   *  Sysl interleaves `invariant` and `variant` statements with the loop body; WhyML wants them
   *  immediately after `do`. The split is positional: leading invariant/variant stmts become
   *  WhyML annotations, the rest becomes the loop body proper. WhyML allows multiple `invariant`
   *  clauses but at most one `variant` — Phase 4b doesn't enforce that, leaving it to Why3 to
   *  reject. The body sequences with `;`. The whole `while ... done` is unit-typed. */
  private def formatWhile(cond: ExpressionAST, body: List[StmtAST]): String =
    val (annotations, realBody) = body.span {
      case _: InvariantStmtAST | _: VariantStmtAST => true
      case _                                        => false
    }
    val annoStr = annotations.map {
      case InvariantStmtAST(e, _) => s"invariant { ${stripOuterParens(formatExpr(e))} }"
      case VariantStmtAST(e)      => s"variant { ${stripOuterParens(formatExpr(e))} }"
      case other                  => unsupported("loop annotation", other.getClass.getSimpleName)
    }.mkString(" ")
    val bodyStr = formatLoopBody(realBody)
    val sep = if annoStr.isEmpty || bodyStr.isEmpty then "" else " "
    s"(while ${formatExpr(cond)} do $annoStr$sep$bodyStr done)"

  /** Format the *body* of a loop — a sequence of unit-typed statements joined with `;`.
   *  Unlike `formatBlockBody`, there is no trailing expression: every statement contributes to
   *  the body's side effects and the loop itself is unit-typed. Supported: assignment, compound
   *  assignment, nested while. */
  private def formatLoopBody(stmts: List[StmtAST]): String =
    stmts.map(formatLoopStmt).mkString("; ")

  private def formatLoopStmt(s: StmtAST): String = s match
    case AssignStmtAST(target, value) =>
      if !refScope(target) then unsupported("assignment to non-mutable binding in loop", target)
      s"${sanitizeName(target)} := ${formatExpr(value)}"
    case CompoundAssignStmtAST(target, op, value) =>
      if !refScope(target) then unsupported("compound assignment in loop", target)
      formatCompoundAssign(target, op, value)
    case WhileStmtAST(cond, body, _) => formatWhile(cond, body)
    case ExprStmtAST(PostIncAST(name)) =>
      if !refScope(name) then unsupported("post-increment of non-mutable binding in loop", name)
      val n = sanitizeName(name); s"$n := (!$n + 1)"
    case ExprStmtAST(PostDecAST(name)) =>
      if !refScope(name) then unsupported("post-decrement of non-mutable binding in loop", name)
      val n = sanitizeName(name); s"$n := (!$n - 1)"
    case ExprStmtAST(e)              => formatExpr(e)
    case other =>
      unsupported(
        "loop body statement",
        s"loops support assignments, compound assignments, nested while, and expression stmts; got ${other.getClass.getSimpleName}")

  /** Recognize a sysl ForStmtAST shape that maps cleanly to WhyML's native `for i = lo to hi`:
   *    - init  is `var i = lo` (mutable)
   *    - update is `i++` / `i = i + 1` / `i += 1` (increment by 1)
   *    - cond  is `i < hi` (effective hi = `hi - 1`) or `i <= hi` (effective hi = `hi`)
   *    - body  doesn't reassign the counter (WhyML for-loop counters are immutable in body)
   *  Returns (counterName, lo, effectiveHi, body) for emission, or None to fall back to while. */
  private def canonicalForLoop(fs: ForStmtAST): Option[(String, ExpressionAST, ExpressionAST, List[StmtAST])] =
    fs.init match
      case VarStmtAST(name, _, lo, true, _, _, _) =>
        val incBy1 = fs.update match
          case ExprStmtAST(PostIncAST(n)) if n == name => true
          case ExprStmtAST(PreIncAST(n))  if n == name => true
          case CompoundAssignStmtAST(n, "+", IntLitAST(1)) if n == name => true
          case AssignStmtAST(n, BinaryAST(VarRefAST(m), "+", IntLitAST(1))) if n == name && m == name => true
          case _ => false
        if !incBy1 then None
        else fs.cond match
          case BinaryAST(VarRefAST(n1), op, hi) if n1 == name =>
            val effectiveHi = op match
              case "<"  => Some(BinaryAST(hi, "-", IntLitAST(1)))
              case "<=" => Some(hi)
              case _    => None
            effectiveHi.flatMap { ehi =>
              if isReassigned(name, fs.body) then None
              else Some((name, lo, ehi, fs.body))
            }
          case _ => None
      case _ => None

  /** True iff any statement in `stmts` (or any of its nested loops/branches/matches) reassigns
   *  the variable named `name`. Used by `formatBlockBody` to decide whether a `var` should be
   *  emitted as a WhyML ref or as an immutable let. */
  private def isReassigned(name: String, stmts: List[StmtAST]): Boolean =
    stmts.exists(stmtAssignsTo(name, _))

  private def stmtAssignsTo(name: String, s: StmtAST): Boolean = s match
    case AssignStmtAST(t, _) if t == name           => true
    case CompoundAssignStmtAST(t, _, _) if t == name => true
    case ExprStmtAST(PostIncAST(t)) if t == name    => true
    case ExprStmtAST(PostDecAST(t)) if t == name    => true
    case ExprStmtAST(PreIncAST(t))  if t == name    => true
    case ExprStmtAST(PreDecAST(t))  if t == name    => true
    case WhileStmtAST(_, body, _)                   => isReassigned(name, body)
    case ForStmtAST(init, _, update, body, _) =>
      stmtAssignsTo(name, init) || stmtAssignsTo(name, update) || isReassigned(name, body)
    case DoWhileStmtAST(_, body, _)                 => isReassigned(name, body)
    case LoopStmtAST(body, _)                       => isReassigned(name, body)
    case ExprStmtAST(IfExprAST(_, t, e)) =>
      isReassigned(name, t) || e.exists(isReassigned(name, _))
    case ExprStmtAST(MatchExprAST(_, arms, default)) =>
      arms.exists(a => isReassigned(name, a.body)) || default.exists(isReassigned(name, _))
    case _ => false

  /** True iff the function body uses any imperative construct (loops, assignment) that prevents
   *  it from being declared as `let function` (which Why3 reflects to logic). Pure function
   *  bodies — even those with local `let` bindings, conditionals, or matches — keep `function`. */
  private def isImpure(stmts: List[StmtAST]): Boolean = stmts.exists(stmtIsImpure)

  private def stmtIsImpure(s: StmtAST): Boolean = s match
    case _: AssignStmtAST | _: CompoundAssignStmtAST              => true
    case _: WhileStmtAST | _: ForStmtAST | _: DoWhileStmtAST | _: LoopStmtAST => true
    case ExprStmtAST(_: PostIncAST | _: PostDecAST | _: PreIncAST | _: PreDecAST) => true
    case ExprStmtAST(IfExprAST(_, t, e)) =>
      isImpure(t) || e.exists(isImpure)
    case ExprStmtAST(MatchExprAST(_, arms, default)) =>
      arms.exists(a => isImpure(a.body)) || default.exists(isImpure)
    case _ => false

  private def isReturn(s: StmtAST): Boolean = s match
    case _: ReturnStmtAST => true
    case _                => false

  private def stmtAsTrailingExpr(s: StmtAST): String = s match
    case ReturnStmtAST(Some(e)) => formatExpr(e)
    case ExprStmtAST(e)         => formatExpr(e)
    case other =>
      unsupported(
        "non-expression trailing statement",
        s"function body must end with `return <expr>` or a bare expression; got ${other.getClass.getSimpleName}")

  private def emitContract(c: ContractClauseAST): Unit =
    val keyword = c.kind match
      case ContractRequire => "requires"
      case ContractEnsure  => "ensures "
      case ContractVariant => "variant "
    line(s"$keyword { ${stripOuterParens(formatExpr(c.expr))} }")

  /** Strip one matched outer paren pair if it wraps the entire string. The formatter
   *  conservatively wraps every binary expression in parens; outermost wrapping inside a
   *  brace-delimited contract clause is redundant and noisy, so peel one layer when safe. */
  private def stripOuterParens(s: String): String =
    if s.length >= 2 && s.startsWith("(") && s.endsWith(")") then
      var depth = 0
      var i = 0
      var allMatched = true
      while i < s.length - 1 && allMatched do
        if s.charAt(i) == '(' then depth += 1
        else if s.charAt(i) == ')' then
          depth -= 1
          if depth == 0 then allMatched = false
        i += 1
      if allMatched then s.substring(1, s.length - 1) else s
    else s

  /** Map a sysl type AST to a WhyML type. Phase 1 collapses every signed/unsigned int width
   *  to mathematical `int` — overflow is a separate verification problem we layer on later
   *  via Why3's `Int32` / `Int64` modules. `bool` maps directly. */
  private def typeOf(t: TypeAST): String = t match
    case NamedTypeAST(name, Nil) => name match
      case "int" | "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" |
           "byte" | "char" | "rune" => "int"
      case "bool" => "bool"
      case n if enumNames(n) =>
        // Lowercase the first letter to match the enum type name in `emitEnum`.
        s"${n.head.toLower}${n.tail}"
      case other  => unsupported("type", other)
    case other => unsupported("type form", other.toString)

  /** Format an expression as a WhyML expression string. Operator translation is identical
   *  in code and contract positions for the Phase 1 subset (= and <>); we don't switch
   *  between `&&`/`/\` because either form is accepted in both positions in WhyML. */
  private def formatExpr(e: ExpressionAST): String = e match
    case IntLitAST(v) =>
      if v < 0 then s"(- ${-v})" else v.toString
    case BoolLitAST(v) => v.toString
    case VarRefAST(name) =>
      // `result` is WhyML's reserved name for a function's return value (only valid inside
      // ensures clauses). Otherwise: deref if the name names a WhyML ref, else plain.
      if name == "result" then "result"
      else if refScope(name) then s"!${sanitizeName(name)}"
      else sanitizeName(name)
    case BinaryAST(l, op, r) =>
      // `div` and `mod` from int.ComputerDivision are plain prefix functions in WhyML,
      // not infix operators — `a div b` would parse as `a` applied to `div b`. Emit them
      // as `(div a b)`. All other ops (arithmetic, comparison, logical) are infix.
      op match
        case "/"          => s"(div ${formatExpr(l)} ${formatExpr(r)})"
        case "%" | "mod"  => s"(mod ${formatExpr(l)} ${formatExpr(r)})"
        case _ =>
          val opStr = mapBinaryOp(op)
          s"(${formatExpr(l)} $opStr ${formatExpr(r)})"
    case UnaryAST(op, x) =>
      // The space after `-` and `not` is significant: `-x` would lex as the binary minus,
      // and `notx` as an identifier. Always render with a separator.
      val opStr = op match
        case "-"   => "- "
        case "not" => "not "
        case "!"   => "not "
        case other => unsupported("unary operator", other)
      s"($opStr${formatExpr(x)})"
    case CallAST("old", List(arg)) =>
      s"(old ${formatExpr(arg)})"
    case FieldAccessAST(VarRefAST(t), member) if enumNames(t) =>
      // sysl `EnumName.Variant` → WhyML bare `Variant`. WhyML constructors live at module
      // scope, not under their type, so we just drop the type prefix.
      member
    case CallAST(n, args) =>
      val argStr = if args.isEmpty then "" else args.map(formatExpr).mkString(" ", " ", "")
      s"(${sanitizeName(n)}$argStr)"
    case IfExprAST(c, tb, eb) =>
      val tExpr = stmtsAsExpr(tb)
      val eExpr = eb match
        case Some(stmts) => stmtsAsExpr(stmts)
        case None        => unsupported("if without else", "WhyML requires both branches")
      s"(if ${formatExpr(c)} then $tExpr else $eExpr)"
    case MatchExprAST(scrutinee, arms, default) =>
      // WhyML: `match e with | pat -> body | ... end`. Each arm's body must be a single
      // expression in Phase 3a — multi-statement arm bodies (let-bindings, sequencing) come
      // with the broader multi-stmt support in a later piece. Guards and multi-pattern arms
      // are also deferred. The default arm (sysl `else`) becomes a wildcard `_ -> ...`.
      //
      // WhyML restricts match patterns to ADT constructors and `_`/variables — integer and
      // bool literal patterns are NOT allowed. Sysl `n match { 0 -> a; 1 -> b; else -> c }`
      // therefore lowers to an if-chain `(if n = 0 then a else if n = 1 then b else c)`
      // when the scrutinee is not an enum value. Detection is syntactic: if every non-default
      // arm's pattern is an ADT constructor, emit `match`; otherwise lower to if-chain.
      if arms.exists(_.guard.isDefined) then unsupported("match arm with guard", "Phase 3a")
      if arms.exists(_.patterns.size > 1) then unsupported("match arm with multiple patterns", "Phase 3a")
      val allCtorArms = arms.forall { a =>
        a.patterns.head match
          case ValuePatternAST(FieldAccessAST(VarRefAST(t), _)) if enumNames(t) => true
          case WildcardPatternAST => true
          case _                  => false
      }
      if allCtorArms then
        val sb = new StringBuilder
        sb.append(s"(match ${formatExpr(scrutinee)} with")
        for arm <- arms do
          sb.append(s" | ${formatPattern(arm.patterns.head)} -> ${stmtsAsExpr(arm.body)}")
        default match
          case Some(stmts) => sb.append(s" | _ -> ${stmtsAsExpr(stmts)}")
          case None        =>
        sb.append(" end)")
        sb.toString
      else
        // Lower literal-pattern match to an if-chain. The scrutinee is evaluated once and
        // each pattern becomes an `=` test against it. Default → final `else`.
        val scr = formatExpr(scrutinee)
        val defaultExpr = default match
          case Some(stmts) => stmtsAsExpr(stmts)
          case None        => unsupported("literal-pattern match without `else` default",
                                          "WhyML cannot pattern-match int/bool literals — needs an exhaustive else")
        val sb = new StringBuilder
        sb.append("(")
        for arm <- arms do
          val key = arm.patterns.head match
            case ValuePatternAST(e)   => formatExpr(e)
            case WildcardPatternAST   => unsupported("wildcard before else in literal match", "Phase 3a")
            case other                => unsupported("literal-pattern shape", other.getClass.getSimpleName)
          sb.append(s"if $scr = $key then ${stmtsAsExpr(arm.body)} else ")
        sb.append(s"$defaultExpr)")
        sb.toString
    case QuantifierAST(kind, name, lo, hi, inclusive, pred) =>
      // sysl `for all x in lo..hi => P`  → `forall x: int. lo <= x <= hi -> P`
      // sysl `for all x in lo..<hi => P` → `forall x: int. lo <= x <  hi -> P`
      // sysl `for some` mirrors with `exists` and conjunction (/\) instead of implication.
      // The bound is `int` because Phase 1 collapses every sysl int width to mathematical int.
      val cmp = if inclusive then "<=" else "<"
      val v = sanitizeName(name)
      val loS = formatExpr(lo)
      val hiS = formatExpr(hi)
      val predS = formatExpr(pred)
      kind match
        case "all"  => s"(forall $v: int. $loS <= $v $cmp $hiS -> $predS)"
        case "some" => s"(exists $v: int. $loS <= $v $cmp $hiS /\\ $predS)"
        case other  => unsupported("quantifier kind", other)
    case other => unsupported("expression", other.getClass.getSimpleName)

  private def stmtsAsExpr(stmts: List[StmtAST]): String = stmts match
    case List(ReturnStmtAST(Some(e))) => formatExpr(e)
    case List(ExprStmtAST(e))         => formatExpr(e)
    case _ =>
      unsupported(
        "if-branch with non-trivial body",
        "Phase 1 supports only a single expression or single `return <expr>` per branch")

  /** Format a sysl match pattern as a WhyML pattern. Phase 3a covers the wildcard, integer
   *  literal patterns, and enum constructor patterns (the most common shapes for verifying
   *  algebraic-type case analysis). Range and destructuring patterns are deferred. */
  private def formatPattern(p: MatchPatternAST): String = p match
    case WildcardPatternAST => "_"
    case ValuePatternAST(IntLitAST(v))   => if v < 0 then s"(- ${-v})" else v.toString
    case ValuePatternAST(BoolLitAST(v))  => v.toString
    case ValuePatternAST(FieldAccessAST(VarRefAST(t), member)) if enumNames(t) => member
    case ValuePatternAST(VarRefAST(name)) => sanitizeName(name)
    case ValuePatternAST(other) => unsupported("match value pattern", other.getClass.getSimpleName)
    case _: RangePatternAST     => unsupported("range match pattern", "Phase 3a")
    case _: DestructurePatternAST => unsupported("destructuring match pattern", "Phase 3a")

  private def mapBinaryOp(op: String): String = op match
    case "==" => "="
    case "!=" => "<>"
    case "&&" => "/\\"
    case "||" => "\\/"
    // `/` and `%` route through int.ComputerDivision's `div` / `mod` — the only sense in
    // which integer division is total in WhyML. The lexer treats `mod` as an identifier;
    // it is recognized as the operator only because we imported ComputerDivision.
    case "/"   => "div"
    case "%"   => "mod"
    case "mod" => "mod"
    case "+" | "-" | "*" | "<" | ">" | "<=" | ">=" => op
    case other => unsupported("binary operator", other)

  /** Strip module-qualified prefixes; lowercase the first letter (WhyML reserves
   *  uppercase-first identifiers for constructors / modules); map sysl identifiers that
   *  collide with WhyML keywords to safe names. Constructor names go through a separate
   *  path (formatPattern / FieldAccessAST handling), so they retain their original case. */
  private def sanitizeName(n: String): String =
    val bare = n.indexOf("__") match
      case -1 => n
      case i  => n.substring(i + 2)
    // WhyML rejects uppercase-first identifiers in value position (those are constructors).
    // For all-uppercase identifiers like `MAX_AGE`, lowercase the entire string to avoid
    // ugly half-cased names like `mAX_AGE`. Mixed-case identifiers (`MyValue`) only need
    // the first letter lowered (`myValue`). The transformation is consistent across decl
    // and use sites because it's the same function.
    val lc =
      if bare.nonEmpty && bare.head.isUpper then
        if bare.forall(c => c.isUpper || c == '_' || c.isDigit) then bare.toLowerCase
        else s"${bare.head.toLower}${bare.tail}"
      else bare
    lc match
      case "function" | "let" | "in" | "with" | "match" | "end" | "module" | "use" |
           "type" | "begin" | "rec" | "and" | "or" | "fun" | "if" | "then" | "else" |
           "for" | "to" | "do" | "done" | "while" | "result" | "old" | "lemma" |
           "axiom" | "theory" | "predicate" | "exception" | "assert" | "assume" |
           "check" | "ghost" | "pure" | "absurd" | "raise" | "any" | "ref" |
           "writes" | "reads" | "requires" | "ensures" | "variant" | "invariant" =>
        lc + "_"
      case _ => lc
