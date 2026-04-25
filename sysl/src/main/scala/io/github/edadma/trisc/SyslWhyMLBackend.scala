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
    val enums = program.decls.collect { case e: EnumDeclAST => e }
    enumNames = enums.map(_.name).toSet
    val fns = program.decls.collect { case f: FunDeclAST => f }
    line(s"module $moduleName")
    indentLevel += 1
    line("use int.Int")
    // ComputerDivision provides truncating `div` and `mod` (C-style). int.Int does not
    // define `/` or `%` because their semantics are debatable; we pin to the C-style choice
    // since it matches sysl's interpreter and codegen behavior. Always-imported is harmless.
    line("use int.ComputerDivision")
    blank()
    var first = true
    for e <- enums do
      if !first then blank()
      first = false
      emitEnum(e)
    for fn <- fns do
      if !first then blank()
      first = false
      emitFunction(fn)
    indentLevel -= 1
    line("end")
    out.toString

  /** sysl `enum Color { Red, Green, Blue }` → WhyML `type color = Red | Green | Blue`.
   *  The integer values that sysl assigns (auto-incrementing or explicit) are dropped —
   *  WhyML algebraic types don't expose a numeric tag, and proofs typically reason about
   *  variant identity rather than its underlying int. WhyML type names are conventionally
   *  lowercase; constructor names stay as-written (sysl convention is also uppercase). */
  private def emitEnum(e: EnumDeclAST): Unit =
    val typeName = e.name.head.toLower + e.name.tail
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
    val (contracts, bodyExpr) = splitBody(fn)
    val isGhost = fn.attributes.exists(_.name == "ghost")

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
    val isPredicateShape = fn.isDef && returnsBool && contracts.isEmpty && isFormula(bodyExpr)
    if isPredicateShape then
      line(s"predicate $name $params = ${stripOuterParens(formatExpr(bodyExpr))}")
      return

    val recKw = if isRecursive(fn) then "rec " else ""
    // WhyML's `ghost` qualifier marks a function as proof-only — it is checked but erased
    // before extraction. Maps 1:1 from sysl's `#ghost`. The required keyword order is
    // `let [rec] [ghost] function f ...` — ghost must follow rec, not precede it.
    val ghostKw = if isGhost then "ghost " else ""
    val ret = fn.returnType match
      case None    => unsupported("function without explicit return type", fn.name)
      case Some(t) => typeOf(t)
    line(s"let $recKw$ghostKw" + s"function $name $params : $ret")
    indentLevel += 1
    for c <- contracts do emitContract(c)
    line(s"= ${formatExpr(bodyExpr)}")
    indentLevel -= 1

  /** True if `e` is a formula-shaped expression — currently just a top-level quantifier.
   *  Extended in later phases to recognize boolean connectives joining quantifiers. */
  private def isFormula(e: ExpressionAST): Boolean = e match
    case _: QuantifierAST => true
    case _                => false

  private def splitBody(fn: FunDeclAST): (List[ContractClauseAST], ExpressionAST) =
    fn.body match
      case ExprBodyAST(e) => (Nil, e)
      case BlockBodyAST(stmts, contracts) =>
        val bodyExpr = stmts match
          case List(ReturnStmtAST(Some(e))) => e
          case List(ExprStmtAST(e))         => e
          case _ =>
            unsupported(
              "block-bodied function with non-trivial body",
              s"${fn.name}: Phase 1 only supports a single expression or a single `return <expr>`")
        (contracts, bodyExpr)

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
        n.head.toLower + n.tail
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
      if name == "result" then "result" else sanitizeName(name)
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

  /** Strip module-qualified prefixes for now; map sysl identifiers that collide with WhyML
   *  keywords to safe names. Phase 1 keeps this near-identity since the test surface is small. */
  private def sanitizeName(n: String): String =
    val bare = n.indexOf("__") match
      case -1 => n
      case i  => n.substring(i + 2)
    bare match
      case "function" | "let" | "in" | "with" | "match" | "end" | "module" | "use" |
           "type" | "begin" | "rec" | "and" | "or" | "fun" | "if" | "then" | "else" |
           "for" | "to" | "do" | "done" | "while" | "result" | "old" | "lemma" |
           "axiom" | "theory" | "predicate" | "exception" | "assert" | "assume" |
           "check" | "ghost" | "pure" | "absurd" | "raise" | "any" | "ref" |
           "writes" | "reads" | "requires" | "ensures" | "variant" | "invariant" =>
        bare + "_"
      case _ => bare
