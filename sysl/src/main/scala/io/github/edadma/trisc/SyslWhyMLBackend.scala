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
    line(s"module $moduleName")
    indentLevel += 1
    line("use int.Int")
    blank()
    val fns = program.decls.collect { case f: FunDeclAST => f }
    var first = true
    for fn <- fns do
      if !first then blank()
      first = false
      emitFunction(fn)
    indentLevel -= 1
    line("end")
    out.toString

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
    val recKw = if isRecursive(fn) then "rec " else ""
    val name = sanitizeName(fn.name)
    val params =
      if fn.params.isEmpty then "()"
      else fn.params.map(p => s"(${sanitizeName(p.name)}: ${typeOf(p.typ)})").mkString(" ")
    val ret = fn.returnType match
      case None       => unsupported("function without explicit return type", fn.name)
      case Some(t)    => typeOf(t)
    line(s"let ${recKw}function $name $params : $ret")
    indentLevel += 1
    val (contracts, bodyExpr) = splitBody(fn)
    for c <- contracts do emitContract(c)
    line(s"= ${formatExpr(bodyExpr)}")
    indentLevel -= 1

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
    case CallAST(n, args) =>
      val argStr = if args.isEmpty then "" else args.map(formatExpr).mkString(" ", " ", "")
      s"(${sanitizeName(n)}$argStr)"
    case IfExprAST(c, tb, eb) =>
      val tExpr = stmtsAsExpr(tb)
      val eExpr = eb match
        case Some(stmts) => stmtsAsExpr(stmts)
        case None        => unsupported("if without else", "WhyML requires both branches")
      s"(if ${formatExpr(c)} then $tExpr else $eExpr)"
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
    case "+" | "-" | "*" | "/" | "<" | ">" | "<=" | ">=" => op
    case "mod" => "mod"
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
