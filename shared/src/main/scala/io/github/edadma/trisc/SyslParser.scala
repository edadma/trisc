package io.github.edadma.trisc

import io.github.edadma.indentation.IndentationLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

object SyslParser extends StandardTokenParsers with PackratParsers {

  override val lexical: SyslLexical = new SyslLexical

  import lexical.{Newline, Indent, Dedent}

  // --- Entry point ---

  def parseProgram(source: String): Either[String, ProgramAST] =
    val tokens = lexical.read(new CharSequenceReader(source))
    phrase(program)(tokens) match
      case Success(result, _) => Right(result)
      case ns: NoSuccess      => Left(ns.toString)

  // --- Program ---

  lazy val program: PackratParser[ProgramAST] =
    repsep(decl, rep1(Newline)) <~ opt(rep(Newline)) ^^ ProgramAST.apply

  // --- Declarations ---

  lazy val decl: PackratParser[DeclAST] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ params ~ ((rt, body)) => FunDeclAST(name, params, rt, body)
    } |
      ident ~ (":" ~> typeExpr) ^^ {
        case name ~ t => VarDeclAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t))
      } |
      ident ~ (":" ~> typeName) ~ ("=" ~> expr) ^^ {
        case name ~ t ~ e => VarDeclAST(name, Some(t), e)
      } |
      ident ~ ("=" ~> expr) ^^ {
        case name ~ e => VarDeclAST(name, None, e)
      }

  lazy val funRest: PackratParser[(Option[String], FunBodyAST)] =
    "->" ~> typeName ~ ("=" ~> bodyExprOrBlock) ^^ { case rt ~ body => (Some(rt), body) } |
      "->" ~> typeName ~ block ^^ { case rt ~ body => (Some(rt), BlockBodyAST(body)) } |
      "=" ~> bodyExprOrBlock ^^ { body => (None, body) } |
      block ^^ { body => (None, BlockBodyAST(body)) }

  lazy val bodyExprOrBlock: PackratParser[FunBodyAST] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent ^^ (s => BlockBodyAST(s)) |
      expr ^^ ExprBodyAST.apply

  lazy val param: PackratParser[ParamAST] =
    ident ~ (":" ~> typeName) ^^ { case name ~ t => ParamAST(name, t) }

  lazy val typeName: PackratParser[String] =
    "int" | "char" | "void" | ident

  lazy val typeExpr: PackratParser[String] =
    "[" ~> numericLit ~ ("]" ~> typeName) ^^ { case n ~ t => s"[$n]$t" }

  // --- Block ---

  lazy val block: PackratParser[List[StmtAST]] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent

  // --- Statements ---

  lazy val stmts: PackratParser[List[StmtAST]] =
    rep1sep(stmt, rep1(Newline))

  lazy val stmt: PackratParser[StmtAST] =
    whileStmt | returnStmt | derefAssignStmt | identStmt | expr ^^ ExprStmtAST.apply

  lazy val identStmt: PackratParser[StmtAST] =
    ident ~ (":" ~> typeExpr) ^^ { case name ~ t => VarStmtAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t)) } |
      ident ~ (":" ~> typeName) ~ ("=" ~> expr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) ^^ { case name ~ idx ~ value =>
        IndexAssignStmtAST(VarRefAST(name), idx, value)
      } |
      ident ~ ("=" ~> expr) ^^ { case name ~ e => AssignStmtAST(name, e) }

  lazy val derefAssignStmt: PackratParser[StmtAST] =
    "*" ~> unary ~ ("=" ~> expr) ^^ { case ptr ~ value => DerefAssignStmtAST(ptr, value) }

  lazy val whileStmt: PackratParser[WhileStmtAST] =
    "while" ~> expr ~ block ^^ { case cond ~ body => WhileStmtAST(cond, body) }

  lazy val returnStmt: PackratParser[ReturnStmtAST] =
    "return" ~> opt(expr) ^^ ReturnStmtAST.apply

  // --- Expressions ---

  lazy val expr: PackratParser[ExpressionAST] = ifExpr | logicalOr

  lazy val ifExpr: PackratParser[IfExprAST] =
    "if" ~> logicalOr ~ ("then" ~> thenBody) ^^ { case cond ~ ((tb, eb)) => IfExprAST(cond, tb, eb) } |
      "if" ~> logicalOr ~ block ~ opt(Newline ~> elseClause) ^^ {
        case cond ~ body ~ elseBody => IfExprAST(cond, body, elseBody)
      }

  lazy val thenBody: PackratParser[(List[StmtAST], Option[List[StmtAST]])] =
    block ~ opt(Newline ~> elseClause) ^^ { case body ~ eb => (body, eb) } |
      inlineStmt ~ opt(elseInline) ^^ { case s ~ eb => (List(s), eb) }

  lazy val elseInline: PackratParser[List[StmtAST]] =
    "else" ~> (ifExpr ^^ (e => List(ExprStmtAST(e))) | inlineStmt ^^ (s => List(s))) |
      Newline ~> elseClause

  lazy val elseClause: PackratParser[List[StmtAST]] =
    "else" ~> (
      ifExpr ^^ (e => List(ExprStmtAST(e))) |
        block |
        inlineStmt ^^ (s => List(s))
    )

  lazy val inlineStmt: PackratParser[StmtAST] =
    returnStmt |
      "*" ~> unary ~ ("=" ~> expr) ^^ { case ptr ~ value => DerefAssignStmtAST(ptr, value) } |
      ident ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) ^^ { case name ~ idx ~ value =>
        IndexAssignStmtAST(VarRefAST(name), idx, value)
      } |
      ident ~ ("=" ~> expr) ^^ { case name ~ e => AssignStmtAST(name, e) } |
      expr ^^ ExprStmtAST.apply

  // --- Precedence climbing ---

  lazy val logicalOr: PackratParser[ExpressionAST] =
    logicalAnd ~ rep("||" ~> logicalAnd) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "||", r))
    }

  lazy val logicalAnd: PackratParser[ExpressionAST] =
    comparison ~ rep("&&" ~> comparison) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "&&", r))
    }

  lazy val comparisonOp: PackratParser[String] =
    "==" | "!=" | "<=" | ">=" | "<" | ">"

  lazy val comparison: PackratParser[ExpressionAST] =
    additive ~ rep(comparisonOp ~ additive) ^^ {
      case first ~ Nil => first
      case first ~ chain =>
        val operands = first :: chain.map { case _ ~ operand => operand }
        val ops = chain.map { case op ~ _ => op }
        val pairs = for i <- ops.indices yield
          BinaryAST(operands(i), ops(i), operands(i + 1))
        pairs.reduceLeft((l, r) => BinaryAST(l, "&&", r))
    }

  lazy val additive: PackratParser[ExpressionAST] =
    multiplicative ~ rep(("+" | "-") ~ multiplicative) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val multiplicative: PackratParser[ExpressionAST] =
    unary ~ rep(("*" | "/" | "%") ~ unary) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val unary: PackratParser[ExpressionAST] =
    "-" ~> unary ^^ (e => UnaryAST("-", e)) |
      "!" ~> unary ^^ (e => UnaryAST("!", e)) |
      "*" ~> unary ^^ DerefAST.apply |
      "&" ~> ident ~ ("[" ~> expr <~ "]") ^^ { case name ~ idx => AddrOfIndexAST(VarRefAST(name), idx) } |
      "&" ~> ident ^^ AddrOfAST.apply |
      postfix

  lazy val postfix: PackratParser[ExpressionAST] =
    primary ~ rep("[" ~> expr <~ "]") ^^ {
      case base ~ indices => indices.foldLeft(base)((e, idx) => IndexAST(e, idx))
    }

  lazy val primary: PackratParser[ExpressionAST] =
    numericLit ^^ (n => IntLitAST(n.toLong)) |
      stringLit ^^ StringLitAST.apply |
      "true" ^^^ BoolLitAST(true) |
      "false" ^^^ BoolLitAST(false) |
      ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => CallAST(name, args) } |
      ident ^^ VarRefAST.apply |
      "(" ~> expr <~ ")"
}
