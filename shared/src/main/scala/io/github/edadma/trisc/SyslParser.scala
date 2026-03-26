package io.github.edadma.trisc

import io.github.edadma.indentation.IndentationLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharSequenceReader

class SyslParser extends StandardTokenParsers {

  override val lexical: SyslLexical = new SyslLexical

  import lexical.{Newline, Indent, Dedent}

  // --- Entry point ---

  def parseProgram(source: String): Either[String, ProgramAST] =
    phrase(program)(lexical.read(new CharSequenceReader(source))) match
      case Success(result, _) => Right(result)
      case ns: NoSuccess      => Left(ns.toString)

  // --- Program ---

  lazy val program: Parser[ProgramAST] =
    repsep(decl, rep1(Newline)) <~ opt(rep(Newline)) ^^ ProgramAST.apply

  // --- Declarations ---

  lazy val decl: Parser[DeclAST] =
    ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ funRest ^^ {
      case name ~ params ~ ((rt, body)) => FunDeclAST(name, params, rt, body)
    } |
      ident ~ (":" ~> typeExpr) ^^ {
        case name ~ t => VarDeclAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t))
      } |
      ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ {
        case name ~ t ~ e => VarDeclAST(name, Some(t), e)
      } |
      ident ~ ("=" ~> expr) ^^ {
        case name ~ e => VarDeclAST(name, None, e)
      }

  lazy val funRest: Parser[(Option[String], FunBodyAST)] =
    "->" ~> typeRef ~ ("=" ~> bodyExprOrBlock) ^^ { case rt ~ body => (Some(rt), body) } |
      "->" ~> typeRef ~ block ^^ { case rt ~ body => (Some(rt), BlockBodyAST(body)) } |
      "=" ~> bodyExprOrBlock ^^ { body => (None, body) } |
      block ^^ { body => (None, BlockBodyAST(body)) }

  lazy val bodyExprOrBlock: Parser[FunBodyAST] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent ^^ (s => BlockBodyAST(s)) |
      expr ^^ ExprBodyAST.apply

  lazy val param: Parser[ParamAST] =
    ident ~ (":" ~> typeRef) ^^ { case name ~ t => ParamAST(name, t) }

  lazy val typeName: Parser[String] =
    "int" | "char" | "byte" | "void" | ident

  // Full type reference: *int, **int, [5]int, int, etc.
  lazy val typeRef: Parser[String] =
    "*" ~> typeRef ^^ (t => s"*$t") |
      "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => s"[$n]$t" } |
      typeName

  // Array type for variable declarations: [5]int
  lazy val typeExpr: Parser[String] =
    "[" ~> numericLit ~ ("]" ~> typeRef) ^^ { case n ~ t => s"[$n]$t" }

  // --- Block ---

  lazy val block: Parser[List[StmtAST]] =
    Newline ~> Indent ~> stmts <~ opt(Newline) <~ Dedent

  // --- Statements ---

  lazy val stmts: Parser[List[StmtAST]] =
    rep1sep(stmt, rep1(Newline))

  lazy val stmt: Parser[StmtAST] =
    whileStmt | returnStmt | derefAssignStmt | identStmt | expr ^^ ExprStmtAST.apply

  lazy val compoundOp: Parser[String] =
    "<<=" | ">>=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="

  lazy val identStmt: Parser[StmtAST] =
    ident ~ (":" ~> typeExpr) ^^ { case name ~ t => VarStmtAST(name, Some(t), ArrayDeclAST(t.drop(1).takeWhile(_.isDigit).toInt, t)) } |
      ident ~ (":" ~> typeRef) ~ ("=" ~> expr) ^^ { case name ~ t ~ e => VarStmtAST(name, Some(t), e) } |
      ident ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) ^^ { case name ~ idx ~ value =>
        IndexAssignStmtAST(VarRefAST(name), idx, value)
      } |
      ident ~ compoundOp ~ expr ^^ { case name ~ op ~ e => CompoundAssignStmtAST(name, op.init, e) } |
      ident ~ ("=" ~> expr) ^^ { case name ~ e => AssignStmtAST(name, e) }

  lazy val derefAssignStmt: Parser[StmtAST] =
    "*" ~> unary ~ ("=" ~> expr) ^^ { case ptr ~ value => DerefAssignStmtAST(ptr, value) }

  lazy val whileStmt: Parser[WhileStmtAST] =
    "while" ~> expr ~ ("do" ~> (block | inlineStmt ^^ (s => List(s)))) ^^ { case cond ~ body => WhileStmtAST(cond, body) } |
      "while" ~> expr ~ block ^^ { case cond ~ body => WhileStmtAST(cond, body) }

  lazy val returnStmt: Parser[ReturnStmtAST] =
    "return" ~> opt(expr) ^^ ReturnStmtAST.apply

  // --- Expressions ---

  lazy val expr: Parser[ExpressionAST] = ifExpr | logicalOr

  lazy val ifExpr: Parser[IfExprAST] =
    "if" ~> logicalOr ~ ("then" ~> thenBody) ^^ { case cond ~ ((tb, eb)) => IfExprAST(cond, tb, eb) } |
      "if" ~> logicalOr ~ block ~ opt(Newline ~> elseOrElif) ^^ {
        case cond ~ body ~ elseBody => IfExprAST(cond, body, elseBody)
      }

  lazy val elifExpr: Parser[IfExprAST] =
    "elif" ~> logicalOr ~ ("then" ~> thenBody) ^^ { case cond ~ ((tb, eb)) => IfExprAST(cond, tb, eb) } |
      "elif" ~> logicalOr ~ block ~ opt(Newline ~> elseOrElif) ^^ {
        case cond ~ body ~ elseBody => IfExprAST(cond, body, elseBody)
      }

  lazy val thenBody: Parser[(List[StmtAST], Option[List[StmtAST]])] =
    block ~ opt(Newline ~> elseOrElif) ^^ { case body ~ eb => (body, eb) } |
      inlineStmt ~ opt(elseInline) ^^ { case s ~ eb => (List(s), eb) }

  lazy val elseInline: Parser[List[StmtAST]] =
    "else" ~> (ifExpr ^^ (e => List(ExprStmtAST(e))) | inlineStmt ^^ (s => List(s))) |
      Newline ~> elseOrElif

  lazy val elseOrElif: Parser[List[StmtAST]] =
    elifExpr ^^ (e => List(ExprStmtAST(e))) |
      "else" ~> (
        ifExpr ^^ (e => List(ExprStmtAST(e))) |
          block |
          inlineStmt ^^ (s => List(s))
      )

  lazy val inlineStmt: Parser[StmtAST] =
    returnStmt |
      "*" ~> unary ~ ("=" ~> expr) ^^ { case ptr ~ value => DerefAssignStmtAST(ptr, value) } |
      ident ~ ("[" ~> expr <~ "]") ~ ("=" ~> expr) ^^ { case name ~ idx ~ value =>
        IndexAssignStmtAST(VarRefAST(name), idx, value)
      } |
      ident ~ compoundOp ~ expr ^^ { case name ~ op ~ e => CompoundAssignStmtAST(name, op.init, e) } |
      ident ~ ("=" ~> expr) ^^ { case name ~ e => AssignStmtAST(name, e) } |
      expr ^^ ExprStmtAST.apply

  // --- Precedence climbing ---

  lazy val logicalOr: Parser[ExpressionAST] =
    logicalAnd ~ rep("||" ~> logicalAnd) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "||", r))
    }

  lazy val logicalAnd: Parser[ExpressionAST] =
    comparison ~ rep("&&" ~> comparison) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "&&", r))
    }

  lazy val comparisonOp: Parser[String] =
    "==" | "!=" | "<=" | ">=" | "<" | ">"

  lazy val comparison: Parser[ExpressionAST] =
    bitwiseOr ~ rep(comparisonOp ~ bitwiseOr) ^^ {
      case first ~ Nil => first
      case first ~ chain =>
        val operands = first :: chain.map { case _ ~ operand => operand }
        val ops = chain.map { case op ~ _ => op }
        val pairs = for i <- ops.indices yield
          BinaryAST(operands(i), ops(i), operands(i + 1))
        pairs.reduceLeft((l, r) => BinaryAST(l, "&&", r))
    }

  lazy val bitwiseOr: Parser[ExpressionAST] =
    bitwiseXor ~ rep("|" ~> bitwiseXor) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "|", r))
    }

  lazy val bitwiseXor: Parser[ExpressionAST] =
    bitwiseAnd ~ rep("^" ~> bitwiseAnd) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "^", r))
    }

  lazy val bitwiseAnd: Parser[ExpressionAST] =
    shift ~ rep("&" ~> shift) ^^ {
      case first ~ rest => rest.foldLeft(first)((l, r) => BinaryAST(l, "&", r))
    }

  lazy val shift: Parser[ExpressionAST] =
    additive ~ rep(("<<" | ">>") ~ additive) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val additive: Parser[ExpressionAST] =
    multiplicative ~ rep(("+" | "-") ~ multiplicative) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val multiplicative: Parser[ExpressionAST] =
    unary ~ rep(("*" | "/" | "%") ~ unary) ^^ {
      case first ~ rest => rest.foldLeft(first) { case (l, op ~ r) => BinaryAST(l, op, r) }
    }

  lazy val unary: Parser[ExpressionAST] =
    "++" ~> ident ^^ PreIncAST.apply |
      "--" ~> ident ^^ PreDecAST.apply |
      "-" ~> unary ^^ (e => UnaryAST("-", e)) |
      "!" ~> unary ^^ (e => UnaryAST("!", e)) |
      "~" ~> unary ^^ (e => UnaryAST("~", e)) |
      "*" ~> unary ^^ DerefAST.apply |
      "&" ~> ident ~ ("[" ~> expr <~ "]") ^^ { case name ~ idx => AddrOfIndexAST(VarRefAST(name), idx) } |
      "&" ~> ident ^^ AddrOfAST.apply |
      postfix

  lazy val postfix: Parser[ExpressionAST] =
    ident <~ "++" ^^ PostIncAST.apply |
      ident <~ "--" ^^ PostDecAST.apply |
      primary ~ rep("[" ~> expr <~ "]") ^^ {
        case base ~ indices => indices.foldLeft(base)((e, idx) => IndexAST(e, idx))
      }

  lazy val charLit: Parser[ExpressionAST] =
    stringLit ^? ({
      case s if s.length == 1 => IntLitAST(s.charAt(0).toLong)
      case s if s.length == 2 && s.charAt(0) == '\\' => IntLitAST(s.charAt(1) match
        case 'n' => '\n'.toLong
        case 't' => '\t'.toLong
        case 'r' => '\r'.toLong
        case '0' => 0L
        case '\\' => '\\'.toLong
        case '\'' => '\''.toLong
        case '"' => '"'.toLong
        case c => c.toLong
      )
    }, s => s"invalid char literal: '$s'")

  lazy val primary: Parser[ExpressionAST] =
    numericLit ^^ (n => IntLitAST(n.toLong)) |
      charLit |
      stringLit ^^ StringLitExprAST.apply |
      "true" ^^^ BoolLitAST(true) |
      "false" ^^^ BoolLitAST(false) |
      ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => CallAST(name, args) } |
      ident ^^ VarRefAST.apply |
      "(" ~> expr <~ ")"
}
