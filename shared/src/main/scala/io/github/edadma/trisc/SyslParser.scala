package io.github.edadma.trisc

import io.github.edadma.gramma.*
import scala.language.implicitConversions

object SyslParser extends StdParsers(SyslLexer):

  // --- Entry point ---

  def parseProgram(source: String): Either[ParseError, ProgramAST] =
    parseSource(source)(program)

  // --- Program ---

  def program(using ctx: ParseCtx): P[ProgramAST] =
    repsep(decl, rep1(newline)) ^^ ProgramAST.apply

  // --- Declarations ---

  def decl(using ctx: ParseCtx): P[DeclAST] =
    typeName ~ ident ~ ("(" ~> repsep(param, ",") <~ ")") ~ block(stmts) ^^ {
      case rt ~ name ~ params ~ body => FunDeclAST(rt, name, params, body): DeclAST
    } |
      typeName ~ ident ~ opt("=" ~> expr) ^^ {
        case t ~ name ~ init => VarDeclAST(t, name, init): DeclAST
      }

  def param(using ctx: ParseCtx): P[ParamAST] =
    typeName ~ ident ^^ { case t ~ name => ParamAST(t, name) }

  def typeName(using ctx: ParseCtx): P[String] =
    "int" | "char" | "void"

  // --- Statements ---

  def stmts(using ctx: ParseCtx): P[List[StmtAST]] =
    rep1sep(stmt, newline)

  def stmt(using ctx: ParseCtx): P[StmtAST] =
    ifStmt ^^ (s => s: StmtAST) |
      whileStmt ^^ (s => s: StmtAST) |
      returnStmt ^^ (s => s: StmtAST) |
      varStmt ^^ (s => s: StmtAST) |
      assignOrExprStmt

  def ifStmt(using ctx: ParseCtx): P[IfStmtAST] =
    "if" ~> expr ~ block(stmts) ~ opt(newline ~> "else" ~> (block(stmts) | (ifStmt ^^ (s => List(s))))) ^^ {
      case cond ~ thenBody ~ elseBody => IfStmtAST(cond, thenBody, elseBody)
    }

  def whileStmt(using ctx: ParseCtx): P[WhileStmtAST] =
    "while" ~> expr ~ block(stmts) ^^ {
      case cond ~ body => WhileStmtAST(cond, body)
    }

  def returnStmt(using ctx: ParseCtx): P[ReturnStmtAST] =
    "return" ~> opt(expr) ^^ ReturnStmtAST.apply

  def varStmt(using ctx: ParseCtx): P[VarStmtAST] =
    typeName ~ ident ~ opt("=" ~> expr) ^^ {
      case t ~ name ~ init => VarStmtAST(t, name, init)
    }

  def assignOrExprStmt(using ctx: ParseCtx): P[StmtAST] =
    ident ~ ("=" ~> expr) ^^ { case name ~ value => AssignStmtAST(name, value) } |
      expr ^^ ExprStmtAST.apply

  // --- Expressions (precedence climbing) ---

  def expr(using ctx: ParseCtx): P[ExpressionAST] = logicalOr

  def logicalOr(using ctx: ParseCtx): P[ExpressionAST] =
    leftAssoc(logicalAnd, "||")((l, op, r) => BinaryAST(l, op, r))

  def logicalAnd(using ctx: ParseCtx): P[ExpressionAST] =
    leftAssoc(equality, "&&")((l, op, r) => BinaryAST(l, op, r))

  def equality(using ctx: ParseCtx): P[ExpressionAST] =
    leftAssoc(comparison, "==" | "!=")((l, op, r) => BinaryAST(l, op, r))

  def comparison(using ctx: ParseCtx): P[ExpressionAST] =
    leftAssoc(additive, "<=" | ">=" | "<" | ">")((l, op, r) => BinaryAST(l, op, r))

  def additive(using ctx: ParseCtx): P[ExpressionAST] =
    leftAssoc(multiplicative, "+" | "-")((l, op, r) => BinaryAST(l, op, r))

  def multiplicative(using ctx: ParseCtx): P[ExpressionAST] =
    leftAssoc(unary, "*" | "/" | "%")((l, op, r) => BinaryAST(l, op, r))

  def unary(using ctx: ParseCtx): P[ExpressionAST] =
    "-" ~> unary ^^ (e => UnaryAST("-", e)) |
      "!" ~> unary ^^ (e => UnaryAST("!", e)) |
      primary

  def primary(using ctx: ParseCtx): P[ExpressionAST] =
    numericLit ^^ (n => IntLitAST(n.toLong)) |
      stringLit ^^ StringLitAST.apply |
      ident ~ ("(" ~> repsep(expr, ",") <~ ")") ^^ { case name ~ args => CallAST(name, args) } |
      ident ^^ VarRefAST.apply |
      "(" ~> expr <~ ")"
