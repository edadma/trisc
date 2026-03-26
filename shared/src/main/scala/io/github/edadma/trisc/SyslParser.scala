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
    ident >> { name =>
      "(" ~> repsep(param, ",") <~ ")" >> { params =>
        funRest(name, params)
      } |
        declRest(name)
    }

  def typeName(using ctx: ParseCtx): P[String] =
    "int" | "char" | "void" | ident

  def funRest(name: String, params: List[ParamAST])(using ctx: ParseCtx): P[DeclAST] =
    "->" ~> typeName >> { rt =>
      "=" ~> bodyExprOrBlock ^^ { body => FunDeclAST(name, params, Some(rt), body): DeclAST } |
        block(stmts) ^^ { body => FunDeclAST(name, params, Some(rt), BlockBodyAST(body)): DeclAST }
    } |
      "=" ~> bodyExprOrBlock ^^ { body => FunDeclAST(name, params, None, body): DeclAST } |
      block(stmts) ^^ { body => FunDeclAST(name, params, None, BlockBodyAST(body)): DeclAST }

  def bodyExprOrBlock(using ctx: ParseCtx): P[FunBodyAST] =
    block(stmts) ^^ (s => BlockBodyAST(s, isExprBlock = true): FunBodyAST) |
      expr ^^ (e => ExprBodyAST(e): FunBodyAST)

  def declRest(name: String)(using ctx: ParseCtx): P[DeclAST] =
    ":" ~> typeName ~ ("=" ~> expr) ^^ { case t ~ e => VarDeclAST(name, Some(t), e): DeclAST } |
      "=" ~> expr ^^ { e => VarDeclAST(name, None, e): DeclAST }

  def param(using ctx: ParseCtx): P[ParamAST] =
    ident ~ (":" ~> typeName) ^^ { case name ~ t => ParamAST(name, t) }

  // --- Statements ---

  def stmts(using ctx: ParseCtx): P[List[StmtAST]] =
    rep1sep(stmt, newline)

  def stmt(using ctx: ParseCtx): P[StmtAST] =
    ifStmt ^^ (s => s: StmtAST) |
      whileStmt ^^ (s => s: StmtAST) |
      returnStmt ^^ (s => s: StmtAST) |
      identStmt |
      expr ^^ (e => ExprStmtAST(e): StmtAST)

  def identStmt(using ctx: ParseCtx): P[StmtAST] =
    ident >> { name =>
      ":" ~> typeName ~ ("=" ~> expr) ^^ { case t ~ e => VarStmtAST(name, Some(t), e): StmtAST } |
        "=" ~> expr ^^ { e => AssignStmtAST(name, e): StmtAST } |
        "(" ~> repsep(expr, ",") <~ ")" ^^ { args => ExprStmtAST(CallAST(name, args)): StmtAST } |
        continueExpr(VarRefAST(name)) ^^ { e => ExprStmtAST(e): StmtAST }
    }

  // Continue parsing an expression given a left-hand operand already parsed
  def continueExpr(left: ExpressionAST)(using ctx: ParseCtx): P[ExpressionAST] =
    val op = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<=" | ">=" | "<" | ">" | "&&" | "||"
    op ~ expr ^^ { case o ~ r => BinaryAST(left, o, r) } |
      succeed(left)

  def ifStmt(using ctx: ParseCtx): P[IfStmtAST] =
    "if" ~> expr ~ block(stmts) >> { case cond ~ thenBody =>
      if peek(newline ~> keyword("else")) then
        newline ~> "else" ~> (block(stmts) | (ifStmt ^^ (s => List(s)))) ^^ { elseBody =>
          IfStmtAST(cond, thenBody, Some(elseBody))
        }
      else
        succeed(IfStmtAST(cond, thenBody, None))
    }

  def whileStmt(using ctx: ParseCtx): P[WhileStmtAST] =
    "while" ~> expr ~ block(stmts) ^^ {
      case cond ~ body => WhileStmtAST(cond, body)
    }

  def returnStmt(using ctx: ParseCtx): P[ReturnStmtAST] =
    "return" ~> opt(expr) ^^ ReturnStmtAST.apply

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
      "true" ^^ (_ => BoolLitAST(true)) |
      "false" ^^ (_ => BoolLitAST(false)) |
      ident >> { name =>
        "(" ~> repsep(expr, ",") <~ ")" ^^ (args => CallAST(name, args): ExpressionAST) |
          succeed(VarRefAST(name): ExpressionAST)
      } |
      "(" ~> expr <~ ")"
