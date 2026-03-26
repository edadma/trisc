package io.github.edadma.trisc

import scala.util.parsing.input.Positional

// Top-level
case class ProgramAST(decls: List[DeclAST])

// Declarations
trait DeclAST extends Positional
case class FunDeclAST(name: String, params: List[ParamAST], returnType: Option[String], body: FunBodyAST) extends DeclAST
case class VarDeclAST(name: String, typ: Option[String], init: ExpressionAST) extends DeclAST

case class ParamAST(name: String, typ: String) extends Positional

// Function body
trait FunBodyAST
case class ExprBodyAST(expr: ExpressionAST) extends FunBodyAST
case class BlockBodyAST(stmts: List[StmtAST]) extends FunBodyAST

// Statements
trait StmtAST extends Positional
case class VarStmtAST(name: String, typ: Option[String], init: ExpressionAST) extends StmtAST
case class AssignStmtAST(target: String, value: ExpressionAST) extends StmtAST
case class ReturnStmtAST(value: Option[ExpressionAST]) extends StmtAST
case class WhileStmtAST(cond: ExpressionAST, body: List[StmtAST]) extends StmtAST
case class ExprStmtAST(expr: ExpressionAST) extends StmtAST

// Expressions
trait ExpressionAST extends Positional
case class IntLitAST(value: Long) extends ExpressionAST
case class CharLitAST(value: Char) extends ExpressionAST
case class StringLitAST(value: String) extends ExpressionAST
case class BoolLitAST(value: Boolean) extends ExpressionAST
case class VarRefAST(name: String) extends ExpressionAST
case class BinaryAST(left: ExpressionAST, op: String, right: ExpressionAST) extends ExpressionAST
case class UnaryAST(op: String, operand: ExpressionAST) extends ExpressionAST
case class CallAST(name: String, args: List[ExpressionAST]) extends ExpressionAST
case class IfExprAST(cond: ExpressionAST, thenBody: List[StmtAST], elseBody: Option[List[StmtAST]]) extends ExpressionAST
