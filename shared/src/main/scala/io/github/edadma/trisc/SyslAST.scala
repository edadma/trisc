package io.github.edadma.trisc

import io.github.edadma.gramma.Positional

// Top-level
case class ProgramAST(decls: List[DeclAST])

// Declarations
trait DeclAST extends Positional
case class FunDeclAST(returnType: String, name: String, params: List[ParamAST], body: List[StmtAST]) extends DeclAST
case class VarDeclAST(typ: String, name: String, init: Option[ExpressionAST]) extends DeclAST

case class ParamAST(typ: String, name: String) extends Positional

// Statements
trait StmtAST extends Positional
case class VarStmtAST(typ: String, name: String, init: Option[ExpressionAST]) extends StmtAST
case class AssignStmtAST(target: String, value: ExpressionAST) extends StmtAST
case class ReturnStmtAST(value: Option[ExpressionAST]) extends StmtAST
case class IfStmtAST(cond: ExpressionAST, thenBody: List[StmtAST], elseBody: Option[List[StmtAST]]) extends StmtAST
case class WhileStmtAST(cond: ExpressionAST, body: List[StmtAST]) extends StmtAST
case class ExprStmtAST(expr: ExpressionAST) extends StmtAST

// Expressions
trait ExpressionAST extends Positional
case class IntLitAST(value: Long) extends ExpressionAST
case class CharLitAST(value: Char) extends ExpressionAST
case class StringLitAST(value: String) extends ExpressionAST
case class VarRefAST(name: String) extends ExpressionAST
case class BinaryAST(left: ExpressionAST, op: String, right: ExpressionAST) extends ExpressionAST
case class UnaryAST(op: String, operand: ExpressionAST) extends ExpressionAST
case class CallAST(name: String, args: List[ExpressionAST]) extends ExpressionAST
