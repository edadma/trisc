package io.github.edadma.trisc

import scala.util.parsing.input.Positional

// Top-level
case class ProgramAST(decls: List[DeclAST])

// Declarations
trait DeclAST extends Positional
case class ImportDeclAST(path: String) extends DeclAST
case class ExternFuncDeclAST(name: String, params: List[ParamAST], returnType: Option[String]) extends DeclAST
case class FunDeclAST(name: String, params: List[ParamAST], returnType: Option[String], body: FunBodyAST, isPrivate: Boolean = false) extends DeclAST
case class VarDeclAST(name: String, typ: Option[String], init: ExpressionAST, isPrivate: Boolean = false, isMutable: Boolean = true) extends DeclAST
case class StructDeclAST(name: String, fields: List[(String, String)]) extends DeclAST
case class EnumDeclAST(name: String, members: List[(String, Option[Long])]) extends DeclAST
case class TypeAliasDeclAST(name: String, target: String) extends DeclAST

case class ParamAST(name: String, typ: String) extends Positional

// Function body
trait FunBodyAST
case class ExprBodyAST(expr: ExpressionAST) extends FunBodyAST
case class BlockBodyAST(stmts: List[StmtAST]) extends FunBodyAST

// Statements
trait StmtAST extends Positional
case class VarStmtAST(name: String, typ: Option[String], init: ExpressionAST, isMutable: Boolean = true) extends StmtAST
case class AssignStmtAST(target: String, value: ExpressionAST) extends StmtAST
case class CompoundAssignStmtAST(target: String, op: String, value: ExpressionAST) extends StmtAST
case class DerefAssignStmtAST(pointer: ExpressionAST, value: ExpressionAST) extends StmtAST
case class IndexAssignStmtAST(array: ExpressionAST, index: ExpressionAST, value: ExpressionAST) extends StmtAST
case class FieldAssignStmtAST(obj: ExpressionAST, field: String, value: ExpressionAST) extends StmtAST
case class FieldCompoundAssignStmtAST(obj: ExpressionAST, field: String, op: String, value: ExpressionAST) extends StmtAST
case class ReturnStmtAST(value: Option[ExpressionAST]) extends StmtAST
case class WhileStmtAST(cond: ExpressionAST, body: List[StmtAST]) extends StmtAST
case class ForStmtAST(init: StmtAST, cond: ExpressionAST, update: StmtAST, body: List[StmtAST]) extends StmtAST
case class DoWhileStmtAST(cond: ExpressionAST, body: List[StmtAST]) extends StmtAST
case class BreakStmtAST() extends StmtAST
case class ContinueStmtAST() extends StmtAST
case class AsmStmtAST(code: String) extends StmtAST
case class ExprStmtAST(expr: ExpressionAST) extends StmtAST

// Expressions
trait ExpressionAST extends Positional
case class IntLitAST(value: Long) extends ExpressionAST
case class TypedIntLitAST(value: Long, typeName: String) extends ExpressionAST  // e.g., 100u32
case class FloatLitAST(value: Double) extends ExpressionAST
case class CharLitAST(value: Char) extends ExpressionAST
case class StringLitAST(value: String) extends ExpressionAST
case class BoolLitAST(value: Boolean) extends ExpressionAST
case class VarRefAST(name: String) extends ExpressionAST
case class BinaryAST(left: ExpressionAST, op: String, right: ExpressionAST) extends ExpressionAST
case class UnaryAST(op: String, operand: ExpressionAST) extends ExpressionAST
case class PreIncAST(name: String) extends ExpressionAST
case class PreDecAST(name: String) extends ExpressionAST
case class PostIncAST(name: String) extends ExpressionAST
case class PostDecAST(name: String) extends ExpressionAST
case class CallAST(name: String, args: List[ExpressionAST]) extends ExpressionAST
case class CastAST(targetType: String, expr: ExpressionAST) extends ExpressionAST
case class IfExprAST(cond: ExpressionAST, thenBody: List[StmtAST], elseBody: Option[List[StmtAST]]) extends ExpressionAST
case class AddrOfAST(name: String) extends ExpressionAST
case class AddrOfIndexAST(array: ExpressionAST, index: ExpressionAST) extends ExpressionAST
case class DerefAST(expr: ExpressionAST) extends ExpressionAST
case class IndexAST(expr: ExpressionAST, index: ExpressionAST) extends ExpressionAST
case class FieldAccessAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPreIncAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPreDecAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPostIncAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPostDecAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class ArrayDeclAST(size: Int, elemType: String) extends ExpressionAST
case class ArrayLitAST(elements: List[ExpressionAST]) extends ExpressionAST
case class StructInitAST(typeName: String) extends ExpressionAST
case class UninitDeclAST(typeName: String) extends ExpressionAST
case class SizeofTypeAST(typeName: String) extends ExpressionAST
case class SizeofExprAST(expr: ExpressionAST) extends ExpressionAST
case class StringLitExprAST(value: String) extends ExpressionAST
