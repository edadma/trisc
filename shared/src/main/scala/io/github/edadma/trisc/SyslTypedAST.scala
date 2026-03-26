package io.github.edadma.trisc

// Top-level
case class TProgram(decls: List[TDecl])

// Declarations
trait TDecl
case class TFunDecl(name: String, params: List[TParam], returnType: SyslType, body: TFunBody) extends TDecl
case class TVarDecl(name: String, typ: SyslType, init: TExpr) extends TDecl

case class TParam(name: String, typ: SyslType)

// Function body
trait TFunBody
case class TExprBody(expr: TExpr) extends TFunBody
case class TBlockBody(stmts: List[TStmt]) extends TFunBody

// Statements
trait TStmt
case class TVarStmt(name: String, typ: SyslType, init: TExpr) extends TStmt
case class TAssignStmt(target: String, value: TExpr) extends TStmt
case class TCompoundAssignStmt(target: String, op: String, value: TExpr) extends TStmt
case class TDerefAssignStmt(pointer: TExpr, value: TExpr) extends TStmt
case class TIndexAssignStmt(array: TExpr, index: TExpr, value: TExpr) extends TStmt
case class TReturnStmt(value: Option[TExpr]) extends TStmt
case class TWhileStmt(cond: TExpr, body: List[TStmt]) extends TStmt
case class TExprStmt(expr: TExpr) extends TStmt

// Expressions — every expression carries its type
trait TExpr:
  def typ: SyslType

case class TIntLit(value: Long, typ: SyslType) extends TExpr
case class TBoolLit(value: Boolean, typ: SyslType) extends TExpr
case class TStringLit(value: String, typ: SyslType) extends TExpr
case class TArrayDecl(size: Int, elemType: String, typ: SyslType) extends TExpr
case class TVarRef(name: String, typ: SyslType) extends TExpr
case class TAddrOf(name: String, typ: SyslType) extends TExpr
case class TAddrOfIndex(array: TExpr, index: TExpr, typ: SyslType) extends TExpr
case class TDeref(expr: TExpr, typ: SyslType) extends TExpr
case class TIndex(expr: TExpr, index: TExpr, typ: SyslType) extends TExpr
case class TPreInc(name: String, typ: SyslType) extends TExpr
case class TPreDec(name: String, typ: SyslType) extends TExpr
case class TPostInc(name: String, typ: SyslType) extends TExpr
case class TPostDec(name: String, typ: SyslType) extends TExpr
case class TUnary(op: String, operand: TExpr, typ: SyslType) extends TExpr
case class TBinary(left: TExpr, op: String, right: TExpr, typ: SyslType) extends TExpr
case class TCall(name: String, args: List[TExpr], typ: SyslType) extends TExpr
case class TCast(expr: TExpr, typ: SyslType) extends TExpr
case class TIfExpr(cond: TExpr, thenBody: List[TStmt], elseBody: Option[List[TStmt]], typ: SyslType) extends TExpr
