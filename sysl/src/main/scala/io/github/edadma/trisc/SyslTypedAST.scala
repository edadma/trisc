package io.github.edadma.trisc

// Top-level
case class TProgram(decls: List[TDecl])

// Declarations
trait TDecl
case class TModuleDecl(path: List[String]) extends TDecl
case class TImportDecl(path: String) extends TDecl
case class TExternFuncDecl(name: String, params: List[SyslType], returnType: SyslType) extends TDecl
case class TExternVarDecl(name: String, typ: SyslType) extends TDecl
case class TFunDecl(name: String, params: List[TParam], returnType: SyslType, body: TFunBody, isPrivate: Boolean = false) extends TDecl
case class TVarDecl(name: String, typ: SyslType, init: TExpr, isPrivate: Boolean = false) extends TDecl
case class TStructDecl(name: String, fields: List[(String, SyslType)]) extends TDecl
case class TEnumDecl(name: String, members: List[(String, Long)]) extends TDecl
case class TTypeAliasDecl(name: String, target: SyslType) extends TDecl

case class TParam(name: String, typ: SyslType)

// Function body
trait TFunBody
case class TExprBody(expr: TExpr) extends TFunBody
case class TBlockBody(stmts: List[TStmt]) extends TFunBody

// Statements
trait TStmt
case class TVarStmt(name: String, typ: SyslType, init: TExpr) extends TStmt
case class TDestructureStmt(names: List[String], types: List[SyslType], init: TExpr) extends TStmt
case class TAssignStmt(target: String, value: TExpr) extends TStmt
case class TCompoundAssignStmt(target: String, op: String, value: TExpr) extends TStmt
case class TDerefAssignStmt(pointer: TExpr, value: TExpr) extends TStmt
case class TIndexAssignStmt(array: TExpr, index: TExpr, value: TExpr) extends TStmt
case class TFieldAssignStmt(obj: TExpr, fieldIndex: Int, value: TExpr) extends TStmt
case class TFieldCompoundAssignStmt(obj: TExpr, fieldIndex: Int, op: String, value: TExpr) extends TStmt
case class TReturnStmt(value: Option[TExpr]) extends TStmt
case class TWhileStmt(cond: TExpr, body: List[TStmt]) extends TStmt
case class TForStmt(init: TStmt, cond: TExpr, update: TStmt, body: List[TStmt]) extends TStmt
case class TDoWhileStmt(cond: TExpr, body: List[TStmt]) extends TStmt
case object TBreakStmt extends TStmt
case object TContinueStmt extends TStmt
case class TDeferStmt(body: TStmt) extends TStmt
case class TAsmStmt(code: String) extends TStmt
case class TExprStmt(expr: TExpr) extends TStmt

// Expressions — every expression carries its type
trait TExpr:
  def typ: SyslType

case class TIntLit(value: Long, typ: SyslType) extends TExpr
case class TFloatLit(value: Double, typ: SyslType) extends TExpr
case class TBoolLit(value: Boolean, typ: SyslType) extends TExpr
case class TStringLit(value: String, typ: SyslType) extends TExpr
case class TArrayDecl(size: Int, typ: SyslType) extends TExpr
case class TArrayLit(elements: List[TExpr], typ: SyslType) extends TExpr
case class TVarRef(name: String, typ: SyslType) extends TExpr
case class TAddrOf(name: String, typ: SyslType) extends TExpr
case class TAddrOfIndex(array: TExpr, index: TExpr, typ: SyslType) extends TExpr
case class TAddrOfField(obj: TExpr, fieldIndex: Int, typ: SyslType) extends TExpr
case class TDeref(expr: TExpr, typ: SyslType) extends TExpr
case class TIndex(expr: TExpr, index: TExpr, typ: SyslType) extends TExpr
case class TFieldAccess(obj: TExpr, fieldIndex: Int, typ: SyslType) extends TExpr
case class TFieldPreInc(obj: TExpr, fieldIndex: Int, typ: SyslType) extends TExpr
case class TFieldPreDec(obj: TExpr, fieldIndex: Int, typ: SyslType) extends TExpr
case class TFieldPostInc(obj: TExpr, fieldIndex: Int, typ: SyslType) extends TExpr
case class TFieldPostDec(obj: TExpr, fieldIndex: Int, typ: SyslType) extends TExpr
case class TStructLit(typ: SyslType) extends TExpr
case class TStructConstruct(structType: SyslType.StructType, args: List[TExpr]) extends TExpr { def typ: SyslType = structType }
case class TSizeof(size: Long, typ: SyslType.IntType) extends TExpr
// Internal: address relative to frame pointer (for hidden return slot args)
case class TAddrLit(fpOffset: Int) extends TExpr { def typ: SyslType = SyslType.PtrType(SyslType.VoidType) }
case class TPreInc(name: String, typ: SyslType) extends TExpr
case class TPreDec(name: String, typ: SyslType) extends TExpr
case class TPostInc(name: String, typ: SyslType) extends TExpr
case class TPostDec(name: String, typ: SyslType) extends TExpr
case class TUnary(op: String, operand: TExpr, typ: SyslType) extends TExpr
case class TBinary(left: TExpr, op: String, right: TExpr, typ: SyslType) extends TExpr
case class TCall(name: String, args: List[TExpr], typ: SyslType) extends TExpr
case class TIndirectCall(callee: TExpr, args: List[TExpr], typ: SyslType) extends TExpr
case class TFuncRef(name: String, typ: SyslType) extends TExpr
case class TCast(expr: TExpr, typ: SyslType) extends TExpr
case class TIfExpr(cond: TExpr, thenBody: List[TStmt], elseBody: Option[List[TStmt]], typ: SyslType) extends TExpr
case class TNew(structType: SyslType.StructType, args: List[TExpr]) extends TExpr { def typ: SyslType = SyslType.RefType(structType) }
case class TNewArray(elemType: SyslType, size: TExpr) extends TExpr { def typ: SyslType = SyslType.RefType(SyslType.SliceType(elemType)) }
case class TLen(expr: TExpr, typ: SyslType) extends TExpr
case class TCap(expr: TExpr, typ: SyslType) extends TExpr
