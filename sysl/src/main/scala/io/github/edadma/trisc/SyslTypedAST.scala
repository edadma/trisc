package io.github.edadma.trisc

// Top-level
case class TProgram(decls: List[TDecl])

// Declarations
trait TDecl
case class TModuleDecl(path: List[String]) extends TDecl
case class TImportDecl(path: String) extends TDecl
case class TExternFuncDecl(name: String, params: List[SyslType], returnType: SyslType) extends TDecl
case class TExternVarDecl(name: String, typ: SyslType) extends TDecl
case class TFunDecl(name: String, params: List[TParam], returnType: SyslType, body: TFunBody, isPrivate: Boolean = false, attributes: List[Attribute] = Nil, isDef: Boolean = false) extends TDecl
case class TVarDecl(name: String, typ: SyslType, init: TExpr, isPrivate: Boolean = false, isVolatile: Boolean = false) extends TDecl
case class TStructDecl(name: String, fields: List[(String, SyslType)], volatileFields: Set[Int] = Set.empty) extends TDecl
case class TEnumDecl(name: String, members: List[(String, Long)]) extends TDecl
case class TDataEnumDecl(name: String, enumType: SyslType.EnumType) extends TDecl
case class TTypeAliasDecl(name: String, target: SyslType) extends TDecl
case class TInterfaceDecl(name: String, ifaceType: SyslType.InterfaceType) extends TDecl

case class TParam(name: String, typ: SyslType, default: Option[TExpr] = None)

// Function body
trait TFunBody
case class TExprBody(expr: TExpr) extends TFunBody
case class TBlockBody(stmts: List[TStmt]) extends TFunBody

// Statements
trait TStmt
case class TVarStmt(name: String, typ: SyslType, init: TExpr, isVolatile: Boolean = false) extends TStmt
case class TDestructureStmt(names: List[String], types: List[SyslType], init: TExpr) extends TStmt
case class TDestructureAssignStmt(names: List[String], types: List[SyslType], init: TExpr) extends TStmt
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
case class TAsmExpr(code: String, typ: SyslType) extends TExpr
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
case class TTempAddr(expr: TExpr, typ: SyslType) extends TExpr  // evaluate expr, store in temp, return pointer
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
case class TMatchExpr(expr: TExpr, arms: List[TMatchArm], default: Option[List[TStmt]], typ: SyslType) extends TExpr
case class TMatchArm(patterns: List[TMatchPattern], guard: Option[TExpr], body: List[TStmt])

sealed trait TMatchPattern
case object TWildcard extends TMatchPattern
case class TValuePattern(expr: TExpr) extends TMatchPattern
case class TRangePattern(low: TExpr, high: TExpr) extends TMatchPattern
case class TDestructurePattern(structType: SyslType.StructType, bindings: List[Option[String]], fieldTypes: List[SyslType]) extends TMatchPattern
case class TVariantPattern(enumType: SyslType.EnumType, variantIndex: Int, bindings: List[Option[String]], fieldTypes: List[SyslType]) extends TMatchPattern
case class TEnumConstruct(enumType: SyslType.EnumType, variantIndex: Int, args: List[TExpr]) extends TExpr { def typ: SyslType = enumType }
case class TNew(structType: SyslType.StructType, args: List[TExpr]) extends TExpr { def typ: SyslType = SyslType.RefType(structType) }
case class TNewEnum(enumType: SyslType.EnumType, variantIndex: Int, args: List[TExpr]) extends TExpr { def typ: SyslType = SyslType.RefType(enumType) }
case class TNewArray(elemType: SyslType, size: TExpr) extends TExpr { def typ: SyslType = SyslType.RefType(SyslType.SliceType(elemType)) }
case class TLen(expr: TExpr, typ: SyslType) extends TExpr
case class TCap(expr: TExpr, typ: SyslType) extends TExpr
case class TSliceExpr(array: TExpr, low: Option[TExpr], high: Option[TExpr], typ: SyslType) extends TExpr
case class TAppend(slice: TExpr, elem: TExpr, typ: SyslType) extends TExpr
case class TStringFromPtr(ptr: TExpr, len: TExpr, typ: SyslType) extends TExpr
case class TStringFromSlice(slice: TExpr, typ: SyslType) extends TExpr
case class TStr(expr: TExpr) extends TExpr { def typ: SyslType = SyslType.StringType }
case class FmtSpec(verb: Char, width: Int = 0, zeroPad: Boolean = false, leftAlign: Boolean = false, showSign: Boolean = false, upperCase: Boolean = false)
case class TFmtStr(expr: TExpr, spec: FmtSpec) extends TExpr { def typ: SyslType = SyslType.StringType }
case class TClosure(params: List[TParam], returnType: SyslType, body: TFunBody, captures: List[(String, SyslType)], escapes: Boolean = true) extends TExpr {
  def typ: SyslType = SyslType.FuncType(params.map(_.typ), returnType)
}
case class TInterfaceBox(expr: TExpr, iface: SyslType.InterfaceType) extends TExpr {
  def typ: SyslType = iface
}
case class TInterfaceDispatch(ifaceVal: TExpr, methodIndex: Int, args: List[TExpr], retType: SyslType) extends TExpr {
  def typ: SyslType = retType
}
// Compiler intrinsic call (wrapping_add, saturating_add, etc.). Polymorphic per integer width.
case class TIntrinsicCall(name: String, args: List[TExpr], typ: SyslType) extends TExpr
// Runtime range check for `within` constrained types. Evaluates `expr`, traps if out of range,
// returns the value typed as `typ` (typically the NamedType). `aliasName` is used for error text.
case class TRangeCheck(expr: TExpr, range: TypeRange, aliasName: String, typ: SyslType) extends TExpr
