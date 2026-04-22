package io.github.edadma.trisc

import scala.util.parsing.input.Positional

// Top-level
case class ProgramAST(decls: List[DeclAST])

// Type syntax AST — structured representation of type references
sealed trait TypeAST
case class NamedTypeAST(name: String, typeArgs: List[TypeAST] = Nil) extends TypeAST
case class PtrTypeAST(inner: TypeAST) extends TypeAST
// `*T not null` — a pointer type constrained to be non-null at produce sites.
// Resolved as a NamedType wrapping PtrType(inner) with a synth predicate.
case class PtrNonNullTypeAST(inner: TypeAST) extends TypeAST
case class ArrayTypeAST(size: Int, elem: TypeAST) extends TypeAST
case class SliceTypeAST(elem: TypeAST) extends TypeAST
case class FuncTypeAST(params: List[TypeAST], ret: TypeAST, escaping: Boolean = false) extends TypeAST
case class TupleTypeAST(elems: List[TypeAST]) extends TypeAST
case class RefTypeAST(inner: TypeAST) extends TypeAST

// Import selectors
sealed trait ImportSelector
case object WildcardImport extends ImportSelector
case object QualifiedImport extends ImportSelector  // import std.strings → access as strings.foo
case class NamedImport(name: String, rename: Option[String] = None) extends ImportSelector

// Attributes (annotations)
sealed trait AttrLiteral
case class AttrLitInt(v: Long) extends AttrLiteral
case class AttrLitString(v: String) extends AttrLiteral
case class AttrLitBool(v: Boolean) extends AttrLiteral
case class AttrLitIdent(v: String) extends AttrLiteral

sealed trait AttrArg
case class AttrPositional(value: AttrLiteral) extends AttrArg
case class AttrNamed(key: String, value: AttrLiteral) extends AttrArg

case class Attribute(name: String, args: List[AttrArg] = Nil) extends Positional

// Declarations
trait DeclAST extends Positional
case class ModuleDeclAST(path: List[String]) extends DeclAST
case class ImportDeclAST(modulePath: String, selectors: List[ImportSelector]) extends DeclAST
case class ExternFuncDeclAST(name: String, params: List[ParamAST], returnType: Option[TypeAST], attributes: List[Attribute] = Nil) extends DeclAST
case class ExternVarDeclAST(name: String, typ: TypeAST, attributes: List[Attribute] = Nil) extends DeclAST
case class FunDeclAST(name: String, params: List[ParamAST], returnType: Option[TypeAST], body: FunBodyAST, isPrivate: Boolean = false, typeParams: List[String] = Nil, typeBounds: Map[String, List[String]] = Map.empty, attributes: List[Attribute] = Nil, isDef: Boolean = false) extends DeclAST
case class VarDeclAST(name: String, typ: Option[TypeAST], init: ExpressionAST, isPrivate: Boolean = false, isMutable: Boolean = true, attributes: List[Attribute] = Nil, isVolatile: Boolean = false, isConst: Boolean = false) extends DeclAST
case class StructDeclAST(name: String, fields: List[(String, TypeAST, Boolean)], typeParams: List[String] = Nil, attributes: List[Attribute] = Nil, invariants: List[ExpressionAST] = Nil) extends DeclAST
case class EnumDeclAST(name: String, members: List[(String, Option[Long])], attributes: List[Attribute] = Nil) extends DeclAST
case class DataEnumDeclAST(name: String, variants: List[EnumVariantAST], typeParams: List[String] = Nil, attributes: List[Attribute] = Nil) extends DeclAST
case class EnumVariantAST(name: String, fields: List[(String, TypeAST)])
case class TypeAliasDeclAST(name: String, target: TypeAST, typeParams: List[String] = Nil, attributes: List[Attribute] = Nil, isNew: Boolean = false, range: Option[RangeAST] = None, predicate: Option[ExpressionAST] = None) extends DeclAST

// Range for `within lo..hi` / `within lo..<hi` type constraints
case class RangeAST(lo: ExpressionAST, hi: ExpressionAST, exclusiveHi: Boolean) extends Positional
case class TraitDeclAST(name: String, typeParam: String, methods: List[TraitMethodAST], attributes: List[Attribute] = Nil) extends DeclAST
case class TraitMethodAST(
    name: String,
    params: List[ParamAST],
    returnType: TypeAST,
    body: Option[FunBodyAST],
    attributes: List[Attribute] = Nil,
) extends Positional
case class ImplDeclAST(traitName: String, targetType: TypeAST, methods: List[FunDeclAST], attributes: List[Attribute] = Nil) extends DeclAST
case class InterfaceDeclAST(name: String, methods: List[InterfaceMethodAST], embedded: List[String], attributes: List[Attribute] = Nil) extends DeclAST
case class InterfaceMethodAST(name: String, params: List[ParamAST], returnType: TypeAST) extends Positional
case class CondDeclAST(cond: CondExpr, thenDecls: List[DeclAST], elseDecls: Option[List[DeclAST]]) extends DeclAST
// `static_assert(cond)` or `static_assert(cond, "message")` at module scope — compile-time check.
case class StaticAssertDeclAST(cond: ExpressionAST, message: Option[String]) extends DeclAST

// Conditional compilation expressions
sealed trait CondExpr
case class CondSymbol(name: String) extends CondExpr
case class CondNot(expr: CondExpr) extends CondExpr
case class CondEq(name: String, value: String) extends CondExpr
case class CondNeq(name: String, value: String) extends CondExpr

case class ParamAST(name: String, typ: TypeAST, default: Option[ExpressionAST] = None) extends Positional

// Function body
trait FunBodyAST
case class ExprBodyAST(expr: ExpressionAST) extends FunBodyAST
case class BlockBodyAST(stmts: List[StmtAST], contracts: List[ContractClauseAST] = Nil) extends FunBodyAST

// Design-by-contract clauses at the top of a function's block body.
sealed trait ContractKind
case object ContractRequire extends ContractKind
case object ContractEnsure extends ContractKind
case class ContractClauseAST(kind: ContractKind, expr: ExpressionAST, message: Option[String] = None) extends Positional

// Statements
trait StmtAST extends Positional
case class VarStmtAST(name: String, typ: Option[TypeAST], init: ExpressionAST, isMutable: Boolean = true, isVolatile: Boolean = false, isConst: Boolean = false) extends StmtAST
case class DestructureStmtAST(names: List[String], init: ExpressionAST, isMutable: Boolean = false) extends StmtAST
case class AssignStmtAST(target: String, value: ExpressionAST) extends StmtAST
case class CompoundAssignStmtAST(target: String, op: String, value: ExpressionAST) extends StmtAST
case class DerefAssignStmtAST(pointer: ExpressionAST, value: ExpressionAST) extends StmtAST
case class IndexAssignStmtAST(array: ExpressionAST, index: ExpressionAST, value: ExpressionAST) extends StmtAST
case class FieldAssignStmtAST(obj: ExpressionAST, field: String, value: ExpressionAST) extends StmtAST
case class FieldCompoundAssignStmtAST(obj: ExpressionAST, field: String, op: String, value: ExpressionAST) extends StmtAST
case class ReturnStmtAST(value: Option[ExpressionAST]) extends StmtAST
case class WhileStmtAST(cond: ExpressionAST, body: List[StmtAST], label: Option[String] = None) extends StmtAST
case class ForStmtAST(init: StmtAST, cond: ExpressionAST, update: StmtAST, body: List[StmtAST], label: Option[String] = None) extends StmtAST
case class DoWhileStmtAST(cond: ExpressionAST, body: List[StmtAST], label: Option[String] = None) extends StmtAST
case class BreakStmtAST(label: Option[String] = None) extends StmtAST
case class ContinueStmtAST(label: Option[String] = None) extends StmtAST
case class DeferStmtAST(body: StmtAST) extends StmtAST
case class AsmStmtAST(code: String) extends StmtAST
// `invariant <bool>` statement — traps on false. Essentially `assert` with nicer name; intended
// at the top of loop bodies for loop-invariant documentation/checking.
case class InvariantStmtAST(expr: ExpressionAST) extends StmtAST
// `variant <expr>` statement — loop termination witness. Must appear at the top level of a
// loop body. The expression must strictly decrease between iterations and stay >= 0; both
// are runtime-asserted. Analyzer hoists a prev-value / init-flag pair into the enclosing scope.
case class VariantStmtAST(expr: ExpressionAST) extends StmtAST
case class AsmExprAST(code: String) extends ExpressionAST
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
case class NamedArgAST(name: String, value: ExpressionAST) extends ExpressionAST
case class IndirectCallAST(callee: ExpressionAST, args: List[ExpressionAST]) extends ExpressionAST
case class MethodCallAST(obj: ExpressionAST, method: String, args: List[ExpressionAST]) extends ExpressionAST
case class CastAST(targetType: TypeAST, expr: ExpressionAST) extends ExpressionAST
case class IfExprAST(cond: ExpressionAST, thenBody: List[StmtAST], elseBody: Option[List[StmtAST]]) extends ExpressionAST
case class TryAST(expr: ExpressionAST) extends ExpressionAST
case class MatchExprAST(expr: ExpressionAST, arms: List[MatchArmAST], default: Option[List[StmtAST]]) extends ExpressionAST
case class MatchArmAST(patterns: List[MatchPatternAST], guard: Option[ExpressionAST], body: List[StmtAST])

sealed trait MatchPatternAST
case object WildcardPatternAST extends MatchPatternAST
case class ValuePatternAST(expr: ExpressionAST) extends MatchPatternAST
case class RangePatternAST(low: ExpressionAST, high: ExpressionAST) extends MatchPatternAST
case class DestructurePatternAST(name: String, fields: List[MatchPatternAST]) extends MatchPatternAST
case class AddrOfAST(name: String) extends ExpressionAST
case class AddrOfIndexAST(array: ExpressionAST, index: ExpressionAST) extends ExpressionAST
case class AddrOfFieldAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class DerefAST(expr: ExpressionAST) extends ExpressionAST
case class IndexAST(expr: ExpressionAST, index: ExpressionAST) extends ExpressionAST
case class SliceExprAST(array: ExpressionAST, low: Option[ExpressionAST], high: Option[ExpressionAST]) extends ExpressionAST
case class FieldAccessAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPreIncAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPreDecAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPostIncAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class FieldPostDecAST(obj: ExpressionAST, field: String) extends ExpressionAST
case class ArrayDeclAST(size: Int, elemType: TypeAST) extends ExpressionAST
case class ArrayLitAST(elements: List[ExpressionAST]) extends ExpressionAST
case class TupleLitAST(elements: List[ExpressionAST]) extends ExpressionAST
case class StructInitAST(typeName: String) extends ExpressionAST
case class UninitDeclAST(typeName: TypeAST) extends ExpressionAST
case class SizeofTypeAST(typeName: TypeAST) extends ExpressionAST
case class SizeofExprAST(expr: ExpressionAST) extends ExpressionAST
case class NewExprAST(typeName: String, args: List[ExpressionAST]) extends ExpressionAST
case class NewArrayAST(size: ExpressionAST, elemType: TypeAST) extends ExpressionAST
case class StringLitExprAST(value: String) extends ExpressionAST
case class ClosureParamAST(name: String, typ: Option[TypeAST])
case class ClosureAST(params: List[ClosureParamAST], body: FunBodyAST) extends ExpressionAST
// Type attribute: `T::First`, `T::Last`, `T::Range`, `T::Image(x)`, `T::Pos(x)`, `T::Val(n)`.
// `arg` is set only for attributes that take one (Image, Pos, Val). `Range` is only valid
// syntactically inside `for i in T::Range` and is desugared at parse time; it is never analyzed.
case class TypeAttrAST(typeName: String, attr: String, arg: Option[ExpressionAST] = None) extends ExpressionAST
