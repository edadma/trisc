package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer:
  case class AnalysisError(msg: String, node: Any = null) extends RuntimeException(msg)

  private case class SymInfo(name: String, typ: SyslType, mutable: Boolean)
  private case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType)

  private val globalScope = new mutable.LinkedHashMap[String, SymInfo]
  private val functions = new mutable.LinkedHashMap[String, FunInfo]
  private var localScope: mutable.LinkedHashMap[String, SymInfo] = null

  private val builtinFunctions = Map(
    "putchar" -> FunInfo("putchar", List("c" -> IntType), IntType),
    "print" -> FunInfo("print", List("n" -> IntType), VoidType),
    "println" -> FunInfo("println", List("n" -> IntType), VoidType),
  )

  def analyze(program: ProgramAST): TProgram =
    // First pass: register all functions and globals
    for decl <- program.decls do
      decl match
        case FunDeclAST(name, params, returnType, _) =>
          val paramTypes = params.map(p => (p.name, resolveTypeName(p.typ)))
          val retType = returnType.map(resolveTypeName).getOrElse(VoidType)
          if functions.contains(name) || builtinFunctions.contains(name) then
            throw AnalysisError(s"duplicate function: '$name'", decl)
          functions(name) = FunInfo(name, paramTypes, retType)
        case VarDeclAST(name, _, _) =>
          if globalScope.contains(name) then
            throw AnalysisError(s"duplicate global: '$name'", decl)

    // Second pass: produce typed AST
    val tDecls = program.decls.map(analyzeDecl)
    TProgram(tDecls)

  private def analyzeDecl(decl: DeclAST): TDecl =
    decl match
      case FunDeclAST(name, params, _, body) =>
        localScope = new mutable.LinkedHashMap
        val funInfo = functions(name)
        for (paramName, paramType) <- funInfo.params do
          localScope(paramName) = SymInfo(paramName, paramType, true)
        val tBody = body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
        val tParams = funInfo.params.map((n, t) => TParam(n, t))
        localScope = null
        TFunDecl(name, tParams, funInfo.returnType, tBody)

      case VarDeclAST(name, typOpt, init) =>
        localScope = new mutable.LinkedHashMap
        val tInit = analyzeExpr(init)
        val declType = typOpt.map(resolveTypeName).getOrElse(tInit.typ)
        globalScope(name) = SymInfo(name, declType, true)
        localScope = null
        TVarDecl(name, declType, tInit)

  private def resolveTypeName(name: String): SyslType = name match
    case "int"  => IntType
    case "char" => CharType
    case "byte" => ByteType
    case "bool" => BoolType
    case "void" => VoidType
    case s if s.startsWith("*") =>
      PtrType(resolveTypeName(s.drop(1)))
    case s if s.startsWith("[") =>
      val size = s.drop(1).takeWhile(_.isDigit).toInt
      val elem = s.dropWhile(_ != ']').drop(1)
      ArrayType(resolveTypeName(elem), size)
    case other => throw AnalysisError(s"unknown type: '$other'")

  private def compatible(from: SyslType, to: SyslType): Boolean =
    (from, to) match
      case (a, b) if a == b => true
      case (IntType, CharType) | (CharType, IntType) => true
      case (IntType, ByteType) | (ByteType, IntType) => true
      case (IntType, BoolType) | (BoolType, IntType) => true
      case (CharType, ByteType) | (ByteType, CharType) => true
      case (ArrayType(e1, _), PtrType(e2)) if e1 == e2 => true
      case (ArrayType(e1, _), ArrayType(e2, _)) if e1 == e2 => true
      case _ => false

  private def lookup(name: String): SymInfo =
    if localScope != null && localScope.contains(name) then localScope(name)
    else if globalScope.contains(name) then globalScope(name)
    else throw AnalysisError(s"undefined variable: '$name'")

  private def lookupOrCreate(name: String, typ: SyslType): SymInfo =
    if localScope != null && localScope.contains(name) then localScope(name)
    else if globalScope.contains(name) then globalScope(name)
    else
      val info = SymInfo(name, typ, true)
      if localScope != null then localScope(name) = info
      else globalScope(name) = info
      info

  private def lookupFun(name: String): FunInfo =
    builtinFunctions.getOrElse(name,
      functions.getOrElse(name,
        throw AnalysisError(s"undefined function: '$name'")))

  private def analyzeBlock(stmts: List[StmtAST]): List[TStmt] =
    stmts.map(analyzeStmt)

  private def analyzeStmt(stmt: StmtAST): TStmt =
    stmt match
      case VarStmtAST(name, typOpt, init) =>
        val tInit = analyzeExpr(init)
        val declType = typOpt.map(resolveTypeName).getOrElse(tInit.typ)
        if localScope != null then
          localScope(name) = SymInfo(name, declType, true)
        TVarStmt(name, declType, tInit)

      case AssignStmtAST(target, value) =>
        val tValue = analyzeExpr(value)
        lookupOrCreate(target, tValue.typ)
        TAssignStmt(target, tValue)

      case CompoundAssignStmtAST(target, op, value) =>
        val sym = lookup(target)
        val tValue = analyzeExpr(value)
        TCompoundAssignStmt(target, op, tValue)

      case DerefAssignStmtAST(pointer, value) =>
        val tPointer = analyzeExpr(pointer)
        val tValue = analyzeExpr(value)
        TDerefAssignStmt(tPointer, tValue)

      case IndexAssignStmtAST(array, index, value) =>
        val tArray = analyzeExpr(array)
        val tIndex = analyzeExpr(index)
        val tValue = analyzeExpr(value)
        TIndexAssignStmt(tArray, tIndex, tValue)

      case ReturnStmtAST(value) =>
        TReturnStmt(value.map(analyzeExpr))

      case WhileStmtAST(cond, body) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"while condition must be bool, got ${tCond.typ}")
        TWhileStmt(tCond, analyzeBlock(body))

      case ExprStmtAST(expr) =>
        TExprStmt(analyzeExpr(expr))

  private def analyzeExpr(expr: ExpressionAST): TExpr =
    expr match
      case IntLitAST(n) => TIntLit(n, IntType)
      case CharLitAST(c) => TIntLit(c.toLong, CharType)
      case BoolLitAST(b) => TBoolLit(b, BoolType)
      case StringLitAST(s) => TStringLit(s, ArrayType(ByteType, 0))
      case StringLitExprAST(s) => TStringLit(s, ArrayType(ByteType, 0))
      case ArrayDeclAST(size, typStr) =>
        val t = resolveTypeName(typStr)
        TArrayDecl(size, typStr, t)

      case VarRefAST(name) =>
        val sym = lookup(name)
        TVarRef(name, sym.typ)

      case AddrOfAST(name) =>
        val sym = lookup(name)
        TAddrOf(name, PtrType(sym.typ))

      case AddrOfIndexAST(array, index) =>
        val tArray = analyzeExpr(array)
        val tIndex = analyzeExpr(index)
        val elemType = tArray.typ match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case _ => throw AnalysisError(s"cannot take address of index on ${tArray.typ}")
        TAddrOfIndex(tArray, tIndex, PtrType(elemType))

      case DerefAST(inner) =>
        val tInner = analyzeExpr(inner)
        val resultType = tInner.typ match
          case PtrType(t) => t
          case ArrayType(t, _) => t
          case t => throw AnalysisError(s"cannot dereference $t")
        TDeref(tInner, resultType)

      case IndexAST(arr, index) =>
        val tArr = analyzeExpr(arr)
        val tIndex = analyzeExpr(index)
        val elemType = tArr.typ match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case t => throw AnalysisError(s"cannot index $t")
        TIndex(tArr, tIndex, elemType)

      case PreIncAST(name) => TPreInc(name, lookup(name).typ)
      case PreDecAST(name) => TPreDec(name, lookup(name).typ)
      case PostIncAST(name) => TPostInc(name, lookup(name).typ)
      case PostDecAST(name) => TPostDec(name, lookup(name).typ)

      case UnaryAST(op, operand) =>
        val tOperand = analyzeExpr(operand)
        val resultType = op match
          case "-" | "~" => tOperand.typ
          case "!" =>
            if tOperand.typ != BoolType then throw AnalysisError(s"unary ! requires bool, got ${tOperand.typ}")
            BoolType
        TUnary(op, tOperand, resultType)

      case BinaryAST(left, op, right) =>
        val tLeft = analyzeExpr(left)
        val tRight = analyzeExpr(right)
        val resultType = op match
          case "+" | "-" if tLeft.typ.isPointerLike && tRight.typ.isNumeric => tLeft.typ
          case "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" =>
            if tLeft.typ == IntType || tRight.typ == IntType then IntType
            else tLeft.typ
          case "==" | "!=" | "<" | ">" | "<=" | ">=" => BoolType
          case "&&" | "||" =>
            if tLeft.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tLeft.typ}")
            if tRight.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tRight.typ}")
            BoolType
          case _ => throw AnalysisError(s"unknown operator: $op")
        TBinary(tLeft, op, tRight, resultType)

      case CallAST(name, args) =>
        val funInfo = lookupFun(name)
        val tArgs = args.map(analyzeExpr)
        TCall(name, tArgs, funInfo.returnType)

      case IfExprAST(cond, thenBody, elseBody) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"if condition must be bool, got ${tCond.typ}")
        val tThen = analyzeBlock(thenBody)
        val tElse = elseBody.map(analyzeBlock)
        val resultType = tThen.lastOption match
          case Some(TExprStmt(e)) => e.typ
          case _ => VoidType
        TIfExpr(tCond, tThen, tElse, resultType)
