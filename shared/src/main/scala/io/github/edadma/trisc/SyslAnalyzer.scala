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

  def analyze(program: ProgramAST): Unit =
    // First pass: register all functions and globals
    for decl <- program.decls do
      decl match
        case FunDeclAST(name, params, returnType, _) =>
          val paramTypes = params.map(p => (p.name, resolveTypeName(p.typ)))
          val retType = returnType.map(resolveTypeName).getOrElse(VoidType)
          if functions.contains(name) || builtinFunctions.contains(name) then
            throw AnalysisError(s"duplicate function: '$name'", decl)
          functions(name) = FunInfo(name, paramTypes, retType)
        case VarDeclAST(name, typOpt, init) =>
          if globalScope.contains(name) then
            throw AnalysisError(s"duplicate global: '$name'", decl)

    // Second pass: type-check function bodies and global initializers
    for decl <- program.decls do
      decl match
        case f @ FunDeclAST(name, params, _, body) =>
          localScope = new mutable.LinkedHashMap
          val funInfo = functions(name)
          for (paramName, paramType) <- funInfo.params do
            localScope(paramName) = SymInfo(paramName, paramType, true)
          body match
            case ExprBodyAST(expr) =>
              val t = analyzeExpr(expr)
              if funInfo.returnType != VoidType && !compatible(t, funInfo.returnType) then
                throw AnalysisError(s"function '$name' returns $t but declared ${funInfo.returnType}", f)
            case BlockBodyAST(stmts) =>
              analyzeBlock(stmts, funInfo.returnType)
          localScope = null
        case VarDeclAST(name, typOpt, init) =>
          localScope = new mutable.LinkedHashMap
          val t = analyzeExpr(init)
          val declType = typOpt.map(resolveTypeName).getOrElse(t)
          if typOpt.isDefined && !compatible(t, declType) then
            throw AnalysisError(s"global '$name' initialized with $t but declared $declType", decl)
          globalScope(name) = SymInfo(name, declType, true)
          localScope = null

  private def resolveTypeName(name: String): SyslType = name match
    case "int"  => IntType
    case "char" => CharType
    case "byte" => ByteType
    case "void" => VoidType
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
      case (ArrayType(e1, _), PtrType(e2)) if e1 == e2 => true // array decays to pointer
      case (ArrayType(e1, _), ArrayType(e2, _)) if e1 == e2 => true // array to array (pointer passing)
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

  private def analyzeBlock(stmts: List[StmtAST], expectedReturn: SyslType): Unit =
    for stmt <- stmts do analyzeStmt(stmt, expectedReturn)

  private def analyzeStmt(stmt: StmtAST, expectedReturn: SyslType): Unit =
    stmt match
      case VarStmtAST(name, typOpt, init) =>
        val t = analyzeExpr(init)
        val declType = typOpt.map(resolveTypeName).getOrElse(t)
        if localScope != null then
          localScope(name) = SymInfo(name, declType, true)

      case AssignStmtAST(target, value) =>
        val vt = analyzeExpr(value)
        lookupOrCreate(target, vt)

      case CompoundAssignStmtAST(target, op, value) =>
        val sym = lookup(target)
        val vt = analyzeExpr(value)
        if !sym.typ.isNumeric || !vt.isNumeric then
          throw AnalysisError(s"compound assignment requires numeric types, got ${sym.typ} $op= $vt")

      case DerefAssignStmtAST(pointer, value) =>
        val pt = analyzeExpr(pointer)
        analyzeExpr(value)

      case IndexAssignStmtAST(array, index, value) =>
        analyzeExpr(array)
        val it = analyzeExpr(index)
        if !it.isNumeric then throw AnalysisError(s"array index must be numeric, got $it")
        analyzeExpr(value)

      case ReturnStmtAST(value) =>
        value.foreach(analyzeExpr)

      case WhileStmtAST(cond, body) =>
        val ct = analyzeExpr(cond)
        if !ct.isNumeric then throw AnalysisError(s"while condition must be numeric, got $ct")
        analyzeBlock(body, expectedReturn)

      case ExprStmtAST(expr) =>
        analyzeExpr(expr)

  def analyzeExpr(expr: ExpressionAST): SyslType =
    val t = inferType(expr)
    expr.typ = t
    t

  private def inferType(expr: ExpressionAST): SyslType =
    expr match
      case IntLitAST(_) => IntType
      case CharLitAST(_) => CharType
      case BoolLitAST(_) => BoolType
      case StringLitAST(_) => ArrayType(ByteType, 0)
      case StringLitExprAST(_) => ArrayType(ByteType, 0)
      case ArrayDeclAST(size, typStr) => resolveTypeName(typStr)

      case VarRefAST(name) => lookup(name).typ

      case AddrOfAST(name) => PtrType(lookup(name).typ)

      case AddrOfIndexAST(array, index) =>
        val at = analyzeExpr(array)
        val it = analyzeExpr(index)
        if !it.isNumeric then throw AnalysisError(s"index must be numeric, got $it")
        at match
          case ArrayType(elem, _) => PtrType(elem)
          case PtrType(elem) => PtrType(elem)
          case _ => throw AnalysisError(s"cannot take address of index on $at")

      case DerefAST(inner) =>
        analyzeExpr(inner) match
          case PtrType(t) => t
          case ArrayType(t, _) => t
          case IntType => IntType // allow deref of int (untyped pointer)
          case t => throw AnalysisError(s"cannot dereference $t")

      case IndexAST(arr, index) =>
        val at = analyzeExpr(arr)
        val it = analyzeExpr(index)
        if !it.isNumeric then throw AnalysisError(s"index must be numeric, got $it")
        at match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case IntType => IntType // allow indexing int (untyped pointer)
          case _ => throw AnalysisError(s"cannot index $at")

      case PreIncAST(name) => lookup(name).typ
      case PreDecAST(name) => lookup(name).typ
      case PostIncAST(name) => lookup(name).typ
      case PostDecAST(name) => lookup(name).typ

      case UnaryAST(op, operand) =>
        val t = analyzeExpr(operand)
        op match
          case "-" | "~" =>
            if !t.isNumeric then throw AnalysisError(s"unary $op requires numeric type, got $t")
            t
          case "!" =>
            if !t.isNumeric then throw AnalysisError(s"unary ! requires numeric type, got $t")
            BoolType

      case BinaryAST(left, op, right) =>
        val lt = analyzeExpr(left)
        val rt = analyzeExpr(right)
        op match
          case "+" | "-" if lt.isPointerLike && rt.isNumeric => lt // pointer arithmetic
          case "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" =>
            if !lt.isNumeric || !rt.isNumeric then
              throw AnalysisError(s"operator $op requires numeric types, got $lt $op $rt")
            if lt == IntType || rt == IntType then IntType
            else lt
          case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
            BoolType
          case "&&" | "||" =>
            BoolType
          case _ => throw AnalysisError(s"unknown operator: $op")

      case CallAST(name, args) =>
        val funInfo = lookupFun(name)
        for (arg, i) <- args.zipWithIndex do
          analyzeExpr(arg)
        funInfo.returnType

      case IfExprAST(cond, thenBody, elseBody) =>
        val ct = analyzeExpr(cond)
        if !ct.isNumeric then throw AnalysisError(s"if condition must be numeric, got $ct")
        analyzeBlock(thenBody, VoidType)
        elseBody.foreach(stmts => analyzeBlock(stmts, VoidType))
        // Type of if-expression: type of last expression in then branch
        thenBody.lastOption match
          case Some(ExprStmtAST(e)) => e.typ
          case _ => VoidType
