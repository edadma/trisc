package io.github.edadma.trisc

import scala.collection.mutable
import SyslType.*

class SyslAnalyzer:
  case class AnalysisError(msg: String, node: Any = null) extends RuntimeException(msg)

  private case class SymInfo(name: String, typ: SyslType, mutable: Boolean)
  private case class FunInfo(name: String, params: List[(String, SyslType)], returnType: SyslType)

  private val globalScope = new mutable.LinkedHashMap[String, SymInfo]
  private val functions = new mutable.LinkedHashMap[String, FunInfo]
  private val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType]
  private val enumTypes = new mutable.LinkedHashMap[String, Map[String, Long]]  // enum name → (member name → value)
  private val typeAliases = new mutable.LinkedHashMap[String, String]  // alias name → target type string
  private val methods = new mutable.LinkedHashMap[String, mutable.Set[String]]  // struct name → set of method names
  private val externalSymbols = new mutable.LinkedHashSet[String]
  private var scopeStack: mutable.ArrayBuffer[mutable.LinkedHashMap[String, SymInfo]] = null
  private var loopDepth: Int = 0

  private def pushScope(): Unit =
    scopeStack += new mutable.LinkedHashMap[String, SymInfo]

  private def popScope(): Unit =
    scopeStack.remove(scopeStack.length - 1)

  private def currentScope: mutable.LinkedHashMap[String, SymInfo] =
    scopeStack.last

  private val builtinFunctions = Map(
    "putchar" -> FunInfo("putchar", List("c" -> U32), U32),
    "print" -> FunInfo("print", List("n" -> I32), VoidType),
    "println" -> FunInfo("println", List("n" -> I32), VoidType),
    "puts" -> FunInfo("puts", List("s" -> StringType), VoidType),
    "puti" -> FunInfo("puti", List("n" -> I32), VoidType),
  )

  def registerImport(meta: ModuleMeta): Unit =
    for sym <- meta.publicSymbols do
      sym.typ match
        case SymbolMeta.Kind.Func(params, returnType) =>
          val paramPairs = params.zipWithIndex.map((t, i) => (s"_p$i", t))
          if functions.contains(sym.name) || builtinFunctions.contains(sym.name) then
            throw AnalysisError(s"imported symbol '${sym.name}' conflicts with existing function")
          functions(sym.name) = FunInfo(sym.name, paramPairs, returnType)
          externalSymbols += sym.name
        case SymbolMeta.Kind.Data(dataType) =>
          if globalScope.contains(sym.name) then
            throw AnalysisError(s"imported symbol '${sym.name}' conflicts with existing global")
          globalScope(sym.name) = SymInfo(sym.name, dataType, mutable = false)
          externalSymbols += sym.name
        case SymbolMeta.Kind.Struct(st) =>
          structTypes(sym.name) = st

  def isExternal(name: String): Boolean = externalSymbols.contains(name)
  def externals: Set[String] = externalSymbols.toSet

  def analyze(program: ProgramAST): TProgram =
    // First pass: register all functions and globals
    for decl <- program.decls do
      decl match
        case ImportDeclAST(_) => // handled later
        case ExternFuncDeclAST(name, params, returnType) =>
          val paramTypes = params.map(p => (p.name, resolveTypeName(p.typ)))
          val retType = returnType.map(resolveTypeName).getOrElse(VoidType)
          if functions.contains(name) || builtinFunctions.contains(name) then
            throw AnalysisError(s"duplicate function: '$name'", decl)
          functions(name) = FunInfo(name, paramTypes, retType)
          externalSymbols += name
        case StructDeclAST(name, fields) =>
          if structTypes.contains(name) then throw AnalysisError(s"duplicate struct: '$name'", decl)
          val resolvedFields = fields.map((n, t) => (n, resolveTypeName(t)))
          structTypes(name) = SyslType.StructType(name, resolvedFields)
        case FunDeclAST(name, params, returnType, _, _) =>
          val paramTypes = params.map(p => (p.name, resolveTypeName(p.typ)))
          val retType = returnType.map(resolveTypeName).getOrElse(VoidType)
          if functions.contains(name) || builtinFunctions.contains(name) then
            throw AnalysisError(s"duplicate function: '$name'", decl)
          functions(name) = FunInfo(name, paramTypes, retType)
          // Register as method if name matches StructName_methodName pattern
          val underscoreIdx = name.indexOf('_')
          if underscoreIdx > 0 && params.nonEmpty && params.head.name == "self" then
            val structName = name.substring(0, underscoreIdx)
            val methodName = name.substring(underscoreIdx + 1)
            if structTypes.contains(structName) then
              methods.getOrElseUpdate(structName, mutable.Set.empty) += methodName
        case EnumDeclAST(name, members) =>
          if enumTypes.contains(name) then throw AnalysisError(s"duplicate enum: '$name'", decl)
          var nextValue = 0L
          val resolved = members.map { (memberName, explicitValue) =>
            val value = explicitValue.getOrElse(nextValue)
            nextValue = value + 1
            (memberName, value)
          }
          enumTypes(name) = resolved.toMap
        case TypeAliasDeclAST(name, target) =>
          if typeAliases.contains(name) then throw AnalysisError(s"duplicate type alias: '$name'", decl)
          typeAliases(name) = target
        case VarDeclAST(name, _, _, _, _) =>
          if globalScope.contains(name) then
            throw AnalysisError(s"duplicate global: '$name'", decl)

    // Second pass: produce typed AST
    val tDecls = program.decls.map(analyzeDecl)
    TProgram(tDecls)

  private def analyzeDecl(decl: DeclAST): TDecl =
    decl match
      case ImportDeclAST(path) =>
        TImportDecl(path)

      case ExternFuncDeclAST(name, params, returnType) =>
        val paramTypes = params.map(p => resolveTypeName(p.typ))
        val retType = returnType.map(resolveTypeName).getOrElse(VoidType)
        TExternFuncDecl(name, paramTypes, retType)

      case StructDeclAST(name, _) =>
        val st = structTypes(name)
        TStructDecl(name, st.fields)

      case EnumDeclAST(name, _) =>
        val members = enumTypes(name).toList.sortBy(_._2)
        TEnumDecl(name, members)

      case TypeAliasDeclAST(name, target) =>
        TTypeAliasDecl(name, resolveTypeName(target))

      case FunDeclAST(name, params, _, body, isPrivate) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val funInfo = functions(name)
        for (paramName, paramType) <- funInfo.params do
          currentScope(paramName) = SymInfo(paramName, paramType, true)
        val tBody = body match
          case ExprBodyAST(expr) => TExprBody(analyzeExpr(expr))
          case BlockBodyAST(stmts) => TBlockBody(analyzeBlock(stmts))
        val tParams = funInfo.params.map((n, t) => TParam(n, t))
        scopeStack = null
        TFunDecl(name, tParams, funInfo.returnType, tBody, isPrivate)

      case VarDeclAST(name, typOpt, init, isPrivate, isMutable) =>
        scopeStack = new mutable.ArrayBuffer
        pushScope()
        val tInit0 = analyzeExpr(init)
        val declType = typOpt.map(resolveTypeName).getOrElse(tInit0.typ)
        val tInit = coerceLiteral(tInit0, declType)
        globalScope(name) = SymInfo(name, declType, isMutable)
        scopeStack = null
        TVarDecl(name, declType, tInit, isPrivate)

  private def resolveTypeName(name: String): SyslType = name match
    case "int" | "i32" => I32
    case "char" => U32
    case "i64" => I64
    case "double" | "f64" => DoubleType
    case "byte" | "i8"  => I8
    case "i16"  => I16
    case "u8"   => U8
    case "u16"  => U16
    case "u32"  => U32
    case "u64"  => U64
    case "bool" => BoolType
    case "void" => VoidType
    case "string" => StringType
    case s if s.startsWith("[]") =>
      SliceType(resolveTypeName(s.drop(2)))
    case s if s.startsWith("*") =>
      PtrType(resolveTypeName(s.drop(1)))
    case s if s.startsWith("[") =>
      val size = s.drop(1).takeWhile(_.isDigit).toInt
      val elem = s.dropWhile(_ != ']').drop(1)
      ArrayType(resolveTypeName(elem), size)
    case name if typeAliases.contains(name) => resolveTypeName(typeAliases(name))
    case name if structTypes.contains(name) => structTypes(name)
    case s if s.startsWith("func(") =>
      val inner = s.drop(5) // after "func("
      val (paramStrs, rest) = parseFuncTypeParams(inner)
      val params = paramStrs.map(resolveTypeName)
      val ret = if rest.startsWith("->") then resolveTypeName(rest.drop(2)) else VoidType
      FuncType(params, ret)
    case other => throw AnalysisError(s"unknown type: '$other'")

  // Parse comma-separated params from "int,int)->int" returning (List("int","int"), "->int")
  private def parseFuncTypeParams(s: String): (List[String], String) =
    var depth = 0
    var i = 0
    val params = new mutable.ListBuffer[String]
    var start = 0
    while i < s.length do
      s(i) match
        case '(' => depth += 1; i += 1
        case ')' =>
          if depth == 0 then
            if i > start then params += s.substring(start, i)
            return (params.toList, s.drop(i + 1))
          depth -= 1; i += 1
        case ',' if depth == 0 =>
          params += s.substring(start, i)
          i += 1
          start = i
        case _ => i += 1
    (params.toList, "")

  private def compatible(from: SyslType, to: SyslType): Boolean =
    (from, to) match
      case (a, b) if a == b => true
      case (IntType(a), IntType(b)) if a <= b => true    // signed widening
      case (UIntType(a), UIntType(b)) if a <= b => true  // unsigned widening
      case (IntType(a), UIntType(b)) if a <= b => true   // signed → unsigned widening
      case (UIntType(a), IntType(b)) if a <= b => true   // unsigned → signed widening
      case (DoubleType, DoubleType) => true
      case (_: IntType, DoubleType) => true    // signed int → float promotion
      case (_: UIntType, DoubleType) => true   // unsigned int → float promotion
      case (DoubleType, _: IntType) => true    // float → signed int (truncation)
      case (DoubleType, _: UIntType) => true   // float → unsigned int (truncation)
      // bool and int are NOT compatible — use explicit casts
      // int ↔ pointer: NOT compatible — use explicit casts: int(ptr), *i8(addr)
      case (_: FuncType, IntType(64) | UIntType(64)) => true // function pointer → i64 (entry point address)
      case (PtrType(_), PtrType(_)) => true           // any pointer ↔ any pointer (like C's void*)
      case (ArrayType(_, _), PtrType(_)) => true          // array decays to any pointer
      case (StringType, PtrType(I8 | U8)) => true          // string decays to *i8 / *byte
      case (ArrayType(e1, _), ArrayType(e2, _)) if e1 == e2 => true
      case (ArrayType(e1, _), SliceType(e2)) if e1 == e2 => true  // fixed array → slice
      case (SliceType(e1), SliceType(e2)) if e1 == e2 => true
      case _ => false

  // Coerce integer literals to the target type (like Rust's untyped integer literals)
  private def coerceLiteral(expr: TExpr, target: SyslType): TExpr =
    expr match
      case TIntLit(value, _) if target.isIntegral => TIntLit(value, target)
      case TIntLit(0, _) if target.isInstanceOf[PtrType] => TIntLit(0, target) // null pointer
      case _ => expr

  // Coerce integer literals to match the target's signedness only (preserving original width)
  private def coerceSignedness(expr: TExpr, target: SyslType): TExpr =
    (expr, target) match
      case (TIntLit(value, IntType(w)), _: UIntType) => TIntLit(value, UIntType(w))
      case (TIntLit(value, UIntType(w)), _: IntType) => TIntLit(value, IntType(w))
      case _ => expr

  private def lookup(name: String): SymInfo =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return scopeStack(i)(name)
        i -= 1
    if globalScope.contains(name) then globalScope(name)
    else throw AnalysisError(s"undefined variable: '$name'")

  private def lookupOrCreate(name: String, typ: SyslType): SymInfo =
    if scopeStack != null then
      var i = scopeStack.length - 1
      while i >= 0 do
        if scopeStack(i).contains(name) then return scopeStack(i)(name)
        i -= 1
    if globalScope.contains(name) then globalScope(name)
    else
      val info = SymInfo(name, typ, true)
      if scopeStack != null then currentScope(name) = info
      else globalScope(name) = info
      info

  private def lookupFun(name: String): FunInfo =
    builtinFunctions.getOrElse(name,
      functions.getOrElse(name,
        throw AnalysisError(s"undefined function: '$name'")))

  private def checkArgs(name: String, params: List[(String, SyslType)], args: List[TExpr]): List[TExpr] =
    if args.length != params.length then
      throw AnalysisError(s"function '$name' expects ${params.length} argument(s), got ${args.length}")
    args.zip(params).map { case (arg, (pName, pType)) =>
      val coerced = coerceLiteral(arg, pType)
      if !compatible(coerced.typ, pType) then
        throw AnalysisError(s"argument '$pName' of '$name' expects $pType, got ${coerced.typ}")
      coerced
    }

  private def analyzeBlock(stmts: List[StmtAST]): List[TStmt] =
    stmts.map(analyzeStmt)

  private def analyzeStmt(stmt: StmtAST): TStmt =
    stmt match
      case VarStmtAST(name, typOpt, init, isMutable) =>
        val tInit0 = analyzeExpr(init)
        val declType = typOpt.map(resolveTypeName).getOrElse(tInit0.typ)
        val tInit = coerceLiteral(tInit0, declType)
        if typOpt.isDefined && !compatible(tInit.typ, declType) then
          throw AnalysisError(s"cannot assign ${tInit.typ} to $declType variable '$name'")
        if scopeStack != null then
          currentScope(name) = SymInfo(name, declType, isMutable)
        TVarStmt(name, declType, tInit)

      case AssignStmtAST(target, value) =>
        val tValue = analyzeExpr(value)
        val sym = lookupOrCreate(target, tValue.typ)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
        TAssignStmt(target, tValue)

      case CompoundAssignStmtAST(target, op, value) =>
        val sym = lookup(target)
        if !sym.mutable then throw AnalysisError(s"cannot assign to immutable variable '$target'")
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

      case FieldAssignStmtAST(obj, field, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldAssignStmt(resolvedObj, idx, tValue)

      case FieldCompoundAssignStmtAST(obj, field, op, value) =>
        val tObj = analyzeExpr(obj)
        val tValue = analyzeExpr(value)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldCompoundAssignStmt(resolvedObj, idx, op, tValue)

      case ReturnStmtAST(value) =>
        TReturnStmt(value.map(analyzeExpr))

      case ForStmtAST(init, cond, update, body) =>
        pushScope()
        val tInit = analyzeStmt(init)
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"for condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        pushScope()
        val tBody = analyzeBlock(body)
        popScope()
        val tUpdate = analyzeStmt(update)
        loopDepth -= 1
        popScope()
        TForStmt(tInit, tCond, tUpdate, tBody)

      case WhileStmtAST(cond, body) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"while condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        pushScope()
        val tBody = analyzeBlock(body)
        popScope()
        loopDepth -= 1
        TWhileStmt(tCond, tBody)

      case DoWhileStmtAST(cond, body) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"do/while condition must be bool, got ${tCond.typ}")
        loopDepth += 1
        pushScope()
        val tBody = analyzeBlock(body)
        popScope()
        loopDepth -= 1
        TDoWhileStmt(tCond, tBody)

      case BreakStmtAST() =>
        if loopDepth == 0 then throw AnalysisError("break outside of loop")
        TBreakStmt

      case ContinueStmtAST() =>
        if loopDepth == 0 then throw AnalysisError("continue outside of loop")
        TContinueStmt

      case DeferStmtAST(body) =>
        TDeferStmt(analyzeStmt(body))

      case AsmStmtAST(code) =>
        TAsmStmt(code)

      case ExprStmtAST(expr) =>
        TExprStmt(analyzeExpr(expr))

  private def analyzeExpr(expr: ExpressionAST): TExpr =
    expr match
      case IntLitAST(n) => TIntLit(n, I32)
      case TypedIntLitAST(n, typeName) => TIntLit(n, resolveTypeName(typeName))
      case FloatLitAST(d) => TFloatLit(d, DoubleType)
      case CharLitAST(c) => TIntLit(c.toLong, U32)
      case BoolLitAST(b) => TBoolLit(b, BoolType)
      case StringLitAST(s) => TStringLit(s, StringType)
      case StringLitExprAST(s) => TStringLit(s, StringType)
      case ArrayDeclAST(size, typStr) =>
        val t = resolveTypeName(typStr)
        TArrayDecl(size, typStr, t)

      case ArrayLitAST(elements) =>
        val tElems = elements.map(analyzeExpr)
        val elemType = tElems.head.typ
        TArrayLit(tElems, SyslType.ArrayType(elemType, tElems.length))

      case SizeofTypeAST(typeName) =>
        val t = resolveTypeName(typeName)
        TSizeof(t.sizeOf, I32)

      case SizeofExprAST(VarRefAST(name)) if structTypes.contains(name) =>
        // sizeof(StructName) — treat as type sizeof
        TSizeof(structTypes(name).sizeOf, I32)

      case SizeofExprAST(inner) =>
        val tInner = analyzeExpr(inner)
        TSizeof(tInner.typ.sizeOf, I32)

      case FieldPreIncAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPreDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPreDec(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostIncAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostInc(resolvedObj, idx, structType.fields(idx)._2)

      case FieldPostDecAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldPostDec(resolvedObj, idx, structType.fields(idx)._2)

      case StructInitAST(typeName) =>
        val t = resolveTypeName(typeName)
        t match
          case st: StructType => TStructLit(st)
          case _ => throw AnalysisError(s"'$typeName' is not a struct type")

      case UninitDeclAST(typeName) =>
        val t = resolveTypeName(typeName)
        t match
          case st: StructType => TStructLit(st)
          case ArrayType(elem, size) => TArrayDecl(size, typeName, t)
          case _ => TIntLit(0, t)  // zero-initialize scalars and pointers

      case VarRefAST(name) =>
        // Check if name is a function (used as a value = function pointer)
        if functions.contains(name) then
          val f = functions(name)
          TFuncRef(name, FuncType(f.params.map(_._2), f.returnType))
        else if builtinFunctions.contains(name) then
          val f = builtinFunctions(name)
          TFuncRef(name, FuncType(f.params.map(_._2), f.returnType))
        else
          val sym = lookup(name)
          TVarRef(name, sym.typ)

      case AddrOfAST(name) =>
        val sym = lookup(name)
        TAddrOf(name, PtrType(sym.typ))

      case AddrOfFieldAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot take address of field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TAddrOfField(resolvedObj, idx, PtrType(structType.fields(idx)._2))

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
          case StringType => throw AnalysisError("cannot dereference string — use indexing instead")
          case SliceType(_) => throw AnalysisError("cannot dereference slice — use indexing instead")
          case t => throw AnalysisError(s"cannot dereference $t")
        TDeref(tInner, resultType)

      case IndexAST(arr, index) =>
        val tArr = analyzeExpr(arr)
        val tIndex = analyzeExpr(index)
        val elemType = tArr.typ match
          case ArrayType(elem, _) => elem
          case PtrType(elem) => elem
          case SliceType(elem) => elem
          case StringType => I8
          case t => throw AnalysisError(s"cannot index $t")
        TIndex(tArr, tIndex, elemType)

      case FieldAccessAST(VarRefAST(enumName), member) if enumTypes.contains(enumName) =>
        val members = enumTypes(enumName)
        if !members.contains(member) then throw AnalysisError(s"enum $enumName has no member '$member'")
        TIntLit(members(member), I32)

      case FieldAccessAST(obj, field) =>
        val tObj = analyzeExpr(obj)
        // Auto-dereference pointers to structs (p.x works like (*p).x)
        val (resolvedObj, structType) = tObj.typ match
          case st: StructType => (tObj, st)
          case PtrType(st: StructType) => (TDeref(tObj, st), st)
          case other => throw AnalysisError(s"cannot access field '$field' on $other")
        val idx = structType.fields.indexWhere(_._1 == field)
        if idx < 0 then throw AnalysisError(s"struct ${structType.name} has no field '$field'")
        TFieldAccess(resolvedObj, idx, structType.fields(idx)._2)

      case PreIncAST(name) => TPreInc(name, lookup(name).typ)
      case PreDecAST(name) => TPreDec(name, lookup(name).typ)
      case PostIncAST(name) => TPostInc(name, lookup(name).typ)
      case PostDecAST(name) => TPostDec(name, lookup(name).typ)

      case UnaryAST(op, operand) =>
        val tOperand = analyzeExpr(operand)
        val resultType = op match
          case "-" => tOperand.typ
          case "~" =>
            if !tOperand.typ.isIntegral then throw AnalysisError(s"unary ~ requires integral type, got ${tOperand.typ}")
            tOperand.typ
          case "!" =>
            if tOperand.typ != BoolType then throw AnalysisError(s"unary ! requires bool, got ${tOperand.typ}")
            BoolType
        TUnary(op, tOperand, resultType)

      case BinaryAST(left, op, right) =>
        val tLeft0 = analyzeExpr(left)
        val tRight0 = analyzeExpr(right)
        // Coerce integer literal signedness to match the other operand (preserve width)
        val tLeft = if tRight0.typ.isIntegral then coerceSignedness(tLeft0, tRight0.typ) else tLeft0
        val tRight = if tLeft.typ.isIntegral then coerceSignedness(tRight0, tLeft.typ) else tRight0
        val resultType = op match
          case "+" | "-" if tLeft.typ == StringType && tRight.typ.isNumeric =>
            throw AnalysisError("pointer arithmetic not allowed on string")
          case "+" | "-" if tLeft.typ.isPointerLike && tRight.typ.isNumeric => tLeft.typ
          case "+" | "-" | "*" | "/" =>
            if !tLeft.typ.isNumeric || !tRight.typ.isNumeric then
              throw AnalysisError(s"operator $op requires numeric types, got ${tLeft.typ} $op ${tRight.typ}")
            // Promote to wider type; float wins over int; no mixed signed/unsigned
            (tLeft.typ, tRight.typ) match
              case (DoubleType, _) | (_, DoubleType) => DoubleType
              case (IntType(a), IntType(b)) => IntType(a max b)
              case (UIntType(a), UIntType(b)) => UIntType(a max b)
              case (l, r) if l.isIntegral && r.isIntegral =>
                throw AnalysisError(s"cannot mix signed and unsigned in $op: ${tLeft.typ} $op ${tRight.typ}")
              case _ => tLeft.typ
          case "%" | "&" | "|" | "^" | "<<" | ">>" =>
            if !tLeft.typ.isIntegral || !tRight.typ.isIntegral then
              throw AnalysisError(s"operator $op requires integral types, got ${tLeft.typ} $op ${tRight.typ}")
            (tLeft.typ, tRight.typ) match
              case (IntType(a), IntType(b)) => IntType(a max b)
              case (UIntType(a), UIntType(b)) => UIntType(a max b)
              case _ =>
                throw AnalysisError(s"cannot mix signed and unsigned in $op: ${tLeft.typ} $op ${tRight.typ}")
          case "==" | "!=" | "<" | ">" | "<=" | ">=" =>
            // Disallow mixed signed/unsigned comparisons
            if tLeft.typ.isIntegral && tRight.typ.isIntegral then
              (tLeft.typ, tRight.typ) match
                case (_: IntType, _: UIntType) | (_: UIntType, _: IntType) =>
                  throw AnalysisError(s"cannot compare signed and unsigned: ${tLeft.typ} $op ${tRight.typ}")
                case _ => // ok
            BoolType
          case "&&" | "||" =>
            if tLeft.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tLeft.typ}")
            if tRight.typ != BoolType then throw AnalysisError(s"$op requires bool operands, got ${tRight.typ}")
            BoolType
          case _ => throw AnalysisError(s"unknown operator: $op")
        // Insert implicit int→float promotion casts for mixed operands
        val promotedLeft = if resultType == DoubleType && tLeft.typ.isIntegral then TCast(tLeft, DoubleType) else tLeft
        val promotedRight = if resultType == DoubleType && tRight.typ.isIntegral then TCast(tRight, DoubleType) else tRight
        TBinary(promotedLeft, op, promotedRight, resultType)

      case CastAST(targetType, inner) =>
        val tInner = analyzeExpr(inner)
        val target = resolveTypeName(targetType)
        // Validate cast is possible
        (tInner.typ, target) match
          case (from, to) if from == to => // no-op cast
          case (from, BoolType) if from.isNumeric => // numeric to bool: != 0
          case (BoolType, to) if to.isNumeric => // bool to numeric: true=1, false=0
          case (from, DoubleType) if from.isIntegral => // int to float (cvt)
          case (DoubleType, to) if to.isIntegral => // float to int (fint)
          case (from, to) if from.isIntegral && to.isIntegral => // integer to integer (including signed↔unsigned)
          case (_: PtrType, to) if to.isIntegral => // pointer to integer
          case (from, _: PtrType) if from.isIntegral => // integer to pointer
          case (from, to) => throw AnalysisError(s"cannot cast $from to $to")
        TCast(tInner, target)

      case CallAST("len", args) =>
        if args.size != 1 then throw AnalysisError("len() takes exactly 1 argument")
        val tArg = analyzeExpr(args.head)
        tArg.typ match
          case StringType | SliceType(_) | ArrayType(_, _) => TLen(tArg, I32)
          case t => throw AnalysisError(s"len() not supported on $t")

      case CallAST("cap", args) =>
        if args.size != 1 then throw AnalysisError("cap() takes exactly 1 argument")
        val tArg = analyzeExpr(args.head)
        tArg.typ match
          case SliceType(_) => TCap(tArg, I32)
          case ArrayType(_, _) => TCap(tArg, I32)
          case t => throw AnalysisError(s"cap() not supported on $t")

      case MethodCallAST(obj, method, args) =>
        val tObj = analyzeExpr(obj)
        val tArgs = args.map(analyzeExpr)
        // Determine the struct type and build self argument
        val (structName, selfArg) = tObj.typ match
          case st @ StructType(name, _) =>
            // Need address of struct — build &obj
            val addr = tObj match
              case TVarRef(n, _) => TAddrOf(n, PtrType(st))
              case TFieldAccess(innerObj, idx, _) => TAddrOfField(innerObj, idx, PtrType(st))
              case _ => throw AnalysisError(s"cannot call method on this struct expression")
            (name, addr)
          case PtrType(StructType(name, _)) => (name, tObj) // already a pointer
          case other => throw AnalysisError(s"cannot call method '$method' on $other")
        // Look up the method
        val funcName = s"${structName}_$method"
        if !functions.contains(funcName) then
          throw AnalysisError(s"struct $structName has no method '$method'")
        val funInfo = functions(funcName)
        val checkedArgs = checkArgs(funcName, funInfo.params.tail, tArgs) // .tail skips self param
        TCall(funcName, selfArg :: checkedArgs, funInfo.returnType)

      case CallAST(name, args) =>
        val tArgs = args.map(analyzeExpr)
        // Check if it's a direct function call or an indirect call through a variable
        if functions.contains(name) || builtinFunctions.contains(name) then
          val funInfo = lookupFun(name)
          val checkedArgs = checkArgs(name, funInfo.params, tArgs)
          TCall(name, checkedArgs, funInfo.returnType)
        else
          // Try as a variable of FuncType
          val sym = lookup(name)
          sym.typ match
            case FuncType(paramTypes, returnType) =>
              val params = paramTypes.zipWithIndex.map { case (t, i) => (s"arg$i", t) }
              val checkedArgs = checkArgs(name, params, tArgs)
              TIndirectCall(TVarRef(name, sym.typ), checkedArgs, returnType)
            case other =>
              throw AnalysisError(s"'$name' is not a function (type: $other)")

      case IfExprAST(cond, thenBody, elseBody) =>
        val tCond = analyzeExpr(cond)
        if tCond.typ != BoolType then throw AnalysisError(s"if condition must be bool, got ${tCond.typ}")
        pushScope()
        val tThen = analyzeBlock(thenBody)
        popScope()
        val tElse = elseBody.map { stmts => pushScope(); val r = analyzeBlock(stmts); popScope(); r }
        val resultType = tThen.lastOption match
          case Some(TExprStmt(e)) => e.typ
          case _ => VoidType
        TIfExpr(tCond, tThen, tElse, resultType)
