package io.github.edadma.trisc

import scala.collection.mutable

class SyslLLVMCodegen:
  private val out = new StringBuilder
  private val stringConstants = new mutable.LinkedHashMap[String, (String, Int)] // value -> (label, byte length including null)
  private val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType] // name -> struct type
  private val deinitFunctions = new mutable.HashMap[String, String] // struct name -> deinit function name
  private var closureCounter = 0
  private val pendingClosures = new mutable.ListBuffer[(String, TClosure)] // (name, closure)
  private val funcWrappers = new mutable.LinkedHashMap[String, String] // original name -> wrapper name
  private val pendingWrappers = new mutable.ListBuffer[(String, String, List[SyslType], SyslType)] // (wrapperName, origName, params, retType)
  private var stringCounter = 0
  private var regCounter = 0
  private var labelCounter = 0

  private def newReg(): String =
    regCounter += 1
    s"%t$regCounter"

  private def newLabel(prefix: String): String =
    labelCounter += 1
    s"${prefix}_$labelCounter"

  private def internString(s: String): (String, Int) =
    stringConstants.getOrElseUpdate(s, {
      stringCounter += 1
      val label = s"@.str.$stringCounter"
      val byteLen = s.getBytes("UTF-8").length + 1 // +1 for null terminator
      (label, byteLen)
    })

  private case class LocalVar(name: String, reg: String, typ: SyslType)

  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var currentFunction: TFunDecl = null
  private var hasReturned = false
  // Break/continue label stacks for loop codegen
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]
  // Defer stack — LIFO execution before returns
  private val deferStack = new mutable.Stack[TStmt]

  def generate(program: TProgram): String =
    out.clear()
    stringConstants.clear()
    stringCounter = 0
    closureCounter = 0
    pendingClosures.clear()
    pendingWrappers.clear()
    funcWrappers.clear()

    // First pass: collect struct type definitions and deinit functions
    structTypes.clear()
    deinitFunctions.clear()
    for decl <- program.decls do
      decl match
        case TStructDecl(name, fields) =>
          structTypes(name) = SyslType.StructType(name, fields)
        case TFunDecl(name, _, _, _, _, _) if name.endsWith("_deinit") =>
          val structName = name.indexOf("__") match
            case -1 => name.dropRight(7) // "Point_deinit" -> "Point"
            case i  => name.substring(i + 2).dropRight(7) // "mod__Point_deinit" -> "Point"
          deinitFunctions(structName) = name
        case _ =>

    // Generate functions into a buffer so string constants are collected first
    out.clear()
    for decl <- program.decls do
      decl match
        case _: TModuleDecl => // skip
        case _: TImportDecl => // skip
        case TExternFuncDecl(name, params, retType) =>
          val paramStr = params.map(llvmType).mkString(", ")
          emit(s"declare ${llvmType(retType)} @$name($paramStr)")
        case TExternVarDecl(name, typ) =>
          emit(s"@$name = external global ${llvmType(typ)}")
        case _: TStructDecl => // skip (handled above)
        case _: TEnumDecl => // type only
        case _: TDataEnumDecl => // type only
        case _: TTypeAliasDecl => // type only
        case _: TInterfaceDecl => // type only
        case f: TFunDecl => genFunction(f)
        case TVarDecl(name, typ, init, _) =>
          val initVal = constValue(init, typ)
          emit(s"@$name = global ${llvmType(typ)} $initVal")
    emit("")
    // Generate pending closure functions and wrappers
    while pendingClosures.nonEmpty || pendingWrappers.nonEmpty do
      val closureBatch = pendingClosures.toList
      pendingClosures.clear()
      for (name, closure) <- closureBatch do
        genClosureFunction(name, closure)
      val wrapperBatch = pendingWrappers.toList
      pendingWrappers.clear()
      for (wn, origName, params, retType) <- wrapperBatch do
        emitFuncWrapper(wn, origName, params, retType)
    val funcCode = out.toString

    // Now build final output with string constants at the top
    out.clear()

    // Declare external C functions
    emit("declare i32 @putchar(i32)")
    emit("declare i32 @printf(i8*, ...)")
    emit("declare i32 @snprintf(i8*, i64, i8*, ...)")
    emit("declare i8* @malloc(i64)")
    emit("declare i64 @strlen(i8*)")
    emit("declare i8* @memcpy(i8*, i8*, i64)")
    emit("declare i8* @memset(i8*, i32, i64)")
    emit("declare void @free(i8*)")
    emit("declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i1)")
    emit("declare i64 @write(i32, i8*, i64)")
    emit("declare i32 @fflush(i8*)")
    emit("")

    // Format strings for print/println builtins
    emit("""@.fmt_d = private unnamed_addr constant [3 x i8] c"%d\00"""")
    emit("""@.fmt_dn = private unnamed_addr constant [4 x i8] c"%d\0A\00"""")
    emit("""@.fmt_f = private unnamed_addr constant [3 x i8] c"%g\00"""")
    emit("""@.fmt_fn = private unnamed_addr constant [4 x i8] c"%g\0A\00"""")
    emit("""@.fmt_ld = private unnamed_addr constant [4 x i8] c"%ld\00"""")
    emit("""@.str.true = private unnamed_addr constant [5 x i8] c"true\00"""")
    emit("""@.str.false = private unnamed_addr constant [6 x i8] c"false\00"""")
    emit("""@.str.newline = private unnamed_addr constant [1 x i8] c"\0A"""")
    emit("")

    // String struct type: { ptr, len }
    emit("%struct.string = type { i8*, i32 }")
    // Slice struct type: { ptr, len, cap }
    emit("%struct.slice = type { i8*, i32, i32 }")
    // Closure struct type: { func_ptr, env_ptr }
    emit("%struct.closure = type { i8*, i8* }")
    emit("")

    // Emit struct type definitions
    for (name, st) <- structTypes do
      val fieldTypes = st.fields.map((_, ft) => llvmType(ft)).mkString(", ")
      emit(s"%struct.$name = type { $fieldTypes }")
    if structTypes.nonEmpty then emit("")

    // Emit string constants
    for (s, (label, byteLen)) <- stringConstants do
      val escaped = s.flatMap {
        case '\n' => "\\0A"
        case '\r' => "\\0D"
        case '\t' => "\\09"
        case '\\' => "\\5C"
        case '"'  => "\\22"
        case '\u0000' => "\\00"
        case c    => c.toString
      }
      emit(s"""$label = private unnamed_addr constant [$byteLen x i8] c"$escaped\\00"""")
    if stringConstants.nonEmpty then emit("")

    // Append function code
    out ++= funcCode

    out.toString

  private def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    regCounter = 0
    labelCounter = 0
    hasReturned = false
    deferStack.clear()

    val retType = if fun.name == "main" then "i64" else llvmType(fun.returnType)
    val params = fun.params.map(p => s"${llvmType(p.typ)} %${p.name}_arg").mkString(", ")

    emit(s"define $retType @${fun.name}($params) {")
    emit("entry:")

    // Allocate and store parameters
    for param <- fun.params do
      val lt = llvmType(param.typ)
      val alloca = newReg()
      emit(s"  $alloca = alloca $lt")
      emit(s"  store $lt %${param.name}_arg, $lt* $alloca")
      locals(param.name) = LocalVar(param.name, alloca, param.typ)
      // Increment refcount for ref-typed params (caller shares ownership)
      if isRef(param.typ) then
        emitRefIncr(s"%${param.name}_arg", refHeaderOffset(param.typ))

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        val result = genExpr(expr)
        val rt = exprType(expr)
        val finalVal = if isAggregate(expr.typ) then
          val loaded = newReg()
          emit(s"  $loaded = load $retType, $retType* $result")
          loaded
        else emitSextIfNeeded(result, rt, retType)
        emitReleaseRefs()
        emit(s"  ret $retType $finalVal")
      case TBlockBody(stmts) =>
        genBlock(stmts, retType)

    emit("}")
    emit("")
    locals = null
    currentFunction = null

  /** Generate a closure function with hidden env_ptr first parameter. */
  private def genClosureFunction(name: String, closure: TClosure): Unit =
    locals = new mutable.LinkedHashMap
    regCounter = 0
    labelCounter = 0
    hasReturned = false
    deferStack.clear()

    val retLt = llvmType(closure.returnType)
    val paramStrs = "i8* %env" +: closure.params.map(p => s"${llvmType(p.typ)} %${p.name}_arg")

    emit(s"define $retLt @$name(${paramStrs.mkString(", ")}) {")
    emit("entry:")

    // Unpack captured variables from env
    var offset = 0L
    for (capName, capType) <- closure.captures do
      val lt = llvmType(capType)
      val envFieldPtr = newReg()
      emit(s"  $envFieldPtr = getelementptr i8, i8* %env, i64 $offset")
      val typedPtr = newReg()
      emit(s"  $typedPtr = bitcast i8* $envFieldPtr to $lt*")
      if isAggregate(capType) then
        locals(capName) = LocalVar(capName, typedPtr, capType)
      else
        val alloca = newReg()
        emit(s"  $alloca = alloca $lt")
        val v = newReg()
        emit(s"  $v = load $lt, $lt* $typedPtr")
        emit(s"  store $lt $v, $lt* $alloca")
        locals(capName) = LocalVar(capName, alloca, capType)
      offset += llvmSizeOf(capType)

    // Allocate and store regular parameters
    for param <- closure.params do
      val lt = llvmType(param.typ)
      val alloca = newReg()
      emit(s"  $alloca = alloca $lt")
      emit(s"  store $lt %${param.name}_arg, $lt* $alloca")
      locals(param.name) = LocalVar(param.name, alloca, param.typ)

    // Generate body
    closure.body match
      case TExprBody(expr) =>
        val result = genExpr(expr)
        val rt = exprType(expr)
        val finalVal = if isAggregate(expr.typ) then
          val loaded = newReg()
          emit(s"  $loaded = load $retLt, $retLt* $result")
          loaded
        else emitSextIfNeeded(result, rt, retLt)
        emitReleaseRefs()
        emit(s"  ret $retLt $finalVal")
      case TBlockBody(stmts) =>
        genBlock(stmts, retLt)

    emit("}")
    emit("")
    locals = null

  /** Generate a wrapper function that adapts a plain function to the closure ABI (env as first param). */
  private def emitFuncWrapper(wrapperName: String, origName: String, params: List[SyslType], retType: SyslType): Unit =
    val retLt = llvmType(retType)
    val paramNames = params.zipWithIndex.map((_, i) => s"%p$i")
    val paramStrs = "i8* %env" +: params.zip(paramNames).map((t, n) => s"${llvmType(t)} $n")
    emit(s"define $retLt @$wrapperName(${paramStrs.mkString(", ")}) {")
    emit("entry:")
    val argStr = params.zip(paramNames).map((t, n) => s"${llvmType(t)} $n").mkString(", ")
    if retLt == "void" then
      emit(s"  call void @$origName($argStr)")
      emit("  ret void")
    else
      emit(s"  %r = call $retLt @$origName($argStr)")
      emit(s"  ret $retLt %r")
    emit("}")
    emit("")

  private def genBlock(stmts: List[TStmt], retType: String): Unit =
    if stmts.nonEmpty then
      for stmt <- stmts.init do
        if !hasReturned then genStmt(stmt)
      if !hasReturned then
        stmts.last match
          case TExprStmt(expr) =>
            val result = genExpr(expr)
            val rt = exprType(expr)
            val finalVal = if isAggregate(expr.typ) then
              val loaded = newReg()
              emit(s"  $loaded = load $retType, $retType* $result")
              loaded
            else emitSextIfNeeded(result, rt, retType)
            emitDefers()
            emitReleaseRefs()
            emit(s"  ret $retType $finalVal")
            hasReturned = true
          case other =>
            genStmt(other)
            if !hasReturned then
              emitDefers()
              emitReleaseRefs()
              emit(s"  ret $retType 0")
              hasReturned = true
    else
      emitDefers()
      emitReleaseRefs()
      emit(s"  ret $retType 0")
      hasReturned = true

  // Get LLVM type string for an expression based on its type
  private def exprType(expr: TExpr): String = llvmType(expr.typ)

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, typ, init) =>
        val lt = llvmType(typ)
        if isAggregate(typ) then
          // Aggregate variable: genExpr returns an alloca pointer — use it directly
          val ptr = genExpr(init)
          locals(name) = LocalVar(name, ptr, typ)
        else
            val alloca = newReg()
            emit(s"  $alloca = alloca $lt")
            val value = genExpr(init)
            val vt = exprType(init)
            val finalVal = emitSextIfNeeded(value, vt, lt)
            emit(s"  store $lt $finalVal, $lt* $alloca")
            locals(name) = LocalVar(name, alloca, typ)
            // Ref init: increment unless we own it (TNew/TNewArray)
            if isRef(typ) && !isOwnedNew(init) then
              emitRefIncr(finalVal, refHeaderOffset(typ))

      case TAssignStmt(target, value) =>
        if !locals.contains(target) && isAggregate(value.typ) then
          // New aggregate variable: genExpr returns an alloca pointer — use it directly
          val ptr = genExpr(value)
          locals(target) = LocalVar(target, ptr, value.typ)
        else
          val v = genExpr(value)
          if locals.contains(target) then
            val local = locals(target)
            val lt = llvmType(local.typ)
            // Reassignment of ref: decrement old, increment new
            if isRef(local.typ) then
              val oldVal = newReg()
              emit(s"  $oldVal = load $lt, $lt* ${local.reg}")
              emitRefDecr(oldVal, refHeaderOffset(local.typ), deinitFor(local.typ))
            if isAggregate(local.typ) then
              // Aggregate reassignment: load value from source, store to target
              val loaded = newReg()
              emit(s"  $loaded = load $lt, $lt* $v")
              emit(s"  store $lt $loaded, $lt* ${local.reg}")
            else
              val vt = exprType(value)
              val finalVal = emitSextIfNeeded(v, vt, lt)
              emit(s"  store $lt $finalVal, $lt* ${local.reg}")
              if isRef(local.typ) && !isOwnedNew(value) then
                emitRefIncr(finalVal, refHeaderOffset(local.typ))
          else
            val lt = exprType(value)
            val alloca = newReg()
            emit(s"  $alloca = alloca $lt")
            emit(s"  store $lt $v, $lt* $alloca")
            locals(target) = LocalVar(target, alloca, value.typ)
            // New ref binding: increment unless owned
            if isRef(value.typ) && !isOwnedNew(value) then
              emitRefIncr(v, refHeaderOffset(value.typ))

      case TReturnStmt(Some(value)) =>
        val v = genExpr(value)
        val retType = if currentFunction.name == "main" then "i64" else llvmType(currentFunction.returnType)
        val vt = exprType(value)
        val finalVal = if isAggregate(value.typ) then
          val loaded = newReg()
          emit(s"  $loaded = load $retType, $retType* $v")
          loaded
        else emitSextIfNeeded(v, vt, retType)
        emitDefers()
        emitReleaseRefs()
        emit(s"  ret $retType $finalVal")
        hasReturned = true

      case TReturnStmt(None) =>
        emitDefers()
        emitReleaseRefs()
        emit("  ret void")
        hasReturned = true

      case TDeferStmt(body) =>
        deferStack.push(body)

      case TExprStmt(expr) =>
        genExpr(expr)

      case TWhileStmt(cond, body) =>
        val condLabel = newLabel("while_cond")
        val bodyLabel = newLabel("while_body")
        val endLabel = newLabel("while_end")
        breakLabels.push(endLabel)
        continueLabels.push(condLabel)
        emit(s"  br label %$condLabel")
        emit(s"$condLabel:")
        val c = genExpr(cond)
        val ct = exprType(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne $ct $c, 0")
        emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emit(s"$bodyLabel:")
        val savedHR = hasReturned
        hasReturned = false
        for s <- body do if !hasReturned then genStmt(s)
        if !hasReturned then emit(s"  br label %$condLabel")
        emit(s"$endLabel:")
        hasReturned = savedHR
        breakLabels.pop()
        continueLabels.pop()

      case TForStmt(init, cond, update, body) =>
        genStmt(init)
        val condLabel = newLabel("for_cond")
        val bodyLabel = newLabel("for_body")
        val updateLabel = newLabel("for_update")
        val endLabel = newLabel("for_end")
        breakLabels.push(endLabel)
        continueLabels.push(updateLabel)
        emit(s"  br label %$condLabel")
        emit(s"$condLabel:")
        val c = genExpr(cond)
        val ct = exprType(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne $ct $c, 0")
        emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emit(s"$bodyLabel:")
        val savedHR = hasReturned
        hasReturned = false
        for s <- body do if !hasReturned then genStmt(s)
        if !hasReturned then emit(s"  br label %$updateLabel")
        emit(s"$updateLabel:")
        if !hasReturned then genStmt(update)
        if !hasReturned then emit(s"  br label %$condLabel")
        emit(s"$endLabel:")
        hasReturned = savedHR
        breakLabels.pop()
        continueLabels.pop()

      case TDoWhileStmt(cond, body) =>
        val bodyLabel = newLabel("dowhile_body")
        val condLabel = newLabel("dowhile_cond")
        val endLabel = newLabel("dowhile_end")
        breakLabels.push(endLabel)
        continueLabels.push(condLabel)
        emit(s"  br label %$bodyLabel")
        emit(s"$bodyLabel:")
        val savedHR = hasReturned
        hasReturned = false
        for s <- body do if !hasReturned then genStmt(s)
        if !hasReturned then emit(s"  br label %$condLabel")
        emit(s"$condLabel:")
        if !hasReturned then
          val c = genExpr(cond)
          val ct = exprType(cond)
          val cBool = newReg()
          emit(s"  $cBool = icmp ne $ct $c, 0")
          emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emit(s"$endLabel:")
        hasReturned = savedHR
        breakLabels.pop()
        continueLabels.pop()

      case TBreakStmt =>
        emit(s"  br label %${breakLabels.top}")
        hasReturned = true // stop emitting after unconditional branch

      case TContinueStmt =>
        emit(s"  br label %${continueLabels.top}")
        hasReturned = true

      case TIndexAssignStmt(array, index, value) =>
        val base = genExpr(array)
        val idx = genExpr(index)
        val v = genExpr(value)
        val elemType = array.typ match
          case SyslType.ArrayType(elem, _) => elem
          case SyslType.SliceType(elem) => elem
          case SyslType.RefType(SyslType.SliceType(elem)) => elem
          case _ => SyslType.IntType(8) // fallback for string indexing
        val elt = llvmType(elemType)
        val elemSize = llvmSizeOf(elemType)
        // Get data pointer
        val dataPtr = array.typ match
          case SyslType.ArrayType(_, _) =>
            val cast = newReg()
            emit(s"  $cast = bitcast ${llvmType(array.typ)}* $base to i8*")
            cast
          case SyslType.SliceType(_) =>
            // Slice struct: ptr at offset 0
            val ptrGep = newReg()
            emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 0")
            val ptr = newReg()
            emit(s"  $ptr = load i8*, i8** $ptrGep")
            ptr
          case _ =>
            base // pointer type — already a data pointer
        // Compute element address
        val idx64 = newReg()
        emit(s"  $idx64 = sext i32 $idx to i64")
        val offset = newReg()
        emit(s"  $offset = mul i64 $idx64, $elemSize")
        val elemAddr = newReg()
        emit(s"  $elemAddr = getelementptr i8, i8* $dataPtr, i64 $offset")
        val typedPtr = newReg()
        emit(s"  $typedPtr = bitcast i8* $elemAddr to $elt*")
        emit(s"  store $elt $v, $elt* $typedPtr")

      case TDerefAssignStmt(pointer, value) =>
        val ptr = genExpr(pointer)
        val v = genExpr(value)
        val pointeeType = pointer.typ match
          case SyslType.PtrType(inner) => inner
          case SyslType.RefType(inner) => inner
          case _ => SyslType.IntType(64)
        val pt = llvmType(pointeeType)
        val typedPtr = newReg()
        emit(s"  $typedPtr = bitcast i8* $ptr to $pt*")
        emit(s"  store $pt $v, $pt* $typedPtr")

      case TFieldAssignStmt(obj, fieldIndex, value) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val structLt = llvmType(obj.typ)
        val fieldType = llvmType(st.fields(fieldIndex)._2)
        val addr = genStructAddr(obj)
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        val v = genExpr(value)
        emit(s"  store $fieldType $v, $fieldType* $gep")

      case _ =>
        emit(s"  ; TODO: ${stmt.getClass.getSimpleName}")

  private def genExpr(expr: TExpr): String =
    val t = exprType(expr)
    expr match
      case TIntLit(n, _) => n.toString
      case TFloatLit(d, _) =>
        // Use LLVM hex format for exact representation
        val bits = java.lang.Double.doubleToRawLongBits(d)
        s"0x${bits.toHexString.toUpperCase}"
      case TBoolLit(true, _) => "1"
      case TBoolLit(false, _) => "0"

      case TStringLit(s, _) =>
        val (label, byteLen) = internString(s)
        val strLen = byteLen - 1 // exclude null terminator for fat string length
        val alloca = newReg()
        emit(s"  $alloca = alloca %struct.string")
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 0")
        val dataPtr = newReg()
        emit(s"  $dataPtr = getelementptr [$byteLen x i8], [$byteLen x i8]* $label, i32 0, i32 0")
        emit(s"  store i8* $dataPtr, i8** $ptrGep")
        val lenGep = newReg()
        emit(s"  $lenGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 1")
        emit(s"  store i32 $strLen, i32* $lenGep")
        alloca

      case TVarRef(name, typ) =>
        if locals.contains(name) then
          val local = locals(name)
          if isAggregate(local.typ) then
            local.reg // aggregates: return address, don't load
          else
            val lt = llvmType(local.typ)
            val r = newReg()
            emit(s"  $r = load $lt, $lt* ${local.reg}")
            r
        else
          val r = newReg()
          emit(s"  $r = load $t, $t* @$name")
          r

      // String concatenation
      case TBinary(left, "+", right, SyslType.StringType) =>
        val lp = genExpr(left) // alloca pointer to %struct.string
        val rp = genExpr(right)
        // Extract ptr and len from both
        val lPtrGep = newReg()
        emit(s"  $lPtrGep = getelementptr %struct.string, %struct.string* $lp, i32 0, i32 0")
        val lPtr = newReg()
        emit(s"  $lPtr = load i8*, i8** $lPtrGep")
        val lLenGep = newReg()
        emit(s"  $lLenGep = getelementptr %struct.string, %struct.string* $lp, i32 0, i32 1")
        val lLen = newReg()
        emit(s"  $lLen = load i32, i32* $lLenGep")
        val rPtrGep = newReg()
        emit(s"  $rPtrGep = getelementptr %struct.string, %struct.string* $rp, i32 0, i32 0")
        val rPtr = newReg()
        emit(s"  $rPtr = load i8*, i8** $rPtrGep")
        val rLenGep = newReg()
        emit(s"  $rLenGep = getelementptr %struct.string, %struct.string* $rp, i32 0, i32 1")
        val rLen = newReg()
        emit(s"  $rLen = load i32, i32* $rLenGep")
        // Total length and allocate
        val totalLen = newReg()
        emit(s"  $totalLen = add i32 $lLen, $rLen")
        val totalLen64 = newReg()
        emit(s"  $totalLen64 = sext i32 $totalLen to i64")
        val buf = newReg()
        emit(s"  $buf = call i8* @malloc(i64 $totalLen64)")
        // Copy left then right
        val lLen64 = newReg()
        emit(s"  $lLen64 = sext i32 $lLen to i64")
        val cp1 = newReg()
        emit(s"  $cp1 = call i8* @memcpy(i8* $buf, i8* $lPtr, i64 $lLen64)")
        val dest = newReg()
        emit(s"  $dest = getelementptr i8, i8* $buf, i64 $lLen64")
        val rLen64 = newReg()
        emit(s"  $rLen64 = sext i32 $rLen to i64")
        val cp2 = newReg()
        emit(s"  $cp2 = call i8* @memcpy(i8* $dest, i8* $rPtr, i64 $rLen64)")
        // Build result %struct.string
        val alloca = newReg()
        emit(s"  $alloca = alloca %struct.string")
        val resPtrGep = newReg()
        emit(s"  $resPtrGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 0")
        emit(s"  store i8* $buf, i8** $resPtrGep")
        val resLenGep = newReg()
        emit(s"  $resLenGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 1")
        emit(s"  store i32 $totalLen, i32* $resLenGep")
        alloca

      case TBinary(left, op, right, _) =>
        val l = genExpr(left)
        val r = genExpr(right)
        val lt = exprType(left)
        val isFloat = left.typ == SyslType.DoubleType
        val isUnsigned = left.typ.isUnsigned
        val result = newReg()
        op match
          case "+" => emit(s"  $result = ${if isFloat then "fadd" else "add"} $lt $l, $r")
          case "-" => emit(s"  $result = ${if isFloat then "fsub" else "sub"} $lt $l, $r")
          case "*" => emit(s"  $result = ${if isFloat then "fmul" else "mul"} $lt $l, $r")
          case "/" => emit(s"  $result = ${if isFloat then "fdiv" else if isUnsigned then "udiv" else "sdiv"} $lt $l, $r")
          case "%" => emit(s"  $result = ${if isFloat then "frem" else if isUnsigned then "urem" else "srem"} $lt $l, $r")
          case "&"  => emit(s"  $result = and $lt $l, $r")
          case "|"  => emit(s"  $result = or $lt $l, $r")
          case "^"  => emit(s"  $result = xor $lt $l, $r")
          case "<<" => emit(s"  $result = shl $lt $l, $r")
          case ">>" => emit(s"  $result = ${if isUnsigned then "lshr" else "ashr"} $lt $l, $r")
          case "==" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp oeq $lt $l, $r")
            else emit(s"  $cmp = icmp eq $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case "!=" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp one $lt $l, $r")
            else emit(s"  $cmp = icmp ne $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case "<" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp olt $lt $l, $r")
            else emit(s"  $cmp = icmp ${if isUnsigned then "ult" else "slt"} $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case ">" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp ogt $lt $l, $r")
            else emit(s"  $cmp = icmp ${if isUnsigned then "ugt" else "sgt"} $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case "<=" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp ole $lt $l, $r")
            else emit(s"  $cmp = icmp ${if isUnsigned then "ule" else "sle"} $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case ">=" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp oge $lt $l, $r")
            else emit(s"  $cmp = icmp ${if isUnsigned then "uge" else "sge"} $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case "&&" =>
            val lBool = newReg()
            val rBool = newReg()
            val andResult = newReg()
            emit(s"  $lBool = icmp ne $lt $l, 0")
            emit(s"  $rBool = icmp ne $lt $r, 0")
            emit(s"  $andResult = and i1 $lBool, $rBool")
            emit(s"  $result = zext i1 $andResult to $t")
          case "||" =>
            val lBool = newReg()
            val rBool = newReg()
            val orResult = newReg()
            emit(s"  $lBool = icmp ne $lt $l, 0")
            emit(s"  $rBool = icmp ne $lt $r, 0")
            emit(s"  $orResult = or i1 $lBool, $rBool")
            emit(s"  $result = zext i1 $orResult to $t")
        result

      case TUnary("-", operand, _) =>
        val v = genExpr(operand)
        val vt = exprType(operand)
        val result = newReg()
        if operand.typ == SyslType.DoubleType then emit(s"  $result = fneg double $v")
        else emit(s"  $result = sub $vt 0, $v")
        result

      case TUnary("!", operand, _) =>
        val v = genExpr(operand)
        val vt = exprType(operand)
        val cmp = newReg()
        val result = newReg()
        emit(s"  $cmp = icmp eq $vt $v, 0")
        emit(s"  $result = zext i1 $cmp to $vt")
        result

      case TUnary("~", operand, _) =>
        val v = genExpr(operand)
        val vt = exprType(operand)
        val result = newReg()
        emit(s"  $result = xor $vt $v, -1")
        result

      case TCall("putchar", List(arg), _) =>
        val v = genExpr(arg)
        val vt = exprType(arg)
        val truncated = if vt != "i32" then
          val tr = newReg()
          emit(s"  $tr = trunc $vt $v to i32")
          tr
        else v
        val result = newReg()
        emit(s"  $result = call i32 @putchar(i32 $truncated)")
        emitSextIfNeeded(result, "i32", t)

      case TCall("print", List(arg), _) =>
        arg.typ match
          case SyslType.StringType =>
            val sp = genExpr(arg) // alloca pointer to %struct.string
            emitWriteString(sp)
            "0"
          case _ =>
            val v = genExpr(arg)
            val vt = exprType(arg)
            val (fmtName, fmtLen) = arg.typ match
              case SyslType.DoubleType => ("@.fmt_f", 3)
              case _                   => ("@.fmt_d", 3)
            val result = newReg()
            emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([$fmtLen x i8], [$fmtLen x i8]* $fmtName, i32 0, i32 0), $vt $v)""")
            "0"

      case TCall("println", List(arg), _) =>
        arg.typ match
          case SyslType.StringType =>
            val sp = genExpr(arg)
            emitWriteString(sp)
            // Write newline
            val nlPtr = newReg()
            emit(s"  $nlPtr = getelementptr [1 x i8], [1 x i8]* @.str.newline, i32 0, i32 0")
            val ignored = newReg()
            emit(s"  $ignored = call i64 @write(i32 1, i8* $nlPtr, i64 1)")
            "0"
          case _ =>
            val v = genExpr(arg)
            val vt = exprType(arg)
            val (fmtName, fmtLen) = arg.typ match
              case SyslType.DoubleType => ("@.fmt_fn", 4)
              case _                   => ("@.fmt_dn", 4)
            val result = newReg()
            emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([$fmtLen x i8], [$fmtLen x i8]* $fmtName, i32 0, i32 0), $vt $v)""")
            "0"

      case TCall("puts", List(arg), _) =>
        val sp = genExpr(arg)
        emitWriteString(sp)
        // puts also writes a newline
        val nlPtr = newReg()
        emit(s"  $nlPtr = getelementptr [1 x i8], [1 x i8]* @.str.newline, i32 0, i32 0")
        val ignored = newReg()
        emit(s"  $ignored = call i64 @write(i32 1, i8* $nlPtr, i64 1)")
        "0"

      case TCall(name, args, _) =>
        val argVals = args.map { a =>
          val v = genExpr(a)
          val vt = exprType(a)
          // For aggregate types, genExpr returns a pointer — load the value for pass-by-value
          if isAggregate(a.typ) then
            val loaded = newReg()
            emit(s"  $loaded = load $vt, $vt* $v")
            (loaded, vt)
          else (v, vt)
        }
        val argStr = argVals.map((v, vt) => s"$vt $v").mkString(", ")
        val retType = llvmType(expr.typ)
        if retType == "void" then
          emit(s"  call void @$name($argStr)")
          "0"
        else
          val result = newReg()
          emit(s"  $result = call $retType @$name($argStr)")
          // If the return type is aggregate, store into alloca so callers get a pointer
          if isAggregate(expr.typ) then
            val alloca = newReg()
            emit(s"  $alloca = alloca $retType")
            emit(s"  store $retType $result, $retType* $alloca")
            alloca
          else result

      case TIfExpr(cond, thenBody, elseBody, typ) =>
        val c = genExpr(cond)
        val ct = exprType(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne $ct $c, 0")
        val thenLabel = newLabel("then")
        val elseLabel = newLabel("else")
        val mergeLabel = newLabel("merge")
        emit(s"  br i1 $cBool, label %$thenLabel, label %$elseLabel")

        emit(s"$thenLabel:")
        val savedHasReturned = hasReturned
        hasReturned = false
        var thenVal = "0"
        for s <- thenBody.init do genStmt(s)
        if !hasReturned then
          thenBody.lastOption match
            case Some(TExprStmt(e)) => thenVal = genExpr(e)
            case Some(other) => genStmt(other)
            case None =>
        val thenReturned = hasReturned
        if !thenReturned then emit(s"  br label %$mergeLabel")
        hasReturned = savedHasReturned

        emit(s"$elseLabel:")
        var elseVal = "0"
        hasReturned = false
        elseBody.foreach { stmts =>
          for s <- stmts.init do genStmt(s)
          if !hasReturned then
            stmts.lastOption match
              case Some(TExprStmt(e)) => elseVal = genExpr(e)
              case Some(other) => genStmt(other)
              case None =>
        }
        val elseReturned = hasReturned
        if !elseReturned then emit(s"  br label %$mergeLabel")
        hasReturned = savedHasReturned

        emit(s"$mergeLabel:")
        if !thenReturned && !elseReturned then
          val phi = newReg()
          emit(s"  $phi = phi $t [ $thenVal, %$thenLabel ], [ $elseVal, %$elseLabel ]")
          phi
        else "0"

      case TStructConstruct(st, args) =>
        // Alloca, zero-init, then fill fields
        val lt = llvmType(st)
        val alloca = newReg()
        emit(s"  $alloca = alloca $lt")
        emit(s"  store $lt zeroinitializer, $lt* $alloca")
        for (arg, i) <- args.zipWithIndex do
          val v = genExpr(arg)
          val fieldSyslType = st.fields(i)._2
          val fieldType = llvmType(fieldSyslType)
          val gep = newReg()
          emit(s"  $gep = getelementptr $lt, $lt* $alloca, i32 0, i32 $i")
          // If the arg is an aggregate, genExpr returned a pointer — load the value
          val storeVal = if isAggregate(fieldSyslType) then
            val loaded = newReg()
            emit(s"  $loaded = load $fieldType, $fieldType* $v")
            loaded
          else v
          emit(s"  store $fieldType $storeVal, $fieldType* $gep")
        alloca

      case TStructLit(st @ SyslType.StructType(_, _)) =>
        val lt = llvmType(st)
        val alloca = newReg()
        emit(s"  $alloca = alloca $lt")
        emit(s"  store $lt zeroinitializer, $lt* $alloca")
        alloca

      case TFieldAccess(obj, fieldIndex, fieldType) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val structLt = llvmType(obj.typ)
        val addr = genStructAddr(obj)
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        if isAggregate(fieldType) then
          gep // return address for aggregate fields
        else
          val ft = llvmType(fieldType)
          val r = newReg()
          emit(s"  $r = load $ft, $ft* $gep")
          r

      // ===== Arrays =====

      case TArrayLit(elements, SyslType.ArrayType(elemType, size)) =>
        val elt = llvmType(elemType)
        val arrType = s"[$size x $elt]"
        val alloca = newReg()
        emit(s"  $alloca = alloca $arrType")
        // Zero-init
        val cast = newReg()
        emit(s"  $cast = bitcast $arrType* $alloca to i8*")
        val byteSize = llvmSizeOf(elemType) * size
        emit(s"  call void @llvm.memset.p0i8.i64(i8* $cast, i8 0, i64 $byteSize, i1 false)")
        // Store each element
        for (elem, i) <- elements.zipWithIndex do
          val v = genExpr(elem)
          val gep = newReg()
          emit(s"  $gep = getelementptr $arrType, $arrType* $alloca, i32 0, i32 $i")
          val storeVal = if isAggregate(elemType) then
            val loaded = newReg()
            emit(s"  $loaded = load $elt, $elt* $v")
            loaded
          else v
          emit(s"  store $elt $storeVal, $elt* $gep")
        alloca

      case TArrayDecl(size, SyslType.ArrayType(elemType, _)) =>
        val elt = llvmType(elemType)
        val arrType = s"[$size x $elt]"
        val alloca = newReg()
        emit(s"  $alloca = alloca $arrType")
        val cast = newReg()
        emit(s"  $cast = bitcast $arrType* $alloca to i8*")
        val byteSize = llvmSizeOf(elemType) * size
        emit(s"  call void @llvm.memset.p0i8.i64(i8* $cast, i8 0, i64 $byteSize, i1 false)")
        alloca

      case TIndex(array, index, elemType) =>
        val base = genExpr(array)
        val idx = genExpr(index)
        array.typ match
          case SyslType.ArrayType(elem, size) =>
            val elt = llvmType(elem)
            val arrType = s"[$size x $elt]"
            val gep = newReg()
            emit(s"  $gep = getelementptr $arrType, $arrType* $base, i32 0, i32 $idx")
            if isAggregate(elem) then gep
            else
              val r = newReg()
              emit(s"  $r = load $elt, $elt* $gep")
              r
          case SyslType.PtrType(elem) =>
            val elt = llvmType(elem)
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            val gep = newReg()
            emit(s"  $gep = getelementptr $elt, $elt* $base, i64 $idx64")
            if isAggregate(elem) then gep
            else
              val r = newReg()
              emit(s"  $r = load $elt, $elt* $gep")
              r
          case _ =>
            // Slice or other — use byte arithmetic on data pointer
            val dataPtr = emitSliceDataPtr(base, array.typ)
            val elemSyslType = elemType match
              case t => t
            val elt = llvmType(elemSyslType)
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            val offset = newReg()
            emit(s"  $offset = mul i64 $idx64, ${llvmSizeOf(elemSyslType)}")
            val elemAddr = newReg()
            emit(s"  $elemAddr = getelementptr i8, i8* $dataPtr, i64 $offset")
            val typedPtr = newReg()
            emit(s"  $typedPtr = bitcast i8* $elemAddr to $elt*")
            if isAggregate(elemSyslType) then typedPtr
            else
              val r = newReg()
              emit(s"  $r = load $elt, $elt* $typedPtr")
              r

      case TLen(array, _) =>
        array.typ match
          case SyslType.ArrayType(_, size) => size.toString
          case SyslType.SliceType(_) =>
            val base = genExpr(array)
            val lenGep = newReg()
            emit(s"  $lenGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 1")
            val len32 = newReg()
            emit(s"  $len32 = load i32, i32* $lenGep")
            len32
          case SyslType.StringType =>
            val sp = genExpr(array) // alloca pointer to %struct.string
            val lenGep = newReg()
            emit(s"  $lenGep = getelementptr %struct.string, %struct.string* $sp, i32 0, i32 1")
            val len32 = newReg()
            emit(s"  $len32 = load i32, i32* $lenGep")
            len32
          case _ =>
            emit(s"  ; TODO: len for ${array.typ}")
            "0"

      case TCap(array, _) =>
        array.typ match
          case SyslType.ArrayType(_, size) => size.toString
          case SyslType.SliceType(_) =>
            val base = genExpr(array)
            val capGep = newReg()
            emit(s"  $capGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 2")
            val cap32 = newReg()
            emit(s"  $cap32 = load i32, i32* $capGep")
            cap32
          case _ =>
            emit(s"  ; TODO: cap for ${array.typ}")
            "0"

      // ===== Pointers =====

      case TAddrOf(name, _) =>
        if locals.contains(name) then locals(name).reg
        else s"@$name"

      case TDeref(pointer, typ) =>
        val ptr = genExpr(pointer)
        val pointeeType = pointer.typ match
          case SyslType.PtrType(inner) => inner
          case SyslType.RefType(inner) => inner
          case _ => typ
        if isAggregate(pointeeType) then
          // For aggregates, the pointer IS the address
          val typedPtr = newReg()
          emit(s"  $typedPtr = bitcast i8* $ptr to ${llvmType(pointeeType)}*")
          typedPtr
        else
          val pt = llvmType(pointeeType)
          val typedPtr = newReg()
          emit(s"  $typedPtr = bitcast i8* $ptr to $pt*")
          val r = newReg()
          emit(s"  $r = load $pt, $pt* $typedPtr")
          r

      // ===== Slices =====

      case TSliceExpr(array, low, high, SyslType.SliceType(elemType)) =>
        val base = genExpr(array)
        val elt = llvmType(elemType)
        val elemSize = llvmSizeOf(elemType)
        // Get data pointer and length from source
        val (dataPtr, srcLen) = array.typ match
          case SyslType.ArrayType(_, size) =>
            val cast = newReg()
            emit(s"  $cast = bitcast ${llvmType(array.typ)}* $base to i8*")
            (cast, size.toString)
          case SyslType.SliceType(_) =>
            val ptrGep = newReg()
            emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 0")
            val ptr = newReg()
            emit(s"  $ptr = load i8*, i8** $ptrGep")
            val lenGep = newReg()
            emit(s"  $lenGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 1")
            val len = newReg()
            emit(s"  $len = load i32, i32* $lenGep")
            (ptr, len)
          case _ => (base, "0")
        val lo = low.map(genExpr).getOrElse("0")
        val hi = high.map(genExpr).getOrElse(srcLen)
        // Compute new data pointer = dataPtr + lo * elemSize
        val lo64 = newReg()
        emit(s"  $lo64 = sext i32 $lo to i64")
        val loOffset = newReg()
        emit(s"  $loOffset = mul i64 $lo64, $elemSize")
        val newPtr = newReg()
        emit(s"  $newPtr = getelementptr i8, i8* $dataPtr, i64 $loOffset")
        // new len = hi - lo
        val newLen = newReg()
        emit(s"  $newLen = sub i32 $hi, $lo")
        // Allocate slice struct on stack
        val alloca = newReg()
        emit(s"  $alloca = alloca %struct.slice")
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 0")
        emit(s"  store i8* $newPtr, i8** $ptrGep")
        val lenGep2 = newReg()
        emit(s"  $lenGep2 = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 1")
        emit(s"  store i32 $newLen, i32* $lenGep2")
        val capGep2 = newReg()
        emit(s"  $capGep2 = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 2")
        emit(s"  store i32 $newLen, i32* $capGep2") // cap = len for slicing
        alloca

      case TAppend(slice, elem, SyslType.SliceType(elemType)) =>
        val base = genExpr(slice)
        val elt = llvmType(elemType)
        val elemSize = llvmSizeOf(elemType)
        // Load current slice fields
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 0")
        val curPtr = newReg()
        emit(s"  $curPtr = load i8*, i8** $ptrGep")
        val lenGep = newReg()
        emit(s"  $lenGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 1")
        val curLen = newReg()
        emit(s"  $curLen = load i32, i32* $lenGep")
        val capGep = newReg()
        emit(s"  $capGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 2")
        val curCap = newReg()
        emit(s"  $curCap = load i32, i32* $capGep")
        // Check if we need to grow
        val needGrow = newReg()
        emit(s"  $needGrow = icmp eq i32 $curLen, $curCap")
        val growLabel = newLabel("append_grow")
        val noGrowLabel = newLabel("append_nogrow")
        val contLabel = newLabel("append_cont")
        emit(s"  br i1 $needGrow, label %$growLabel, label %$noGrowLabel")
        // Grow path
        emit(s"$growLabel:")
        val isZero = newReg()
        emit(s"  $isZero = icmp eq i32 $curCap, 0")
        val newCap = newReg()
        val doubled = newReg()
        emit(s"  $doubled = mul i32 $curCap, 2")
        emit(s"  $newCap = select i1 $isZero, i32 1, i32 $doubled")
        val newCap64 = newReg()
        emit(s"  $newCap64 = sext i32 $newCap to i64")
        val allocSize = newReg()
        emit(s"  $allocSize = mul i64 $newCap64, $elemSize")
        val newBuf = newReg()
        emit(s"  $newBuf = call i8* @malloc(i64 $allocSize)")
        // Copy old data
        val curLen64 = newReg()
        emit(s"  $curLen64 = sext i32 $curLen to i64")
        val copySize = newReg()
        emit(s"  $copySize = mul i64 $curLen64, $elemSize")
        val ignored = newReg()
        emit(s"  $ignored = call i8* @memcpy(i8* $newBuf, i8* $curPtr, i64 $copySize)")
        emit(s"  br label %$contLabel")
        // No-grow path
        emit(s"$noGrowLabel:")
        emit(s"  br label %$contLabel")
        // Continue — phi for ptr and cap
        emit(s"$contLabel:")
        val finalPtr = newReg()
        emit(s"  $finalPtr = phi i8* [ $newBuf, %$growLabel ], [ $curPtr, %$noGrowLabel ]")
        val finalCap = newReg()
        emit(s"  $finalCap = phi i32 [ $newCap, %$growLabel ], [ $curCap, %$noGrowLabel ]")
        // Store new element
        val v = genExpr(elem)
        val len64 = newReg()
        emit(s"  $len64 = sext i32 $curLen to i64")
        val elemOffset = newReg()
        emit(s"  $elemOffset = mul i64 $len64, $elemSize")
        val elemAddr = newReg()
        emit(s"  $elemAddr = getelementptr i8, i8* $finalPtr, i64 $elemOffset")
        val typedElemPtr = newReg()
        emit(s"  $typedElemPtr = bitcast i8* $elemAddr to $elt*")
        emit(s"  store $elt $v, $elt* $typedElemPtr")
        // Build result slice
        val newLen = newReg()
        emit(s"  $newLen = add i32 $curLen, 1")
        val result = newReg()
        emit(s"  $result = alloca %struct.slice")
        val rPtrGep = newReg()
        emit(s"  $rPtrGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 0")
        emit(s"  store i8* $finalPtr, i8** $rPtrGep")
        val rLenGep = newReg()
        emit(s"  $rLenGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 1")
        emit(s"  store i32 $newLen, i32* $rLenGep")
        val rCapGep = newReg()
        emit(s"  $rCapGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 2")
        emit(s"  store i32 $finalCap, i32* $rCapGep")
        result

      // ===== Refs (heap allocation) =====

      case TNew(structType, args) =>
        val st = structType
        val dataSize = st.sizeOf
        val totalSize = dataSize + 8 // 8-byte refcount header
        val buf = newReg()
        emit(s"  $buf = call i8* @malloc(i64 $totalSize)")
        // Init refcount = 1
        val rcPtr = newReg()
        emit(s"  $rcPtr = bitcast i8* $buf to i64*")
        emit(s"  store i64 1, i64* $rcPtr")
        // Data pointer = buf + 8
        val dataPtr = newReg()
        emit(s"  $dataPtr = getelementptr i8, i8* $buf, i64 8")
        // Zero-init data
        emit(s"  call void @llvm.memset.p0i8.i64(i8* $dataPtr, i8 0, i64 $dataSize, i1 false)")
        // Store constructor args
        val structLt = llvmType(st)
        val typedData = newReg()
        emit(s"  $typedData = bitcast i8* $dataPtr to $structLt*")
        for (arg, i) <- args.zipWithIndex do
          val v = genExpr(arg)
          val fieldType = llvmType(st.fields(i)._2)
          val gep = newReg()
          emit(s"  $gep = getelementptr $structLt, $structLt* $typedData, i32 0, i32 $i")
          val storeVal = if isAggregate(st.fields(i)._2) then
            val loaded = newReg()
            emit(s"  $loaded = load $fieldType, $fieldType* $v")
            loaded
          else v
          emit(s"  store $fieldType $storeVal, $fieldType* $gep")
        dataPtr // return pointer to data (past refcount)

      case TNewArray(elemType, size) =>
        val elemSize = llvmSizeOf(elemType)
        val sizeVal = genExpr(size)
        val sizeVal64 = newReg()
        emit(s"  $sizeVal64 = sext i32 $sizeVal to i64")
        val dataBytes = newReg()
        emit(s"  $dataBytes = mul i64 $sizeVal64, $elemSize")
        // Total = 16 (refcount 8 + length 4 + cap 4) + data
        val totalSize = newReg()
        emit(s"  $totalSize = add i64 $dataBytes, 16")
        val buf = newReg()
        emit(s"  $buf = call i8* @malloc(i64 $totalSize)")
        // Zero the whole thing
        emit(s"  call void @llvm.memset.p0i8.i64(i8* $buf, i8 0, i64 $totalSize, i1 false)")
        // Refcount = 1 at offset 0
        val rcPtr = newReg()
        emit(s"  $rcPtr = bitcast i8* $buf to i64*")
        emit(s"  store i64 1, i64* $rcPtr")
        // Length at offset 8
        val lenPtr = newReg()
        emit(s"  $lenPtr = getelementptr i8, i8* $buf, i64 8")
        val lenTyped = newReg()
        emit(s"  $lenTyped = bitcast i8* $lenPtr to i32*")
        emit(s"  store i32 $sizeVal, i32* $lenTyped")
        // Cap at offset 12
        val capPtr = newReg()
        emit(s"  $capPtr = getelementptr i8, i8* $buf, i64 12")
        val capTyped = newReg()
        emit(s"  $capTyped = bitcast i8* $capPtr to i32*")
        emit(s"  store i32 $sizeVal, i32* $capTyped")
        // Data pointer at offset 16
        val dataPtr = newReg()
        emit(s"  $dataPtr = getelementptr i8, i8* $buf, i64 16")
        dataPtr

      // ===== Enums =====

      case TEnumConstruct(et, variantIndex, args) =>
        val totalSize = et.sizeOf
        val lt = llvmType(et)
        val alloca = newReg()
        emit(s"  $alloca = alloca $lt")
        // Zero-init
        val cast = newReg()
        emit(s"  $cast = bitcast $lt* $alloca to i8*")
        emit(s"  call void @llvm.memset.p0i8.i64(i8* $cast, i8 0, i64 $totalSize, i1 false)")
        // Store tag at offset 0
        val tagPtr = newReg()
        emit(s"  $tagPtr = bitcast i8* $cast to i32*")
        emit(s"  store i32 $variantIndex, i32* $tagPtr")
        // Store variant fields at data offset
        if args.nonEmpty then
          val dataOffset = et.dataOffset
          val variantFields = et.variants(variantIndex)._2
          var fieldOffset = 0L
          for (arg, i) <- args.zipWithIndex do
            val (_, fieldType) = variantFields(i)
            val align = fieldType.alignOf
            fieldOffset = ((fieldOffset + align - 1) / align) * align
            val v = genExpr(arg)
            val ft = llvmType(fieldType)
            val fieldAddr = newReg()
            emit(s"  $fieldAddr = getelementptr i8, i8* $cast, i64 ${dataOffset + fieldOffset}")
            val typedAddr = newReg()
            emit(s"  $typedAddr = bitcast i8* $fieldAddr to $ft*")
            val storeVal = if isAggregate(fieldType) then
              val loaded = newReg()
              emit(s"  $loaded = load $ft, $ft* $v")
              loaded
            else v
            emit(s"  store $ft $storeVal, $ft* $typedAddr")
            fieldOffset += fieldType.sizeOf
        alloca

      case TMatchExpr(scrutinee, arms, default, typ) =>
        val scrut = genExpr(scrutinee)
        val endLabel = newLabel("match_end")
        val resultAlloca = newReg()
        val resultLt = llvmType(typ)
        if resultLt != "void" then
          emit(s"  $resultAlloca = alloca $resultLt")
        // For each arm, generate: check pattern, if match -> execute body, store result, br to end
        val armLabels = arms.indices.map(_ => newLabel("match_arm"))
        val nextLabels = arms.indices.map(_ => newLabel("match_next"))
        val defaultLabel = newLabel("match_default")
        // Branch to first arm check
        emit(s"  br label %${if arms.nonEmpty then nextLabels(0) else defaultLabel}")
        for (arm, i) <- arms.zipWithIndex do
          emit(s"${nextLabels(i)}:")
          // Check patterns (OR — any pattern matching is enough)
          val matched = arm.patterns.map { pat =>
            pat match
              case TValuePattern(expr) =>
                val patVal = genExpr(expr)
                val cmp = newReg()
                val st = exprType(scrutinee)
                emit(s"  $cmp = icmp eq $st $scrut, $patVal")
                cmp
              case TRangePattern(low, high) =>
                val lo = genExpr(low)
                val hi = genExpr(high)
                val st = exprType(scrutinee)
                val cmpLo = newReg()
                val cmpHi = newReg()
                val both = newReg()
                emit(s"  $cmpLo = icmp sge $st $scrut, $lo")
                emit(s"  $cmpHi = icmp sle $st $scrut, $hi")
                emit(s"  $both = and i1 $cmpLo, $cmpHi")
                both
              case TWildcard =>
                "true" // always matches
              case TVariantPattern(et, variantIdx, bindings, fieldTypes) =>
                // Load tag from scrutinee
                val scrutCast = newReg()
                emit(s"  $scrutCast = bitcast ${exprType(scrutinee)}* $scrut to i32*")
                val tag = newReg()
                emit(s"  $tag = load i32, i32* $scrutCast")
                val cmp = newReg()
                emit(s"  $cmp = icmp eq i32 $tag, $variantIdx")
                cmp
              case _ =>
                emit(s"  ; TODO: match pattern ${pat.getClass.getSimpleName}")
                "true"
          }
          // OR all pattern results
          val finalCond = if matched.length == 1 then matched.head
          else matched.reduce { (a, b) =>
            if a == "true" || b == "true" then "true"
            else
              val r = newReg()
              emit(s"  $r = or i1 $a, $b")
              r
          }
          // Check guard if present
          val guardedCond = arm.guard match
            case Some(guardExpr) if finalCond != "true" =>
              // Only eval guard if pattern matched
              val guardLabel = newLabel("match_guard")
              val afterGuard = newLabel("match_after_guard")
              emit(s"  br i1 $finalCond, label %$guardLabel, label %${if i + 1 < arms.length then nextLabels(i + 1) else defaultLabel}")
              emit(s"$guardLabel:")
              val g = genExpr(guardExpr)
              val gBool = newReg()
              emit(s"  $gBool = icmp ne ${exprType(guardExpr)} $g, 0")
              gBool
            case Some(guardExpr) =>
              val g = genExpr(guardExpr)
              val gBool = newReg()
              emit(s"  $gBool = icmp ne ${exprType(guardExpr)} $g, 0")
              gBool
            case None => finalCond
          // Branch
          if guardedCond == "true" then
            emit(s"  br label %${armLabels(i)}")
          else
            emit(s"  br i1 $guardedCond, label %${armLabels(i)}, label %${if i + 1 < arms.length then nextLabels(i + 1) else defaultLabel}")
          // Arm body
          emit(s"${armLabels(i)}:")
          // Bind variant fields if this is a variant pattern
          arm.patterns.headOption match
            case Some(TVariantPattern(et, variantIdx, bindings, fieldTypes)) =>
              val dataOffset = et.dataOffset
              val scrutCast2 = newReg()
              emit(s"  $scrutCast2 = bitcast ${exprType(scrutinee)}* $scrut to i8*")
              var fOffset = 0L
              for (binding, j) <- bindings.zipWithIndex do
                val ft = fieldTypes(j)
                val align = ft.alignOf
                fOffset = ((fOffset + align - 1) / align) * align
                binding.foreach { bName =>
                  val flt = llvmType(ft)
                  val fAddr = newReg()
                  emit(s"  $fAddr = getelementptr i8, i8* $scrutCast2, i64 ${dataOffset + fOffset}")
                  val typedFAddr = newReg()
                  emit(s"  $typedFAddr = bitcast i8* $fAddr to $flt*")
                  if isAggregate(ft) then
                    locals(bName) = LocalVar(bName, typedFAddr, ft)
                  else
                    val alloc = newReg()
                    emit(s"  $alloc = alloca $flt")
                    val loaded = newReg()
                    emit(s"  $loaded = load $flt, $flt* $typedFAddr")
                    emit(s"  store $flt $loaded, $flt* $alloc")
                    locals(bName) = LocalVar(bName, alloc, ft)
                }
                fOffset += ft.sizeOf
            case _ => // no bindings needed
          val savedHR = hasReturned
          hasReturned = false
          if arm.body.nonEmpty then
            for s <- arm.body.init do if !hasReturned then genStmt(s)
            if !hasReturned then
              arm.body.last match
                case TExprStmt(e) =>
                  val v = genExpr(e)
                  if resultLt != "void" then emit(s"  store $resultLt $v, $resultLt* $resultAlloca")
                case other => genStmt(other)
          if !hasReturned then emit(s"  br label %$endLabel")
          hasReturned = savedHR
        // Default
        emit(s"$defaultLabel:")
        default match
          case Some(stmts) if stmts.nonEmpty =>
            val savedHR = hasReturned
            hasReturned = false
            for s <- stmts.init do if !hasReturned then genStmt(s)
            if !hasReturned then
              stmts.last match
                case TExprStmt(e) =>
                  val v = genExpr(e)
                  if resultLt != "void" then emit(s"  store $resultLt $v, $resultLt* $resultAlloca")
                case other => genStmt(other)
            if !hasReturned then emit(s"  br label %$endLabel")
            hasReturned = savedHR
          case _ =>
            if resultLt != "void" then emit(s"  store $resultLt 0, $resultLt* $resultAlloca")
            emit(s"  br label %$endLabel")
        emit(s"$endLabel:")
        if resultLt != "void" then
          val r = newReg()
          emit(s"  $r = load $resultLt, $resultLt* $resultAlloca")
          r
        else "0"

      // ===== Function pointers and closures =====

      case TFuncRef(name, typ) =>
        // Build a %struct.closure { wrapper_func_ptr, null }
        val wrapperName = funcWrappers.getOrElseUpdate(name, {
          val wn = s"__wrap_$name"
          typ match
            case SyslType.FuncType(params, retType) =>
              pendingWrappers += ((wn, name, params, retType))
            case _ =>
          wn
        })
        val alloca = newReg()
        emit(s"  $alloca = alloca %struct.closure")
        // Store func ptr
        val fpGep = newReg()
        emit(s"  $fpGep = getelementptr %struct.closure, %struct.closure* $alloca, i32 0, i32 0")
        val fpCast = newReg()
        typ match
          case SyslType.FuncType(params, retType) =>
            val paramStr = ("i8*" +: params.map(llvmType)).mkString(", ")
            emit(s"  $fpCast = bitcast ${llvmType(retType)} ($paramStr)* @$wrapperName to i8*")
          case _ =>
            emit(s"  $fpCast = bitcast i8* null to i8*")
        emit(s"  store i8* $fpCast, i8** $fpGep")
        // Store null env
        val envGep = newReg()
        emit(s"  $envGep = getelementptr %struct.closure, %struct.closure* $alloca, i32 0, i32 1")
        emit(s"  store i8* null, i8** $envGep")
        alloca

      case c: TClosure =>
        // Generate closure function (deferred)
        closureCounter += 1
        val closureName = s"__closure_$closureCounter"
        pendingClosures += ((closureName, c))
        // Build environment on heap
        val envSize = c.captures.map((_, t) => llvmSizeOf(t)).sum
        val envPtr = if c.captures.nonEmpty then
          val ep = newReg()
          emit(s"  $ep = call i8* @malloc(i64 $envSize)")
          // Store captured values into environment
          var offset = 0L
          for (capName, capType) <- c.captures do
            val lt = llvmType(capType)
            val v = if locals.contains(capName) then
              val local = locals(capName)
              if isAggregate(local.typ) then local.reg
              else
                val r = newReg()
                emit(s"  $r = load $lt, $lt* ${local.reg}")
                r
            else
              val r = newReg()
              emit(s"  $r = load $lt, $lt* @$capName")
              r
            val envFieldPtr = newReg()
            emit(s"  $envFieldPtr = getelementptr i8, i8* $ep, i64 $offset")
            val typedEnvPtr = newReg()
            emit(s"  $typedEnvPtr = bitcast i8* $envFieldPtr to $lt*")
            if isAggregate(capType) then
              val loaded = newReg()
              emit(s"  $loaded = load $lt, $lt* $v")
              emit(s"  store $lt $loaded, $lt* $typedEnvPtr")
            else
              emit(s"  store $lt $v, $lt* $typedEnvPtr")
            offset += llvmSizeOf(capType)
          ep
        else "null"
        // Build %struct.closure
        val alloca = newReg()
        emit(s"  $alloca = alloca %struct.closure")
        val fpGep = newReg()
        emit(s"  $fpGep = getelementptr %struct.closure, %struct.closure* $alloca, i32 0, i32 0")
        val retLt = llvmType(c.returnType)
        val paramStr = ("i8*" +: c.params.map(p => llvmType(p.typ))).mkString(", ")
        val fpCast = newReg()
        emit(s"  $fpCast = bitcast $retLt ($paramStr)* @$closureName to i8*")
        emit(s"  store i8* $fpCast, i8** $fpGep")
        val envGep = newReg()
        emit(s"  $envGep = getelementptr %struct.closure, %struct.closure* $alloca, i32 0, i32 1")
        emit(s"  store i8* $envPtr, i8** $envGep")
        alloca

      case TIndirectCall(callee, args, typ) =>
        // callee is a %struct.closure — extract func_ptr and env_ptr
        val closurePtr = genExpr(callee) // returns alloca pointer (aggregate)
        val fpGep = newReg()
        emit(s"  $fpGep = getelementptr %struct.closure, %struct.closure* $closurePtr, i32 0, i32 0")
        val fpRaw = newReg()
        emit(s"  $fpRaw = load i8*, i8** $fpGep")
        val envGep = newReg()
        emit(s"  $envGep = getelementptr %struct.closure, %struct.closure* $closurePtr, i32 0, i32 1")
        val envPtr = newReg()
        emit(s"  $envPtr = load i8*, i8** $envGep")
        callee.typ match
          case SyslType.FuncType(params, retType) =>
            val paramTypes = params.map(llvmType)
            val argVals = args.zip(paramTypes).map { (a, pt) =>
              val v = genExpr(a)
              if isAggregate(a.typ) then
                val loaded = newReg()
                emit(s"  $loaded = load $pt, $pt* $v")
                (loaded, pt)
              else (v, pt)
            }
            // All indirect calls pass env as first arg
            val allArgStr = s"i8* $envPtr" + (if argVals.nonEmpty then ", " + argVals.map((v, vt) => s"$vt $v").mkString(", ") else "")
            val retLt = llvmType(retType)
            val allParamStr = ("i8*" +: paramTypes).mkString(", ")
            val typedFp = newReg()
            emit(s"  $typedFp = bitcast i8* $fpRaw to $retLt ($allParamStr)*")
            if retLt == "void" then
              emit(s"  call void $typedFp($allArgStr)")
              "0"
            else
              val result = newReg()
              emit(s"  $result = call $retLt $typedFp($allArgStr)")
              if isAggregate(typ) then
                val ra = newReg()
                emit(s"  $ra = alloca $retLt")
                emit(s"  store $retLt $result, $retLt* $ra")
                ra
              else result
          case _ =>
            emit(s"  ; TODO: indirect call on non-function type")
            "0"

      case TStr(inner) =>
        inner.typ match
          case SyslType.StringType =>
            // String → string: identity (already a fat string)
            genExpr(inner)
          case SyslType.BoolType =>
            val v = genExpr(inner)
            // bool → "true" or "false" via select
            val cmp = newReg()
            emit(s"  $cmp = icmp ne i8 $v, 0")
            val truePtr = newReg()
            val falsePtr = newReg()
            emit(s"  $truePtr = getelementptr [5 x i8], [5 x i8]* @.str.true, i32 0, i32 0")
            emit(s"  $falsePtr = getelementptr [6 x i8], [6 x i8]* @.str.false, i32 0, i32 0")
            val selPtr = newReg()
            emit(s"  $selPtr = select i1 $cmp, i8* $truePtr, i8* $falsePtr")
            val selLen = newReg()
            emit(s"  $selLen = select i1 $cmp, i32 4, i32 5")
            emitMakeString(selPtr, selLen)
          case SyslType.DoubleType =>
            val v = genExpr(inner)
            // double → string via snprintf with %g
            emitSnprintfToString("@.fmt_f", 3, s"double $v")
          case t if t.isIntegral =>
            val v = genExpr(inner)
            val vt = exprType(inner)
            // int → string via snprintf
            val (fmtName, fmtLen, arg) = vt match
              case "i64" => ("@.fmt_ld", 4, s"i64 $v")
              case "i32" => ("@.fmt_d", 3, s"i32 $v")
              case _ =>
                // Extend narrower types to i32 for %d
                val ext = newReg()
                if t.isSigned then emit(s"  $ext = sext $vt $v to i32")
                else emit(s"  $ext = zext $vt $v to i32")
                ("@.fmt_d", 3, s"i32 $ext")
            emitSnprintfToString(fmtName, fmtLen, arg)
          case _ =>
            emit(s"  ; TODO: str() for ${inner.typ}")
            val (label, byteLen) = internString("???")
            val dataPtr = newReg()
            emit(s"  $dataPtr = getelementptr [$byteLen x i8], [$byteLen x i8]* $label, i32 0, i32 0")
            emitMakeString(dataPtr, s"${byteLen - 1}")

      case TCast(inner, targetType) =>
        val v = genExpr(inner)
        val fromLt = llvmType(inner.typ)
        val toLt = llvmType(targetType)
        if fromLt == toLt then v
        else
          val result = newReg()
          (inner.typ, targetType) match
            case (_: SyslType.IntType, SyslType.DoubleType) | (_: SyslType.UIntType, SyslType.DoubleType) =>
              if inner.typ.isSigned then emit(s"  $result = sitofp $fromLt $v to double")
              else emit(s"  $result = uitofp $fromLt $v to double")
            case (SyslType.DoubleType, _: SyslType.IntType) =>
              emit(s"  $result = fptosi double $v to $toLt")
            case (SyslType.DoubleType, _: SyslType.UIntType) =>
              emit(s"  $result = fptoui double $v to $toLt")
            case _ if inner.typ.isIntegral && targetType.isIntegral =>
              val fromWidth = fromLt.stripPrefix("i").toInt
              val toWidth = toLt.stripPrefix("i").toInt
              if toWidth > fromWidth then
                if inner.typ.isSigned then emit(s"  $result = sext $fromLt $v to $toLt")
                else emit(s"  $result = zext $fromLt $v to $toLt")
              else
                emit(s"  $result = trunc $fromLt $v to $toLt")
            case _ =>
              emit(s"  $result = bitcast $fromLt $v to $toLt")
          result

      case _ =>
        emit(s"  ; TODO: ${expr.getClass.getSimpleName}")
        "0"

  // Get the address (alloca pointer) for a struct-typed expression
  private def genStructAddr(obj: TExpr): String =
    obj match
      case TVarRef(name, _) =>
        if locals.contains(name) then locals(name).reg
        else s"@$name"
      case TFieldAccess(innerObj, fieldIndex, _) =>
        val st = innerObj.typ.asInstanceOf[SyslType.StructType]
        val structLt = llvmType(innerObj.typ)
        val addr = genStructAddr(innerObj)
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        gep
      case _ =>
        genExpr(obj) // for other expressions, genExpr returns pointer for struct types

  // Emit deferred statements in LIFO order (does not pop — they may run again on another return path)
  private def emitDefers(): Unit =
    for stmt <- deferStack do genStmt(stmt)

  // Extract the data pointer from a slice or ref-to-slice
  private def emitSliceDataPtr(base: String, typ: SyslType): String =
    typ match
      case SyslType.SliceType(_) =>
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 0")
        val ptr = newReg()
        emit(s"  $ptr = load i8*, i8** $ptrGep")
        ptr
      case SyslType.RefType(SyslType.SliceType(_)) =>
        base // ref-to-slice: base IS the data pointer
      case SyslType.StringType =>
        // Fat string: extract ptr field
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $base, i32 0, i32 0")
        val ptr = newReg()
        emit(s"  $ptr = load i8*, i8** $ptrGep")
        ptr
      case _ =>
        base

  /** Write a fat string to stdout via write(2). sp is alloca pointer to %struct.string. */
  private def emitWriteString(sp: String): Unit =
    val ptrGep = newReg()
    emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $sp, i32 0, i32 0")
    val ptr = newReg()
    emit(s"  $ptr = load i8*, i8** $ptrGep")
    val lenGep = newReg()
    emit(s"  $lenGep = getelementptr %struct.string, %struct.string* $sp, i32 0, i32 1")
    val len32 = newReg()
    emit(s"  $len32 = load i32, i32* $lenGep")
    val len64 = newReg()
    emit(s"  $len64 = sext i32 $len32 to i64")
    val ignored = newReg()
    emit(s"  $ignored = call i64 @write(i32 1, i8* $ptr, i64 $len64)")

  /** Build a %struct.string from an i8* pointer and i32 length. Returns alloca pointer. */
  private def emitMakeString(ptr: String, len: String): String =
    val alloca = newReg()
    emit(s"  $alloca = alloca %struct.string")
    val ptrGep = newReg()
    emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 0")
    emit(s"  store i8* $ptr, i8** $ptrGep")
    val lenGep = newReg()
    emit(s"  $lenGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 1")
    emit(s"  store i32 $len, i32* $lenGep")
    alloca

  /** Emit snprintf-based conversion: measure, malloc, format. Returns alloca pointer to %struct.string. */
  private def emitSnprintfToString(fmtName: String, fmtLen: Int, typedArg: String): String =
    val fmtPtr = newReg()
    emit(s"  $fmtPtr = getelementptr [$fmtLen x i8], [$fmtLen x i8]* $fmtName, i32 0, i32 0")
    val len = newReg()
    emit(s"  $len = call i32 (i8*, i64, i8*, ...) @snprintf(i8* null, i64 0, i8* $fmtPtr, $typedArg)")
    val len64 = newReg()
    emit(s"  $len64 = sext i32 $len to i64")
    val size = newReg()
    emit(s"  $size = add i64 $len64, 1")
    val buf = newReg()
    emit(s"  $buf = call i8* @malloc(i64 $size)")
    val ignored = newReg()
    emit(s"  $ignored = call i32 (i8*, i64, i8*, ...) @snprintf(i8* $buf, i64 $size, i8* $fmtPtr, $typedArg)")
    emitMakeString(buf, len)

  // Emit sext only when fromType != toType; return the (possibly cast) register
  private def emitSextIfNeeded(value: String, fromType: String, toType: String): String =
    if fromType == toType then value
    else
      val cast = newReg()
      emit(s"  $cast = sext $fromType $value to $toType")
      cast

  private def llvmType(t: SyslType): String = t match
    case SyslType.IntType(w) => s"i$w"
    case SyslType.UIntType(w) => s"i$w"
    case SyslType.BoolType => "i8"
    case SyslType.DoubleType => "double"
    case SyslType.VoidType => "void"
    case SyslType.StringType => "%struct.string"
    case SyslType.StructType(name, _) => s"%struct.$name"
    case SyslType.ArrayType(elem, size) => s"[$size x ${llvmType(elem)}]"
    case et: SyslType.EnumType => s"[${et.sizeOf} x i8]" // opaque byte array for tagged union
    case SyslType.SliceType(_) => "%struct.slice"
    case SyslType.PtrType(_) => "i8*"
    case SyslType.RefType(_) => "i8*"
    case SyslType.FuncType(_, _) => "%struct.closure"
    case _ => "i64"

  // LLVM-side size in bytes (may differ from Sysl's sizeOf for types like strings)
  private def llvmSizeOf(t: SyslType): Long = t match
    case SyslType.StringType => 16  // {i8*, i32} — matches Sysl's sizeOf
    case SyslType.PtrType(_) => 8
    case SyslType.RefType(_) => 8
    case SyslType.FuncType(_, _) => 16  // {i8*, i8*}
    case SyslType.BoolType => 1
    case SyslType.SliceType(_) => 16  // {i8*, i32, i32}
    case SyslType.StructType(_, fields) =>
      // Use LLVM's struct layout (simplified — no padding calc, just sum field sizes aligned)
      fields.map(_._2).map(llvmSizeOf).sum // simplified
    case SyslType.ArrayType(elem, size) => llvmSizeOf(elem) * size
    case other => other.sizeOf

  // Types that are passed by pointer (alloca) rather than by value
  private def isAggregate(t: SyslType): Boolean = t match
    case _: SyslType.StructType | _: SyslType.ArrayType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | SyslType.StringType => true
    case _ => false

  // ===== Refcounting helpers =====

  private def isRef(t: SyslType): Boolean = t match
    case _: SyslType.RefType => true
    case _ => false

  /** Header offset: bytes from data pointer back to refcount field. */
  private def refHeaderOffset(t: SyslType): Int = t match
    case SyslType.RefType(SyslType.SliceType(_)) => 16  // refcount(8) + len(4) + cap(4)
    case SyslType.RefType(_) => 8                        // refcount(8) only
    case _ => 8

  /** Emit inline refcount increment. ptr is the data pointer (past header). */
  private def emitRefIncr(ptr: String, headerOffset: Int): Unit =
    val skip = newLabel("rc_skip")
    val doIncr = newLabel("rc_incr")
    // Null check
    val isNull = newReg()
    emit(s"  $isNull = icmp eq i8* $ptr, null")
    emit(s"  br i1 $isNull, label %$skip, label %$doIncr")
    emit(s"$doIncr:")
    // Get refcount pointer: ptr - headerOffset
    val base = newReg()
    emit(s"  $base = getelementptr i8, i8* $ptr, i64 -$headerOffset")
    val rcPtr = newReg()
    emit(s"  $rcPtr = bitcast i8* $base to i64*")
    val rc = newReg()
    emit(s"  $rc = load i64, i64* $rcPtr")
    // Check immortal (-1)
    val isImmortal = newReg()
    emit(s"  $isImmortal = icmp eq i64 $rc, -1")
    val doStore = newLabel("rc_store")
    emit(s"  br i1 $isImmortal, label %$skip, label %$doStore")
    emit(s"$doStore:")
    val newRc = newReg()
    emit(s"  $newRc = add i64 $rc, 1")
    emit(s"  store i64 $newRc, i64* $rcPtr")
    emit(s"  br label %$skip")
    emit(s"$skip:")

  /** Look up deinit function name for a RefType's inner type. */
  private def deinitFor(typ: SyslType): Option[String] = typ match
    case SyslType.RefType(SyslType.StructType(name, _)) => deinitFunctions.get(name)
    case _ => None

  /** Emit inline refcount decrement + free when count reaches 0.
    * ptr is the data pointer (past header).
    * deinit is an optional function to call before freeing. */
  private def emitRefDecr(ptr: String, headerOffset: Int, deinit: Option[String] = None): Unit =
    val skip = newLabel("rcd_skip")
    val doDecr = newLabel("rcd_decr")
    // Null check
    val isNull = newReg()
    emit(s"  $isNull = icmp eq i8* $ptr, null")
    emit(s"  br i1 $isNull, label %$skip, label %$doDecr")
    emit(s"$doDecr:")
    // Get refcount pointer
    val base = newReg()
    emit(s"  $base = getelementptr i8, i8* $ptr, i64 -$headerOffset")
    val rcPtr = newReg()
    emit(s"  $rcPtr = bitcast i8* $base to i64*")
    val rc = newReg()
    emit(s"  $rc = load i64, i64* $rcPtr")
    // Check immortal
    val isImmortal = newReg()
    emit(s"  $isImmortal = icmp eq i64 $rc, -1")
    val doStore = newLabel("rcd_store")
    emit(s"  br i1 $isImmortal, label %$skip, label %$doStore")
    emit(s"$doStore:")
    val newRc = newReg()
    emit(s"  $newRc = sub i64 $rc, 1")
    emit(s"  store i64 $newRc, i64* $rcPtr")
    val isZero = newReg()
    emit(s"  $isZero = icmp eq i64 $newRc, 0")
    val doFree = newLabel("rcd_free")
    emit(s"  br i1 $isZero, label %$doFree, label %$skip")
    emit(s"$doFree:")
    // Set refcount to IMMORTAL (-1) to prevent re-entrant deinit
    emit(s"  store i64 -1, i64* $rcPtr")
    // Call deinit if present (passes data pointer, not base)
    deinit.foreach { name =>
      emit(s"  call i32 @$name(i8* $ptr)")
    }
    // Free the base allocation
    emit(s"  call void @free(i8* $base)")
    emit(s"  br label %$skip")
    emit(s"$skip:")

  /** Decrement refcounts for all ref-typed locals before function exit. */
  private def emitReleaseRefs(): Unit =
    val hasRefs = locals.exists((_, l) => isRef(l.typ))
    if hasRefs then
      // Flush stdout before deinit functions might write to it
      val flushIgnored = newReg()
      emit(s"  $flushIgnored = call i32 @fflush(i8* null)")
    for (_, local) <- locals if isRef(local.typ) do
      val hoff = refHeaderOffset(local.typ)
      val ptr = newReg()
      emit(s"  $ptr = load i8*, i8** ${local.reg}")
      emitRefDecr(ptr, hoff, deinitFor(local.typ))

  /** Check if an expression is a TNew/TNewArray (already owns the ref, no incr needed). */
  private def isOwnedNew(expr: TExpr): Boolean = expr match
    case _: TNew | _: TNewArray => true
    case _ => false

  /** Convert a TExpr to an LLVM constant initializer for global variables. */
  private def constValue(expr: TExpr, typ: SyslType): String = expr match
    case TIntLit(v, _) => v.toString
    case TFloatLit(v, _) =>
      val bits = java.lang.Double.doubleToRawLongBits(v)
      s"0x${bits.toHexString.toUpperCase}"
    case TBoolLit(v, _) => if v then "1" else "0"
    case TStringLit(s, _) =>
      val (label, byteLen) = internString(s)
      val strLen = byteLen - 1
      s"{ i8* getelementptr inbounds ([$byteLen x i8], [$byteLen x i8]* $label, i32 0, i32 0), i32 $strLen }"
    case _ =>
      typ match
        case SyslType.StringType => "{ i8* null, i32 0 }"
        case _ => "0"

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
