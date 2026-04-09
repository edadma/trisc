package io.github.edadma.trisc

import scala.collection.mutable

class SyslLLVMCodegen:
  private val out = new StringBuilder
  private val stringConstants = new mutable.LinkedHashMap[String, (String, Int)] // value -> (label, byte length including null)
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

  def generate(program: TProgram): String =
    out.clear()
    stringConstants.clear()
    stringCounter = 0

    // Generate functions into a buffer so string constants are collected first
    out.clear()
    for decl <- program.decls do
      decl match
        case _: TModuleDecl => // skip
        case _: TImportDecl => // skip
        case _: TExternFuncDecl => // skip
        case _: TExternVarDecl => // skip
        case _: TStructDecl => // type only
        case _: TEnumDecl => // type only
        case _: TTypeAliasDecl => // type only
        case _: TInterfaceDecl => // type only
        case f: TFunDecl => genFunction(f)
        case TVarDecl(name, typ, init, _) =>
          (typ, init) match
            case (SyslType.ArrayType(SyslType.IntType(8) | SyslType.UIntType(8), size), TStringLit(s, _)) =>
              // String literal initializer for byte array: emit as c"..." constant
              val bytes = s.getBytes("UTF-8")
              val escaped = bytes.map(b => f"\\${b & 0xff}%02X").mkString
              val padded = if bytes.length < size then escaped + ("\\00" * (size - bytes.length)) else escaped
              emit(s"""@$name = global [$size x i8] c"$padded"""")
            case (SyslType.ArrayType(_, size), _) =>
              emit(s"@$name = global ${llvmType(typ)} zeroinitializer")
            case _ =>
              emit(s"@$name = global ${llvmType(typ)} 0")
    emit("")
    val funcCode = out.toString

    // Now build final output with string constants at the top
    out.clear()

    // Declare external C functions
    emit("declare i32 @putchar(i32)")
    emit("declare i32 @printf(i8*, ...)")
    emit("declare i32 @puts(i8*)")
    emit("declare i32 @snprintf(i8*, i64, i8*, ...)")
    emit("declare i8* @malloc(i64)")
    emit("declare i64 @strlen(i8*)")
    emit("declare i8* @memcpy(i8*, i8*, i64)")
    emit("")

    // Format strings for print/println builtins
    emit("""@.fmt_d = private unnamed_addr constant [3 x i8] c"%d\00"""")
    emit("""@.fmt_dn = private unnamed_addr constant [4 x i8] c"%d\0A\00"""")
    emit("""@.fmt_f = private unnamed_addr constant [3 x i8] c"%g\00"""")
    emit("""@.fmt_fn = private unnamed_addr constant [4 x i8] c"%g\0A\00"""")
    emit("""@.fmt_s = private unnamed_addr constant [3 x i8] c"%s\00"""")
    emit("""@.fmt_sn = private unnamed_addr constant [4 x i8] c"%s\0A\00"""")
    emit("""@.fmt_ld = private unnamed_addr constant [4 x i8] c"%ld\00"""")
    emit("""@.str.true = private unnamed_addr constant [5 x i8] c"true\00"""")
    emit("""@.str.false = private unnamed_addr constant [6 x i8] c"false\00"""")
    emit("")

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

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        val result = genExpr(expr)
        val rt = exprType(expr)
        val finalVal = emitSextIfNeeded(result, rt, retType)
        emit(s"  ret $retType $finalVal")
      case TBlockBody(stmts) =>
        genBlock(stmts, retType)

    emit("}")
    emit("")
    locals = null
    currentFunction = null

  private def genBlock(stmts: List[TStmt], retType: String): Unit =
    if stmts.nonEmpty then
      for stmt <- stmts.init do
        if !hasReturned then genStmt(stmt)
      if !hasReturned then
        stmts.last match
          case TExprStmt(expr) =>
            val result = genExpr(expr)
            val rt = exprType(expr)
            val finalVal = emitSextIfNeeded(result, rt, retType)
            emit(s"  ret $retType $finalVal")
            hasReturned = true
          case other =>
            genStmt(other)
            if !hasReturned then
              emit(s"  ret $retType 0")
              hasReturned = true
    else
      emit(s"  ret $retType 0")
      hasReturned = true

  // Get LLVM type string for an expression based on its type
  private def exprType(expr: TExpr): String = llvmType(expr.typ)

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, typ, init) =>
        val lt = llvmType(typ)
        val alloca = newReg()
        emit(s"  $alloca = alloca $lt")
        val value = genExpr(init)
        val vt = exprType(init)
        val finalVal = emitSextIfNeeded(value, vt, lt)
        emit(s"  store $lt $finalVal, $lt* $alloca")
        locals(name) = LocalVar(name, alloca, typ)

      case TAssignStmt(target, value) =>
        val v = genExpr(value)
        if locals.contains(target) then
          val local = locals(target)
          val lt = llvmType(local.typ)
          val vt = exprType(value)
          val finalVal = emitSextIfNeeded(v, vt, lt)
          emit(s"  store $lt $finalVal, $lt* ${local.reg}")
        else
          val lt = exprType(value)
          val alloca = newReg()
          emit(s"  $alloca = alloca $lt")
          emit(s"  store $lt $v, $lt* $alloca")
          locals(target) = LocalVar(target, alloca, value.typ)

      case TReturnStmt(Some(value)) =>
        val v = genExpr(value)
        val retType = if currentFunction.name == "main" then "i64" else llvmType(currentFunction.returnType)
        val vt = exprType(value)
        val finalVal = emitSextIfNeeded(v, vt, retType)
        emit(s"  ret $retType $finalVal")
        hasReturned = true

      case TReturnStmt(None) =>
        emit("  ret void")
        hasReturned = true

      case TExprStmt(expr) =>
        genExpr(expr)

      case TWhileStmt(cond, body) =>
        val condLabel = newLabel("while_cond")
        val bodyLabel = newLabel("while_body")
        val endLabel = newLabel("while_end")
        emit(s"  br label %$condLabel")
        emit(s"$condLabel:")
        val c = genExpr(cond)
        val ct = exprType(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne $ct $c, 0")
        emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emit(s"$bodyLabel:")
        for s <- body do genStmt(s)
        if !hasReturned then emit(s"  br label %$condLabel")
        emit(s"$endLabel:")

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
        val r = newReg()
        emit(s"  $r = getelementptr [$byteLen x i8], [$byteLen x i8]* $label, i32 0, i32 0")
        r

      case TVarRef(name, _) =>
        if locals.contains(name) then
          val local = locals(name)
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
        val l = genExpr(left)
        val r = genExpr(right)
        val len1 = newReg()
        val len2 = newReg()
        val total = newReg()
        val size = newReg()
        val buf = newReg()
        val dest = newReg()
        val end = newReg()
        emit(s"  $len1 = call i64 @strlen(i8* $l)")
        emit(s"  $len2 = call i64 @strlen(i8* $r)")
        emit(s"  $total = add i64 $len1, $len2")
        emit(s"  $size = add i64 $total, 1")
        emit(s"  $buf = call i8* @malloc(i64 $size)")
        val cp1 = newReg()
        emit(s"  $cp1 = call i8* @memcpy(i8* $buf, i8* $l, i64 $len1)")
        emit(s"  $dest = getelementptr i8, i8* $buf, i64 $len1")
        val cp2 = newReg()
        emit(s"  $cp2 = call i8* @memcpy(i8* $dest, i8* $r, i64 $len2)")
        emit(s"  $end = getelementptr i8, i8* $buf, i64 $total")
        emit(s"  store i8 0, i8* $end")
        buf

      case TBinary(left, op, right, _) =>
        val l = genExpr(left)
        val r = genExpr(right)
        val lt = exprType(left)
        val isFloat = left.typ == SyslType.DoubleType
        val result = newReg()
        op match
          case "+" => emit(s"  $result = ${if isFloat then "fadd" else "add"} $lt $l, $r")
          case "-" => emit(s"  $result = ${if isFloat then "fsub" else "sub"} $lt $l, $r")
          case "*" => emit(s"  $result = ${if isFloat then "fmul" else "mul"} $lt $l, $r")
          case "/" => emit(s"  $result = ${if isFloat then "fdiv" else "sdiv"} $lt $l, $r")
          case "%" => emit(s"  $result = ${if isFloat then "frem" else "srem"} $lt $l, $r")
          case "&"  => emit(s"  $result = and $lt $l, $r")
          case "|"  => emit(s"  $result = or $lt $l, $r")
          case "^"  => emit(s"  $result = xor $lt $l, $r")
          case "<<" => emit(s"  $result = shl $lt $l, $r")
          case ">>" => emit(s"  $result = ashr $lt $l, $r")
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
            else emit(s"  $cmp = icmp slt $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case ">" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp ogt $lt $l, $r")
            else emit(s"  $cmp = icmp sgt $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case "<=" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp ole $lt $l, $r")
            else emit(s"  $cmp = icmp sle $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $t")
          case ">=" =>
            val cmp = newReg()
            if isFloat then emit(s"  $cmp = fcmp oge $lt $l, $r")
            else emit(s"  $cmp = icmp sge $lt $l, $r")
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
        val v = genExpr(arg)
        val vt = exprType(arg)
        val (fmtName, fmtLen) = arg.typ match
          case SyslType.DoubleType  => ("@.fmt_f", 3)
          case SyslType.StringType  => ("@.fmt_s", 3)
          case _                    => ("@.fmt_d", 3)
        val result = newReg()
        emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([$fmtLen x i8], [$fmtLen x i8]* $fmtName, i32 0, i32 0), $vt $v)""")
        "0"

      case TCall("println", List(arg), _) =>
        val v = genExpr(arg)
        val vt = exprType(arg)
        val (fmtName, fmtLen) = arg.typ match
          case SyslType.DoubleType  => ("@.fmt_fn", 4)
          case SyslType.StringType  => ("@.fmt_sn", 4)
          case _                    => ("@.fmt_dn", 4)
        val result = newReg()
        emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([$fmtLen x i8], [$fmtLen x i8]* $fmtName, i32 0, i32 0), $vt $v)""")
        "0"

      case TCall("puts", List(arg), _) =>
        val v = genExpr(arg)
        val result = newReg()
        emit(s"  $result = call i32 @puts(i8* $v)")
        "0"

      case TCall(name, args, _) =>
        val argVals = args.map(a => (genExpr(a), exprType(a)))
        val argStr = argVals.map((v, vt) => s"$vt $v").mkString(", ")
        val retType = llvmType(expr.typ)
        if retType == "void" then
          emit(s"  call void @$name($argStr)")
          "0"
        else
          val result = newReg()
          emit(s"  $result = call $retType @$name($argStr)")
          result

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

      case TStr(inner) =>
        val v = genExpr(inner)
        val vt = exprType(inner)
        inner.typ match
          case SyslType.BoolType =>
            // bool → "true" or "false" via select
            val cmp = newReg()
            val result = newReg()
            emit(s"  $cmp = icmp ne i8 $v, 0")
            val truePtr = newReg()
            val falsePtr = newReg()
            emit(s"  $truePtr = getelementptr [5 x i8], [5 x i8]* @.str.true, i32 0, i32 0")
            emit(s"  $falsePtr = getelementptr [6 x i8], [6 x i8]* @.str.false, i32 0, i32 0")
            emit(s"  $result = select i1 $cmp, i8* $truePtr, i8* $falsePtr")
            result
          case SyslType.DoubleType =>
            // double → string via snprintf with %g
            emitSnprintfToString("@.fmt_f", 3, s"double $v")
          case t if t.isIntegral =>
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
            val r = newReg()
            emit(s"  $r = getelementptr [$byteLen x i8], [$byteLen x i8]* $label, i32 0, i32 0")
            r

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

  // Emit snprintf-based conversion: measure, malloc, format. Returns i8* register.
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
    buf

  // Emit sext only when fromType != toType; return the (possibly cast) register
  private def emitSextIfNeeded(value: String, fromType: String, toType: String): String =
    if fromType == toType then value
    else
      val cast = newReg()
      emit(s"  $cast = sext $fromType $value to $toType")
      cast

  private def llvmType(t: SyslType): String = t match
    case SyslType.IntType(w) => s"i$w"
    case SyslType.UIntType(w) => s"i$w"  // LLVM uses same integer type for signed/unsigned
    case SyslType.BoolType => "i8"
    case SyslType.DoubleType => "double"
    case SyslType.VoidType => "void"
    case SyslType.StringType => "i8*"
    case SyslType.PtrType(_) => "i8*"
    case SyslType.ArrayType(elem, size) => s"[$size x ${llvmType(elem)}]"
    case _ => "i64"

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
