package io.github.edadma.trisc

import scala.collection.mutable

class SyslLLVMCodegen:
  private val out = new StringBuilder
  private var regCounter = 0
  private var labelCounter = 0

  private def newReg(): String =
    regCounter += 1
    s"%t$regCounter"

  private def newLabel(prefix: String): String =
    labelCounter += 1
    s"${prefix}_$labelCounter"

  private case class LocalVar(name: String, reg: String) // reg is the alloca'd pointer

  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var currentFunction: TFunDecl = null
  private var hasReturned = false

  def generate(program: TProgram): String =
    out.clear()

    // Declare external C functions
    emit("declare i32 @putchar(i32)")
    emit("declare i32 @printf(i8*, ...)")
    emit("")

    // Generate functions
    for decl <- program.decls do
      decl match
        case _: TImportDecl => // skip
        case f: TFunDecl => genFunction(f)
        case TVarDecl(name, _, _, _) =>
          emit(s"@$name = global i64 0")
    emit("")

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
      val alloca = newReg()
      emit(s"  $alloca = alloca i64")
      emit(s"  store i64 %${param.name}_arg, i64* $alloca")
      locals(param.name) = LocalVar(param.name, alloca)

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        val result = genExpr(expr)
        emit(s"  ret $retType $result")
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
            emit(s"  ret $retType $result")
            hasReturned = true
          case other =>
            genStmt(other)
            if !hasReturned then
              emit(s"  ret $retType 0")
              hasReturned = true
    else
      emit(s"  ret $retType 0")
      hasReturned = true

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, _, init) =>
        val alloca = newReg()
        emit(s"  $alloca = alloca i64")
        val value = genExpr(init)
        emit(s"  store i64 $value, i64* $alloca")
        locals(name) = LocalVar(name, alloca)

      case TAssignStmt(target, value) =>
        val v = genExpr(value)
        if locals.contains(target) then
          emit(s"  store i64 $v, i64* ${locals(target).reg}")
        else
          // New local
          val alloca = newReg()
          emit(s"  $alloca = alloca i64")
          emit(s"  store i64 $v, i64* $alloca")
          locals(target) = LocalVar(target, alloca)

      case TReturnStmt(Some(value)) =>
        val v = genExpr(value)
        val retType = if currentFunction.name == "main" then "i64" else llvmType(currentFunction.returnType)
        emit(s"  ret $retType $v")
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
        val cBool = newReg()
        emit(s"  $cBool = icmp ne i64 $c, 0")
        emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emit(s"$bodyLabel:")
        for s <- body do genStmt(s)
        if !hasReturned then emit(s"  br label %$condLabel")
        emit(s"$endLabel:")

      case _ =>
        emit(s"  ; TODO: ${stmt.getClass.getSimpleName}")

  private def genExpr(expr: TExpr): String =
    expr match
      case TIntLit(n, _) => n.toString
      case TBoolLit(true, _) => "1"
      case TBoolLit(false, _) => "0"

      case TVarRef(name, _) =>
        if locals.contains(name) then
          val r = newReg()
          emit(s"  $r = load i64, i64* ${locals(name).reg}")
          r
        else
          val r = newReg()
          emit(s"  $r = load i64, i64* @$name")
          r

      case TBinary(left, op, right, _) =>
        val l = genExpr(left)
        val r = genExpr(right)
        val result = newReg()
        op match
          case "+"  => emit(s"  $result = add i64 $l, $r")
          case "-"  => emit(s"  $result = sub i64 $l, $r")
          case "*"  => emit(s"  $result = mul i64 $l, $r")
          case "/"  => emit(s"  $result = sdiv i64 $l, $r")
          case "%"  => emit(s"  $result = srem i64 $l, $r")
          case "&"  => emit(s"  $result = and i64 $l, $r")
          case "|"  => emit(s"  $result = or i64 $l, $r")
          case "^"  => emit(s"  $result = xor i64 $l, $r")
          case "<<" => emit(s"  $result = shl i64 $l, $r")
          case ">>" => emit(s"  $result = ashr i64 $l, $r")
          case "==" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp eq i64 $l, $r")
            emit(s"  $result = zext i1 $cmp to i64")
          case "!=" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp ne i64 $l, $r")
            emit(s"  $result = zext i1 $cmp to i64")
          case "<" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp slt i64 $l, $r")
            emit(s"  $result = zext i1 $cmp to i64")
          case ">" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp sgt i64 $l, $r")
            emit(s"  $result = zext i1 $cmp to i64")
          case "<=" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp sle i64 $l, $r")
            emit(s"  $result = zext i1 $cmp to i64")
          case ">=" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp sge i64 $l, $r")
            emit(s"  $result = zext i1 $cmp to i64")
          case "&&" =>
            val lBool = newReg()
            val rBool = newReg()
            val andResult = newReg()
            emit(s"  $lBool = icmp ne i64 $l, 0")
            emit(s"  $rBool = icmp ne i64 $r, 0")
            emit(s"  $andResult = and i1 $lBool, $rBool")
            emit(s"  $result = zext i1 $andResult to i64")
          case "||" =>
            val lBool = newReg()
            val rBool = newReg()
            val orResult = newReg()
            emit(s"  $lBool = icmp ne i64 $l, 0")
            emit(s"  $rBool = icmp ne i64 $r, 0")
            emit(s"  $orResult = or i1 $lBool, $rBool")
            emit(s"  $result = zext i1 $orResult to i64")
        result

      case TUnary("-", operand, _) =>
        val v = genExpr(operand)
        val result = newReg()
        emit(s"  $result = sub i64 0, $v")
        result

      case TUnary("!", operand, _) =>
        val v = genExpr(operand)
        val cmp = newReg()
        val result = newReg()
        emit(s"  $cmp = icmp eq i64 $v, 0")
        emit(s"  $result = zext i1 $cmp to i64")
        result

      case TUnary("~", operand, _) =>
        val v = genExpr(operand)
        val result = newReg()
        emit(s"  $result = xor i64 $v, -1")
        result

      case TCall("putchar", List(arg), _) =>
        val v = genExpr(arg)
        val truncated = newReg()
        val result = newReg()
        emit(s"  $truncated = trunc i64 $v to i32")
        emit(s"  $result = call i32 @putchar(i32 $truncated)")
        val ext = newReg()
        emit(s"  $ext = sext i32 $result to i64")
        ext

      case TCall("print", List(arg), _) =>
        val v = genExpr(arg)
        val result = newReg()
        emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.fmt_d, i32 0, i32 0), i64 $v)""")
        "0"

      case TCall("println", List(arg), _) =>
        val v = genExpr(arg)
        val result = newReg()
        emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([5 x i8], [5 x i8]* @.fmt_dn, i32 0, i32 0), i64 $v)""")
        "0"

      case TCall(name, args, _) =>
        val argVals = args.map(genExpr)
        val argStr = argVals.map(v => s"i64 $v").mkString(", ")
        val retType = llvmType(expr.typ)
        if retType == "void" then
          emit(s"  call void @$name($argStr)")
          "0"
        else
          val result = newReg()
          emit(s"  $result = call $retType @$name($argStr)")
          result

      case TIfExpr(cond, thenBody, elseBody, _) =>
        val c = genExpr(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne i64 $c, 0")
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
        val thenExitLabel = if !thenReturned then
          emit(s"  br label %$mergeLabel")
          thenLabel + "_exit"
        else thenLabel
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
          emit(s"  $phi = phi i64 [ $thenVal, %$thenLabel ], [ $elseVal, %$elseLabel ]")
          phi
        else "0"

      case _ =>
        emit(s"  ; TODO: ${expr.getClass.getSimpleName}")
        "0"

  private def llvmType(t: SyslType): String = t match
    case SyslType.IntType(64) => "i64"
    case SyslType.IntType(32) => "i32"
    case SyslType.IntType(16) => "i16"
    case SyslType.IntType(8) => "i8"
    case _: SyslType.IntType => "i64"
    case SyslType.BoolType => "i64"
    case SyslType.VoidType => "void"
    case _ => "i64"

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
