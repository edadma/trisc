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

  private case class LocalVar(name: String, reg: String, typ: SyslType)

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
        case _: TStructDecl => // type only
        case f: TFunDecl => genFunction(f)
        case TVarDecl(name, typ, _, _) =>
          emit(s"@$name = global ${llvmType(typ)} 0")
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
        if retType != rt then
          val cast = newReg()
          emit(s"  $cast = sext $rt $result to $retType")
          emit(s"  ret $retType $cast")
        else
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
            val rt = exprType(expr)
            if retType != rt then
              val cast = newReg()
              emit(s"  $cast = sext $rt $result to $retType")
              emit(s"  ret $retType $cast")
            else
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
        if vt != lt then
          val cast = newReg()
          emit(s"  $cast = sext $vt $value to $lt")
          emit(s"  store $lt $cast, $lt* $alloca")
        else
          emit(s"  store $lt $value, $lt* $alloca")
        locals(name) = LocalVar(name, alloca, typ)

      case TAssignStmt(target, value) =>
        val v = genExpr(value)
        if locals.contains(target) then
          val local = locals(target)
          val lt = llvmType(local.typ)
          val vt = exprType(value)
          if vt != lt then
            val cast = newReg()
            emit(s"  $cast = sext $vt $v to $lt")
            emit(s"  store $lt $cast, $lt* ${local.reg}")
          else
            emit(s"  store $lt $v, $lt* ${local.reg}")
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
        if vt != retType then
          val cast = newReg()
          emit(s"  $cast = sext $vt $v to $retType")
          emit(s"  ret $retType $cast")
        else
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
      case TBoolLit(true, _) => "1"
      case TBoolLit(false, _) => "0"

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

      case TBinary(left, op, right, _) =>
        val l = genExpr(left)
        val r = genExpr(right)
        val lt = exprType(left)
        val result = newReg()
        op match
          case "+"  => emit(s"  $result = add $lt $l, $r")
          case "-"  => emit(s"  $result = sub $lt $l, $r")
          case "*"  => emit(s"  $result = mul $lt $l, $r")
          case "/"  => emit(s"  $result = sdiv $lt $l, $r")
          case "%"  => emit(s"  $result = srem $lt $l, $r")
          case "&"  => emit(s"  $result = and $lt $l, $r")
          case "|"  => emit(s"  $result = or $lt $l, $r")
          case "^"  => emit(s"  $result = xor $lt $l, $r")
          case "<<" => emit(s"  $result = shl $lt $l, $r")
          case ">>" => emit(s"  $result = ashr $lt $l, $r")
          case "==" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp eq $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $lt")
          case "!=" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp ne $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $lt")
          case "<" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp slt $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $lt")
          case ">" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp sgt $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $lt")
          case "<=" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp sle $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $lt")
          case ">=" =>
            val cmp = newReg()
            emit(s"  $cmp = icmp sge $lt $l, $r")
            emit(s"  $result = zext i1 $cmp to $lt")
          case "&&" =>
            val lBool = newReg()
            val rBool = newReg()
            val andResult = newReg()
            emit(s"  $lBool = icmp ne $lt $l, 0")
            emit(s"  $rBool = icmp ne $lt $r, 0")
            emit(s"  $andResult = and i1 $lBool, $rBool")
            emit(s"  $result = zext i1 $andResult to $lt")
          case "||" =>
            val lBool = newReg()
            val rBool = newReg()
            val orResult = newReg()
            emit(s"  $lBool = icmp ne $lt $l, 0")
            emit(s"  $rBool = icmp ne $lt $r, 0")
            emit(s"  $orResult = or i1 $lBool, $rBool")
            emit(s"  $result = zext i1 $orResult to $lt")
        result

      case TUnary("-", operand, _) =>
        val v = genExpr(operand)
        val vt = exprType(operand)
        val result = newReg()
        emit(s"  $result = sub $vt 0, $v")
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
        val ext = newReg()
        emit(s"  $ext = sext i32 $result to $t")
        ext

      case TCall("print", List(arg), _) =>
        val v = genExpr(arg)
        val vt = exprType(arg)
        val result = newReg()
        emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.fmt_d, i32 0, i32 0), $vt $v)""")
        "0"

      case TCall("println", List(arg), _) =>
        val v = genExpr(arg)
        val vt = exprType(arg)
        val result = newReg()
        emit(s"""  $result = call i32 (i8*, ...) @printf(i8* getelementptr ([5 x i8], [5 x i8]* @.fmt_dn, i32 0, i32 0), $vt $v)""")
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

      case _ =>
        emit(s"  ; TODO: ${expr.getClass.getSimpleName}")
        "0"

  private def llvmType(t: SyslType): String = t match
    case SyslType.IntType(w) => s"i$w"
    case SyslType.BoolType => "i8"
    case SyslType.VoidType => "void"
    case SyslType.PtrType(_) => "i64"
    case _ => "i64"

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
