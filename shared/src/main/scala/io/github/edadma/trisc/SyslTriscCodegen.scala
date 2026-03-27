package io.github.edadma.trisc

import scala.collection.mutable

class SyslTriscCodegen(addresses: Int = 2):
  private val out = new StringBuilder
  private var labelCounter = 0

  private def newLabel(prefix: String): String =
    labelCounter += 1
    s".${prefix}_$labelCounter"

  def generate(program: TProgram): String =
    out.clear()
    labelCounter = 0

    // Emit entry point and global directives from module metadata
    val meta = ModuleMeta.fromProgram(program)
    val hasMain = meta.symbols.exists(s => s.name == "main" && s.typ.isInstanceOf[SymbolMeta.Kind.Func])
    if hasMain then emit("entry main")
    out ++= meta.toAsmGlobals

    // Emit functions
    for decl <- program.decls do
      decl match
        case _: TImportDecl => // skip
        case f: TFunDecl => genFunction(f)
        case TVarDecl(name, typ, init, _) =>
          emit(s"# global: $name")
          emit(s"$name")
          emit(s"  dw 0") // TODO: global initializers

    out.toString

  private case class LocalVar(name: String, offset: Int)

  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var stackOffset: Int = 0
  private var currentFunction: TFunDecl = null

  private def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    stackOffset = 0

    emit(s"# function: ${fun.name}")
    emit(s"${fun.name}")

    // Prologue: save lr, fp, set up frame
    emit("  pshd r6")       // save link register
    emit("  pshd r5")       // save frame pointer
    emit("  mov r5, r7")    // frame pointer = stack pointer

    // Allocate space for parameters on the frame
    // Parameters come in r1-r4, copy to stack
    for (param, i) <- fun.params.zipWithIndex do
      stackOffset -= 8
      locals(param.name) = LocalVar(param.name, stackOffset)
      if i < 4 then
        emit(s"  pshd r${i + 1}")  // push param register to stack
      // TODO: handle > 4 params from stack

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        genExpr(expr) // result in r1
        emit("  mov r7, r5")    // restore stack
        emit("  popd r5")       // restore frame pointer
        emit("  popd r6")       // restore link register
        if fun.name == "main" then emit("  halt")
        else emit("  jalr r0, r6") // return
      case TBlockBody(stmts) =>
        genBlock(stmts)

    locals = null
    currentFunction = null

  private def genBlock(stmts: List[TStmt]): Unit =
    if stmts.nonEmpty then
      for stmt <- stmts.init do genStmt(stmt)
      stmts.last match
        case TExprStmt(expr) =>
          genExpr(expr) // result in r1
          emitEpilogue()
        case other =>
          genStmt(other)
          // If no explicit return, return 0
          emit("  ldi r1, 0")
          emitEpilogue()
    else
      emit("  ldi r1, 0")
      emitEpilogue()

  private def emitEpilogue(): Unit =
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    if currentFunction.name == "main" then emit("  halt")
    else emit("  jalr r0, r6")

  // Break/continue label stacks
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, _, init) =>
        genExpr(init) // result in r1
        stackOffset -= 8
        locals(name) = LocalVar(name, stackOffset)
        emit("  pshd r1") // push to stack

      case TAssignStmt(target, value) =>
        genExpr(value) // result in r1
        if locals.contains(target) then
          val local = locals(target)
          emit(s"  addi r2, r5, ${local.offset}")
          emit(s"  std r1, r2, r0")
        else
          // New local variable (first assignment = declaration)
          stackOffset -= 8
          locals(target) = LocalVar(target, stackOffset)
          emit("  pshd r1")

      case TCompoundAssignStmt(target, op, value) =>
        genExpr(value) // r1 = right operand
        emit("  pshd r1")
        val local = locals(target)
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  ldd r1, r2, r0") // r1 = current value
        emit("  popd r3")         // r3 = right operand
        op match
          case "+"  => emit("  add r1, r1, r3")
          case "-"  => emit("  sub r1, r1, r3")
          case "*"  => emit("  mul r1, r1, r3")
          case "/"  => emit("  div r1, r1, r3")
          case "%"  => emit("  rem r1, r1, r3")
          case "&"  => emit("  and r1, r1, r3")
          case "|"  => emit("  or r1, r1, r3")
          case "^"  => emit("  xor r1, r1, r3")
          case "<<" => emit("  lsl r1, r1, r3")
          case ">>" => emit("  asr r1, r1, r3")
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  std r1, r2, r0")

      case TReturnStmt(Some(value)) =>
        genExpr(value) // result in r1
        emitEpilogue()

      case TReturnStmt(None) =>
        emit("  ldi r1, 0")
        emitEpilogue()

      case TExprStmt(expr) =>
        genExpr(expr) // result in r1, discarded

      case TWhileStmt(cond, body) =>
        val loopLabel = newLabel("while")
        val endLabel = newLabel("endwhile")
        breakLabels.push(endLabel)
        continueLabels.push(loopLabel)
        emit(s"$loopLabel")
        genExpr(cond)
        emit(s"  beq r1, r0, $endLabel")
        for stmt <- body do genStmt(stmt)
        emit(s"  bra $loopLabel")
        emit(s"$endLabel")
        breakLabels.pop()
        continueLabels.pop()

      case TForStmt(init, cond, update, body) =>
        val loopLabel = newLabel("for")
        val updateLabel = newLabel("forupdate")
        val endLabel = newLabel("endfor")
        genStmt(init)
        breakLabels.push(endLabel)
        continueLabels.push(updateLabel)
        emit(s"$loopLabel")
        genExpr(cond)
        emit(s"  beq r1, r0, $endLabel")
        for stmt <- body do genStmt(stmt)
        emit(s"$updateLabel")
        genStmt(update)
        emit(s"  bra $loopLabel")
        emit(s"$endLabel")
        breakLabels.pop()
        continueLabels.pop()

      case TDoWhileStmt(cond, body) =>
        val loopLabel = newLabel("dowhile")
        val endLabel = newLabel("enddowhile")
        breakLabels.push(endLabel)
        continueLabels.push(loopLabel)
        emit(s"$loopLabel")
        for stmt <- body do genStmt(stmt)
        genExpr(cond)
        emit(s"  bne r1, r0, $loopLabel")
        emit(s"$endLabel")
        breakLabels.pop()
        continueLabels.pop()

      case TBreakStmt =>
        emit(s"  bra ${breakLabels.top}")

      case TContinueStmt =>
        emit(s"  bra ${continueLabels.top}")

      case TDerefAssignStmt(_, _) =>
        emit("  # TODO: TDerefAssignStmt (needs pointer support)")

      case TIndexAssignStmt(_, _, _) =>
        emit("  # TODO: TIndexAssignStmt (needs array/pointer support)")

      case _ =>
        emit(s"  # TODO: ${stmt.getClass.getSimpleName}")

  private def genExpr(expr: TExpr): Unit =
    // Result always in r1
    expr match
      case TIntLit(n, _) =>
        if n >= 0 && n <= 255 then
          emit(s"  ldi r1, $n")
        else
          emit(s"  movi r1, $n")

      case TBoolLit(true, _) => emit("  ldi r1, 1")
      case TBoolLit(false, _) => emit("  ldi r1, 0")

      case TVarRef(name, _) =>
        if locals != null && locals.contains(name) then
          val local = locals(name)
          emit(s"  addi r2, r5, ${local.offset}")
          emit(s"  ldd r1, r2, r0")
        else
          emit(s"  movi r1, $name")
          emit(s"  ldw r1, r1, r0")

      case TBinary(left, "&&", right, _) =>
        val falseLabel = newLabel("and_false")
        val endLabel = newLabel("and_end")
        genExpr(left)
        emit(s"  beq r1, r0, $falseLabel") // short-circuit: left is false
        genExpr(right)
        emit(s"  beq r1, r0, $falseLabel") // right is false
        emit("  ldi r1, 1")
        emit(s"  bra $endLabel")
        emit(s"$falseLabel")
        emit("  ldi r1, 0")
        emit(s"$endLabel")

      case TBinary(left, "||", right, _) =>
        val trueLabel = newLabel("or_true")
        val endLabel = newLabel("or_end")
        genExpr(left)
        emit(s"  bne r1, r0, $trueLabel") // short-circuit: left is true
        genExpr(right)
        emit(s"  bne r1, r0, $trueLabel") // right is true
        emit("  ldi r1, 0")
        emit(s"  bra $endLabel")
        emit(s"$trueLabel")
        emit("  ldi r1, 1")
        emit(s"$endLabel")

      case TBinary(left, op, right, _) =>
        genExpr(left)        // r1 = left
        emit("  pshd r1")   // save left on stack
        genExpr(right)       // r1 = right
        emit("  mov r2, r1") // r2 = right
        emit("  popd r1")   // r1 = left
        op match
          case "+"  => emit("  add r1, r1, r2")
          case "-"  => emit("  sub r1, r1, r2")
          case "*"  => emit("  mul r1, r1, r2")
          case "/"  => emit("  div r1, r1, r2")
          case "%"  => emit("  rem r1, r1, r2")
          case "&"  => emit("  and r1, r1, r2")
          case "|"  => emit("  or r1, r1, r2")
          case "^"  => emit("  xor r1, r1, r2")
          case "<<" => emit("  lsl r1, r1, r2")
          case ">>" => emit("  asr r1, r1, r2")
          case "==" =>
            val eq = newLabel("eq")
            val end = newLabel("end")
            emit(s"  beq r1, r2, $eq")
            emit("  ldi r1, 0")
            emit(s"  bra $end")
            emit(s"$eq")
            emit("  ldi r1, 1")
            emit(s"$end")
          case "!=" =>
            val ne = newLabel("ne")
            val end = newLabel("end")
            emit(s"  beq r1, r2, $ne")
            emit("  ldi r1, 1")
            emit(s"  bra $end")
            emit(s"$ne")
            emit("  ldi r1, 0")
            emit(s"$end")
          case "<" =>
            emit("  slt r1, r1, r2") // r1 = (r1 < r2) ? 1 : 0
          case ">" =>
            emit("  slt r1, r2, r1") // r1 = (r2 < r1) ? 1 : 0
          case "<=" =>
            // r1 <= r2 iff !(r2 < r1)
            emit("  slt r1, r2, r1") // r1 = (r2 < r1)
            emit("  ldi r3, 1")
            emit("  xor r1, r1, r3") // flip: 0→1, 1→0
          case ">=" =>
            // r1 >= r2 iff !(r1 < r2)
            emit("  slt r1, r1, r2") // r1 = (r1 < r2)
            emit("  ldi r3, 1")
            emit("  xor r1, r1, r3") // flip

      case TPreInc(name, _) =>
        val local = locals(name)
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  ldd r1, r2, r0")
        emit("  addi r1, r1, 1")
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  std r1, r2, r0") // store incremented, return new value

      case TPreDec(name, _) =>
        val local = locals(name)
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  ldd r1, r2, r0")
        emit("  addi r1, r1, -1")
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  std r1, r2, r0")

      case TPostInc(name, _) =>
        val local = locals(name)
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  ldd r1, r2, r0") // r1 = old value (returned)
        emit("  addi r3, r1, 1")  // r3 = new value
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  std r3, r2, r0") // store new value

      case TPostDec(name, _) =>
        val local = locals(name)
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  ldd r1, r2, r0")
        emit("  addi r3, r1, -1")
        emit(s"  addi r2, r5, ${local.offset}")
        emit(s"  std r3, r2, r0")

      case TCast(inner, target) =>
        genExpr(inner)
        import SyslType.*
        target match
          case BoolType =>
            // nonzero → 1, zero → 0
            val isZero = newLabel("iszero")
            val end = newLabel("end")
            emit(s"  beq r1, r0, $isZero")
            emit("  ldi r1, 1")
            emit(s"  bra $end")
            emit(s"$isZero")
            emit("  ldi r1, 0")
            emit(s"$end")
          case ByteType =>
            emit("  ldi r2, 255")
            emit("  and r1, r1, r2") // mask to 8 bits
          case CharType =>
            // mask to 32 bits — no-op on 64-bit registers for values < 2^32
            // TODO: proper 32-bit mask when needed (needs movi for 0xFFFFFFFF)
          case IntType =>
            // no-op — already int-sized
          case _ =>

      case TUnary("-", operand, _) =>
        genExpr(operand)
        emit("  neg r1, r1")

      case TUnary("!", operand, _) =>
        genExpr(operand)
        val isZero = newLabel("iszero")
        val end = newLabel("end")
        emit(s"  beq r1, r0, $isZero")
        emit("  ldi r1, 0")
        emit(s"  bra $end")
        emit(s"$isZero")
        emit("  ldi r1, 1")
        emit(s"$end")

      case TUnary("~", operand, _) =>
        genExpr(operand)
        emit("  not r1, r1")

      case TCall(name, args, _) =>
        // Push args in reverse, then call
        for (arg, i) <- args.zipWithIndex.reverse do
          genExpr(arg)
          if i < 4 then
            emit(s"  mov r${i + 1}, r1") // TODO: this clobbers r1 for later args
          else
            emit("  pshd r1")
        // For now, simple approach: evaluate args left to right into regs
        for (arg, i) <- args.zipWithIndex do
          if i < 4 then
            genExpr(arg)
            if i > 0 then emit(s"  mov r${i + 1}, r1")
            // r1 stays for first arg
        emit(s"  movi r4, $name") // use r4 as temp for function address
        emit(s"  jalr r6, r4")

      case TIfExpr(cond, thenBody, elseBody, _) =>
        val elseLabel = newLabel("else")
        val endLabel = newLabel("endif")
        genExpr(cond)
        emit(s"  beq r1, r0, $elseLabel")
        for stmt <- thenBody do genStmt(stmt)
        emit(s"  bra $endLabel")
        emit(s"$elseLabel")
        elseBody.foreach(stmts => for stmt <- stmts do genStmt(stmt))
        emit(s"$endLabel")

      case _ =>
        emit(s"  # TODO: ${expr.getClass.getSimpleName}")

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
