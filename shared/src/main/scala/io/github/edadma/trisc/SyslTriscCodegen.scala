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
        case _: TStructDecl => // type-only, no code to emit
        case f: TFunDecl => genFunction(f)
        case TVarDecl(name, typ, init, _) =>
          emit(s"# global: $name")
          emit(s"$name")
          init match
            case TIntLit(n, _) => emit(s"  dl $n")
            case TBoolLit(b, _) => emit(s"  dl ${if b then 1 else 0}")
            case _ => emit(s"  dl 0") // complex initializers not yet supported

    out.toString

  private case class LocalVar(name: String, offset: Int, typ: SyslType)

  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var stackOffset: Int = 0
  private var currentFunction: TFunDecl = null

  // Size of a type on the stack in bytes, rounded up to alignment
  private def stackSize(typ: SyslType): Int =
    val raw = typ.sizeOf.toInt
    val align = stackAlign(typ)
    ((raw + align - 1) / align) * align

  // Natural alignment for a type
  private def stackAlign(typ: SyslType): Int = typ match
    case SyslType.IntType(w) => (w / 8).min(8)
    case SyslType.BoolType => 1
    case SyslType.PtrType(_) => 8
    case SyslType.FuncType(_, _) => 8
    case SyslType.ArrayType(elem, _) => stackAlign(elem)
    case SyslType.StructType(_, fields) => if fields.isEmpty then 1 else fields.map(f => stackAlign(f._2)).max
    case _ => 8

  // Emit load from [rBase + 0] into rDest, using width-appropriate instruction
  private def emitLoad(destReg: Int, addrReg: Int, typ: SyslType): Unit =
    typ match
      case SyslType.IntType(8) | SyslType.BoolType =>
        emit(s"  ldb r$destReg, r$addrReg, r0")
        emit(s"  ldi r2, 8")
        emit(s"  sext r$destReg, r2")
      case SyslType.IntType(16) =>
        emit(s"  lds r$destReg, r$addrReg, r0")
        emit(s"  ldi r2, 16")
        emit(s"  sext r$destReg, r2")
      case SyslType.IntType(32) =>
        emit(s"  ldw r$destReg, r$addrReg, r0")
        emit(s"  ldi r2, 32")
        emit(s"  sext r$destReg, r2")
      case _ =>
        emit(s"  ldd r$destReg, r$addrReg, r0")

  // Emit store from rSrc to [rBase + 0], using width-appropriate instruction
  private def emitStore(srcReg: Int, addrReg: Int, typ: SyslType): Unit =
    typ match
      case SyslType.IntType(8) | SyslType.BoolType =>
        emit(s"  stb r$srcReg, r$addrReg, r0")
      case SyslType.IntType(16) =>
        emit(s"  sts r$srcReg, r$addrReg, r0")
      case SyslType.IntType(32) =>
        emit(s"  stw r$srcReg, r$addrReg, r0")
      case _ =>
        emit(s"  std r$srcReg, r$addrReg, r0")

  // Emit push r1 with type-appropriate width
  private def emitPush(typ: SyslType): Unit =
    val size = stackSize(typ)
    typ match
      case SyslType.IntType(8) | SyslType.BoolType =>
        emit("  pshb r1")
      case SyslType.IntType(16) =>
        emit("  pshs r1")
      case SyslType.IntType(32) =>
        emit("  pshw r1")
      case _ =>
        emit("  pshd r1")

  // Emit pop into r1 with type-appropriate width + sign extend
  private def emitPop(reg: Int, typ: SyslType): Unit =
    typ match
      case SyslType.IntType(8) | SyslType.BoolType =>
        emit(s"  popb r$reg")
        emit(s"  ldi r2, 8")
        emit(s"  sext r$reg, r2")
      case SyslType.IntType(16) =>
        emit(s"  pops r$reg")
        emit(s"  ldi r2, 16")
        emit(s"  sext r$reg, r2")
      case SyslType.IntType(32) =>
        emit(s"  popw r$reg")
        emit(s"  ldi r2, 32")
        emit(s"  sext r$reg, r2")
      case _ =>
        emit(s"  popd r$reg")

  // Allocate a local variable on the stack, return its offset from fp
  // Emits code to grow the stack pointer
  private def allocLocal(name: String, typ: SyslType): LocalVar =
    val size = stackSize(typ)
    val align = stackAlign(typ)
    val oldOffset = stackOffset
    // Align the stack offset downward
    stackOffset -= size
    stackOffset = stackOffset & ~(align - 1) // align down
    // Grow the stack by the difference
    val growth = oldOffset - stackOffset
    emitAddImm(7, 7, -growth)
    val local = LocalVar(name, stackOffset, typ)
    locals(name) = local
    local

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

    // First param comes in r1, push to local frame with proper width
    // Remaining params were pushed by caller above our frame
    if fun.params.nonEmpty then
      val p = fun.params.head
      val local = allocLocal(p.name, p.typ)
      emitAddImm(2, 5, local.offset)
      emitStore(1, 2, p.typ)
    // Stack args (params 1+) are above saved lr/fp
    // Caller pushed them with proper widths, calculate offsets
    var callerArgOffset = 16 // start above saved lr(8) + fp(8)
    for (param, i) <- fun.params.zipWithIndex.drop(1) do
      locals(param.name) = LocalVar(param.name, callerArgOffset, param.typ)
      callerArgOffset += stackSize(param.typ)

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

  // Emit binary operation: r1 = r1 op r3
  private def emitBinOp(op: String): Unit =
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
      case TVarStmt(name, typ, init) =>
        genExpr(init) // result in r1
        val local = allocLocal(name, typ)
        emitAddImm(2, 5, local.offset)
        emitStore(1, 2, typ)

      case TAssignStmt(target, value) =>
        genExpr(value) // result in r1
        if locals != null && locals.contains(target) then
          val local = locals(target)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else
          // New local variable (first assignment = declaration, infer type)
          val typ = value.typ
          val local = allocLocal(target, typ)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, typ)

      case TCompoundAssignStmt(target, op, value) =>
        genExpr(value) // r1 = right operand
        emit("  pshd r1") // always save as 64-bit temp
        if locals != null && locals.contains(target) then
          val local = locals(target)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit("  popd r3")
          emitBinOp(op)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else
          emit(s"  movi r2, $target")
          emit(s"  ldd r1, r2, r0") // globals are always 64-bit for now
          emit("  popd r3")
          emitBinOp(op)
          emit(s"  movi r2, $target")
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

      case TAsmStmt(code) =>
        // Emit each line of inline assembly verbatim
        for line <- code.split("\\\\n|\\n") do
          emit(s"  ${line.trim}")

      case TDerefAssignStmt(pointer, value) =>
        genExpr(value)           // r1 = value to store
        emit("  pshd r1")       // save as 64-bit temp
        genExpr(pointer)         // r1 = address
        emit("  popd r2")        // r2 = value
        // Store with width matching pointee type
        pointer.typ match
          case SyslType.PtrType(pointee) => emitStore(2, 1, pointee)
          case _ => emit("  std r2, r1, r0")

      case TIndexAssignStmt(array, index, value) =>
        val elemType = array.typ match
          case SyslType.ArrayType(e, _) => e
          case SyslType.PtrType(e) => e
          case _ => SyslType.I64
        val elemSize = stackSize(elemType)
        genExpr(value)           // r1 = value
        emit("  pshd r1")       // save as 64-bit temp
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array base address
        emit("  popd r2")        // r2 = index
        emit(s"  ldi r3, $elemSize")
        emit("  mul r2, r2, r3") // r2 = index * elemSize
        emit("  add r1, r1, r2") // r1 = base + offset
        emit("  popd r2")        // r2 = value
        emitStore(2, 1, elemType)

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
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
        else
          emit(s"  movi r1, $name")
          emit(s"  ldd r1, r1, r0") // globals are 64-bit for now

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

      case TBinary(left, op @ ("+" | "-"), right, _) if left.typ.isPointerLike =>
        // Pointer arithmetic: ptr + int → ptr (scale by element size)
        val elemSize = left.typ match
          case SyslType.PtrType(e) => stackSize(e)
          case SyslType.ArrayType(e, _) => stackSize(e)
          case _ => 8
        genExpr(left)        // r1 = pointer
        emit("  pshd r1")
        genExpr(right)       // r1 = integer offset
        emit(s"  ldi r3, $elemSize")
        emit("  mul r1, r1, r3") // scale by element size
        emit("  popd r2")   // r2 = pointer
        if op == "+" then emit("  add r1, r2, r1")
        else emit("  sub r1, r2, r1")

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

      case TPreInc(name, typ) =>
        val step = if typ.isPointerLike then stackSize(typ).max(1) else 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r1, r1, $step")
        emitAddImm(2, 5, local.offset)
        emitStore(1, 2, local.typ)

      case TPreDec(name, typ) =>
        val step = if typ.isPointerLike then stackSize(typ).max(1) else 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r1, r1, -$step")
        emitAddImm(2, 5, local.offset)
        emitStore(1, 2, local.typ)

      case TPostInc(name, typ) =>
        val step = if typ.isPointerLike then stackSize(typ).max(1) else 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r3, r1, $step")
        emitAddImm(2, 5, local.offset)
        emitStore(3, 2, local.typ)

      case TPostDec(name, typ) =>
        val step = if typ.isPointerLike then stackSize(typ).max(1) else 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r3, r1, -$step")
        emitAddImm(2, 5, local.offset)
        emitStore(3, 2, local.typ)

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
          case IntType(8) =>
            emit("  zeb r1, r1")   // zero-extend byte: mask to 8 bits
          case IntType(16) =>
            emit("  zes r1, r1")   // zero-extend short: mask to 16 bits
          case IntType(32) =>
            emit("  zew r1, r1")   // zero-extend word: mask to 32 bits
          case IntType(64) =>
            // no-op — already 64-bit
          case _: IntType =>
            // other widths: no-op
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

      case TFuncRef(name, _) =>
        emit(s"  movi r1, $name") // r1 = address of function

      case TCall(name, args, _) =>
        // Push stack args (args 1+) right-to-left so arg[1] is at lowest addr
        for arg <- args.drop(1).reverse do
          genExpr(arg)
          emit("  pshd r1")
        // First arg (if any) goes in r1
        if args.nonEmpty then genExpr(args.head)
        // Call
        emit(s"  movi r4, $name")
        emit("  jalr r6, r4")
        // Clean up stack args
        if args.length > 1 then
          val stackArgBytes = (args.length - 1) * 8
          emitAddImm(7, 7, stackArgBytes)

      case TIndirectCall(callee, args, _) =>
        // Push stack args (args 1+) right-to-left
        for arg <- args.drop(1).reverse do
          genExpr(arg)
          emit("  pshd r1")
        // First arg (if any) goes in r1
        if args.nonEmpty then genExpr(args.head)
        // Save r1 (first arg), load function pointer into r4, restore r1
        if args.nonEmpty then emit("  pshd r1")
        genExpr(callee)          // r1 = function pointer
        emit("  mov r4, r1")     // r4 = function address
        if args.nonEmpty then emit("  popd r1")
        emit("  jalr r6, r4")
        // Clean up stack args
        if args.length > 1 then
          val stackArgBytes = (args.length - 1) * 8
          emitAddImm(7, 7, stackArgBytes)

      case TAddrOf(name, _) =>
        // Compute stack address of local variable
        emitLocalAddr(name, 1) // r1 = address of variable

      case TAddrOfIndex(array, index, SyslType.PtrType(elemType)) =>
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array base address
        emit("  popd r2")        // r2 = index
        emit(s"  ldi r3, $elemSize")
        emit("  mul r2, r2, r3") // r2 = index * elemSize
        emit("  add r1, r1, r2") // r1 = base + offset

      case TDeref(inner, typ) =>
        genExpr(inner)           // r1 = pointer address
        emitLoad(1, 1, typ)      // load with width matching pointee type

      case TIndex(array, index, elemType) =>
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array base address
        emit("  popd r2")        // r2 = index
        emit(s"  ldi r3, $elemSize")
        emit("  mul r2, r2, r3") // r2 = index * elemSize
        emit("  add r1, r1, r2") // r1 = element address
        emitLoad(1, 1, elemType) // load with proper width

      case TArrayDecl(size, elemTypStr, typ) =>
        // Allocate array on stack with proper element size
        val elemType = typ match
          case SyslType.ArrayType(e, _) => e
          case _ => SyslType.I64
        val totalBytes = size * stackSize(elemType)
        emitAddImm(7, 7, -totalBytes)
        emit("  mov r1, r7")     // r1 = address of array start
        stackOffset -= totalBytes

      case TStringLit(value, _) =>
        // Allocate string bytes on stack (UTF-8 + null terminator)
        val bytes = value.getBytes("UTF-8")
        val totalSlots = bytes.length + 1 // +1 for null terminator
        val totalBytes = totalSlots * 8
        emitAddImm(7, 7, -totalBytes)
        emit("  mov r1, r7") // r1 = base address
        // Initialize each byte as a 64-bit value
        for (b, i) <- bytes.zipWithIndex do
          emit(s"  ldi r2, ${b & 0xff}")
          emit(s"  addi r3, r1, ${i * 8}")
          emit(s"  std r2, r3, r0")
        // Null terminator
        emit(s"  addi r3, r1, ${bytes.length * 8}")
        emit(s"  std r0, r3, r0")
        stackOffset -= totalBytes

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

  // Element size in bytes for stack/memory layout (all values stored as 64-bit)
  private def elemSize(typ: SyslType): Int = 8

  // Emit reg = base + offset, handling large offsets that don't fit in addi
  private def emitAddImm(destReg: Int, baseReg: Int, offset: Int): Unit =
    if offset >= -64 && offset <= 63 then
      emit(s"  addi r$destReg, r$baseReg, $offset")
    else if offset >= 0 then
      emit(s"  movi r$destReg, $offset")
      emit(s"  add r$destReg, r$baseReg, r$destReg")
    else
      // Negative offset: load absolute value, subtract
      emit(s"  movi r$destReg, ${-offset}")
      emit(s"  sub r$destReg, r$baseReg, r$destReg")

  // Emit address of local variable into target register
  private def emitLocalAddr(name: String, reg: Int): Unit =
    val local = locals(name)
    emitAddImm(reg, 5, local.offset)

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
