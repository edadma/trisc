package io.github.edadma.trisc

import scala.collection.mutable

class SyslTriscCodegen(addresses: Int = 4):
  private val out = new StringBuilder
  private var labelCounter = 0
  private val stringLiterals = new mutable.ListBuffer[(String, String)]() // (label, value)

  private def newLabel(prefix: String): String =
    labelCounter += 1
    s".${prefix}_$labelCounter"

  def generate(program: TProgram): String =
    out.clear()
    labelCounter = 0
    stringLiterals.clear()

    // Emit entry point and global directives from module metadata
    val meta = ModuleMeta.fromProgram(program)
    val hasMain = meta.symbols.exists(s => s.name == "main" && s.typ.isInstanceOf[SymbolMeta.Kind.Func])
    if hasMain then emit("entry main")
    out ++= meta.toAsmGlobals

    // Emit ALL globals first (with alignment), then ALL functions
    // Pass 1: emit globals
    for decl <- program.decls do
      decl match
        case _: TImportDecl => // skip
        case _: TExternFuncDecl => // skip — resolved by linker
        case _: TStructDecl => // type-only, no code to emit
        case _: TEnumDecl => // type-only, no code to emit
        case _: TTypeAliasDecl => // type-only, no code to emit
        case _: TFunDecl => // skip — emitted in Pass 2
        case TVarDecl(name, typ, init, _) =>
          val align = stackAlign(typ)
          if align > 1 then emit(s"  align $align")
          globals(name) = typ
          emit(s"# global: $name")
          emit(s"$name:")
          init match
            case TArrayLit(elements, _) =>
              val declElemType = typ match
                case SyslType.ArrayType(e, _) => e
                case _ => SyslType.I64
              val elemDir = emitDataDirective(declElemType)
              for elem <- elements do
                elem match
                  case TIntLit(n, _) => emit(s"  $elemDir $n")
                  case TBoolLit(b, _) => emit(s"  $elemDir ${if b then 1 else 0}")
                  case _ => emit(s"  $elemDir 0")
            case _ =>
              typ match
                case SyslType.ArrayType(elem, count) =>
                  emit(s"  resb ${stackSize(elem) * count}")
                case _: SyslType.StructType =>
                  emit(s"  resb ${stackSize(typ)}")
                case _ =>
                  val directive = emitDataDirective(typ)
                  init match
                    case TIntLit(n, _) => emit(s"  $directive $n")
                    case TBoolLit(b, _) => emit(s"  $directive ${if b then 1 else 0}")
                    case _ => emit(s"  $directive 0")
        case _ => // skip non-globals in first pass

    // Pass 2: emit functions
    for decl <- program.decls do
      decl match
        case f: TFunDecl => genFunction(f)
        case _ => // skip

    // Emit string literal data
    if stringLiterals.nonEmpty then
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"global $label, data, ${bytes.length + 1}")
      emit("  align 8")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"$label:")
        for b <- bytes do emit(s"  db ${b & 0xff}")
        emit("  db 0") // null terminator for C interop

    out.toString

  private case class LocalVar(name: String, offset: Int, typ: SyslType)

  private val globals = new mutable.LinkedHashMap[String, SyslType]
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
    case SyslType.UIntType(w) => (w / 8).min(8)
    case SyslType.BoolType => 1
    case SyslType.PtrType(_) => 8
    case SyslType.FuncType(_, _) => 8
    case SyslType.ArrayType(elem, _) => stackAlign(elem)
    case SyslType.StructType(_, fields) => if fields.isEmpty then 1 else fields.map(f => stackAlign(f._2)).max
    case SyslType.StringType => 8    // contains a pointer
    case SyslType.SliceType(_) => 8  // contains a pointer
    case _ => 8

  // Emit load from [rBase + 0] into rDest, using width-appropriate instruction.
  // The CPU's ldb/lds/ldw already sign-extend via Int→Long in Register.write,
  // so no explicit sext is needed for signed types.
  // For unsigned types, load + zero-extend to clear sign-extended bits.
  private def emitLoad(destReg: Int, addrReg: Int, typ: SyslType): Unit =
    typ match
      case SyslType.IntType(8) | SyslType.BoolType =>
        emit(s"  ldb r$destReg, r$addrReg, r0")
      case SyslType.IntType(16) =>
        emit(s"  lds r$destReg, r$addrReg, r0")
      case SyslType.IntType(32) =>
        emit(s"  ldw r$destReg, r$addrReg, r0")
      case SyslType.UIntType(8) =>
        emit(s"  ldb r$destReg, r$addrReg, r0")
        emit(s"  zeb r$destReg, r$destReg")
      case SyslType.UIntType(16) =>
        emit(s"  lds r$destReg, r$addrReg, r0")
        emit(s"  zes r$destReg, r$destReg")
      case SyslType.UIntType(32) =>
        emit(s"  ldw r$destReg, r$addrReg, r0")
        emit(s"  zew r$destReg, r$destReg")
      case _ =>
        emit(s"  ldd r$destReg, r$addrReg, r0")

  // Emit store from rSrc to [rBase + 0], using width-appropriate instruction
  private def emitStore(srcReg: Int, addrReg: Int, typ: SyslType): Unit =
    typ match
      case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType =>
        emit(s"  stb r$srcReg, r$addrReg, r0")
      case SyslType.IntType(16) | SyslType.UIntType(16) =>
        emit(s"  sts r$srcReg, r$addrReg, r0")
      case SyslType.IntType(32) | SyslType.UIntType(32) =>
        emit(s"  stw r$srcReg, r$addrReg, r0")
      case _ =>
        emit(s"  std r$srcReg, r$addrReg, r0")

  // Allocate a local variable on the stack, return its offset from fp.
  // SP must stay 8-byte aligned (pshd/popd require it), so the growth
  // is always rounded up to a multiple of 8.  The variable still uses
  // width-aware loads/stores via its typ.
  private def allocLocal(name: String, typ: SyslType): LocalVar =
    val size = stackSize(typ)
    val oldOffset = stackOffset
    stackOffset -= size
    stackOffset = stackOffset & ~7 // keep 8-byte aligned
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
    emit(s"${fun.name}:")

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
    // Stack args (params 1+) are above saved lr/fp in 64-bit slots
    for (param, i) <- fun.params.zipWithIndex.drop(1) do
      val callerOffset = 16 + (i - 1) * 8 // 8-byte slots above saved lr(+8) and fp(+8)
      locals(param.name) = LocalVar(param.name, callerOffset, SyslType.I64)

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        genExpr(expr) // result in r1
        emit("  mov r7, r5")    // restore stack
        emit("  popd r5")       // restore frame pointer
        emit("  popd r6")       // restore link register
        emit("  jalr r0, r6") // return
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
    emit("  jalr r0, r6")

  // Break/continue label stacks
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, typ, init) =>
        init match
          case TArrayLit(elements, SyslType.ArrayType(elemType, size)) =>
            // Allocate array inline on stack (same layout as TArrayDecl)
            val rawBytes = size * stackSize(elemType)
            val totalBytes = (rawBytes + 7) & ~7
            emitAddImm(7, 7, -totalBytes)
            stackOffset -= totalBytes
            val local = LocalVar(name, stackOffset, typ)
            locals(name) = local
            // Store each element
            for (elem, i) <- elements.zipWithIndex do
              genExpr(elem)
              val off = local.offset + i * stackSize(elemType)
              emitAddImm(2, 5, off)
              emitStore(1, 2, elemType)
          case TStructLit(st @ SyslType.StructType(_, _)) =>
            // Allocate struct on stack and zero-initialize
            val totalSize = stackSize(st)
            val aligned = (totalSize + 7) & ~7
            emitAddImm(7, 7, -aligned)
            stackOffset -= aligned
            val local = LocalVar(name, stackOffset, typ)
            locals(name) = local
            // Zero-fill
            emitAddImm(1, 5, local.offset)
            for i <- 0 until aligned by 8 do
              emitAddImm(2, 1, i)
              emit("  std r0, r2, r0")
          case _ =>
            genExpr(init) // result in r1
            val local = allocLocal(name, typ)
            emitAddImm(2, 5, local.offset)
            emitStore(1, 2, typ)

      case TAssignStmt(target, value) =>
        if locals != null && locals.contains(target) then
          genExpr(value)
          val local = locals(target)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else if globals.contains(target) then
          genExpr(value)
          emit(s"  pshd r1")
          emit(s"  movi r1, $target")
          emit(s"  popd r2")
          emitStore(2, 1, globals(target))
        else
          // New local variable (first assignment = declaration, infer type)
          value match
            case TArrayLit(elements, SyslType.ArrayType(elemType, size)) =>
              val rawBytes = size * stackSize(elemType)
              val totalBytes = (rawBytes + 7) & ~7
              emitAddImm(7, 7, -totalBytes)
              stackOffset -= totalBytes
              val local = LocalVar(target, stackOffset, value.typ)
              locals(target) = local
              for (elem, i) <- elements.zipWithIndex do
                genExpr(elem)
                val off = local.offset + i * stackSize(elemType)
                emitAddImm(2, 5, off)
                emitStore(1, 2, elemType)
            case TStructLit(st @ SyslType.StructType(_, _)) =>
              val totalSize = stackSize(st)
              val aligned = (totalSize + 7) & ~7
              emitAddImm(7, 7, -aligned)
              stackOffset -= aligned
              val local = LocalVar(target, stackOffset, value.typ)
              locals(target) = local
              emitAddImm(1, 5, local.offset)
              for i <- 0 until aligned by 8 do
                emitAddImm(2, 1, i)
                emit("  std r0, r2, r0")
            case _ =>
              genExpr(value)
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
          val gtyp = globals.getOrElse(target, SyslType.I64)
          emit(s"  movi r2, $target")
          emitLoad(1, 2, gtyp)
          emit("  popd r3")
          emitBinOp(op)
          emit(s"  movi r2, $target")
          emitStore(1, 2, gtyp)

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

      case TFieldAssignStmt(obj, fieldIndex, value) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        genExpr(value)             // r1 = value
        emit("  pshd r1")
        emitStructAddr(obj)        // r1 = struct address
        if off != 0 then emitAddImm(1, 1, off)
        emit("  popd r2")          // r2 = value
        emitStore(2, 1, fieldType)

      case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        // Step 1: compute field address and push it (safe from mul d+1 clobber)
        emitStructAddr(obj)        // r1 = struct address
        if off != 0 then emitAddImm(1, 1, off)
        emit("  pshd r1")        // save field address on stack
        // Step 2: load current value
        emitLoad(1, 1, fieldType) // r1 = current field value
        emit("  pshd r1")        // save current value
        // Step 3: compute rhs
        genExpr(value)             // r1 = rhs value
        // Step 4: arithmetic (current op rhs)
        emit("  popd r2")        // r2 = current value
        op match
          case "+"  => emit("  add r2, r2, r1")
          case "-"  => emit("  sub r2, r2, r1")
          case "*"  => emit("  mul r2, r2, r1")
          case "/"  => emit("  div r2, r2, r1")
          case "%"  => emit("  rem r2, r2, r1")
          case "&"  => emit("  and r2, r2, r1")
          case "|"  => emit("  or r2, r2, r1")
          case "^"  => emit("  xor r2, r2, r1")
          case "<<" => emit("  lsl r2, r2, r1")
          case ">>" => emit("  asr r2, r2, r1")
        // Step 5: store result (field address is safely on stack)
        emit("  popd r1")        // r1 = field address
        emitStore(2, 1, fieldType)

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

      case TVarRef(name, typ) =>
        if locals != null && locals.contains(name) then
          val local = locals(name)
          local.typ match
            case _: SyslType.ArrayType | _: SyslType.StructType =>
              emitAddImm(1, 5, local.offset) // arrays/structs: address, not value
            case _ =>
              emitAddImm(2, 5, local.offset)
              emitLoad(1, 2, local.typ)
        else
          emit(s"  movi r1, $name")
          globals.getOrElse(name, SyslType.I64) match
            case _: SyslType.ArrayType | _: SyslType.StructType =>
              () // arrays/structs: address is the value
            case gt =>
              emitLoad(1, 1, gt)

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
        val unsigned = left.typ.isUnsigned
        op match
          case "+"  => emit("  add r1, r1, r2")
          case "-"  => emit("  sub r1, r1, r2")
          case "*"  => emit(if unsigned then "  mulu r1, r1, r2" else "  mul r1, r1, r2")
          case "/"  => emit(if unsigned then "  divu r1, r1, r2" else "  div r1, r1, r2")
          case "%"  => emit(if unsigned then "  remu r1, r1, r2" else "  rem r1, r1, r2")
          case "&"  => emit("  and r1, r1, r2")
          case "|"  => emit("  or r1, r1, r2")
          case "^"  => emit("  xor r1, r1, r2")
          case "<<" => emit("  lsl r1, r1, r2")
          case ">>" => emit(if unsigned then "  lsr r1, r1, r2" else "  asr r1, r1, r2")
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
            emit(if unsigned then "  sltu r1, r1, r2" else "  slt r1, r1, r2")
          case ">" =>
            emit(if unsigned then "  sltu r1, r2, r1" else "  slt r1, r2, r1")
          case "<=" =>
            emit(if unsigned then "  sltu r1, r2, r1" else "  slt r1, r2, r1")
            emit("  ldi r3, 1")
            emit("  xor r1, r1, r3") // flip: 0→1, 1→0
          case ">=" =>
            emit(if unsigned then "  sltu r1, r1, r2" else "  slt r1, r1, r2")
            emit("  ldi r3, 1")
            emit("  xor r1, r1, r3") // flip

      case TPreInc(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r1, r1, $step")
        emitAddImm(2, 5, local.offset)
        emitStore(1, 2, local.typ)

      case TPreDec(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r1, r1, -$step")
        emitAddImm(2, 5, local.offset)
        emitStore(1, 2, local.typ)

      case TPostInc(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r3, r1, $step")
        emitStore(3, 2, local.typ)

      case TPostDec(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        val local = locals(name)
        emitAddImm(2, 5, local.offset)
        emitLoad(1, 2, local.typ)
        emit(s"  addi r3, r1, -$step")
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
            emit("  seb r1, r1")   // sign-extend byte
          case IntType(16) =>
            emit("  ses r1, r1")   // sign-extend short
          case IntType(32) =>
            emit("  sew r1, r1")   // sign-extend word
          case IntType(64) | _: IntType =>
            // no-op — already 64-bit
          case UIntType(8) =>
            emit("  zeb r1, r1")   // zero-extend byte
          case UIntType(16) =>
            emit("  zes r1, r1")   // zero-extend short
          case UIntType(32) =>
            emit("  zew r1, r1")   // zero-extend word
          case UIntType(64) | _: UIntType =>
            // no-op — already 64-bit
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
        if locals != null && locals.contains(name) then
          emitLocalAddr(name, 1) // r1 = stack address of local variable
        else
          emit(s"  movi r1, $name") // r1 = address of global variable

      case TAddrOfIndex(array, index, SyslType.PtrType(elemType)) =>
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array base address
        emit("  popd r2")        // r2 = index
        emit(s"  ldi r3, $elemSize")
        emit("  mul r2, r2, r3") // r2 = index * elemSize
        emit("  add r1, r1, r2") // r1 = base + offset

      case TAddrOfField(obj, fieldIndex, _) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        emitStructAddr(obj)        // r1 = struct address
        if off != 0 then emitAddImm(1, 1, off)
        // r1 = address of field (don't load — just the address)

      case TFieldAccess(obj, fieldIndex, fieldType) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        emitStructAddr(obj)        // r1 = struct address
        if off != 0 then emitAddImm(1, 1, off)
        emitLoad(1, 1, fieldType)  // r1 = field value

      case TDeref(inner, typ) =>
        genExpr(inner)           // r1 = pointer address
        emitLoad(1, 1, typ)      // load with width matching pointee type

      case TIndex(array, index, elemType) if array.typ == SyslType.StringType =>
        // String indexing: bounds-checked byte access
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = string struct address
        emit("  popd r2")        // r2 = index
        // Bounds check: 0 <= index < len
        emit("  addi r3, r1, 8")
        emit("  ldw r3, r3, r0") // r3 = len
        emit("  slt r4, r2, r0") // r4 = (index < 0)
        val boundsOk = newLabel("bounds_ok")
        emit(s"  bne r4, r0, .bounds_error")
        emit("  slt r4, r2, r3") // r4 = (index < len)
        emit(s"  bne r4, r0, $boundsOk")
        emit(".bounds_error")
        emit("  brk")            // trap on out of bounds
        emit(s"$boundsOk")
        // Load byte at ptr + index
        emit("  ldd r1, r1, r0") // r1 = ptr (from struct offset 0)
        emit("  add r1, r1, r2") // r1 = ptr + index
        emit("  ldb r1, r1, r0") // r1 = byte at that address

      case TIndex(array, index, elemType) if array.typ.isInstanceOf[SyslType.SliceType] =>
        // Slice indexing: bounds-checked element access
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = slice struct address
        emit("  popd r2")        // r2 = index
        // Bounds check
        emit("  addi r3, r1, 8")
        emit("  ldw r3, r3, r0") // r3 = len
        emit("  slt r4, r2, r0")
        val boundsOk = newLabel("bounds_ok")
        emit(s"  bne r4, r0, .bounds_error")
        emit("  slt r4, r2, r3")
        emit(s"  bne r4, r0, $boundsOk")
        emit(".bounds_error")
        emit("  brk")
        emit(s"$boundsOk")
        // Load element at ptr + index * elemSize
        emit("  ldd r1, r1, r0") // r1 = ptr
        emit(s"  ldi r3, $elemSize")
        emit("  mul r2, r2, r3")
        emit("  add r1, r1, r2")
        emitLoad(1, 1, elemType)

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
        // Allocate array on stack with proper element size, rounded up to 8
        val elemType = typ match
          case SyslType.ArrayType(e, _) => e
          case _ => SyslType.I64
        val rawBytes = size * stackSize(elemType)
        val totalBytes = (rawBytes + 7) & ~7 // keep SP 8-byte aligned
        emitAddImm(7, 7, -totalBytes)
        emit("  mov r1, r7")     // r1 = address of array start
        stackOffset -= totalBytes

      case TArrayLit(elements, SyslType.ArrayType(elemType, size)) =>
        // Allocate on stack, then store each element
        val rawBytes = size * stackSize(elemType)
        val totalBytes = (rawBytes + 7) & ~7
        emitAddImm(7, 7, -totalBytes)
        stackOffset -= totalBytes
        val baseOffset = stackOffset
        // Store each element
        for (elem, i) <- elements.zipWithIndex do
          genExpr(elem) // r1 = value
          val off = baseOffset + i * stackSize(elemType)
          emitAddImm(2, 5, off)
          emitStore(1, 2, elemType)
        // r1 = base address
        emitAddImm(1, 5, baseOffset)

      case TStringLit(value, _) =>
        // String struct: ptr(8 bytes) + len(4 bytes) = 16 bytes (aligned)
        val bytes = value.getBytes("UTF-8")
        labelCounter += 1
        val strLabel = s"__str_$labelCounter"
        stringLiterals += ((strLabel, value))
        // Allocate 16 bytes on stack for string struct
        emitAddImm(7, 7, -16)
        emit("  mov r1, r7")         // r1 = struct address
        emit(s"  movi r2, $strLabel") // r2 = ptr to static data
        emit("  std r2, r1, r0")      // store ptr at offset 0
        emit(s"  ldi r2, ${bytes.length}")
        emit("  addi r3, r1, 8")
        emit("  stw r2, r3, r0")      // store len at offset 8
        stackOffset -= 16

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

      case TLen(inner, _) =>
        genExpr(inner)           // r1 = struct address (string or slice)
        inner.typ match
          case SyslType.StringType | SyslType.SliceType(_) =>
            emit("  addi r1, r1, 8")
            emit("  ldw r1, r1, r0") // len at offset 8
          case SyslType.ArrayType(_, size) =>
            emit(s"  ldi r1, $size") // compile-time constant
          case _ =>
            emit("  # TODO: len on unsupported type")

      case TCap(inner, _) =>
        genExpr(inner)           // r1 = struct address
        inner.typ match
          case SyslType.SliceType(_) =>
            emit("  addi r1, r1, 12")
            emit("  ldw r1, r1, r0") // cap at offset 12
          case SyslType.ArrayType(_, size) =>
            emit(s"  ldi r1, $size") // cap == size for fixed arrays
          case _ =>
            emit("  # TODO: cap on unsupported type")

      case TFloatLit(d, _) =>
        val bits = java.lang.Double.doubleToRawLongBits(d)
        emit(s"  movi r1, $bits  # float $d")

      case TSizeof(size, _) =>
        if size >= 0 && size <= 255 then
          emit(s"  ldi r1, $size")
        else
          emit(s"  movi r1, $size")

      case TStructLit(st @ SyslType.StructType(_, fields)) =>
        val totalSize = stackSize(st)
        val aligned = (totalSize + 7) & ~7
        emitAddImm(7, 7, -aligned)
        stackOffset -= aligned
        // Zero-initialize the struct via byte fill
        emit("  mov r1, r7")  // r1 = struct base address
        for i <- 0 until aligned by 8 do
          emitAddImm(2, 1, i)
          emit("  std r0, r2, r0")

      case TFieldPreInc(obj, fieldIndex, _) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        emitStructAddr(obj)
        if off != 0 then emitAddImm(1, 1, off)
        emit("  pshd r1")           // save field address
        emitLoad(1, 1, fieldType)    // r1 = current value
        emit("  addi r1, r1, 1")    // increment
        emit("  popd r2")           // r2 = field address
        emitStore(1, 2, fieldType)   // store incremented value
        // r1 = new value (returned)

      case TFieldPreDec(obj, fieldIndex, _) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        emitStructAddr(obj)
        if off != 0 then emitAddImm(1, 1, off)
        emit("  pshd r1")
        emitLoad(1, 1, fieldType)
        emit("  addi r1, r1, -1")
        emit("  popd r2")
        emitStore(1, 2, fieldType)

      case TFieldPostInc(obj, fieldIndex, _) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        emitStructAddr(obj)
        if off != 0 then emitAddImm(1, 1, off)
        emit("  pshd r1")           // save field address
        emitLoad(1, 1, fieldType)    // r1 = current value (return this)
        emit("  addi r3, r1, 1")    // r3 = incremented
        emit("  popd r2")           // r2 = field address
        emitStore(3, 2, fieldType)   // store incremented value
        // r1 = old value (returned)

      case TFieldPostDec(obj, fieldIndex, _) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        emitStructAddr(obj)
        if off != 0 then emitAddImm(1, 1, off)
        emit("  pshd r1")
        emitLoad(1, 1, fieldType)
        emit("  addi r3, r1, -1")
        emit("  popd r2")
        emitStore(3, 2, fieldType)

      case _ =>
        emit(s"  # TODO: ${expr.getClass.getSimpleName}")

  // Emit reg = base + offset, handling large offsets that don't fit in addi
  private def emitAddImm(destReg: Int, baseReg: Int, offset: Int): Unit =
    if offset >= -64 && offset <= 63 then
      emit(s"  addi r$destReg, r$baseReg, $offset")
    else
      // Use a temp register to avoid clobbering baseReg when destReg == baseReg
      val tmp = if destReg == 3 then 2 else 3
      if offset >= 0 then
        emit(s"  movi r$tmp, $offset")
        emit(s"  add r$destReg, r$baseReg, r$tmp")
      else
        emit(s"  movi r$tmp, ${-offset}")
        emit(s"  sub r$destReg, r$baseReg, r$tmp")

  // Compute byte offset of field at given index within a struct type
  private def fieldOffset(structType: SyslType.StructType, fieldIndex: Int): Int =
    var offset = 0
    for (_, typ) <- structType.fields.take(fieldIndex) do
      val align = stackAlign(typ)
      offset = ((offset + align - 1) / align) * align
      offset += typ.sizeOf.toInt
    // Align the target field itself
    if fieldIndex < structType.fields.length then
      val align = stackAlign(structType.fields(fieldIndex)._2)
      offset = ((offset + align - 1) / align) * align
    offset

  // Emit code to compute the address of a struct from a TFieldAccess obj expression.
  // The analyzer wraps pointer-to-struct access in TDeref, so we unwrap it to get the address.
  private def emitStructAddr(obj: TExpr): Unit =
    obj match
      case TDeref(ptr, _) => genExpr(ptr) // pointer to struct — address is the pointer value
      case _ => genExpr(obj) // struct value (local/global) — genExpr produces address for struct types

  // Data directive for a type: db (1 byte), ds (2), dw (4), dl (8)
  private def emitDataDirective(typ: SyslType): String = typ match
    case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType => "db"
    case SyslType.IntType(16) | SyslType.UIntType(16) => "ds"
    case SyslType.IntType(32) | SyslType.UIntType(32) => "dw"
    case _ => "dl"

  // Emit address of local variable into target register
  private def emitLocalAddr(name: String, reg: Int): Unit =
    val local = locals(name)
    emitAddImm(reg, 5, local.offset)

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
