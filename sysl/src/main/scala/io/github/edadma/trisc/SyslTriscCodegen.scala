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

    // Collect globals into data (initialized) and bss (zero-initialized) lists
    val dataGlobals = new mutable.ListBuffer[TDecl]
    val bssGlobals = new mutable.ListBuffer[TDecl]

    for decl <- program.decls do
      decl match
        case v @ TVarDecl(_, typ, init, _) =>
          globals(v.name) = typ
          if isZeroInit(typ, init) then bssGlobals += v
          else dataGlobals += v
        case _ => // functions, externs, types — handled below

    // Emit code segment — functions
    emit("segment code")
    for decl <- program.decls do
      decl match
        case f: TFunDecl => genFunction(f)
        case _ => // skip

    // Emit rodata segment — string literals
    if stringLiterals.nonEmpty then
      emit("segment rodata")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"global $label, data, ${bytes.length + 1}")
      emit("  align 8")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"$label:")
        for b <- bytes do emit(s"  db ${b & 0xff}")
        emit("  db 0") // null terminator

    // Emit data segment — initialized globals
    if dataGlobals.nonEmpty then
      emit("segment data")
      for decl <- dataGlobals do
        decl match
          case TVarDecl(name, typ, init, _) =>
            val align = stackAlign(typ)
            if align > 1 then emit(s"  align $align")
            emit(s"# global: $name")
            emit(s"$name:")
            init match
              case TArrayLit(elements, _) =>
                val declElemType = typ match
                  case SyslType.ArrayType(e, _) => e
                  case _ => SyslType.I64
                val elemDir = emitDataDirective(declElemType)
                for elem <- elements do
                  constEval(elem) match
                    case Some(n) => emit(s"  $elemDir $n")
                    case None => emit(s"  $elemDir 0")
              case _ =>
                val directive = emitDataDirective(typ)
                constEval(init) match
                  case Some(n) => emit(s"  $directive $n")
                  case None => emit(s"  $directive 0")
          case _ =>

    // Emit bss segment — zero-initialized globals (arrays, structs, uninitialized)
    if bssGlobals.nonEmpty then
      emit("segment bss")
      for decl <- bssGlobals do
        decl match
          case TVarDecl(name, typ, _, _) =>
            val align = stackAlign(typ)
            if align > 1 then emit(s"  align $align")
            emit(s"# global: $name")
            emit(s"$name:")
            typ match
              case SyslType.ArrayType(elem, count) =>
                emit(s"  rb ${stackSize(elem) * count}")
              case _: SyslType.StructType =>
                emit(s"  rb ${stackSize(typ)}")
              case _ =>
                emit(s"  rb ${stackSize(typ)}")
          case _ =>

    out.toString

  private case class LocalVar(name: String, offset: Int, typ: SyslType)

  private val globals = new mutable.LinkedHashMap[String, SyslType]
  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var stackOffset: Int = 0
  private val savedScopes = new mutable.Stack[(Map[String, LocalVar], Int)]
  private val loopScopeOffsets = new mutable.Stack[Int]

  private def enterScope(): Unit =
    savedScopes.push((locals.toMap, stackOffset))

  private def leaveScope(): Unit =
    val (savedLocals, savedOffset) = savedScopes.pop()
    // Decrement refcounts for ref-typed locals leaving scope
    for (name, local) <- locals if !savedLocals.contains(name) do
      local.typ match
        case rt: SyslType.RefType =>
          val hoff = refHeaderOffset(rt)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")
          emitRefDecr(1, hoff)
          emit("  popd r1")
        case _ =>
    locals.clear()
    locals ++= savedLocals
    if stackOffset != savedOffset then
      emitAddImm(7, 7, savedOffset - stackOffset)
      stackOffset = savedOffset
  private var currentFunction: TFunDecl = null

  // Determine if a global variable should go in bss (zero-initialized) vs data
  private def isZeroInit(typ: SyslType, init: TExpr): Boolean =
    init match
      case TArrayLit(_, _) => false // array literal has explicit values → data
      case _ =>
        typ match
          case SyslType.ArrayType(_, _) => true // uninitialized array → bss
          case _: SyslType.StructType => true // struct → bss
          case _ =>
            constEval(init) match
              case Some(0) => true // explicitly zero → bss
              case None => true // no initializer → bss
              case _ => false // nonzero constant → data

  // Does this return type require a caller-allocated return slot?
  private def returnsViaPointer(typ: SyslType): Boolean = typ.isInstanceOf[SyslType.StructType]

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

  // Try to evaluate a constant expression at compile time.
  // Returns Some(value) for integer constants, None otherwise.
  private def constEval(expr: TExpr): Option[Long] = expr match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(b, _) => Some(if b then 1 else 0)
    case TUnary("-", operand, _) => constEval(operand).map(-_)
    case TUnary("~", operand, _) => constEval(operand).map(~_)
    case TBinary(left, "+", right, _) => for l <- constEval(left); r <- constEval(right) yield l + r
    case TBinary(left, "-", right, _) => for l <- constEval(left); r <- constEval(right) yield l - r
    case TBinary(left, "*", right, _) => for l <- constEval(left); r <- constEval(right) yield l * r
    case _ => None

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
      case st: SyslType.StructType =>
        // Struct copy: srcReg = source address, addrReg = dest address
        val size = stackSize(st)
        for i <- 0 until size by 8 do
          emitAddImm(4, srcReg, i)
          emit("  ldd r4, r4, r0")
          emitAddImm(3, addrReg, i)
          emit("  std r4, r3, r0")
      case _ =>
        emit(s"  std r$srcReg, r$addrReg, r0")

  // Refcount header offset: structs have 8-byte header, slices have 16-byte header (refcount + length)
  private def refHeaderOffset(typ: SyslType): Int = typ match
    case SyslType.RefType(SyslType.SliceType(_)) => 16
    case _ => 8

  // Emit refcount increment: ptr in rPtr, refcount is at [rPtr - headerOffset]
  // Clobbers r3, r4. Skips if rPtr == 0 (null).
  private def emitRefIncr(ptrReg: Int, headerOff: Int = 8): Unit =
    val skip = newLabel("skip_incr")
    emit(s"  beq r$ptrReg, r0, $skip")
    emitAddImm(3, ptrReg, -headerOff) // r3 = &refcount
    emit("  ldd r4, r3, r0")          // r4 = refcount
    emit("  addi r4, r4, 1")          // r4++
    emit("  std r4, r3, r0")          // store back
    emit(s"$skip")

  // Emit refcount decrement + free-at-zero: ptr in rPtr, refcount at [rPtr - headerOffset]
  // Clobbers r3, r4. Skips if rPtr == 0 (null). Calls free(base) when refcount hits 0.
  private def emitRefDecr(ptrReg: Int, headerOff: Int = 8): Unit =
    val skip = newLabel("skip_decr")
    val noFree = newLabel("no_free")
    emit(s"  beq r$ptrReg, r0, $skip")
    emitAddImm(3, ptrReg, -headerOff) // r3 = &refcount (also base for free)
    emit("  ldd r4, r3, r0")          // r4 = refcount
    emit("  addi r4, r4, -1")         // r4--
    emit("  std r4, r3, r0")          // store back
    emit(s"  bne r4, r0, $noFree")
    // refcount == 0 → free(base)
    emit("  pshd r1")                 // save r1
    emit("  mov r1, r3")              // r1 = base pointer
    emit("  movi r4, free")
    emit("  jalr r6, r4")
    emit("  popd r1")                 // restore r1
    emit(s"$noFree")
    emit(s"$skip")

  // Decrement refcounts for all ref-typed locals in the current scope
  private def emitRefCleanup(): Unit =
    for (_, local) <- locals do
      local.typ match
        case rt: SyslType.RefType =>
          val hoff = refHeaderOffset(rt)
          emit("  pshd r1")         // save r1 (may hold return value)
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")  // r1 = ref pointer
          emitRefDecr(1, hoff)
          emit("  popd r1")         // restore r1
        case _ =>

  // Allocate a local variable on the stack, return its offset from fp.
  // The variable is aligned to the greater of its natural alignment and 8
  // (pshd/popd require SP to stay 8-byte aligned).
  private def allocLocal(name: String, typ: SyslType): LocalVar =
    val size = stackSize(typ)
    val align = stackAlign(typ).max(8) // type alignment, but at least 8 for SP
    val mask = ~(align - 1)
    val oldOffset = stackOffset
    stackOffset -= size
    stackOffset = stackOffset & mask
    val growth = oldOffset - stackOffset
    emitAddImm(7, 7, -growth)
    val local = LocalVar(name, stackOffset, typ)
    locals(name) = local
    local

  private def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    stackOffset = 0
    deferStack.clear()

    val structReturn = returnsViaPointer(fun.returnType)

    emit(s"# function: ${fun.name}")
    emit(s"${fun.name}:")

    // ABI: params 0-2 in r1-r3, params 3+ on caller's stack
    // If the function returns a struct, r1 = hidden return pointer (before user params).
    // User params shift: param 0 in r2, param 1 in r3, param 2+ on stack.
    val allRegSlots = if structReturn then 1 + fun.params.length else fun.params.length
    val nRegPushed = allRegSlots.min(3)
    // Save register params BEFORE prologue so they have known offsets from fp
    for i <- 0 until nRegPushed do
      emit(s"  pshd r${i + 1}")   // push r1, r2, r3 in order

    // Prologue: save lr, fp, set up frame
    emit("  pshd r6")       // save link register
    emit("  pshd r5")       // save frame pointer
    emit("  mov r5, r7")    // frame pointer = stack pointer

    // Register params are above saved lr/fp on the stack:
    //   [r5+0] = saved r5, [r5+8] = saved r6/lr
    //   [r5+16] = last pushed param, ... [r5+16+(nRegPushed-1)*8] = first pushed param
    val retPtrOffset = if structReturn then
      // Hidden return pointer was in r1, pushed first among register args
      val off = 16 + (nRegPushed - 1) * 8  // r1 was pushed first, so it's at the highest offset
      locals("_ret_ptr") = LocalVar("_ret_ptr", off, SyslType.PtrType(fun.returnType))
      off
    else -1

    // Map user params to their stack locations
    val userParamRegStart = if structReturn then 1 else 0  // user params start at r2 if struct return
    val userRegParams = fun.params.length.min(3 - userParamRegStart)
    for (param, i) <- fun.params.take(userRegParams).zipWithIndex do
      val regIndex = userParamRegStart + i  // which register slot (0-based from r1)
      val callerOffset = 16 + (nRegPushed - 1 - regIndex) * 8
      locals(param.name) = LocalVar(param.name, callerOffset, SyslType.I64)
    // Stack params: those beyond register capacity
    val nUserStackStart = 3 - userParamRegStart  // how many user params fit in registers
    for (param, i) <- fun.params.zipWithIndex.drop(nUserStackStart) do
      val callerOffset = 16 + nRegPushed * 8 + (i - nUserStackStart) * 8
      locals(param.name) = LocalVar(param.name, callerOffset, SyslType.I64)

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        genExpr(expr) // result in r1
        if structReturn then emitStructReturn()
        emitDefers()
        emitRefCleanup()
        emitEpilogue()
      case TBlockBody(stmts) =>
        genBlock(stmts)

    locals = null
    currentFunction = null

  private def genBlock(stmts: List[TStmt]): Unit =
    val sr = currentFunction != null && returnsViaPointer(currentFunction.returnType)
    if stmts.nonEmpty then
      for stmt <- stmts.init do genStmt(stmt)
      stmts.last match
        case TExprStmt(expr) =>
          genExpr(expr) // result in r1
          if sr then emitStructReturn()
          emitDefers()
          emitRefCleanup()
          emitEpilogue()
        case other =>
          genStmt(other)
          // If no explicit return, return 0
          emit("  ldi r1, 0")
          emitDefers()
          emitRefCleanup()
          emitEpilogue()
    else
      emit("  ldi r1, 0")
      emitDefers()
      emitRefCleanup()
      emitEpilogue()

  // Emit binary operation: r1 = r1 op r3
  private def emitBinOp(op: String): Unit =
    op match
      case "+"  => emit("  add r1, r1, r3")
      case "-"  => emit("  sub r1, r1, r3")
      case "*"  => emit("  mul r1, r1, r3")
      case "/"  => emit("  div r1, r1, r3")
      case "%"  => emit("  div r1, r1, r3"); emit("  mov r1, r2") // remainder in r2
      case "&"  => emit("  and r1, r1, r3")
      case "|"  => emit("  or r1, r1, r3")
      case "^"  => emit("  xor r1, r1, r3")
      case "<<" => emit("  lsl r1, r1, r3")
      case ">>" => emit("  asr r1, r1, r3")

  // Copy struct from src address (r1) to _ret_ptr, then set r1 = _ret_ptr
  private def emitStructReturn(): Unit =
    val st = currentFunction.returnType.asInstanceOf[SyslType.StructType]
    val size = stackSize(st)
    val retLocal = locals("_ret_ptr")
    // r1 = source struct address; load _ret_ptr into r2
    emit("  pshd r1")                       // save source
    emitAddImm(2, 5, retLocal.offset)
    emit("  ldd r2, r2, r0")               // r2 = _ret_ptr (destination)
    emit("  popd r3")                       // r3 = source
    // Copy size bytes from r3 to r2
    for i <- 0 until size by 8 do
      emitAddImm(4, 3, i)
      emit("  ldd r4, r4, r0")
      emitAddImm(1, 2, i)
      emit("  std r4, r1, r0")
    // r1 = _ret_ptr (for the caller)
    emit("  mov r1, r2")

  private def emitDefers(): Unit =
    if deferStack.nonEmpty then
      emit("  pshd r1") // save return value
      for stmt <- deferStack.reverseIterator do
        genStmt(stmt)
      emit("  popd r1") // restore return value

  private def emitEpilogue(): Unit =
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    // Skip past pre-prologue pushed register params (including hidden return ptr if any)
    val nRegPushed = if currentFunction != null then
      val sr = returnsViaPointer(currentFunction.returnType)
      val allSlots = (if sr then 1 else 0) + currentFunction.params.length
      allSlots.min(3)
    else 0
    if nRegPushed > 0 then
      emitAddImm(7, 7, nRegPushed * 8)
    emit("  jalr r0, r6")

  // Break/continue label stacks
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]

  // Defer stack — deferred statements executed in LIFO order before return/epilogue
  private val deferStack = new mutable.ArrayBuffer[TStmt]

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
          case TStructConstruct(st, args) =>
            // Allocate struct on stack, zero-initialize, then set fields from args
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
            // Initialize fields from constructor args
            for (arg, i) <- args.zipWithIndex do
              val (_, fieldType) = st.fields(i)
              val off = fieldOffset(st, i)
              genExpr(arg)                              // r1 = field value
              emitAddImm(2, 5, local.offset + off)     // r2 = field address (via fp)
              emitStore(1, 2, fieldType)
          case call @ TCall(_, _, retType) if returnsViaPointer(retType) =>
            // Function returns struct via caller-allocated slot.
            // genExpr allocates the return slot and returns its address in r1.
            // The slot is already on our stack at the current stackOffset after genExpr.
            genExpr(call)
            // r1 = address of return slot. Register the local at the slot's stack position.
            // The return slot was the last thing allocated, so it's at stackOffset.
            locals(name) = LocalVar(name, stackOffset, typ)
          case _ =>
            genExpr(init) // result in r1
            typ match
              case rt: SyslType.RefType => emitRefIncr(1, refHeaderOffset(rt))
              case _ =>
            val local = allocLocal(name, typ)
            emitAddImm(2, 5, local.offset)
            emitStore(1, 2, typ)

      case TDestructureStmt(names, types, init) =>
        // Evaluate the tuple (struct) — result is struct base address in r1
        genExpr(init)
        // Save tuple base address as a hidden local (stable across further allocations)
        val tmpLocal = allocLocal(s"_tup$$${newLabel("t")}", SyslType.PtrType(init.typ))
        emitAddImm(2, 5, tmpLocal.offset)
        emit("  std r1, r2, r0")
        val st = init.typ.asInstanceOf[SyslType.StructType]
        // Extract each field into a new local
        for ((name, fieldType), i) <- names.zip(types).zipWithIndex do
          val off = fieldOffset(st, i)
          emitAddImm(1, 5, tmpLocal.offset)
          emit("  ldd r1, r1, r0")  // r1 = tuple address
          if off != 0 then emitAddImm(1, 1, off)
          emitLoad(1, 1, fieldType)  // r1 = field value
          val local = allocLocal(name, fieldType)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, fieldType)

      case TAssignStmt(target, value) =>
        if locals != null && locals.contains(target) then
          val local = locals(target)
          local.typ match
            case rt: SyslType.RefType =>
              val hoff = refHeaderOffset(rt)
              // Decrement old ref before overwrite
              emitAddImm(1, 5, local.offset)
              emit("  ldd r1, r1, r0")
              emitRefDecr(1, hoff)
              genExpr(value)
              emitRefIncr(1, hoff) // increment new ref
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)
            case _ =>
              genExpr(value)
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)
        else if globals.contains(target) then
          val gtyp = globals(target)
          gtyp match
            case rt: SyslType.RefType =>
              val hoff = refHeaderOffset(rt)
              // Decrement old global ref
              emit(s"  movi r1, $target")
              emit("  ldd r1, r1, r0")
              emitRefDecr(1, hoff)
              genExpr(value)
              emitRefIncr(1, hoff)
              emit("  pshd r1")
              emit(s"  movi r1, $target")
              emit("  popd r2")
              emitStore(2, 1, gtyp)
            case _ =>
              genExpr(value)
              emit(s"  pshd r1")
              emit(s"  movi r1, $target")
              emit(s"  popd r2")
              emitStore(2, 1, gtyp)
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
        if currentFunction != null && returnsViaPointer(currentFunction.returnType) then
          emitStructReturn()
        emitDefers()
        emitRefCleanup()
        emitEpilogue()

      case TReturnStmt(None) =>
        emit("  ldi r1, 0")
        emitDefers()
        emitRefCleanup()
        emitEpilogue()

      case TDeferStmt(body) =>
        deferStack += body

      case TExprStmt(expr) =>
        genExpr(expr) // result in r1, discarded

      case TWhileStmt(cond, body) =>
        val loopLabel = newLabel("while")
        val endLabel = newLabel("endwhile")
        breakLabels.push(endLabel)
        continueLabels.push(loopLabel)
        loopScopeOffsets.push(stackOffset)
        emit(s"$loopLabel")
        genExpr(cond)
        emit(s"  beq r1, r0, $endLabel")
        enterScope()
        for stmt <- body do genStmt(stmt)
        leaveScope()
        emit(s"  bra $loopLabel")
        emit(s"$endLabel")
        loopScopeOffsets.pop()
        breakLabels.pop()
        continueLabels.pop()

      case TForStmt(init, cond, update, body) =>
        val loopLabel = newLabel("for")
        val updateLabel = newLabel("forupdate")
        val endLabel = newLabel("endfor")
        enterScope()
        genStmt(init)
        breakLabels.push(endLabel)
        continueLabels.push(updateLabel)
        loopScopeOffsets.push(stackOffset)
        emit(s"$loopLabel")
        genExpr(cond)
        emit(s"  beq r1, r0, $endLabel")
        enterScope()
        for stmt <- body do genStmt(stmt)
        leaveScope()
        emit(s"$updateLabel")
        genStmt(update)
        emit(s"  bra $loopLabel")
        emit(s"$endLabel")
        loopScopeOffsets.pop()
        breakLabels.pop()
        continueLabels.pop()
        leaveScope()

      case TDoWhileStmt(cond, body) =>
        val loopLabel = newLabel("dowhile")
        val endLabel = newLabel("enddowhile")
        breakLabels.push(endLabel)
        continueLabels.push(loopLabel)
        loopScopeOffsets.push(stackOffset)
        emit(s"$loopLabel")
        enterScope()
        for stmt <- body do genStmt(stmt)
        leaveScope()
        genExpr(cond)
        emit(s"  bne r1, r0, $loopLabel")
        emit(s"$endLabel")
        loopScopeOffsets.pop()
        breakLabels.pop()
        continueLabels.pop()

      case TBreakStmt =>
        val loopOffset = loopScopeOffsets.top
        if stackOffset != loopOffset then
          emitAddImm(7, 7, loopOffset - stackOffset)
        emit(s"  bra ${breakLabels.top}")

      case TContinueStmt =>
        val loopOffset = loopScopeOffsets.top
        if stackOffset != loopOffset then
          emitAddImm(7, 7, loopOffset - stackOffset)
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
          case SyslType.RefType(SyslType.SliceType(e)) => e
          case _ => SyslType.I64
        val elemSize = stackSize(elemType)
        genExpr(value)           // r1 = value
        emit("  pshd r1")       // save as 64-bit temp
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array base address
        emit("  popd r2")        // r2 = index
        emitLoadImm(3, elemSize)
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
          case "%"  => emit("  div r2, r2, r1"); emit("  mov r2, r3") // remainder in r3
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
        emitLoadImm(3, elemSize)
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
          case "%"  => emit(if unsigned then "  divu r1, r1, r2" else "  div r1, r1, r2"); emit("  mov r1, r2") // remainder in r2
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

      case TCall(name, args, retType) =>
        val callStructReturn = returnsViaPointer(retType)
        // If struct return, allocate space on caller's stack for the return value
        // and prepend hidden pointer as first arg
        val retSlotOffset = if callStructReturn then
          val st = retType.asInstanceOf[SyslType.StructType]
          val size = stackSize(st)
          val aligned = (size + 7) & ~7
          emitAddImm(7, 7, -aligned)
          stackOffset -= aligned
          // Zero-initialize the return slot
          emit("  mov r1, r7")
          for i <- 0 until aligned by 8 do
            emitAddImm(2, 1, i)
            emit("  std r0, r2, r0")
          stackOffset  // remember where the return slot is
        else 0

        // Build full arg list (with hidden pointer prepended for struct return)
        val allArgs = if callStructReturn then
          // The hidden arg is the address of the return slot (current r7)
          TAddrLit(retSlotOffset) :: args
        else args

        // ABI: args 0-2 in r1-r3, args 3+ on stack (right-to-left)
        // r4 is reserved for the call address (movi r4, name)
        val nRegArgs = allArgs.length.min(3)
        val stackArgs = allArgs.drop(3)
        // Track stack before arg evaluation (genExpr may allocate temps)
        val savedOffset = stackOffset
        // Push stack args (3+) right-to-left
        for arg <- stackArgs.reverse do
          arg match
            case TAddrLit(off) => emitAddImm(1, 5, off)
            case _ => genExpr(arg)
          emit("  pshd r1")
          stackOffset -= 8
        // Evaluate register args in reverse, push as temporaries
        for arg <- allArgs.take(nRegArgs).reverse do
          arg match
            case TAddrLit(off) => emitAddImm(1, 5, off)
            case _ => genExpr(arg)
          emit("  pshd r1")
          stackOffset -= 8
        // Pop into r1-rN
        for i <- 0 until nRegArgs do
          emit(s"  popd r${i + 1}")
          stackOffset += 8
        // Call
        emit(s"  movi r4, $name")
        emit("  jalr r6, r4")
        // Clean up stack args (NOT the return slot — caller needs it)
        val argsAllocated = savedOffset - stackOffset
        if argsAllocated != 0 then
          emitAddImm(7, 7, argsAllocated)
          stackOffset = savedOffset
        // For struct return, r1 = pointer to return slot (which is on our stack)

      case TIndirectCall(callee, args, _) =>
        // ABI: args 0-2 in r1-r3, args 3+ on stack
        val nRegArgs = args.length.min(3)
        val stackArgs = args.drop(3)
        for arg <- stackArgs.reverse do
          genExpr(arg)
          emit("  pshd r1")
        // Evaluate register args in reverse, push as temporaries
        for arg <- args.take(nRegArgs).reverse do
          genExpr(arg)
          emit("  pshd r1")
        // Evaluate callee (function pointer) — push to save
        genExpr(callee)
        emit("  pshd r1")
        // Pop callee into r4, then pop register args into r1-rN
        emit("  popd r4")
        for i <- 0 until nRegArgs do
          emit(s"  popd r${i + 1}")
        emit("  jalr r6, r4")
        // Clean up stack args
        if stackArgs.nonEmpty then
          val stackArgBytes = stackArgs.length * 8
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
        emitLoadImm(3, elemSize)
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
        emitLoadImm(3, elemSize)
        emit("  mul r2, r2, r3")
        emit("  add r1, r1, r2")
        emitLoad(1, 1, elemType)

      case TIndex(array, index, elemType) if array.typ.isInstanceOf[SyslType.RefType] =>
        // &[]T indexing: data pointer at r1, length at [r1 - 8]
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        stackOffset -= 8
        genExpr(array)           // r1 = data pointer
        emit("  popd r2")        // r2 = index
        stackOffset += 8
        // Bounds check: load length from [r1 - 8]
        emitAddImm(3, 1, -8)
        emit("  ldd r3, r3, r0") // r3 = length
        // TODO: emit trap/abort if r2 >= r3 (bounds check)
        emitLoadImm(3, elemSize)
        emit("  mul r2, r2, r3") // r2 = index * elemSize
        emit("  add r1, r1, r2") // r1 = element address
        elemType match
          case _: SyslType.StructType | _: SyslType.ArrayType => ()
          case _ => emitLoad(1, 1, elemType)

      case TIndex(array, index, elemType) =>
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array base address
        emit("  popd r2")        // r2 = index
        emitLoadImm(3, elemSize)
        emit("  mul r2, r2, r3") // r2 = index * elemSize
        emit("  add r1, r1, r2") // r1 = element address
        elemType match
          case _: SyslType.StructType | _: SyslType.ArrayType => () // address is the value for aggregates
          case _ => emitLoad(1, 1, elemType) // load scalar with proper width

      case TArrayDecl(size, typ) =>
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
        // Emit pointer to static string data (null-terminated)
        val bytes = value.getBytes("UTF-8")
        labelCounter += 1
        val strLabel = s"__str_$labelCounter"
        stringLiterals += ((strLabel, value))
        emit(s"  movi r1, $strLabel") // r1 = ptr to static string data

      case TIfExpr(cond, thenBody, elseBody, _) =>
        val elseLabel = newLabel("else")
        val endLabel = newLabel("endif")
        genExpr(cond)
        emit(s"  beq r1, r0, $elseLabel")
        enterScope()
        for stmt <- thenBody do genStmt(stmt)
        leaveScope()
        emit(s"  bra $endLabel")
        emit(s"$elseLabel")
        elseBody.foreach { stmts =>
          enterScope()
          for stmt <- stmts do genStmt(stmt)
          leaveScope()
        }
        emit(s"$endLabel")

      case TLen(inner, _) =>
        genExpr(inner)           // r1 = struct address (string or slice)
        inner.typ match
          case SyslType.StringType | SyslType.SliceType(_) =>
            emit("  addi r1, r1, 8")
            emit("  ldw r1, r1, r0") // len at offset 8
          case SyslType.ArrayType(_, size) =>
            emitLoadImm(1, size) // compile-time constant
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // r1 = data pointer; length is at [r1 - 8]
            emitAddImm(1, 1, -8)
            emit("  ldd r1, r1, r0")
          case _ =>
            emit("  # TODO: len on unsupported type")

      case TCap(inner, _) =>
        genExpr(inner)           // r1 = struct address
        inner.typ match
          case SyslType.SliceType(_) =>
            emit("  addi r1, r1, 12")
            emit("  ldw r1, r1, r0") // cap at offset 12
          case SyslType.ArrayType(_, size) =>
            emitLoadImm(1, size) // cap == size for fixed arrays
          case _ =>
            emit("  # TODO: cap on unsupported type")

      case TFloatLit(d, _) =>
        val bits = java.lang.Double.doubleToRawLongBits(d)
        emit(s"  movi r1, $bits  # float $d")

      case TSizeof(size, _) =>
        emitLoadImm(1, size.toInt)

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

      case TNewArray(elemType, sizeExpr) =>
        // Heap-allocate ref-counted array: [refcount_i64 | length_i64 | elem0 | elem1 | ...]
        val elemSize = stackSize(elemType)
        // Compute total allocation size: 16 + ((n * elemSize + 7) & ~7)
        genExpr(sizeExpr) // r1 = n
        emit("  pshd r1")  // save n
        stackOffset -= 8
        val nOffset = stackOffset
        emitLoadImm(2, elemSize)
        emit("  mul r1, r1, r2")     // r1 = n * elemSize
        // Round data size up to 8-byte boundary: (size + 7) & ~7
        emit("  addi r1, r1, 7")
        emit("  not r2, r0")         // r2 = ~0 = -1
        emit("  addi r2, r2, -6")    // r2 = -7... no
        // Simpler: shift right 3, shift left 3 to clear low 3 bits
        emit("  movi r2, 3")
        emit("  lsr r1, r1, r2")     // r1 = (size+7) >> 3
        emit("  lsl r1, r1, r2")     // r1 = ((size+7) >> 3) << 3 = aligned
        emitAddImm(1, 1, 16)         // r1 = 16 + aligned data size
        // Call malloc
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")
        // r1 = allocated pointer. Save it.
        emit("  pshd r1")
        stackOffset -= 8
        val ptrOffset = stackOffset
        // Initialize refcount = 1
        emitLoadImm(2, 1)
        emit("  std r2, r1, r0")     // [ptr+0] = refcount
        // Store length
        emitAddImm(2, 5, nOffset)
        emit("  ldd r2, r2, r0")     // r2 = n
        emitAddImm(3, 1, 8)
        emit("  std r2, r3, r0")     // [ptr+8] = length
        // Zero-fill data area: ptr+16 for the allocated (aligned) data region
        val loopLabel = newLabel("zero_loop")
        val doneLabel = newLabel("zero_done")
        emitAddImm(3, 1, 16)         // r3 = data start
        emitLoadImm(4, elemSize)
        emit("  mul r2, r2, r4")     // r2 = n * elemSize
        // Round up to 8-byte boundary
        emit("  addi r2, r2, 7")
        emit("  movi r4, 3")
        emit("  lsr r2, r2, r4")
        emit("  lsl r2, r2, r4")     // r2 = aligned data size
        emit("  add r2, r3, r2")     // r2 = data end
        emit(s"$loopLabel")
        emit(s"  beq r3, r2, $doneLabel")
        emit("  std r0, r3, r0")     // zero 8 bytes
        emitAddImm(3, 3, 8)
        emit(s"  bra $loopLabel")
        emit(s"$doneLabel")
        // r1 = data pointer (past header)
        emitAddImm(1, 5, ptrOffset)
        emit("  ldd r1, r1, r0")
        emitAddImm(1, 1, 16)         // skip refcount + length
        // Clean up temps
        emitAddImm(7, 7, 16)
        stackOffset += 16

      case TNew(st, args) =>
        // Heap-allocate ref-counted struct: [refcount_i64 | fields...]
        val dataSize = stackSize(st)
        val totalAlloc = dataSize + 8 // 8 bytes for refcount header
        // Call malloc(totalAlloc) — result in r1
        emitLoadImm(1, totalAlloc)
        emit("  pshd r1")
        stackOffset -= 8
        // Convert to i64 arg
        emit("  popd r1")
        stackOffset += 8
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")
        // r1 = allocated pointer. Save it as a temp on stack.
        emit("  pshd r1")
        stackOffset -= 8
        val ptrOffset = stackOffset
        // Initialize refcount = 1
        emitLoadImm(2, 1)
        emit("  std r2, r1, r0") // store refcount at [ptr+0]
        // Zero-fill data area
        emitAddImm(1, 1, 8) // r1 = data start
        for i <- 0 until ((dataSize + 7) & ~7) by 8 do
          emitAddImm(2, 1, i)
          emit("  std r0, r2, r0")
        // Store each field
        for (arg, i) <- args.zipWithIndex do
          val (_, fieldType) = st.fields(i)
          val off = fieldOffset(st, i)
          genExpr(arg) // r1 = value
          // Reload base pointer from stack
          emitAddImm(3, 5, ptrOffset)
          emit("  ldd r3, r3, r0") // r3 = malloc result
          emitAddImm(2, 3, 8 + off) // r2 = field address (past header)
          emitStore(1, 2, fieldType)
        // r1 = data pointer (past refcount header)
        emitAddImm(1, 5, ptrOffset)
        emit("  ldd r1, r1, r0")
        emitAddImm(1, 1, 8)
        // Clean up temp
        emitAddImm(7, 7, 8)
        stackOffset += 8

      case TStructConstruct(st, args) =>
        // Allocate struct on stack and zero-initialize
        val totalSize = stackSize(st)
        val aligned = (totalSize + 7) & ~7
        emitAddImm(7, 7, -aligned)
        stackOffset -= aligned
        val structBaseOffset = stackOffset  // fp-relative offset of the struct
        emit("  mov r1, r7")
        for i <- 0 until aligned by 8 do
          emitAddImm(2, 1, i)
          emit("  std r0, r2, r0")
        // Evaluate each arg and store into the corresponding field.
        // Use fp-relative addressing since genExpr(arg) may move SP.
        for (arg, i) <- args.zipWithIndex do
          val (_, fieldType) = st.fields(i)
          val off = fieldOffset(st, i)
          genExpr(arg)                                    // r1 = field value
          emitAddImm(2, 5, structBaseOffset + off)       // r2 = field address (fp-relative)
          emitStore(1, 2, fieldType)
        // r1 = struct base address (fp-relative, stable)
        emitAddImm(1, 5, structBaseOffset)

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

  // Emit reg = immediate value, choosing ldi (byte range) or movi (larger)
  private def emitLoadImm(reg: Int, value: Int): Unit =
    if value >= 0 && value <= 255 then emit(s"  ldi r$reg, $value")
    else emit(s"  movi r$reg, $value")

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
      case TFieldAccess(innerObj, fieldIndex, _) =>
        // Embedded struct — compute address of the field without loading
        val st = innerObj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        emitStructAddr(innerObj)  // r1 = parent struct address
        if off != 0 then emitAddImm(1, 1, off)
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
