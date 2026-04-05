package io.github.edadma.trisc

import scala.collection.mutable

class SyslTriscCodegen(addresses: Int = 4):
  private val out = new StringBuilder
  private var labelCounter = 0
  private val stringLiterals = new mutable.ListBuffer[(String, String)]() // (label, value)
  private var needsAllocExtern = false // set when codegen emits malloc/free references
  private var needsStrInt = false // set when codegen needs __str_int helper

  private def newLabel(prefix: String): String =
    labelCounter += 1
    s".${prefix}_$labelCounter"

  // Struct types that have a deinit method (populated during generate)
  private val deinitTypes = new mutable.HashSet[String]

  def generate(program: TProgram): String =
    out.clear()
    labelCounter = 0
    stringLiterals.clear()
    deinitTypes.clear()
    needsAllocExtern = false
    needsStrInt = false

    // Scan for deinit methods: functions named TypeName_deinit
    for decl <- program.decls do
      decl match
        case TFunDecl(name, _, _, _, _, _) if name.endsWith("_deinit") =>
          deinitTypes += name.dropRight(7) // remove "_deinit" suffix
        case _ =>

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

    // Emit __str_int helper if needed (integer to string conversion)
    if needsStrInt then emitStrIntHelper()

    // Emit rodata segment — string literals with immortal refcount headers
    if stringLiterals.nonEmpty then
      emit("segment rodata")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        // Total: 8 (refcount) + bytes + 1 (null terminator)
        emit(s"global $label, data, ${bytes.length + 9}")
      emit("  align 8")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"  dl -1") // immortal refcount header
        emit(s"$label:")
        for b <- bytes do emit(s"  db ${b & 0xff}")
        emit("  db 0") // null terminator for *i8 decay compatibility

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

    // Emit extern declarations for malloc/free if referenced by refcount management
    if needsAllocExtern then
      // Only emit if not already defined in this module
      val definedSymbols = (for decl <- program.decls yield decl match
        case TFunDecl(name, _, _, _, _, _) => Some(name)
        case TVarDecl(name, _, _, _) => Some(name)
        case _ => None).flatten.toSet
      if !definedSymbols.contains("malloc") then emit("extern malloc")
      if !definedSymbols.contains("free") then emit("extern free")

    out.toString

  private case class LocalVar(name: String, offset: Int, typ: SyslType)

  private val globals = new mutable.LinkedHashMap[String, SyslType]
  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var refParams: mutable.LinkedHashMap[String, SyslType.RefType] = null // ref-typed params for cleanup
  private var stackOffset: Int = 0
  private val savedScopes = new mutable.Stack[(Map[String, LocalVar], Int)]
  private val loopScopeOffsets = new mutable.Stack[Int]

  private def enterScope(): Unit =
    savedScopes.push((locals.toMap, stackOffset))

  private def leaveScope(): Unit =
    val (savedLocals, savedOffset) = savedScopes.pop()
    // Decrement refcounts for ref-typed locals leaving scope
    // Skip params (positive offsets) — they are borrowed, not owned
    for (name, local) <- locals if !savedLocals.contains(name) && local.offset < 0 do
      local.typ match
        case rt: SyslType.RefType =>
          val hoff = refHeaderOffset(rt)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")
          emitRefDecr(1, hoff, deinitFor(rt))
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
  private def returnsViaPointer(typ: SyslType): Boolean = typ.isInstanceOf[SyslType.StructType] || typ == SyslType.StringType || typ.isInstanceOf[SyslType.SliceType] || typ.isInstanceOf[SyslType.EnumType]

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
    case SyslType.EnumType(_, variants) =>
      val fieldAligns = variants.flatMap(_._2.map(f => stackAlign(f._2)))
      if fieldAligns.isEmpty then 4 else fieldAligns.max.max(4)
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
      case SyslType.StringType | SyslType.SliceType(_) =>
        // 16-byte copy: srcReg = source address, addrReg = dest address
        emit(s"  ldd r4, r$srcReg, r0")
        emit(s"  std r4, r$addrReg, r0")
        emitAddImm(4, srcReg, 8)
        emit("  ldd r4, r4, r0")
        emitAddImm(3, addrReg, 8)
        emit("  std r4, r3, r0")
      case st: SyslType.StructType =>
        // Struct copy: srcReg = source address, addrReg = dest address
        val size = stackSize(st)
        for i <- 0 until size by 8 do
          emitAddImm(4, srcReg, i)
          emit("  ldd r4, r4, r0")
          emitAddImm(3, addrReg, i)
          emit("  std r4, r3, r0")
      case et: SyslType.EnumType =>
        // Enum copy: srcReg = source address, addrReg = dest address
        val size = stackSize(et)
        for i <- 0 until size by 8 do
          emitAddImm(4, srcReg, i)
          emit("  ldd r4, r4, r0")
          emitAddImm(3, addrReg, i)
          emit("  std r4, r3, r0")
      case _ =>
        emit(s"  std r$srcReg, r$addrReg, r0")

  // Refcount header offset: refcount is at [ptr - headerOffset]
  // Structs: 8 (just refcount), Slices: 16 (refcount + length), Strings: 8 (just refcount, length in fat pointer)
  private def refHeaderOffset(typ: SyslType): Int = typ match
    case SyslType.RefType(SyslType.SliceType(_)) => 16
    case SyslType.StringType => 8
    case _ => 8

  // Get deinit function name for a ref type, if one exists
  private def deinitFor(typ: SyslType): Option[String] = typ match
    case SyslType.RefType(SyslType.StructType(name, _)) if deinitTypes.contains(name) =>
      Some(s"${name}_deinit")
    case _ => None

  // Emit refcount increment: ptr in rPtr, refcount is at [rPtr - headerOffset]
  // Clobbers r3, r4. Skips if rPtr == 0 (null).
  private def emitRefIncr(ptrReg: Int, headerOff: Int = 8): Unit =
    val skip = newLabel("skip_incr")
    emit(s"  beq r$ptrReg, r0, $skip")
    emitAddImm(3, ptrReg, -headerOff) // r3 = &refcount
    emit("  ldd r4, r3, r0")          // r4 = refcount
    emit("  addi r3, r0, -1")         // r3 = -1 (immortal sentinel)
    emit(s"  beq r4, r3, $skip")      // skip if immortal
    emit("  addi r4, r4, 1")          // r4++
    emitAddImm(3, ptrReg, -headerOff) // r3 = &refcount (reload)
    emit("  std r4, r3, r0")          // store back
    emit(s"$skip")

  // Emit refcount decrement + free-at-zero: ptr in rPtr, refcount at [rPtr - headerOffset]
  // Clobbers r3, r4. Skips if rPtr == 0 (null). Calls deinit then free(base) when refcount hits 0.
  private def emitRefDecr(ptrReg: Int, headerOff: Int = 8, deinitFunc: Option[String] = None): Unit =
    val skip = newLabel("skip_decr")
    val noFree = newLabel("no_free")
    emit(s"  beq r$ptrReg, r0, $skip")
    emitAddImm(3, ptrReg, -headerOff) // r3 = &refcount (also base for free)
    emit("  ldd r4, r3, r0")          // r4 = refcount
    emit("  addi r3, r0, -1")         // r3 = -1 (immortal sentinel)
    emit(s"  beq r4, r3, $skip")      // skip if immortal
    emit("  addi r4, r4, -1")         // r4--
    emitAddImm(3, ptrReg, -headerOff) // r3 = &refcount (reload)
    emit("  std r4, r3, r0")          // store back
    emit(s"  bne r4, r0, $noFree")
    // refcount == 0 → call deinit then free(base)
    emit("  pshd r1")                 // save r1
    deinitFunc.foreach { name =>
      // Call deinit(dataPtr) — dataPtr is ptrReg (past header)
      emit(s"  mov r1, r$ptrReg")     // r1 = data pointer (self)
      emit(s"  movi r4, $name")
      emit("  jalr r6, r4")
    }
    emit("  mov r1, r3")              // r1 = base pointer (for free)
    emit("  movi r4, free")
    emit("  jalr r6, r4")
    emit("  popd r1")                 // restore r1
    emit(s"$noFree")
    emit(s"$skip")

  // Decrement refcounts for all ref-typed and string-typed locals and params
  private def emitRefCleanup(): Unit =
    // Decrement owned locals (negative fp offsets)
    for (_, local) <- locals if local.offset < 0 do
      local.typ match
        case rt: SyslType.RefType =>
          val hoff = refHeaderOffset(rt)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")
          emitRefDecr(1, hoff, deinitFor(rt))
          emit("  popd r1")
        case SyslType.StringType if needsAllocExtern =>
          // Only emit string cleanup when heap strings are possible (concat/allocation used)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")       // r1 = ptr field
          emitRefDecr(1, 8)
          emit("  popd r1")
        case _ =>
    // Decrement ref params (caller transferred ownership)
    for (name, rt) <- refParams do
      val local = locals(name)
      val hoff = refHeaderOffset(rt)
      emit("  pshd r1")
      emitAddImm(1, 5, local.offset)
      emit("  ldd r1, r1, r0")
      emitRefDecr(1, hoff, deinitFor(rt))
      emit("  popd r1")
    // Decrement string params (only when heap strings are possible)
    if needsAllocExtern then
      for param <- if currentFunction != null then currentFunction.params else Nil do
        if param.typ == SyslType.StringType then
          val local = locals(param.name)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")       // load ptr from string struct
          emitRefDecr(1, 8)
          emit("  popd r1")

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
    refParams = new mutable.LinkedHashMap
    stackOffset = 0
    deferStack.clear()

    val structReturn = returnsViaPointer(fun.returnType)

    emit(s"# function: ${fun.name}")
    emit(s"${fun.name}:")

    // ABI: first non-string param in r1, all others on caller's stack.
    // If the function returns a struct/string, r1 = hidden return pointer and all user params are on stack.
    val allRegSlots = if structReturn then 1 + fun.params.length else fun.params.length
    val nRegPushed = allRegSlots.min(1)
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
      val off = 16 + (nRegPushed - 1) * 8
      locals("_ret_ptr") = LocalVar("_ret_ptr", off, SyslType.PtrType(fun.returnType))
      off
    else -1

    // Map user params to their stack locations
    val userParamRegStart = if structReturn then 1 else 0
    val userRegParams = fun.params.length.min(1 - userParamRegStart)
    for (param, i) <- fun.params.take(userRegParams).zipWithIndex do
      val regIndex = userParamRegStart + i
      val callerOffset = 16 + (nRegPushed - 1 - regIndex) * 8
      locals(param.name) = LocalVar(param.name, callerOffset, SyslType.I64)
    // Stack params: those beyond register capacity
    // String params take 16 bytes on the caller stack, others take 8
    val nUserStackStart = 1 - userParamRegStart
    var stackParamOffset = 16 + nRegPushed * 8
    for param <- fun.params.drop(nUserStackStart) do
      if param.typ == SyslType.StringType then
        locals(param.name) = LocalVar(param.name, stackParamOffset, SyslType.StringType)
        stackParamOffset += 16
      else
        locals(param.name) = LocalVar(param.name, stackParamOffset, SyslType.I64)
        stackParamOffset += 8

    // Track ref-typed params for cleanup on function exit
    for param <- fun.params do
      param.typ match
        case rt: SyslType.RefType => refParams(param.name) = rt
        case _ =>

    // Copy string params into local 16-byte slots so TVarRef works uniformly.
    // Register string params: the register holds an 8-byte address → dereference and copy.
    // Stack string params: 16 bytes {ptr, len} on caller stack → copy directly.
    for (param, i) <- fun.params.zipWithIndex if param.typ == SyslType.StringType do
      val srcLocal = locals(param.name)
      val isRegParam = i < (1 - userParamRegStart)
      emitAddImm(7, 7, -16)
      stackOffset -= 16
      val strLocal = LocalVar(param.name, stackOffset, SyslType.StringType)
      if isRegParam then
        // Register param: srcLocal holds an 8-byte address → dereference
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r1, r1, r0")        // r1 = caller's string struct address
        emit("  ldd r2, r1, r0")        // r2 = ptr
        emit("  std r2, r7, r0")        // store ptr at local+0
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")        // r2 = len
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")        // store len at local+8
      else
        // Stack param: 16 bytes {ptr, len} already on caller stack
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r2, r1, r0")        // r2 = ptr
        emit("  std r2, r7, r0")        // store ptr at local+0
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")        // r2 = len
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")        // store len at local+8
      locals(param.name) = strLocal

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

  // Copy multi-word value from src address (r1) to _ret_ptr, then set r1 = _ret_ptr
  // Works for both StructType and StringType (16 bytes)
  private def emitStructReturn(): Unit =
    val size = currentFunction.returnType match
      case st: SyslType.StructType => stackSize(st)
      case et: SyslType.EnumType => stackSize(et)
      case SyslType.StringType => 16
      case _ => 8
    val retLocal = locals("_ret_ptr")
    // r1 = source address; load _ret_ptr into r2
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
      allSlots.min(1)
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
          case TArrayDecl(size, SyslType.ArrayType(elemType, _)) =>
            // Allocate array inline on stack and zero-initialize
            val rawBytes = size * stackSize(elemType)
            val totalBytes = (rawBytes + 7) & ~7
            emitAddImm(7, 7, -totalBytes)
            stackOffset -= totalBytes
            val local = LocalVar(name, stackOffset, typ)
            locals(name) = local
            // Zero-fill
            emitAddImm(1, 5, local.offset)
            for i <- 0 until totalBytes by 8 do
              emitAddImm(2, 1, i)
              emit("  std r0, r2, r0")
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
          case TEnumConstruct(et, variantIndex, args) =>
            // Allocate enum on stack, zero-initialize, set tag + fields
            val totalSize = stackSize(et)
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
            // Write tag (i32 at offset 0)
            emitLoadImm(1, variantIndex)
            emitAddImm(2, 5, local.offset)
            emit("  stw r1, r2, r0")
            // Write variant fields
            val dataOff = et.dataOffset.toInt
            val variantFields = et.variants(variantIndex)._2
            var fieldOff = 0
            for (arg, i) <- args.zipWithIndex do
              val (_, fieldType) = variantFields(i)
              val align = stackAlign(fieldType)
              fieldOff = ((fieldOff + align - 1) / align) * align
              genExpr(arg)
              emitAddImm(2, 5, local.offset + dataOff + fieldOff)
              emitStore(1, 2, fieldType)
              fieldOff += fieldType.sizeOf.toInt
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
            // Increment refcount for copies (not for new — TNew already sets refcount=1)
            (typ, init) match
              case (rt: SyslType.RefType, _: TNew | _: TNewArray) => // owned, no incr needed
              case (rt: SyslType.RefType, _) => emitRefIncr(1, refHeaderOffset(rt))
              case (SyslType.StringType, _: TBinary) => // concat result already has refcount=1
              case (SyslType.StringType, _) if needsAllocExtern =>
                // Incr refcount of the ptr field (only when heap strings exist)
                emit("  pshd r1")           // save string struct address
                emit("  ldd r1, r1, r0")    // r1 = ptr field
                emitRefIncr(1, 8)
                emit("  popd r1")           // restore string struct address
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

      case TDestructureAssignStmt(names, types, init) =>
        // Parallel assignment: evaluate RHS tuple, then assign all fields to existing locals
        genExpr(init)
        // Save tuple base address as a hidden local
        val tmpLocal = allocLocal(s"_tup$$${newLabel("t")}", SyslType.PtrType(init.typ))
        emitAddImm(2, 5, tmpLocal.offset)
        emit("  std r1, r2, r0")
        val st = init.typ.asInstanceOf[SyslType.StructType]
        // Extract each field and store to existing variable
        for ((name, fieldType), i) <- names.zip(types).zipWithIndex do
          val off = fieldOffset(st, i)
          emitAddImm(1, 5, tmpLocal.offset)
          emit("  ldd r1, r1, r0")  // r1 = tuple address
          if off != 0 then emitAddImm(1, 1, off)
          emitLoad(1, 1, fieldType)  // r1 = field value
          if locals != null && locals.contains(name) then
            val local = locals(name)
            emitAddImm(2, 5, local.offset)
            emitStore(1, 2, local.typ)
          else
            emit(s"  movi r2, $name")
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
              emitRefDecr(1, hoff, deinitFor(rt))
              genExpr(value)
              // Increment only for copies, not new allocations
              value match
                case _: TNew | _: TNewArray => // owned, no incr needed
                case _ => emitRefIncr(1, hoff)
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)
            case SyslType.StringType =>
              if needsAllocExtern then
                // Decrement old string's ptr refcount (only when heap strings exist)
                emitAddImm(1, 5, local.offset)
                emit("  ldd r1, r1, r0")    // r1 = old ptr field
                emitRefDecr(1, 8)
              genExpr(value)              // r1 = address of new {ptr, len}
              // Increment new string's refcount (skip for concat results)
              if needsAllocExtern then
                value match
                  case _: TBinary => // concat already has refcount=1
                  case _ =>
                    emit("  pshd r1")
                    emit("  ldd r1, r1, r0")
                    emitRefIncr(1, 8)
                    emit("  popd r1")
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
              emitRefDecr(1, hoff, deinitFor(rt))
              genExpr(value)
              value match
                case _: TNew | _: TNewArray =>
                case _ => emitRefIncr(1, hoff)
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
        // For pointer types, scale the offset by element size
        val varType = if locals != null && locals.contains(target) then locals(target).typ
          else globals.getOrElse(target, value.typ)
        varType match
          case SyslType.PtrType(elem) if op == "+" || op == "-" =>
            val elemSize = stackSize(elem)
            if elemSize != 1 then
              emitLoadImm(3, elemSize)
              emit("  mul r1, r1, r3") // r1 = offset * elemSize (r2 clobbered, ok)
          case _ =>
        emit("  pshd r1") // save scaled operand
        if locals != null && locals.contains(target) then
          val local = locals(target)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit("  popd r3")
          emitBinOp(op)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else
          val gtyp = globals.getOrElse(target, value.typ)
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
        val condLabel = newLabel("dowhile_cond")
        val endLabel = newLabel("enddowhile")
        breakLabels.push(endLabel)
        continueLabels.push(condLabel) // continue jumps to condition, not body
        loopScopeOffsets.push(stackOffset)
        emit(s"$loopLabel")
        enterScope()
        for stmt <- body do genStmt(stmt)
        leaveScope()
        emit(s"$condLabel")
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

      case other =>
        throw new RuntimeException(s"codegen: unhandled statement type: ${other.getClass.getSimpleName}")

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
            case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType =>
              emitAddImm(1, 5, local.offset) // aggregates: address, not value
            case _ =>
              emitAddImm(2, 5, local.offset)
              emitLoad(1, 2, local.typ)
        else
          emit(s"  movi r1, $name")
          val gt = globals.getOrElse(name, typ) // use AST type for cross-unit globals
          gt match
            case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType =>
              () // aggregates: address is the value
            case _ =>
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

      case TBinary(left, "+", right, _) if left.typ == SyslType.StringType =>
        // String concatenation: allocate new string, copy bytes from both sides
        // Helper: evaluate string expr, extract ptr/len, reclaim any temps, push ptr then len
        def evalStringAndPush(expr: TExpr): Unit =
          val pre = stackOffset
          genExpr(expr)
          emit("  ldd r2, r1, r0")        // r2 = ptr
          emit("  addi r3, r1, 8")
          emit("  ldd r3, r3, r0")        // r3 = len
          // Reclaim genExpr temps (e.g., TStringLit 16-byte struct)
          val extra = pre - stackOffset
          if extra > 0 then
            emitAddImm(7, 7, extra)
            stackOffset = pre
          emit("  pshd r2")               // push ptr
          emit("  pshd r3")               // push len
          stackOffset -= 16

        evalStringAndPush(left)    // Stack: [len1] [ptr1]
        evalStringAndPush(right)   // Stack: [len2] [ptr2] [len1] [ptr1]
        // sp+0=len2, sp+8=ptr2, sp+16=len1, sp+24=ptr1
        // Compute malloc size = 8 + len1 + len2
        emitAddImm(1, 7, 16)              // r1 = &len1
        emit("  ldd r1, r1, r0")          // r1 = len1
        emit("  ldd r2, r7, r0")          // r2 = len2
        emit("  add r1, r1, r2")          // r1 = len1 + len2
        emit("  addi r1, r1, 8")          // r1 = 8 + len1 + len2
        emit("  pshd r1")                 // push malloc arg
        stackOffset -= 8
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")
        needsAllocExtern = true
        emit("  popd r3")                 // clean malloc arg
        stackOffset += 8
        // Null check: trap if malloc returned 0
        val allocOk = newLabel("alloc_ok")
        emit(s"  bne r1, r0, $allocOk")
        emit("  ldi r1, 2")         // error code: 2 = null pointer
        emit("  trap 1")
        emit(s"$allocOk")
        // r1 = base of allocated block
        emit("  pshd r1")                 // save base
        stackOffset -= 8
        // Set refcount = 1 at [base]
        emit("  ldi r2, 1")
        emit("  std r2, r1, r0")
        // sp+0=base, sp+8=len2, sp+16=ptr2, sp+24=len1, sp+32=ptr1
        // Copy len1 bytes from ptr1 to base+8
        val copyLoop1 = newLabel("strcpy1")
        val copyDone1 = newLabel("strcpy1_done")
        emit("  addi r1, r1, 8")          // r1 = dest = base+8
        emitAddImm(2, 7, 32)              // r2 = &ptr1
        emit("  ldd r2, r2, r0")          // r2 = ptr1
        emitAddImm(3, 7, 24)              // r3 = &len1
        emit("  ldd r3, r3, r0")          // r3 = len1
        emit(s"$copyLoop1")
        emit(s"  beq r3, r0, $copyDone1")
        emit("  ldb r4, r2, r0")
        emit("  stb r4, r1, r0")
        emit("  addi r1, r1, 1")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, -1")
        emit(s"  bra $copyLoop1")
        emit(s"$copyDone1")
        // Copy len2 bytes from ptr2 to base+8+len1 (r1 already at right position)
        val copyLoop2 = newLabel("strcpy2")
        val copyDone2 = newLabel("strcpy2_done")
        emitAddImm(2, 7, 16)              // r2 = &ptr2
        emit("  ldd r2, r2, r0")          // r2 = ptr2
        emitAddImm(3, 7, 8)               // r3 = &len2
        emit("  ldd r3, r3, r0")          // r3 = len2
        emit(s"$copyLoop2")
        emit(s"  beq r3, r0, $copyDone2")
        emit("  ldb r4, r2, r0")
        emit("  stb r4, r1, r0")
        emit("  addi r1, r1, 1")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, -1")
        emit(s"  bra $copyLoop2")
        emit(s"$copyDone2")
        // Build result {ptr, len} on stack
        // ptr = base + 8, len = len1 + len2
        emit("  ldd r1, r7, r0")          // r1 = base
        emit("  addi r1, r1, 8")          // r1 = data ptr (base + 8)
        emitAddImm(2, 7, 24)              // r2 = &len1
        emit("  ldd r2, r2, r0")          // r2 = len1
        emitAddImm(3, 7, 8)               // r3 = &len2
        emit("  ldd r3, r3, r0")          // r3 = len2
        emit("  add r2, r2, r3")          // r2 = total len
        // Clean up saved values: base(8) + len2(8) + ptr2(8) + len1(8) + ptr1(8) = 40 bytes
        emitAddImm(7, 7, 40)
        stackOffset += 40
        // Allocate 16-byte result slot
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r1, r7, r0")          // store ptr at offset 0
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")          // store len at offset 8
        emit("  mov r1, r7")              // r1 = address of result string struct

      case TBinary(left, op @ ("==" | "!="), right, _) if left.typ == SyslType.StringType =>
        // String comparison: compare lengths first, then bytes
        // Eval left: extract ptr/len, reclaim temps, push
        val preLeft = stackOffset
        genExpr(left)                      // r1 = addr of {ptr1, len1}
        emit("  ldd r2, r1, r0")          // r2 = ptr1
        emit("  addi r3, r1, 8")
        emit("  ldd r3, r3, r0")          // r3 = len1
        val extraLeft = preLeft - stackOffset
        if extraLeft > 0 then
          emitAddImm(7, 7, extraLeft)
          stackOffset = preLeft
        emit("  pshd r2")                 // push ptr1
        emit("  pshd r3")                 // push len1
        stackOffset -= 16
        // Eval right: extract ptr/len, reclaim temps, push
        val preRight = stackOffset
        genExpr(right)                     // r1 = addr of {ptr2, len2}
        emit("  ldd r2, r1, r0")          // r2 = ptr2
        emit("  addi r3, r1, 8")
        emit("  ldd r3, r3, r0")          // r3 = len2
        val extraRight = preRight - stackOffset
        if extraRight > 0 then
          emitAddImm(7, 7, extraRight)
          stackOffset = preRight
        emit("  pshd r2")                 // push ptr2
        emit("  pshd r3")                 // push len2
        stackOffset -= 16
        // Stack: sp+0=len2, sp+8=ptr2, sp+16=len1, sp+24=ptr1
        val notEqual = newLabel("str_ne")
        val equal = newLabel("str_eq")
        val strEnd = newLabel("str_cmp_end")
        // Compare lengths
        emitAddImm(1, 7, 16)
        emit("  ldd r1, r1, r0")          // r1 = len1
        emit("  ldd r2, r7, r0")          // r2 = len2
        emit(s"  bne r1, r2, $notEqual")  // lengths differ → not equal
        // Lengths match — compare bytes
        emitAddImm(2, 7, 24)
        emit("  ldd r2, r2, r0")          // r2 = ptr1
        emitAddImm(3, 7, 8)
        emit("  ldd r3, r3, r0")          // r3 = ptr2
        // r1 = len (loop counter)
        val cmpLoop = newLabel("str_cmp_loop")
        val cmpMismatch = newLabel("str_cmp_mismatch")
        emit(s"$cmpLoop")
        emit(s"  beq r1, r0, $equal")
        emit("  ldb r4, r2, r0")
        emit("  pshd r1")
        emit("  ldb r1, r3, r0")
        emit(s"  bne r4, r1, $cmpMismatch")
        emit("  popd r1")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, 1")
        emit("  addi r1, r1, -1")
        emit(s"  bra $cmpLoop")
        emit(s"$cmpMismatch")
        emit("  popd r1")                 // clean saved counter
        emit(s"  bra $notEqual")
        emit(s"$equal")
        emit(s"  ldi r1, ${if op == "==" then 1 else 0}")
        emit(s"  bra $strEnd")
        emit(s"$notEqual")
        emit(s"  ldi r1, ${if op == "==" then 0 else 1}")
        emit(s"$strEnd")
        // Clean up: 32 bytes of saved values
        emitAddImm(7, 7, 32)
        stackOffset += 32

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
        val isFloat = left.typ == SyslType.DoubleType
        val unsigned = left.typ.isUnsigned
        if isFloat then
          op match
            case "+"  => emit("  fadd r1, r1, r2")
            case "-"  => emit("  fsub r1, r1, r2")
            case "*"  => emit("  fmul r1, r1, r2")
            case "/"  => emit("  fdiv r1, r1, r2")
            case "==" =>
              emit("  fseq r1, r1, r2")  // r1 = 1 if equal, 0 if not
            case "!=" =>
              emit("  fseq r1, r1, r2")
              emit("  ldi r3, 1")
              emit("  xor r1, r1, r3")   // flip
            case "<" =>
              emit("  fslt r1, r1, r2")
            case ">" =>
              emit("  fslt r1, r2, r1")
            case "<=" =>
              emit("  fslt r1, r2, r1")  // r1 = (right < left)
              emit("  ldi r3, 1")
              emit("  xor r1, r1, r3")   // flip: !(right < left) = left <= right
            case ">=" =>
              emit("  fslt r1, r1, r2")  // r1 = (left < right)
              emit("  ldi r3, 1")
              emit("  xor r1, r1, r3")   // flip: !(left < right) = left >= right
            case _ => // unsupported float op — fall through
        else
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

      case TCast(TStringLit(value, _), target) if target.isInstanceOf[SyslType.PtrType] =>
        // String literal → *i8 decay: emit data pointer directly, no fat pointer needed
        val bytes = value.getBytes("UTF-8")
        labelCounter += 1
        val strLabel = s"__str_$labelCounter"
        stringLiterals += ((strLabel, value))
        emit(s"  movi r1, $strLabel")  // r1 = ptr to byte data (past refcount header)

      case TCast(inner, target) if inner.typ == SyslType.StringType && target.isInstanceOf[SyslType.PtrType] =>
        // String→*i8 decay: load ptr field from the 16-byte string struct
        genExpr(inner)               // r1 = address of {ptr, len}
        emit("  ldd r1, r1, r0")    // r1 = ptr (offset 0)

      case TCast(inner, target) =>
        genExpr(inner)
        import SyslType.*
        val srcIsFloat = inner.typ == DoubleType
        val tgtIsFloat = target == DoubleType
        // Float → int: convert float bits to integer value first
        if srcIsFloat && !tgtIsFloat then emit("  fint r1, r1")
        // Int → float: convert integer value to float bits
        else if !srcIsFloat && tgtIsFloat then emit("  cvt r1, r1")
        target match
          case DoubleType => // cvt already emitted above (or no-op if src is already float)
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
        if operand.typ == SyslType.DoubleType then emit("  fneg r1, r1")
        else emit("  neg r1, r1")

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

      case TCall("abort", _, _) =>
        emit("  ldi r1, 3")           // error code: 3 = abort
        emit("  trap 1")

      case TCall("panic", _, _) =>
        // message arg is discarded in codegen — emulator has no stderr channel
        emit("  ldi r1, 4")           // error code: 4 = panic
        emit("  trap 1")

      case TCall("assert", List(cond, _), _) =>
        // evaluate cond only; if false (r1 == 0), trap with error code 4
        genExpr(cond)
        val passLabel = newLabel("assert_pass")
        emit(s"  bne r1, r0, $passLabel")
        emit("  ldi r1, 4")           // error code: 4 = assert/panic
        emit("  trap 1")
        emit(s"$passLabel:")

      case TCall(name, args, retType) =>
        val callStructReturn = returnsViaPointer(retType)
        // If struct/string return, allocate space on caller's stack for the return value
        // and prepend hidden pointer as first arg
        val retSlotOffset = if callStructReturn then
          val size = retType match
            case st: SyslType.StructType => stackSize(st)
            case et: SyslType.EnumType => stackSize(et)
            case SyslType.StringType => 16
            case _ => 8
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

        // ABI: arg 0 in r1, args 1+ on stack (right-to-left)
        // r4 is reserved for the call address (movi r4, name)
        val nRegArgs = allArgs.length.min(1)
        val stackArgs = allArgs.drop(1)
        val savedOffset = stackOffset

        def evalAndPush(arg: TExpr): Unit =
          val preOffset = stackOffset
          arg match
            case TAddrLit(off) => emitAddImm(1, 5, off)
            case _ => genExpr(arg)
          arg.typ match
            case rt: SyslType.RefType => arg match
              case _: TNew | _: TNewArray =>
              case _ => emitRefIncr(1, refHeaderOffset(rt))
            case SyslType.StringType if needsAllocExtern => arg match
              case _: TBinary =>
              case _ =>
                emit("  pshd r1")
                emit("  ldd r1, r1, r0")
                emitRefIncr(1, 8)
                emit("  popd r1")
            case _ =>
          if arg.typ == SyslType.StringType then
            // String stack args: extract ptr/len, clean up temps, push 16 bytes
            emit("  addi r2, r1, 8")
            emit("  ldd r2, r2, r0")     // r2 = len
            emit("  ldd r1, r1, r0")     // r1 = ptr
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r2")
            emit("  pshd r1")
            stackOffset -= 16
          else if arg.typ.isInstanceOf[SyslType.EnumType] || arg.typ.isInstanceOf[SyslType.StructType] || arg.typ.isInstanceOf[SyslType.SliceType] then
            // Aggregate args: r1 is an address into our stack — do NOT reclaim the temp!
            emit("  pshd r1")
            stackOffset -= 8
          else
            // Scalar args: clean up temps, push 8 bytes
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r1")
            stackOffset -= 8

        // With r1-only ABI, there's at most one register arg.
        // If it's a string: pre-evaluate it FIRST (pushing 16-byte data above stack args),
        // then push stack args, then push the string data address as the register arg.
        val regArgOpt = allArgs.headOption.filter(_ => nRegArgs > 0)
        var regStringDataOffset = 0 // fp-relative offset of pre-pushed string data
        regArgOpt.foreach { arg =>
          if arg.typ == SyslType.StringType then
            val preOffset = stackOffset
            genExpr(arg)
            if needsAllocExtern then
              arg match
                case _: TBinary =>
                case _ =>
                  emit("  pshd r1")
                  emit("  ldd r1, r1, r0")
                  emitRefIncr(1, 8)
                  emit("  popd r1")
            // Extract ptr/len, reclaim temp, push as stable data
            emit("  addi r2, r1, 8")
            emit("  ldd r2, r2, r0")
            emit("  ldd r1, r1, r0")
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r2")            // push len
            emit("  pshd r1")            // push ptr
            stackOffset -= 16
            regStringDataOffset = stackOffset
        }
        // Push stack args (1+) right-to-left
        for arg <- stackArgs.reverse do
          evalAndPush(arg)
        // Push the register arg, then pop into r1
        regArgOpt.foreach { arg =>
          if arg.typ == SyslType.StringType then
            // Address of the pre-pushed 16-byte string data (above stack args)
            emitAddImm(1, 5, regStringDataOffset)
          else
            val preOffset = stackOffset
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            arg.typ match
              case rt: SyslType.RefType => arg match
                case _: TNew | _: TNewArray =>
                case _ => emitRefIncr(1, refHeaderOffset(rt))
              case _ =>
            // Don't reclaim temp stack space for aggregate types — r1 is a pointer into that space
            if !arg.typ.isInstanceOf[SyslType.EnumType] && !arg.typ.isInstanceOf[SyslType.StructType] && !arg.typ.isInstanceOf[SyslType.SliceType] then
              val extra = preOffset - stackOffset
              if extra > 0 then
                emitAddImm(7, 7, extra)
                stackOffset = preOffset
          emit("  pshd r1")
          stackOffset -= 8
        }
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
        // ABI: arg 0 in r1, args 1+ on stack
        val nRegArgs = args.length.min(1)
        val stackArgs = args.drop(1)
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
        fieldType match
          case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType =>
            () // aggregate types: address is the value (don't dereference)
          case _ =>
            emitLoad(1, 1, fieldType) // scalar types: load the value

      case TDeref(inner, typ) =>
        genExpr(inner)           // r1 = pointer address
        emitLoad(1, 1, typ)      // load with width matching pointee type

      case TIndex(array, index, elemType) if array.typ == SyslType.StringType =>
        // String indexing: bounds-checked byte access
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = address of {ptr, len} string struct
        emit("  popd r2")        // r2 = index
        // Bounds check: 0 <= index < len
        emit("  addi r3, r1, 8")
        emit("  ldd r3, r3, r0") // r3 = len (i64 from fat pointer)
        emit("  slt r4, r2, r0") // r4 = (index < 0)
        val boundsOk = newLabel("bounds_ok")
        val boundsErr = newLabel("bounds_error")
        emit(s"  bne r4, r0, $boundsErr")
        emit("  slt r4, r2, r3") // r4 = (index < len)
        emit(s"  bne r4, r0, $boundsOk")
        emit(s"$boundsErr")
        emit("  ldi r1, 1")     // error code: 1 = out-of-bounds
        emit("  trap 1")
        emit(s"$boundsOk")
        // Load byte at ptr + index
        emit("  ldd r1, r1, r0") // r1 = ptr (from struct offset 0)
        emit("  add r1, r1, r2") // r1 = ptr + index
        emit("  ldb r1, r1, r0") // r1 = byte at that address
        emit("  zeb r1, r1")     // zero-extend byte to unsigned

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
        val boundsErr = newLabel("bounds_err")
        emit(s"  bne r4, r0, $boundsErr")
        emit("  slt r4, r2, r3")
        emit(s"  bne r4, r0, $boundsOk")
        emit(s"$boundsErr")
        emit("  ldi r1, 1")     // error code: 1 = out-of-bounds
        emit("  trap 1")
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
        emit("  slt r4, r2, r0")
        val boundsOk = newLabel("bounds_ok")
        val boundsErr = newLabel("bounds_err")
        emit(s"  bne r4, r0, $boundsErr")
        emit("  slt r4, r2, r3")
        emit(s"  bne r4, r0, $boundsOk")
        emit(s"$boundsErr")
        emit("  ldi r1, 1")      // error code: 1 = out-of-bounds
        emit("  trap 1")
        emit(s"$boundsOk")
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
        stackOffset -= totalBytes
        // Zero-initialize
        emit("  mov r1, r7")
        for i <- 0 until totalBytes by 8 do
          emitAddImm(2, 1, i)
          emit("  std r0, r2, r0")
        emit("  mov r1, r7")     // r1 = address of array start

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
        // Allocate 16-byte {ptr, len} fat pointer on stack
        val bytes = value.getBytes("UTF-8")
        labelCounter += 1
        val strLabel = s"__str_$labelCounter"
        stringLiterals += ((strLabel, value))
        // Allocate 16 bytes on stack for the string struct
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        // Store ptr field at [sp+0]
        emit(s"  movi r1, $strLabel")
        emit("  std r1, r7, r0") // store ptr at offset 0
        // Store len field at [sp+8]
        emitLoadImm(1, bytes.length)
        emitAddImm(2, 7, 8)
        emit("  std r1, r2, r0") // store len at offset 8
        // r1 = address of the 16-byte string struct
        emit("  mov r1, r7")

      case TStr(inner) =>
        // Convert integer/bool value to string via __str_int helper
        genExpr(inner) // r1 = integer value
        needsStrInt = true
        needsAllocExtern = true
        // Allocate 16-byte return slot for the result string
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r0, r7, r0")
        emitAddImm(2, 7, 8)
        emit("  std r0, r2, r0")
        // Push value as stack arg for __str_int
        emit("  pshd r1")
        stackOffset -= 8
        // r1 = hidden return slot ptr (just above the pushed value)
        emitAddImm(1, 7, 8)
        // Call __str_int
        emit("  movi r4, __str_int")
        emit("  jalr r6, r4")
        // Clean value arg
        emitAddImm(7, 7, 8)
        stackOffset += 8
        // r1 = address of return slot (which now contains {ptr, len})
        emit("  mov r1, r7")

      case TIfExpr(cond, thenBody, elseBody, typ) =>
        val elseLabel = newLabel("else")
        val endLabel = newLabel("endif")
        val isString = typ == SyslType.StringType
        val isAggregate = typ.isInstanceOf[SyslType.EnumType] || typ.isInstanceOf[SyslType.StructType] || typ.isInstanceOf[SyslType.SliceType]
        val aggregateSize = if isAggregate then stackSize(typ) else 0
        // For string/aggregate results: pre-allocate a result slot BEFORE the if/else
        val resultSlotOffset = if isString then
          emitAddImm(7, 7, -16)
          stackOffset -= 16
          stackOffset
        else if isAggregate then
          val aligned = (aggregateSize + 7) & ~7
          emitAddImm(7, 7, -aligned)
          stackOffset -= aligned
          stackOffset
        else 0
        genExpr(cond)
        emit(s"  beq r1, r0, $elseLabel")
        enterScope()
        for stmt <- thenBody do genStmt(stmt)
        if isString then
          // Copy result into pre-allocated slot
          emit("  ldd r2, r1, r0")           // r2 = ptr
          emitAddImm(3, 5, resultSlotOffset)
          emit("  std r2, r3, r0")           // store ptr
          emit("  addi r1, r1, 8")
          emit("  ldd r2, r1, r0")           // r2 = len
          emitAddImm(3, 5, resultSlotOffset + 8)
          emit("  std r2, r3, r0")           // store len
        else if isAggregate then
          // Copy aggregate data into pre-allocated slot
          emitAddImm(2, 5, resultSlotOffset)
          emitStore(1, 2, typ)
        leaveScope()
        if isString || isAggregate then emitAddImm(1, 5, resultSlotOffset)
        emit(s"  bra $endLabel")
        emit(s"$elseLabel")
        elseBody.foreach { stmts =>
          enterScope()
          for stmt <- stmts do genStmt(stmt)
          if isString then
            emit("  ldd r2, r1, r0")
            emitAddImm(3, 5, resultSlotOffset)
            emit("  std r2, r3, r0")
            emit("  addi r1, r1, 8")
            emit("  ldd r2, r1, r0")
            emitAddImm(3, 5, resultSlotOffset + 8)
            emit("  std r2, r3, r0")
          else if isAggregate then
            emitAddImm(2, 5, resultSlotOffset)
            emitStore(1, 2, typ)
          leaveScope()
          if isString || isAggregate then emitAddImm(1, 5, resultSlotOffset)
        }
        emit(s"$endLabel")

      case TMatchExpr(scrutinee, arms, default, _) =>
        // Evaluate scrutinee once, save on stack
        genExpr(scrutinee)
        emit("  pshd r1")
        stackOffset -= 8
        val scrutineeOffset = stackOffset
        val endLabel = newLabel("match_end")
        // Emit each arm
        for arm <- arms do
          val hitLabel = newLabel("match_hit")
          val nextArm = newLabel("match_next")
          // Check patterns — jump to hit if any matches
          for pat <- arm.patterns do
            pat match
              case TWildcard =>
                emit(s"  bra $hitLabel")
              case TValuePattern(v) =>
                genExpr(v)
                emitAddImm(2, 5, scrutineeOffset)
                emit("  ldd r2, r2, r0")
                emit(s"  beq r1, r2, $hitLabel")
              case TRangePattern(low, high) =>
                val rangeCheck = newLabel("range_chk")
                emitAddImm(1, 5, scrutineeOffset)
                emit("  ldd r1, r1, r0")     // r1 = scrutinee
                emit("  pshd r1")
                genExpr(low)                  // r1 = low
                emit("  popd r2")             // r2 = scrutinee
                emit("  slt r3, r2, r1")      // scrutinee < low?
                emit(s"  bne r3, r0, $rangeCheck") // out of range
                emit("  pshd r2")
                genExpr(high)                 // r1 = high
                emit("  popd r2")             // r2 = scrutinee
                emit("  slt r3, r1, r2")      // high < scrutinee?
                emit(s"  beq r3, r0, $hitLabel") // hit if high >= scrutinee
                emit(s"$rangeCheck")
              case TDestructurePattern(_, _, _) =>
                emit(s"  bra $hitLabel")      // destructure always matches
              case TVariantPattern(_, variantIndex, _, _) =>
                // Load tag from scrutinee enum and compare with variant index
                emitAddImm(1, 5, scrutineeOffset)
                emit("  ldd r1, r1, r0")     // r1 = enum address
                emit("  ldw r1, r1, r0")     // r1 = tag (i32 at offset 0)
                emitLoadImm(2, variantIndex)
                emit(s"  beq r1, r2, $hitLabel")
          emit(s"  bra $nextArm")
          emit(s"$hitLabel")
          enterScope()
          // Bind destructure/variant patterns BEFORE guard (guard may reference bindings)
          for pat <- arm.patterns do
            pat match
              case TDestructurePattern(st, bindings, fieldTypes) =>
                for (binding, i) <- bindings.zipWithIndex do
                  binding.foreach { name =>
                    val off = fieldOffset(st, i)
                    // Reload scrutinee address each time (allocLocal may move sp)
                    emitAddImm(1, 5, scrutineeOffset)
                    emit("  ldd r1, r1, r0")  // r1 = scrutinee address
                    if off != 0 then emitAddImm(1, 1, off)
                    emitLoad(1, 1, fieldTypes(i))
                    val local = allocLocal(name, fieldTypes(i))
                    emitAddImm(2, 5, local.offset)
                    emitStore(1, 2, fieldTypes(i))
                  }
              case TVariantPattern(et, variantIndex, bindings, fieldTypes) =>
                val dataOff = et.dataOffset.toInt
                val variantFields = et.variants(variantIndex)._2
                var fieldOff = 0
                for (binding, i) <- bindings.zipWithIndex do
                  val (_, fieldType) = variantFields(i)
                  val align = stackAlign(fieldType)
                  fieldOff = ((fieldOff + align - 1) / align) * align
                  binding.foreach { name =>
                    // Reload scrutinee address each time (allocLocal may move sp)
                    emitAddImm(1, 5, scrutineeOffset)
                    emit("  ldd r1, r1, r0")  // r1 = enum address
                    if dataOff + fieldOff != 0 then emitAddImm(1, 1, dataOff + fieldOff)
                    emitLoad(1, 1, fieldType)
                    val local = allocLocal(name, fieldType)
                    emitAddImm(2, 5, local.offset)
                    emitStore(1, 2, fieldType)
                  }
                  fieldOff += fieldType.sizeOf.toInt
              case _ =>
          // Guard check (after bindings so guard can reference bound variables)
          arm.guard.foreach { guard =>
            genExpr(guard)
            emit(s"  beq r1, r0, $nextArm")  // guard false → skip
          }
          for stmt <- arm.body do genStmt(stmt)
          leaveScope()
          emit(s"  bra $endLabel")
          emit(s"$nextArm")
        // Default arm
        default match
          case Some(stmts) =>
            enterScope()
            for stmt <- stmts do genStmt(stmt)
            leaveScope()
          case None =>
            emit("  ldi r1, 0")
        emit(s"$endLabel")
        // Clean up scrutinee from stack
        emitAddImm(7, 7, 8)
        stackOffset += 8

      case TLen(inner, _) =>
        genExpr(inner)           // r1 = struct address (string or slice)
        inner.typ match
          case SyslType.StringType =>
            emit("  addi r1, r1, 8")
            emit("  ldd r1, r1, r0") // len at offset 8 (i64 in fat pointer)
          case SyslType.SliceType(_) =>
            emit("  addi r1, r1, 8")
            emit("  ldw r1, r1, r0") // len at offset 8
          case SyslType.ArrayType(_, size) =>
            emitLoadImm(1, size) // compile-time constant
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // r1 = data pointer; length is at [r1 - 8]
            emitAddImm(1, 1, -8)
            emit("  ldd r1, r1, r0")
          case other =>
            throw new RuntimeException(s"codegen: len() not supported on ${other}")

      case TCap(inner, _) =>
        genExpr(inner)           // r1 = struct address or data pointer
        inner.typ match
          case SyslType.SliceType(_) =>
            emit("  addi r1, r1, 12")
            emit("  ldw r1, r1, r0") // cap at offset 12
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // r1 = data pointer; length (== cap) at [r1 - 8]
            emitAddImm(1, 1, -8)
            emit("  ldd r1, r1, r0")
          case SyslType.ArrayType(_, size) =>
            emitLoadImm(1, size) // cap == size for fixed arrays
          case other =>
            throw new RuntimeException(s"codegen: cap() not supported on ${other}")

      case TSliceExpr(array, low, high, SyslType.SliceType(elemType)) =>
        val elemSize = stackSize(elemType)
        // Evaluate array and push {ptr, len, cap} onto stack
        genExpr(array)
        array.typ match
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // r1 = data pointer; len at [r1 - 8], cap = len
            emitAddImm(2, 1, -8)
            emit("  ldd r2, r2, r0")    // r2 = length
            emit("  pshd r2")           // push cap (== len)
            emit("  pshd r2")           // push len
            emit("  pshd r1")           // push ptr
            stackOffset -= 24
          case SyslType.SliceType(_) =>
            // r1 = address of 16-byte slice struct {ptr(8), len(4), cap(4)}
            emit("  addi r2, r1, 12")
            emit("  ldw r2, r2, r0")    // r2 = cap (i32)
            emit("  pshd r2")
            emit("  addi r2, r1, 8")
            emit("  ldw r2, r2, r0")    // r2 = len (i32)
            emit("  pshd r2")
            emit("  ldd r2, r1, r0")    // r2 = ptr
            emit("  pshd r2")
            stackOffset -= 24
          case SyslType.ArrayType(_, size) =>
            // r1 = address of array
            emitLoadImm(2, size)
            emit("  pshd r2")           // cap
            emit("  pshd r2")           // len
            emit("  pshd r1")           // ptr
            stackOffset -= 24
          case _ => throw new RuntimeException(s"codegen: cannot sub-slice ${array.typ}")
        // Stack (top to bottom): [ptr] [len] [cap]

        // Evaluate lo (default 0)
        low match
          case Some(loExpr) => genExpr(loExpr) // r1 = lo
          case None => emit("  ldi r1, 0")
        emit("  pshd r1")              // push lo
        stackOffset -= 8
        // Stack: [lo] [ptr] [len] [cap]

        // Evaluate hi (default len)
        high match
          case Some(hiExpr) => genExpr(hiExpr) // r1 = hi
          case None =>
            // hi = len, at sp+16
            emitAddImm(1, 7, 16)
            emit("  ldd r1, r1, r0")
        emit("  pshd r1")              // push hi
        stackOffset -= 8
        // Stack: [hi] [lo] [ptr] [len] [cap]

        // Load all values from stack into registers
        emit("  popd r1")              // r1 = hi
        emit("  popd r2")              // r2 = lo
        emit("  popd r3")              // r3 = ptr
        // len and cap still on stack
        stackOffset += 24

        // Bounds check: 0 <= lo <= hi <= len
        val errLabel = newLabel("slice_err")
        val okLabel = newLabel("slice_ok")
        emit("  slt r4, r2, r0")       // lo < 0?
        emit(s"  bne r4, r0, $errLabel")
        emit("  slt r4, r1, r2")       // hi < lo?
        emit(s"  bne r4, r0, $errLabel")
        // Check hi <= len: load len from stack (now at sp+0)
        emit("  ldd r4, r7, r0")       // r4 = len
        emit("  slt r4, r4, r1")       // len < hi?
        emit(s"  bne r4, r0, $errLabel")
        emit(s"  bra $okLabel")
        emit(s"$errLabel")
        emit("  ldi r1, 1")           // error code: 1 = out-of-bounds
        emit("  trap 1")
        emit(s"$okLabel")

        // Pop len and cap
        emit("  popd r4")              // r4 = len (unused now, needed only for bounds)
        stackOffset += 8
        emit("  popd r4")              // r4 = cap
        stackOffset += 8

        // Compute result fields:
        // new_len = hi - lo (r1 = hi, r2 = lo)
        emit("  sub r1, r1, r2")       // r1 = new_len
        // new_cap = cap - lo (r4 = cap, r2 = lo)
        emit("  sub r4, r4, r2")       // r4 = new_cap
        // new_ptr = ptr + lo * elemSize (r3 = ptr, r2 = lo)
        if elemSize == 1 then
          emit("  add r3, r3, r2")
        else
          emit("  pshd r1")            // save new_len
          emit("  pshd r3")            // save ptr (mul clobbers r(d+1)=r3)
          emitLoadImm(1, elemSize)
          emit("  mul r2, r2, r1")     // r2 = lo * elemSize (clobbers r3)
          emit("  popd r3")            // restore ptr
          emit("  popd r1")            // restore new_len
          emit("  add r3, r3, r2")     // r3 = new_ptr

        // Allocate 16-byte result on stack: {ptr(8), len(4), cap(4)}
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r3, r7, r0")       // result.ptr = new_ptr
        emit("  addi r2, r7, 8")
        emit("  stw r1, r2, r0")       // result.len = new_len (i32)
        emit("  addi r2, r7, 12")
        emit("  stw r4, r2, r0")       // result.cap = new_cap (i32)
        emit("  mov r1, r7")           // r1 = address of result

      case TAppend(sliceExpr, elemExpr, SyslType.SliceType(elemType)) =>
        val elemSize = stackSize(elemType)
        needsAllocExtern = true

        // Pre-allocate 16-byte result slot
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        val resultOffset = stackOffset

        // Evaluate slice → push ptr, len, cap onto stack
        genExpr(sliceExpr)
        emit("  ldd r2, r1, r0")       // ptr
        emit("  addi r3, r1, 8")
        emit("  ldw r3, r3, r0")       // len
        emit("  addi r4, r1, 12")
        emit("  ldw r4, r4, r0")       // cap
        emit("  pshd r4")              // [cap]
        emit("  pshd r3")              // [len] [cap]
        emit("  pshd r2")              // [ptr] [len] [cap]
        stackOffset -= 24

        // Evaluate elem, push
        genExpr(elemExpr)
        emit("  pshd r1")              // [elem] [ptr] [len] [cap]
        stackOffset -= 8

        // Load all into regs: r1=elem, r2=ptr, r3=len, r4=cap
        emit("  popd r1")
        emit("  popd r2")
        emit("  popd r3")
        emit("  popd r4")
        stackOffset += 32

        val growLabel = newLabel("append_grow")
        val doneLabel = newLabel("append_done")
        emit(s"  beq r3, r4, $growLabel")

        // === No grow: write elem at ptr[len], build result ===
        // Push values we'll need for the result
        emit("  pshd r4")              // save cap
        emit("  pshd r2")              // save ptr
        emit("  pshd r1")              // save elem
        // Stack: [elem] [ptr] [cap]
        // Compute dest = ptr + len * elemSize
        emit("  mov r1, r3")           // r1 = len
        if elemSize != 1 then
          emitLoadImm(4, elemSize)
          emit("  mul r1, r1, r4")     // r1 = len * elemSize (clobbers r2!)
          // reload ptr from stack (at sp+8)
          emitAddImm(2, 7, 8)
          emit("  ldd r2, r2, r0")     // r2 = ptr (reloaded)
        emit("  add r1, r2, r1")       // r1 = dest addr
        emit("  popd r2")              // r2 = elem
        emitStore(2, 1, elemType)      // store elem at dest
        emit("  popd r2")              // r2 = ptr
        emit("  popd r4")              // r4 = cap
        emit("  addi r3, r3, 1")       // new_len = len + 1
        // Write result struct (save r3 — emitAddImm may use r3 as temp for large offsets)
        emit("  pshd r3")              // save new_len
        emitAddImm(1, 5, resultOffset)
        emit("  std r2, r1, r0")       // result.ptr
        emit("  popd r3")              // restore new_len
        emit("  addi r2, r1, 8")
        emit("  stw r3, r2, r0")       // result.len
        emit("  addi r2, r1, 12")
        emit("  stw r4, r2, r0")       // result.cap
        emit(s"  bra $doneLabel")

        // === Grow: malloc, copy, write elem ===
        emit(s"$growLabel")
        // r1=elem, r2=old_ptr, r3=len, r4=cap
        emit("  pshd r1")              // save elem
        emit("  pshd r2")              // save old_ptr
        emit("  pshd r3")              // save len
        // new_cap = max(1, cap * 2)
        val capOk = newLabel("cap_ok")
        val capSet = newLabel("cap_set")
        emit(s"  bne r4, r0, $capOk")
        emit("  ldi r4, 1")            // cap was 0 → new_cap = 1
        emit(s"  bra $capSet")         // skip doubling
        emit(s"$capOk")
        emit("  add r4, r4, r4")       // new_cap = cap * 2
        emit(s"$capSet")
        emit("  pshd r4")              // save new_cap
        // malloc(new_cap * elemSize)
        emit("  mov r1, r4")
        if elemSize != 1 then
          emitLoadImm(2, elemSize)
          emit("  mul r1, r1, r2")     // r1 = new_cap * elemSize
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")          // r1 = new_ptr
        // Null check: trap if malloc returned 0
        val allocOk2 = newLabel("alloc_ok")
        emit(s"  bne r1, r0, $allocOk2")
        emit("  ldi r1, 2")           // error code: 2 = null pointer
        emit("  trap 1")
        emit(s"$allocOk2")
        emit("  pshd r1")              // save new_ptr
        // Stack: [new_ptr] [new_cap] [len] [old_ptr] [elem]
        // Copy len * elemSize bytes from old_ptr to new_ptr
        emit("  mov r2, r1")           // r2 = dst (new_ptr)
        emitAddImm(3, 7, 24)
        emit("  ldd r3, r3, r0")       // r3 = old_ptr
        emitAddImm(4, 7, 16)
        emit("  ldd r4, r4, r0")       // r4 = len
        if elemSize != 1 then
          // mul r4 would clobber r5 (frame pointer!), so compute in r1 instead
          emit("  pshd r2")            // save new_ptr (mul r1 clobbers r2)
          emit("  mov r1, r4")         // r1 = len
          emitLoadImm(4, elemSize)
          emit("  mul r1, r1, r4")     // r1 = len * elemSize (clobbers r2)
          emit("  mov r4, r1")         // r4 = bytes to copy
          emit("  popd r2")            // restore new_ptr
        val copyLoop = newLabel("acopy")
        val copyDone = newLabel("acopy_d")
        emit(s"$copyLoop")
        emit(s"  beq r4, r0, $copyDone")
        emit("  ldb r1, r3, r0")
        emit("  stb r1, r2, r0")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, 1")
        emit("  addi r4, r4, -1")
        emit(s"  bra $copyLoop")
        emit(s"$copyDone")
        // Write elem at new_ptr + len * elemSize
        emit("  popd r2")              // r2 = new_ptr
        emit("  pshd r2")              // re-save new_ptr
        emitAddImm(3, 7, 16)
        emit("  ldd r3, r3, r0")       // r3 = len
        emit("  mov r1, r3")           // r1 = len
        if elemSize != 1 then
          emitLoadImm(4, elemSize)
          emit("  mul r1, r1, r4")     // r1 = len * elemSize (clobbers r2!)
          // reload new_ptr from stack (at sp+0)
          emit("  ldd r2, r7, r0")     // r2 = new_ptr (reloaded)
        emit("  add r1, r2, r1")       // r1 = dest addr
        emitAddImm(4, 7, 32)
        emit("  ldd r4, r4, r0")       // r4 = elem
        emitStore(4, 1, elemType)      // store elem
        // Build result: new_ptr, len+1, new_cap
        emit("  popd r2")              // r2 = new_ptr
        emit("  popd r4")              // r4 = new_cap
        emit("  popd r3")              // r3 = len
        emitAddImm(7, 7, 16)           // pop old_ptr, elem
        emit("  addi r3, r3, 1")       // new_len
        emit("  pshd r3")              // save new_len (emitAddImm may use r3 as temp)
        emitAddImm(1, 5, resultOffset)
        emit("  std r2, r1, r0")       // result.ptr
        emit("  popd r3")              // restore new_len
        emit("  addi r2, r1, 8")
        emit("  stw r3, r2, r0")       // result.len
        emit("  addi r2, r1, 12")
        emit("  stw r4, r2, r0")       // result.cap

        emit(s"$doneLabel")
        emitAddImm(1, 5, resultOffset) // r1 = address of result

      case TStringFromPtr(ptrExpr, lenExpr, _) =>
        // string(ptr, len): allocate refcounted string, copy bytes, build {data_ptr, len}
        needsAllocExtern = true
        genExpr(lenExpr)          // r1 = len
        emit("  pshd r1")        // save len
        stackOffset -= 8
        genExpr(ptrExpr)          // r1 = src ptr
        emit("  pshd r1")        // save src ptr
        stackOffset -= 8
        // Stack: [src_ptr] [len]
        // malloc(8 + len) for refcount header + data
        emitAddImm(1, 7, 8)
        emit("  ldd r1, r1, r0")  // r1 = len
        emit("  addi r1, r1, 8")  // r1 = 8 + len
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")     // r1 = base
        val allocOkS = newLabel("alloc_ok")
        emit(s"  bne r1, r0, $allocOkS")
        emit("  ldi r1, 2")
        emit("  trap 1")
        emit(s"$allocOkS")
        emit("  pshd r1")        // save base
        stackOffset -= 8
        // Stack: [base] [src_ptr] [len]
        // Set refcount = 1
        emit("  ldi r2, 1")
        emit("  std r2, r1, r0")
        // Copy len bytes from src_ptr to base+8
        emit("  addi r2, r1, 8")  // r2 = dst = base+8
        emitAddImm(3, 7, 8)
        emit("  ldd r3, r3, r0")  // r3 = src_ptr
        emitAddImm(4, 7, 16)
        emit("  ldd r4, r4, r0")  // r4 = len
        val cpLoop = newLabel("strcpy")
        val cpDone = newLabel("strcpy_done")
        emit(s"$cpLoop")
        emit(s"  beq r4, r0, $cpDone")
        emit("  ldb r1, r3, r0")
        emit("  stb r1, r2, r0")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, 1")
        emit("  addi r4, r4, -1")
        emit(s"  bra $cpLoop")
        emit(s"$cpDone")
        // Build 16-byte string struct on stack: {ptr=base+8, len}
        emit("  popd r1")        // r1 = base
        stackOffset += 8
        emit("  addi r1, r1, 8")  // r1 = data ptr
        emitAddImm(2, 7, 8)
        emit("  ldd r2, r2, r0")  // r2 = len
        // Clean up src_ptr and len from stack
        emitAddImm(7, 7, 16)
        stackOffset += 16
        // Allocate 16-byte result for string fat pointer
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r1, r7, r0")       // result.ptr
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")       // result.len
        emit("  mov r1, r7")           // r1 = address of result

      case TStringFromSlice(sliceExpr, _) =>
        // string(slice): extract ptr and len from slice, then same as string(ptr, len)
        needsAllocExtern = true
        genExpr(sliceExpr)         // r1 = address of slice struct
        // Load ptr and len from slice
        emit("  addi r2, r1, 8")
        emit("  ldw r2, r2, r0")  // r2 = len (i32)
        emit("  ldd r1, r1, r0")  // r1 = ptr
        emit("  pshd r2")         // save len
        stackOffset -= 8
        emit("  pshd r1")         // save src ptr
        stackOffset -= 8
        // Stack: [src_ptr] [len]
        // malloc(8 + len)
        emit("  addi r1, r2, 8")  // r1 = 8 + len
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")
        val allocOkSl = newLabel("alloc_ok")
        emit(s"  bne r1, r0, $allocOkSl")
        emit("  ldi r1, 2")
        emit("  trap 1")
        emit(s"$allocOkSl")
        emit("  pshd r1")        // save base
        stackOffset -= 8
        // Stack: [base] [src_ptr] [len]
        emit("  ldi r2, 1")
        emit("  std r2, r1, r0") // refcount = 1
        // Copy len bytes from src_ptr to base+8
        emit("  addi r2, r1, 8")  // r2 = dst
        emitAddImm(3, 7, 8)
        emit("  ldd r3, r3, r0")  // r3 = src_ptr
        emitAddImm(4, 7, 16)
        emit("  ldd r4, r4, r0")  // r4 = len
        val cpLoop2 = newLabel("strcpy")
        val cpDone2 = newLabel("strcpy_done")
        emit(s"$cpLoop2")
        emit(s"  beq r4, r0, $cpDone2")
        emit("  ldb r1, r3, r0")
        emit("  stb r1, r2, r0")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, 1")
        emit("  addi r4, r4, -1")
        emit(s"  bra $cpLoop2")
        emit(s"$cpDone2")
        // Build string struct
        emit("  popd r1")        // r1 = base
        stackOffset += 8
        emit("  addi r1, r1, 8")  // r1 = data ptr
        emitAddImm(2, 7, 8)
        emit("  ldd r2, r2, r0")  // r2 = len
        emitAddImm(7, 7, 16)
        stackOffset += 16
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r1, r7, r0")
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
        emit("  mov r1, r7")

      case TFloatLit(d, _) =>
        emit(s"  ldc r1, $d")

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
        needsAllocExtern = true
        // Null check: trap if malloc returned 0
        val allocOk = newLabel("alloc_ok")
        emit(s"  bne r1, r0, $allocOk")
        emit("  ldi r1, 2")         // error code: 2 = null pointer
        emit("  trap 1")
        emit(s"$allocOk")
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
        emit("  pshd r3")            // save data start (mul r2 clobbers r3)
        emit("  mul r2, r2, r4")     // r2 = n * elemSize (clobbers r3)
        emit("  popd r3")            // restore data start
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
        needsAllocExtern = true
        // Null check: trap if malloc returned 0
        val allocOk = newLabel("alloc_ok")
        emit(s"  bne r1, r0, $allocOk")
        emit("  ldi r1, 2")         // error code: 2 = null pointer
        emit("  trap 1")
        emit(s"$allocOk")
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

      case TEnumConstruct(et, variantIndex, args) =>
        // Allocate enum-sized space on stack and zero-initialize
        val totalSize = stackSize(et)
        val aligned = (totalSize + 7) & ~7
        emitAddImm(7, 7, -aligned)
        stackOffset -= aligned
        val enumBaseOffset = stackOffset
        emit("  mov r1, r7")
        for i <- 0 until aligned by 8 do
          emitAddImm(2, 1, i)
          emit("  std r0, r2, r0")
        // Write tag (i32 at offset 0)
        emitLoadImm(1, variantIndex)
        emitAddImm(2, 5, enumBaseOffset)
        emit("  stw r1, r2, r0")
        // Write variant fields at data offset
        val dataOff = et.dataOffset.toInt
        val variantFields = et.variants(variantIndex)._2
        // Compute field offsets within variant data (laid out like a struct)
        var fieldOff = 0
        for (arg, i) <- args.zipWithIndex do
          val (_, fieldType) = variantFields(i)
          val align = stackAlign(fieldType)
          fieldOff = ((fieldOff + align - 1) / align) * align
          genExpr(arg) // r1 = field value
          emitAddImm(2, 5, enumBaseOffset + dataOff + fieldOff)
          emitStore(1, 2, fieldType)
          fieldOff += fieldType.sizeOf.toInt
        // r1 = enum base address
        emitAddImm(1, 5, enumBaseOffset)

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

      case other =>
        throw new RuntimeException(s"codegen: unhandled expression type: ${other.getClass.getSimpleName}")

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

  // Emit the __str_int helper function: converts i64 in stack arg to a refcounted string
  // ABI: r1 = hidden return slot ptr, [fp+24] = integer value
  // Returns: {ptr, len} written to return slot, r1 = return slot address
  private def emitStrIntHelper(): Unit =
    emit("# helper: __str_int(value: int) -> string")
    emit("global __str_int, func, 1 i64 i64")
    emit("__str_int:")
    // Pre-prologue: save register arg (hidden return ptr)
    emit("  pshd r1")
    // Prologue
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    // [fp+16] = hidden return ptr, [fp+24] = integer value
    // Allocate locals: 8 (is_negative) + 8 (digit_count) + 24 (digit buffer) = 40 bytes
    emitAddImm(7, 7, -40)
    // [fp-8]  = is_negative
    // [fp-16] = digit_count
    // [fp-40] = digit_buffer[24]

    // Load integer value from [fp+24]
    emitAddImm(2, 5, 24)
    emit("  ldd r1, r2, r0")       // r1 = value

    // Check sign: slt r2, r1, r0 → r2 = 1 if value < 0
    emit("  slt r2, r1, r0")
    emitAddImm(3, 5, -8)
    emit("  std r2, r3, r0")       // save is_negative
    val posLabel = newLabel("str_pos")
    emit(s"  beq r2, r0, $posLabel")
    emit("  neg r1, r1")           // r1 = |value|
    emit(s"$posLabel")

    // Extract digits reversed into buffer
    emit("  ldi r3, 0")            // r3 = digit_count

    // Special case: value == 0
    val loopLabel = newLabel("str_loop")
    val doneLabel = newLabel("str_done")
    emit(s"  bne r1, r0, $loopLabel")
    emitAddImm(4, 5, -40)
    emit("  ldi r2, 48")           // '0'
    emit("  stb r2, r4, r0")
    emit("  ldi r3, 1")
    emit(s"  bra $doneLabel")

    // Division loop: extract digits
    emit(s"$loopLabel")
    emit(s"  beq r1, r0, $doneLabel")
    // Save digit_count to [fp-16] (div will clobber r2)
    emitAddImm(4, 5, -16)
    emit("  std r3, r4, r0")
    emit("  ldi r3, 10")
    emit("  div r1, r1, r3")       // r1 = quotient, r2 = remainder
    emit("  addi r2, r2, 48")      // r2 = ASCII digit
    // Restore digit_count
    emitAddImm(4, 5, -16)
    emit("  ldd r3, r4, r0")
    // Store digit at buffer[count]
    emitAddImm(4, 5, -40)
    emit("  add r4, r4, r3")       // r4 = &buffer[count]
    emit("  stb r2, r4, r0")
    emit("  addi r3, r3, 1")       // count++
    emit(s"  bra $loopLabel")

    emit(s"$doneLabel")
    // Save digit_count to [fp-16]
    emitAddImm(4, 5, -16)
    emit("  std r3, r4, r0")

    // Compute total_length = digit_count + is_negative
    emitAddImm(4, 5, -8)
    emit("  ldd r2, r4, r0")       // r2 = is_negative
    emit("  add r1, r3, r2")       // r1 = total_length
    emit("  pshd r1")              // save total_length

    // Malloc(8 + total_length) for refcount header + data
    emit("  addi r1, r1, 8")
    emit("  movi r4, malloc")
    emit("  jalr r6, r4")

    // Null check
    val allocOkLabel = newLabel("str_alloc_ok")
    emit(s"  bne r1, r0, $allocOkLabel")
    emit("  ldi r1, 2")
    emit("  trap 1")
    emit(s"$allocOkLabel")

    // Set refcount = 1 at [base+0]
    emit("  ldi r2, 1")
    emit("  std r2, r1, r0")

    // data_ptr = base + 8
    emit("  addi r1, r1, 8")

    // Pop total_length
    emit("  popd r3")               // r3 = total_length

    // Save data_ptr and total_length
    emit("  pshd r1")               // save data_ptr
    emit("  pshd r3")               // save total_length

    // Write '-' if negative
    emitAddImm(4, 5, -8)
    emit("  ldd r2, r4, r0")        // r2 = is_negative
    val noSignLabel = newLabel("str_nosign")
    emit(s"  beq r2, r0, $noSignLabel")
    emit("  ldi r2, 45")            // '-'
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")
    emit(s"$noSignLabel")

    // Copy digits in reverse: buffer[count-1]..buffer[0] → data
    emitAddImm(4, 5, -16)
    emit("  ldd r3, r4, r0")        // r3 = digit_count
    emitAddImm(4, 5, -40)
    emit("  add r4, r4, r3")        // r4 = &buffer[count] (one past last)

    val copyLabel = newLabel("str_copy")
    val copyDoneLabel = newLabel("str_copy_done")
    emit(s"$copyLabel")
    emit(s"  beq r3, r0, $copyDoneLabel")
    emit("  addi r4, r4, -1")       // r4 = &buffer[--i]
    emit("  ldb r2, r4, r0")        // r2 = digit
    emit("  stb r2, r1, r0")        // *dest = digit
    emit("  addi r1, r1, 1")
    emit("  addi r3, r3, -1")
    emit(s"  bra $copyLabel")
    emit(s"$copyDoneLabel")

    // Pop total_length and data_ptr
    emit("  popd r3")               // r3 = total_length
    emit("  popd r1")               // r1 = data_ptr

    // Write {ptr, len} to return slot
    emitAddImm(4, 5, 16)
    emit("  ldd r4, r4, r0")        // r4 = return slot address
    emit("  std r1, r4, r0")        // [ret+0] = data_ptr
    emit("  addi r2, r4, 8")
    emit("  std r3, r2, r0")        // [ret+8] = total_length

    // r1 = return slot address (ABI)
    emit("  mov r1, r4")

    // Epilogue
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emitAddImm(7, 7, 8)             // skip 1 reg param
    emit("  jalr r0, r6")

  private def emit(line: String): Unit =
    out ++= line
    out += '\n'
