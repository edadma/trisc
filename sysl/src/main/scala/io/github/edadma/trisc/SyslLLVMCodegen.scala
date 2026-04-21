package io.github.edadma.trisc

import scala.collection.mutable
import scala.compiletime.uninitialized

class SyslLLVMCodegen(target: String = "host"):
  private val out = new StringBuilder
  private var activeOut: StringBuilder = out // emit writes here; switches between out and bodyBuf
  private val bodyBuf = new StringBuilder // body code buffer during function generation
  private val deferredAllocas = new mutable.ListBuffer[(String, String)] // (reg, llvmType) — allocas deferred to entry block
  private val stringConstants = new mutable.LinkedHashMap[String, (String, Int)] // value -> (label, byte length including null) — sysl strings, with i64 refcount header
  private val cStringConstants = new mutable.LinkedHashMap[String, (String, Int)] // value -> (label, byte length including null) — raw C strings (for printf format strings, etc.)
  private val structTypes = new mutable.LinkedHashMap[String, SyslType.StructType] // name -> struct type
  private val deinitFunctions = new mutable.HashMap[String, String] // struct name -> deinit function name

  // Slice deinit functions to emit (for &[]T where T contains rc data). Generated
  // on demand when a slice with rc-content elements is freed; emitted after all
  // user functions. Each takes the data pointer (past the 16-byte rc/len header)
  // and decrs every element before returning.
  private val sliceElemDeinitsNeeded = new mutable.LinkedHashMap[String, SyslType] // deinit name -> elem type

  // Per-enum-type deinit functions to emit (for &MyEnum where the enum has
  // string-bearing variants). Generated on demand when an enum ref is freed;
  // each takes the data ptr (past the rc header) and walks the active
  // variant's strings via emitEnumStringFieldsDecr.
  private val enumDeinitsNeeded = new mutable.LinkedHashMap[String, SyslType.EnumType] // deinit name -> enum type

  // Auto-synthesized struct deinit functions (for &MyStruct where the struct
  // has string-bearing fields and the user hasn't defined `TypeName.deinit`).
  // Called by emitRefDecr at rc=0 before free().
  private val structDeinitsNeeded = new mutable.LinkedHashMap[String, SyslType.StructType] // deinit name -> struct type
  private var closureCounter = 0
  private val pendingClosures = new mutable.ListBuffer[(String, TClosure)] // (name, closure)
  // Per-closure-id env deinit functions to emit (only for closures whose captures carry rc content).
  // Maps closure name → its TClosure (for capture layout).
  private val closureEnvDeinitsNeeded = new mutable.LinkedHashMap[String, TClosure]
  private var closureEnvDispatchNeeded = false
  // Borrowed-capture local names in a closure body (skip rc cleanup — env owns the buffers)
  private var captureBorrows: Set[String] = Set.empty
  // FuncType/InterfaceType param names in current function — borrowed; do not decr env on exit
  private var funcBorrowParams: Set[String] = Set.empty

  /** Kind of env backing a closure descriptor. See SyslTriscCodegen for full
    * notes — same model on LLVM. */
  private enum FuncKind:
    case NullEnv, StackEnv, HeapEnv

  /** Per-FuncType-local kind tracking. Reset in genFunction / genClosureFunction. */
  private val closureLocalKind = new mutable.HashMap[String, FuncKind]

  private def captureNeedsRc(t: SyslType): Boolean =
    t.isInstanceOf[SyslType.RefType] || structHasStringFields(t)

  private def closureKindOf(c: TClosure): FuncKind =
    if c.captures.isEmpty then FuncKind.NullEnv
    else if c.escapes || c.captures.exists((_, t) => captureNeedsRc(t)) then FuncKind.HeapEnv
    else FuncKind.StackEnv

  /** TCall / TIndirectCall return: assume HeapEnv. The callee can't return a
    * StackEnv (its stack is gone after return), so the value is either NullEnv
    * (env_ptr=null — dispatch's null-check skips) or HeapEnv (decr properly).
    * Defaulting to HeapEnv closes the cross-function heap-env return leak with
    * a tiny runtime null-check cost on NullEnv returns. LLVM unconditionally
    * declares malloc/free in the preamble so there's no spurious-symbol concern. */
  private def funcKindOfExpr(e: TExpr): FuncKind = e match
    case c: TClosure => closureKindOf(c)
    case _: TFuncRef => FuncKind.NullEnv
    case TVarRef(name, _) if funcBorrowParams.contains(name) =>
      // FuncType params are borrowed from the caller — treat as HeapEnv (could
      // be NullEnv at runtime; dispatch's null-check handles that). Lets the
      // copy/return paths emit the right incr to balance shared ownership.
      FuncKind.HeapEnv
    case TVarRef(name, _) => closureLocalKind.getOrElse(name, FuncKind.NullEnv)
    case _: TCall | _: TIndirectCall | _: TInterfaceDispatch => FuncKind.HeapEnv
    case _ => FuncKind.NullEnv
  private val funcWrappers = new mutable.LinkedHashMap[String, String] // original name -> wrapper name
  private val pendingWrappers = new mutable.ListBuffer[(String, String, List[SyslType], SyslType)] // (wrapperName, origName, params, retType)
  private val emittedFunctions = new mutable.HashSet[String] // track emitted function names to avoid duplicates
  // C library functions declared in the preamble — skip any extern decl with these names
  private val preambleNames = Set("putchar", "printf", "snprintf", "malloc", "strlen", "memcpy", "memcmp", "memset", "free", "write", "fflush", "abort", "exit")
  // Function parameter types — used to widen arguments at call sites (e.g., i8 → i32 for char params)
  private val funcParamTypes = new mutable.HashMap[String, List[String]]
  // Track pointer variables derived from slice element addresses (&slot[i])
  // Maps pointer variable name → source slice variable name
  private var derivedFromSlice: mutable.HashMap[String, String] = uninitialized
  // Module-level global variable types — needed for compound assignment on globals
  private val globalVarTypes = new mutable.HashMap[String, SyslType]

  /** Resolve a struct type to its canonical (field-populated) version from structTypes.
    * Handles stale placeholder StructType(_, Nil) references that can appear in expression types. */
  private def canonicalStruct(st: SyslType.StructType): SyslType.StructType =
    structTypes.getOrElse(st.name, st)
  private var stringCounter = 0
  private var regCounter = 0
  private var labelCounter = 0

  private def newReg(): String =
    regCounter += 1
    s"%t$regCounter"

  private def newLabel(prefix: String): String =
    labelCounter += 1
    s"${prefix}_$labelCounter"

  /** Allocate in the entry block. Returns the register name. The actual alloca instruction is emitted later. */
  private def deferAlloca(lt: String): String =
    val reg = newReg()
    deferredAllocas += ((reg, lt))
    reg

  /** Intern a sysl string literal — emits a global with an immortal refcount header.
    * The label refers to the WRAPPING `<{ i64, [byteLen x i8] }>` constant, NOT the data ptr.
    * Use `gepStringDataConst(label, byteLen)` to get a constant data-ptr expression. */
  private def internString(s: String): (String, Int) =
    stringConstants.getOrElseUpdate(s, {
      stringCounter += 1
      val label = s"@.sstr.$stringCounter"
      val byteLen = s.getBytes("UTF-8").length + 1 // +1 for null terminator
      (label, byteLen)
    })

  /** Intern a raw C-style string (e.g. printf format string) — no refcount header.
    * Returns (label, byteLen) where label refers to a `[byteLen x i8]` global. */
  private def internCString(s: String): (String, Int) =
    cStringConstants.getOrElseUpdate(s, {
      stringCounter += 1
      val label = s"@.cstr.$stringCounter"
      val byteLen = s.getBytes("UTF-8").length + 1
      (label, byteLen)
    })

  /** Build an LLVM constant expression for the data pointer of a sysl string literal. */
  private def gepStringDataConst(label: String, byteLen: Int): String =
    s"getelementptr inbounds (<{ i64, [$byteLen x i8] }>, <{ i64, [$byteLen x i8] }>* $label, i32 0, i32 1, i32 0)"

  private case class LocalVar(name: String, reg: String, typ: SyslType, isVolatile: Boolean = false)

  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private val volatileGlobals = new mutable.HashSet[String] // global variable names that are volatile
  private var currentFunction: TFunDecl = null
  private var hasReturned = false
  private var currentBlock = "" // tracks the current basic block label for phi predecessors
  // Break/continue label stacks for loop codegen
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]
  // Scope snapshots for loop body cleanup (parallel to break/continue stacks)
  private val loopScopeSnapshots = new mutable.Stack[Set[String]]
  // Defer stack — LIFO execution before returns
  private val deferStack = new mutable.Stack[TStmt]

  def generate(program: TProgram): String =
    out.clear()
    stringConstants.clear()
    cStringConstants.clear()
    stringCounter = 0
    closureCounter = 0
    pendingClosures.clear()
    closureEnvDeinitsNeeded.clear()
    closureEnvDispatchNeeded = false
    pendingWrappers.clear()
    funcWrappers.clear()
    emittedFunctions.clear()

    // First pass: collect struct type definitions and deinit functions
    structTypes.clear()
    deinitFunctions.clear()
    sliceElemDeinitsNeeded.clear()
    enumDeinitsNeeded.clear()
    structDeinitsNeeded.clear()
    for decl <- program.decls do
      decl match
        case TStructDecl(name, fields, volFields) =>
          structTypes(name) = SyslType.StructType(name, fields, volFields)
        case TFunDecl(name, _, _, _, _, _, _) if name.endsWith("_deinit") =>
          val structName = name.indexOf("__") match
            case -1 => name.dropRight(7) // "Point_deinit" -> "Point"
            case i  => name.substring(i + 2).dropRight(7) // "mod__Point_deinit" -> "Point"
          deinitFunctions(structName) = name
        case _ =>

    // Collect all function names that will be defined in this compilation unit
    val definedFuncNames = program.decls.collect { case TFunDecl(name, _, _, _, _, _, _) => name }.toSet

    // Pre-populate funcParamTypes for ALL functions before generating any code.
    // Without this, calls to functions defined later in the file would not know
    // the parameter types, causing aggregate arguments (arrays, structs) to be
    // passed by value instead of by pointer — a silent ABI mismatch.
    for decl <- program.decls do
      decl match
        case TExternFuncDecl(name, params, _) =>
          funcParamTypes(name) = params.map(llvmType)
        case f: TFunDecl =>
          funcParamTypes(f.name) = f.params.map(p => llvmType(p.typ))
        case _ =>

    // Generate functions into a buffer so string constants are collected first
    out.clear()
    for decl <- program.decls do
      decl match
        case _: TModuleDecl => // skip
        case _: TImportDecl => // skip
        case TExternFuncDecl(name, params, retType) =>
          // Skip extern declarations that conflict with preamble C declarations
          // or that are defined later in this compilation unit
          if !preambleNames.contains(name) && !definedFuncNames.contains(name) then
            val paramStr = params.map(llvmType).mkString(", ")
            emit(s"declare ${llvmType(retType)} @$name($paramStr)")
        case TExternVarDecl(name, typ) =>
          emit(s"@$name = external global ${llvmType(typ)}")
        case _: TStructDecl => // skip (handled above)
        case _: TEnumDecl => // type only
        case _: TDataEnumDecl => // type only
        case _: TTypeAliasDecl => // type only
        case _: TConstDecl => // const is fully folded at analyzer level
        case _: TInterfaceDecl => // type only
        case f: TFunDecl =>
          if !emittedFunctions.contains(f.name) then
            emittedFunctions += f.name
            genFunction(f)
        case TVarDecl(name, typ, init, _, isVolatile) =>
          val initVal = constValue(init, typ)
          emit(s"@$name = global ${llvmType(typ)} $initVal")
          globalVarTypes(name) = typ
          if isVolatile then volatileGlobals += name
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
    // Emit slice element deinit functions (registered when freeing slices of
    // rc-content elements). Iterating may register more types, so drain a worklist.
    val emittedSliceDeinits = mutable.Set.empty[String]
    while sliceElemDeinitsNeeded.exists((n, _) => !emittedSliceDeinits.contains(n)) do
      val pending = sliceElemDeinitsNeeded.filterNot((n, _) => emittedSliceDeinits.contains(n)).toList
      for (name, elem) <- pending do
        emittedSliceDeinits += name
        emitSliceDeinit(name, elem)
    // Per-enum-type deinit functions (for &MyEnum with rc-bearing variants)
    val emittedEnumDeinits = mutable.Set.empty[String]
    while enumDeinitsNeeded.exists((n, _) => !emittedEnumDeinits.contains(n)) do
      val pending = enumDeinitsNeeded.filterNot((n, _) => emittedEnumDeinits.contains(n)).toList
      for (name, et) <- pending do
        emittedEnumDeinits += name
        emitEnumDeinit(name, et)

    // Auto-synthesized per-struct-type deinit functions (for &MyStruct with
    // rc-bearing fields and no user `TypeName.deinit`).
    val emittedStructDeinits = mutable.Set.empty[String]
    while structDeinitsNeeded.exists((n, _) => !emittedStructDeinits.contains(n)) do
      val pending = structDeinitsNeeded.filterNot((n, _) => emittedStructDeinits.contains(n)).toList
      for (name, st) <- pending do
        emittedStructDeinits += name
        emitStructDeinit(name, st)
    // Per-closure-id env deinit functions
    for (name, closure) <- closureEnvDeinitsNeeded do
      emitClosureEnvDeinit(name, closure)
    if closureEnvDispatchNeeded then emitClosureEnvDispatch()
    val funcCode = out.toString

    // Now build final output with string constants at the top
    out.clear()

    // Target datalayout and triple — required for LLVM to compute correct struct layout
    target match
      case "x86_64" | "x86_64-elf" =>
        emit("""target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"""")
        emit("""target triple = "x86_64-unknown-elf"""")
      case "x86_64-linux" =>
        emit("""target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"""")
        emit("""target triple = "x86_64-unknown-linux-gnu"""")
      case "aarch64" | "aarch64-elf" =>
        emit("""target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"""")
        emit("""target triple = "aarch64-unknown-elf"""")
      case "aarch64-linux" =>
        emit("""target datalayout = "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128"""")
        emit("""target triple = "aarch64-unknown-linux-gnu"""")
      case _ => // no target declarations for unknown targets
    emit("")

    // Declare external C functions (skip any that are defined by Sysl code)
    val definePattern = """(?m)^define [^@]*@(\w+)\(""".r
    val definedNames = definePattern.findAllMatchIn(funcCode).map(_.group(1)).toSet
    def declareIfNotDefined(decl: String, name: String): Unit =
      if !definedNames.contains(name) then emit(decl)
    declareIfNotDefined("declare i32 @putchar(i32)", "putchar")
    declareIfNotDefined("declare i32 @printf(i8*, ...)", "printf")
    declareIfNotDefined("declare i32 @snprintf(i8*, i64, i8*, ...)", "snprintf")
    declareIfNotDefined("declare i8* @malloc(i64)", "malloc")
    declareIfNotDefined("declare i64 @strlen(i8*)", "strlen")
    declareIfNotDefined("declare i8* @memcpy(i8*, i8*, i64)", "memcpy")
    declareIfNotDefined("declare i32 @memcmp(i8*, i8*, i64)", "memcmp")
    declareIfNotDefined("declare i8* @memset(i8*, i32, i64)", "memset")
    declareIfNotDefined("declare void @free(i8*)", "free")
    emit("declare void @llvm.memset.p0i8.i64(i8*, i8, i64, i1)")
    // Saturating arithmetic intrinsics for wrapping_*/saturating_* builtins.
    for w <- Seq(8, 16, 32, 64) do
      emit(s"declare i$w @llvm.sadd.sat.i$w(i$w, i$w)")
      emit(s"declare i$w @llvm.uadd.sat.i$w(i$w, i$w)")
      emit(s"declare i$w @llvm.ssub.sat.i$w(i$w, i$w)")
      emit(s"declare i$w @llvm.usub.sat.i$w(i$w, i$w)")
      emit(s"declare {i$w, i1} @llvm.smul.with.overflow.i$w(i$w, i$w)")
      emit(s"declare {i$w, i1} @llvm.umul.with.overflow.i$w(i$w, i$w)")
    declareIfNotDefined("declare i64 @write(i32, i8*, i64)", "write")
    declareIfNotDefined("declare i32 @fflush(i8*)", "fflush")
    declareIfNotDefined("declare void @abort()", "abort")
    declareIfNotDefined("declare void @exit(i32)", "exit")
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
    // Slice struct type: { ptr, len, cap, backref }
    // backref: pointer to allocation base (refcount header) when slice borrows from a ref, null otherwise
    emit("%struct.slice = type { i8*, i32, i32, i8* }")
    // Closure struct type: { func_ptr, env_ptr }
    emit("%struct.closure = type { i8*, i8* }")
    emit("")

    // Emit struct type definitions
    for (name, st) <- structTypes do
      val fieldTypes = st.fields.map((_, ft) => llvmType(ft)).mkString(", ")
      emit(s"%struct.$name = type { $fieldTypes }")
    if structTypes.nonEmpty then emit("")

    // Helper: escape a string for LLVM c"..." form
    def escapeForLlvm(s: String): String = s.flatMap {
      case '\n' => "\\0A"
      case '\r' => "\\0D"
      case '\t' => "\\09"
      case '\\' => "\\5C"
      case '"'  => "\\22"
      case '\u0000' => "\\00"
      case c    => c.toString
    }
    // Emit raw C-string constants (no refcount header) — used for printf format strings, etc.
    for (s, (label, byteLen)) <- cStringConstants do
      emit(s"""$label = private unnamed_addr constant [$byteLen x i8] c"${escapeForLlvm(s)}\\00"""")
    if cStringConstants.nonEmpty then emit("")
    // Emit sysl string literal constants — packed `<{ i64, [N x i8] }>` with refcount sentinel -1 (immortal).
    // Data pointer in fat string descriptors points past the i64 to the bytes.
    for (s, (label, byteLen)) <- stringConstants do
      emit(s"""$label = private unnamed_addr constant <{ i64, [$byteLen x i8] }> <{ i64 -1, [$byteLen x i8] c"${escapeForLlvm(s)}\\00" }>""")
    if stringConstants.nonEmpty then emit("")

    // Built-in panic function: write message to stderr and abort
    emit("@.str.panic_prefix = private unnamed_addr constant [8 x i8] c\"panic: \\00\"")
    emit("")
    emit("define void @panic(%struct.string %msg) {")
    emit("entry:")
    emit("  %prefix = getelementptr [8 x i8], [8 x i8]* @.str.panic_prefix, i32 0, i32 0")
    emit("  %w1 = call i64 @write(i32 2, i8* %prefix, i64 7)")
    emit("  %ptr = extractvalue %struct.string %msg, 0")
    emit("  %len = extractvalue %struct.string %msg, 1")
    emit("  %len64 = sext i32 %len to i64")
    emit("  %w2 = call i64 @write(i32 2, i8* %ptr, i64 %len64)")
    emit("  %nl = getelementptr [1 x i8], [1 x i8]* @.str.newline, i32 0, i32 0")
    emit("  %w3 = call i64 @write(i32 2, i8* %nl, i64 1)")
    emit("  call void @abort()")
    emit("  unreachable")
    emit("}")
    emit("")

    // Built-in range-check failure helper: write "range check failed: <alias>\n" to stderr, abort.
    emit("@.str.range_prefix = private unnamed_addr constant [21 x i8] c\"range check failed: \\00\"")
    emit("")
    emit("define void @__range_fail(i8* %name, i64 %len) {")
    emit("entry:")
    emit("  %prefix = getelementptr [21 x i8], [21 x i8]* @.str.range_prefix, i32 0, i32 0")
    emit("  %w1 = call i64 @write(i32 2, i8* %prefix, i64 20)")
    emit("  %w2 = call i64 @write(i32 2, i8* %name, i64 %len)")
    emit("  %nl = getelementptr [1 x i8], [1 x i8]* @.str.newline, i32 0, i32 0")
    emit("  %w3 = call i64 @write(i32 2, i8* %nl, i64 1)")
    emit("  call void @abort()")
    emit("  unreachable")
    emit("}")
    emit("")

    // Built-in assert function: if !cond then panic(msg)
    emit("@.str.assert_prefix = private unnamed_addr constant [19 x i8] c\"assertion failed: \\00\"")
    emit("")
    emit("define void @assert(i8 %cond, %struct.string %msg) {")
    emit("entry:")
    emit("  %c = icmp ne i8 %cond, 0")
    emit("  br i1 %c, label %ok, label %fail")
    emit("ok:")
    emit("  ret void")
    emit("fail:")
    emit("  %prefix = getelementptr [19 x i8], [19 x i8]* @.str.assert_prefix, i32 0, i32 0")
    emit("  %w1 = call i64 @write(i32 2, i8* %prefix, i64 18)")
    emit("  %ptr = extractvalue %struct.string %msg, 0")
    emit("  %len = extractvalue %struct.string %msg, 1")
    emit("  %len64 = sext i32 %len to i64")
    emit("  %w2 = call i64 @write(i32 2, i8* %ptr, i64 %len64)")
    emit("  %nl = getelementptr [1 x i8], [1 x i8]* @.str.newline, i32 0, i32 0")
    emit("  %w3 = call i64 @write(i32 2, i8* %nl, i64 1)")
    emit("  call void @abort()")
    emit("  unreachable")
    emit("}")
    emit("")

    // Append function code
    out ++= funcCode

    out.toString

  private def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    derivedFromSlice = new mutable.HashMap
    regCounter = 0
    labelCounter = 0
    hasReturned = false
    deferStack.clear()
    funcBorrowParams = fun.params.collect {
      case p if p.typ.isInstanceOf[SyslType.FuncType] || p.typ.isInstanceOf[SyslType.InterfaceType] => p.name
    }.toSet
    closureLocalKind.clear()

    val retType = llvmType(fun.returnType)
    val params = fun.params.map(p => s"${llvmType(p.typ)} %${p.name}_arg").mkString(", ")

    emit(s"define $retType @${fun.name}($params) {")
    emitLabel("entry")

    // Allocate and store parameters (these stay in entry block directly)
    for param <- fun.params do
      val lt = llvmType(param.typ)
      val alloca = newReg()
      emit(s"  $alloca = alloca $lt")
      emit(s"  store $lt %${param.name}_arg, $lt* $alloca")
      locals(param.name) = LocalVar(param.name, alloca, param.typ)
      // Increment refcount for ref-typed params (caller shares ownership)
      if isRef(param.typ) then
        emitRefIncr(s"%${param.name}_arg", refHeaderOffset(param.typ))
      // Increment slice backref for slice params (callee holds a copy)
      if isSliceType(param.typ) then
        emitSliceBackrefIncr(alloca)
      // Increment string buffer refcount for string params (callee holds a copy)
      if param.typ == SyslType.StringType then
        emitStringDescrIncr(alloca)
      // Value-struct params containing strings: incr string fields (callee holds a copy)
      param.typ match
        case st: SyslType.StructType if structHasStringFields(st) =>
          emitStructStringFieldsIncr(alloca, st)
        case _ =>

    // Switch to body buffer for the function body
    deferredAllocas.clear()
    bodyBuf.clear()
    activeOut = bodyBuf

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        val result = genExpr(expr)
        emitFuncReturnIncrIfBorrowed(expr, result)
        val rt = exprType(expr)
        val finalVal = if isAggregate(expr.typ) then
          val loaded = newReg()
          emit(s"  $loaded = load $retType, $retType* $result")
          loaded
        else if rt == "void" && retType != "void" then
          if retType.startsWith("[") || retType.startsWith("%struct.") then
            val loaded = newReg()
            emit(s"  $loaded = load $retType, $retType* $result")
            loaded
          else result
        else emitSextIfNeeded(result, rt, retType, expr.typ.isSigned)
        emitReleaseRefs(returnedSliceAllocas(expr))
        emitRet(retType, finalVal)
      case TBlockBody(stmts) =>
        genBlock(stmts, retType)

    // Assemble: switch back to out, emit deferred allocas, then body
    activeOut = out
    for (reg, lt) <- deferredAllocas do
      emit(s"  $reg = alloca $lt")
      // Zero-initialize allocas that may be decremented on unexecuted paths
      if lt == "%struct.slice" then
        emit(s"  store %struct.slice zeroinitializer, %struct.slice* $reg")
      else if lt == "i8*" then
        emit(s"  store i8* null, i8** $reg")
    out ++= bodyBuf

    emit("}")
    emit("")
    locals = null
    currentFunction = null
    funcBorrowParams = Set.empty

  /** Generate a closure function with hidden env_ptr first parameter. */
  private def genClosureFunction(name: String, closure: TClosure): Unit =
    locals = new mutable.LinkedHashMap
    derivedFromSlice = new mutable.HashMap
    regCounter = 0
    labelCounter = 0
    hasReturned = false
    deferStack.clear()
    captureBorrows = closure.captures.map(_._1).toSet
    funcBorrowParams = closure.params.collect {
      case p if p.typ.isInstanceOf[SyslType.FuncType] || p.typ.isInstanceOf[SyslType.InterfaceType] => p.name
    }.toSet
    closureLocalKind.clear()

    val retLt = llvmType(closure.returnType)
    val paramStrs = "i8* %env" +: closure.params.map(p => s"${llvmType(p.typ)} %${p.name}_arg")

    emit(s"define $retLt @$name(${paramStrs.mkString(", ")}) {")
    emitLabel("entry")

    // Unpack captured variables from env (stays in entry block)
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

    // Allocate and store regular parameters (stays in entry block)
    for param <- closure.params do
      val lt = llvmType(param.typ)
      val alloca = newReg()
      emit(s"  $alloca = alloca $lt")
      emit(s"  store $lt %${param.name}_arg, $lt* $alloca")
      locals(param.name) = LocalVar(param.name, alloca, param.typ)
      if isSliceType(param.typ) then
        emitSliceBackrefIncr(alloca)

    // Switch to body buffer
    deferredAllocas.clear()
    bodyBuf.clear()
    activeOut = bodyBuf

    // Generate body
    closure.body match
      case TExprBody(expr) =>
        val result = genExpr(expr)
        emitFuncReturnIncrIfBorrowed(expr, result)
        val rt = exprType(expr)
        val finalVal = if isAggregate(expr.typ) then
          val loaded = newReg()
          emit(s"  $loaded = load $retLt, $retLt* $result")
          loaded
        else emitSextIfNeeded(result, rt, retLt, expr.typ.isSigned)
        emitReleaseRefs(returnedSliceAllocas(expr))
        emitRet(retLt, finalVal)
      case TBlockBody(stmts) =>
        genBlock(stmts, retLt)

    // Assemble: switch back to out, emit deferred allocas, then body
    activeOut = out
    for (reg, lt) <- deferredAllocas do
      emit(s"  $reg = alloca $lt")
      if lt == "%struct.slice" then
        emit(s"  store %struct.slice zeroinitializer, %struct.slice* $reg")
      else if lt == "i8*" then
        emit(s"  store i8* null, i8** $reg")
    out ++= bodyBuf

    emit("}")
    emit("")
    locals = null
    captureBorrows = Set.empty
    funcBorrowParams = Set.empty

  /** Generate a wrapper function that adapts a plain function to the closure ABI (env as first param). */
  private def emitFuncWrapper(wrapperName: String, origName: String, params: List[SyslType], retType: SyslType): Unit =
    val retLt = llvmType(retType)
    val paramNames = params.zipWithIndex.map((_, i) => s"%p$i")
    val paramStrs = "i8* %env" +: params.zip(paramNames).map((t, n) => s"${llvmType(t)} $n")
    emit(s"define $retLt @$wrapperName(${paramStrs.mkString(", ")}) {")
    emitLabel("entry")
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
            else if rt == "void" && retType != "void" then
              // Match/expression type is void (diverging arms) but function expects a value.
              if retType.startsWith("[") || retType.startsWith("%struct.") then
                val loaded = newReg()
                emit(s"  $loaded = load $retType, $retType* $result")
                loaded
              else result
            else emitSextIfNeeded(result, rt, retType, expr.typ.isSigned)
            emitDefers()
            emitReleaseRefs(returnedSliceAllocas(expr))
            emitRet(retType, finalVal)
            hasReturned = true
          case TAsmStmt(code) =>
            // asm as last statement in a function body
            val escaped = code.replace("\\n", "\n").replace("\"", "\\22")
            emit(s"""  call void asm sideeffect "$escaped", ""()""")
            emitDefers()
            emitReleaseRefs()
            emitRet(retType)
            hasReturned = true
          case other =>
            genStmt(other)
            if !hasReturned then
              emitDefers()
              emitReleaseRefs()
              emitRet(retType)
              hasReturned = true
    else
      emitDefers()
      emitReleaseRefs()
      emitRet(retType)
      hasReturned = true

  // Get LLVM type string for an expression based on its type
  private def exprType(expr: TExpr): String = llvmType(expr.typ)

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, typ, init, isVolatile) =>
        val lt = llvmType(typ)
        if isStringType(typ) then
          // String locals always get a stable entry-block alloca with the descriptor copied in.
          // Aliasing the source (e.g. for `var t = s`) is unsafe because the source may be a
          // transient SSA pointer (from slice[i], struct.field, etc.) that doesn't dominate
          // function-exit cleanup blocks.
          val src = genExpr(init)
          val alloca = deferAlloca("%struct.string")
          val loaded = newReg()
          emit(s"  $loaded = load %struct.string, %struct.string* $src")
          emit(s"  store %struct.string $loaded, %struct.string* $alloca")
          locals(name) = LocalVar(name, alloca, typ, isVolatile)
          if !isOwnedString(init) then emitStringDescrIncr(alloca)
        else if isAggregate(typ) then
          typ match
            case st: SyslType.StructType if structHasStringFields(st) && !isOwnedStruct(init) =>
              // Borrowed value struct: copy bytes into a fresh alloca, then incr string fields
              val src = genExpr(init)
              val alloca = deferAlloca(lt)
              val loaded = newReg()
              emit(s"  $loaded = load $lt, $lt* $src")
              emit(s"  store $lt $loaded, $lt* $alloca")
              locals(name) = LocalVar(name, alloca, typ, isVolatile)
              emitStructStringFieldsIncr(alloca, st)
            case _: SyslType.FuncType =>
              // Closure descriptor: copy bytes into a fresh alloca so `var g = f`
              // doesn't alias f's storage (which would cause double-decr at scope
              // exit and shared-mutation through one binding to the other).
              val src = genExpr(init)
              val alloca = deferAlloca(lt)
              val loaded = newReg()
              emit(s"  $loaded = load $lt, $lt* $src")
              emit(s"  store $lt $loaded, $lt* $alloca")
              locals(name) = LocalVar(name, alloca, typ, isVolatile)
              val rhsKind = funcKindOfExpr(init)
              closureLocalKind(name) = rhsKind
              // Descriptor copy (var g = f) shares env_ptr with source — incr env
              // rc so each descriptor's scope-exit decr is balanced.
              init match
                case _: TVarRef if rhsKind == FuncKind.HeapEnv =>
                  emitClosureDescrIncr(alloca)
                case _ =>
            case _ =>
              // Aggregate variable: genExpr returns an alloca pointer — use it directly
              val ptr = genExpr(init)
              locals(name) = LocalVar(name, ptr, typ, isVolatile)
              // Slice backref: increment if copying from a non-owned source
              if isSliceType(typ) && !isSliceOwned(init) then
                emitSliceBackrefIncr(ptr)
        else
            val alloca = deferAlloca(lt)
            val value = genExpr(init)
            val vt = exprType(init)
            val finalVal = emitSextIfNeeded(value, vt, lt, init.typ.isSigned)
            emit(s"  store $lt $finalVal, $lt* $alloca")
            locals(name) = LocalVar(name, alloca, typ, isVolatile)
            // Ref init: increment unless we own it (TNew/TNewArray)
            if isRef(typ) && !isOwnedNew(init) then
              emitRefIncr(finalVal, refHeaderOffset(typ))
            // Track pointer-from-slice derivation: &slot[i] produces a pointer whose
            // backing storage is owned by the slice. If this pointer is returned,
            // the slice's scope cleanup must be skipped to prevent use-after-free.
            init match
              case TAddrOfIndex(TVarRef(sliceName, _), _, _) if locals.contains(sliceName) && isSliceType(locals(sliceName).typ) =>
                derivedFromSlice(name) = sliceName
              case _ =>

      case TAssignStmt(target, value) =>
        if !locals.contains(target) && !globalVarTypes.contains(target) && isStringType(value.typ) then
          // First-time string assignment without a prior `var` — same stable-alloca pattern as TVarStmt.
          val src = genExpr(value)
          val alloca = deferAlloca("%struct.string")
          val loaded = newReg()
          emit(s"  $loaded = load %struct.string, %struct.string* $src")
          emit(s"  store %struct.string $loaded, %struct.string* $alloca")
          locals(target) = LocalVar(target, alloca, value.typ)
          if !isOwnedString(value) then emitStringDescrIncr(alloca)
        else if !locals.contains(target) && !globalVarTypes.contains(target) && isAggregate(value.typ) then
          // New aggregate variable: genExpr returns an alloca pointer — use it directly
          val ptr = genExpr(value)
          locals(target) = LocalVar(target, ptr, value.typ)
          if isSliceType(value.typ) && !isSliceOwned(value) then
            emitSliceBackrefIncr(ptr)
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
            // Slice reassignment: decrement old backref before overwrite
            if isSliceType(local.typ) then
              emitSliceBackrefDecr(local.reg, local.typ)
            // String reassignment: decrement old buffer refcount before overwrite
            if isStringType(local.typ) then
              emitStringDescrDecr(local.reg)
            // Value-struct/enum reassignment: decrement old's string fields before overwrite
            local.typ match
              case st: SyslType.StructType if structHasStringFields(st) =>
                emitStructStringFieldsDecr(local.reg, st)
              case et: SyslType.EnumType if structHasStringFields(et) =>
                emitEnumStringFieldsDecr(local.reg, et)
              case _ =>
            // Closure descriptor reassignment: decr old env before overwrite
            if local.typ.isInstanceOf[SyslType.FuncType]
              && closureLocalKind.get(target).contains(FuncKind.HeapEnv) then
              emitClosureDescrDecr(local.reg)
            if isAggregate(local.typ) then
              // Aggregate reassignment: load value from source, store to target
              val loaded = newReg()
              emit(s"  $loaded = load $lt, $lt* $v")
              emit(s"  store $lt $loaded, $lt* ${local.reg}")
              // Slice reassignment: increment new backref if not owned
              if isSliceType(local.typ) && !isSliceOwned(value) then
                emitSliceBackrefIncr(local.reg)
              // String reassignment: increment new buffer refcount if not owned
              if isStringType(local.typ) && !isOwnedString(value) then
                emitStringDescrIncr(local.reg)
              // Value-struct/enum reassignment: increment new aggregate's string fields if borrowed
              local.typ match
                case st: SyslType.StructType if structHasStringFields(st) && !isOwnedStruct(value) =>
                  emitStructStringFieldsIncr(local.reg, st)
                case et: SyslType.EnumType if structHasStringFields(et) && !isOwnedStruct(value) =>
                  emitEnumStringFieldsIncr(local.reg, et)
                case _ =>
              // Closure descriptor reassignment: track new kind, incr if borrowed copy
              if local.typ.isInstanceOf[SyslType.FuncType] then
                val rhsKind = funcKindOfExpr(value)
                closureLocalKind(target) = rhsKind
                value match
                  case _: TVarRef if rhsKind == FuncKind.HeapEnv =>
                    emitClosureDescrIncr(local.reg)
                  case _ =>
            else
              val vt = exprType(value)
              val finalVal = emitSextIfNeeded(v, vt, lt, value.typ.isSigned)
              val vol = if local.isVolatile then " volatile" else ""
              emit(s"  store$vol $lt $finalVal, $lt* ${local.reg}")
              if isRef(local.typ) && !isOwnedNew(value) then
                emitRefIncr(finalVal, refHeaderOffset(local.typ))
          else if globalVarTypes.contains(target) then
            // Assignment to a module-level global variable
            val gt = globalVarTypes(target)
            val lt = llvmType(gt)
            val vol = if volatileGlobals.contains(target) then " volatile" else ""
            if isAggregate(gt) then
              val loaded = newReg()
              emit(s"  $loaded = load $lt, $lt* $v")
              emit(s"  store$vol $lt $loaded, $lt* @$target")
            else
              val vt = exprType(value)
              val finalVal = emitSextIfNeeded(v, vt, lt, value.typ.isSigned)
              emit(s"  store$vol $lt $finalVal, $lt* @$target")
          else
            val lt = exprType(value)
            val alloca = deferAlloca(lt)
            emit(s"  store $lt $v, $lt* $alloca")
            locals(target) = LocalVar(target, alloca, value.typ)
            // New ref binding: increment unless owned
            if isRef(value.typ) && !isOwnedNew(value) then
              emitRefIncr(v, refHeaderOffset(value.typ))

      case TReturnStmt(Some(value)) =>
        val v = genExpr(value)
        emitFuncReturnIncrIfBorrowed(value, v)
        val retType = llvmType(currentFunction.returnType)
        val vt = exprType(value)
        val finalVal = if isAggregate(value.typ) then
          val loaded = newReg()
          emit(s"  $loaded = load $retType, $retType* $v")
          loaded
        else emitSextIfNeeded(v, vt, retType, value.typ.isSigned)
        emitDefers()
        emitReleaseRefs(returnedSliceAllocas(value))
        emitRet(retType, finalVal)
        hasReturned = true

      case TReturnStmt(None) =>
        emitDefers()
        emitReleaseRefs()
        val retType = llvmType(currentFunction.returnType)
        if retType == "void" then emit("  ret void")
        else emitRet(retType, "zeroinitializer")
        hasReturned = true

      case TDeferStmt(body) =>
        deferStack.push(body)

      case TMultiStmt(children) =>
        children.foreach(genStmt)

      case TContractCheck(kind, expr, _) =>
        val v = genExpr(expr)
        val lt = llvmType(expr.typ)
        val cmp = newReg()
        emit(s"  $cmp = icmp ne $lt $v, 0")
        val failLbl = s"contract_fail_${labelCounter}"
        val passLbl = s"contract_pass_${labelCounter}"
        labelCounter += 1
        emit(s"  br i1 $cmp, label %$passLbl, label %$failLbl")
        emit(s"$failLbl:")
        val (nameLbl, nameLen) = internCString(kind)
        emit(s"  %${failLbl}_name = getelementptr [$nameLen x i8], [$nameLen x i8]* $nameLbl, i32 0, i32 0")
        emit(s"  call void @__range_fail(i8* %${failLbl}_name, i64 ${nameLen - 1})")
        emit(s"  unreachable")
        emit(s"$passLbl:")
        currentBlock = passLbl

      case TExprStmt(expr) =>
        genExpr(expr)

      case TAsmStmt(code) =>
        // Emit LLVM inline assembly — bare instruction(s), no inputs/outputs
        val escaped = code.replace("\\n", "\n").replace("\"", "\\22")
        emit(s"""  call void asm sideeffect "$escaped", ""()""")

      case TWhileStmt(cond, body) =>
        val condLabel = newLabel("while_cond")
        val bodyLabel = newLabel("while_body")
        val endLabel = newLabel("while_end")
        val preLoopLocals = locals.keySet.toSet
        breakLabels.push(endLabel)
        continueLabels.push(condLabel)
        loopScopeSnapshots.push(preLoopLocals)
        emit(s"  br label %$condLabel")
        emitLabel(condLabel)
        val c = genExpr(cond)
        val ct = exprType(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne $ct $c, 0")
        emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emitLabel(bodyLabel)
        val savedHR = hasReturned
        hasReturned = false
        for s <- body do if !hasReturned then genStmt(s)
        if !hasReturned then
          emitScopeCleanup(preLoopLocals)
          emit(s"  br label %$condLabel")
        emitLabel(endLabel)
        hasReturned = savedHR
        breakLabels.pop()
        continueLabels.pop()
        loopScopeSnapshots.pop()

      case TForStmt(init, cond, update, body) =>
        genStmt(init)
        val condLabel = newLabel("for_cond")
        val bodyLabel = newLabel("for_body")
        val updateLabel = newLabel("for_update")
        val endLabel = newLabel("for_end")
        val preLoopLocals = locals.keySet.toSet // after init, before body
        breakLabels.push(endLabel)
        continueLabels.push(updateLabel)
        loopScopeSnapshots.push(preLoopLocals)
        emit(s"  br label %$condLabel")
        emitLabel(condLabel)
        val c = genExpr(cond)
        val ct = exprType(cond)
        val cBool = newReg()
        emit(s"  $cBool = icmp ne $ct $c, 0")
        emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emitLabel(bodyLabel)
        val savedHR = hasReturned
        hasReturned = false
        for s <- body do if !hasReturned then genStmt(s)
        if !hasReturned then
          emitScopeCleanup(preLoopLocals)
          emit(s"  br label %$updateLabel")
        emitLabel(updateLabel)
        if !hasReturned then genStmt(update)
        if !hasReturned then emit(s"  br label %$condLabel")
        emitLabel(endLabel)
        hasReturned = savedHR
        breakLabels.pop()
        continueLabels.pop()
        loopScopeSnapshots.pop()

      case TDoWhileStmt(cond, body) =>
        val bodyLabel = newLabel("dowhile_body")
        val condLabel = newLabel("dowhile_cond")
        val endLabel = newLabel("dowhile_end")
        val preLoopLocals = locals.keySet.toSet
        breakLabels.push(endLabel)
        continueLabels.push(condLabel)
        loopScopeSnapshots.push(preLoopLocals)
        emit(s"  br label %$bodyLabel")
        emitLabel(bodyLabel)
        val savedHR = hasReturned
        hasReturned = false
        for s <- body do if !hasReturned then genStmt(s)
        if !hasReturned then
          emitScopeCleanup(preLoopLocals)
          emit(s"  br label %$condLabel")
        emitLabel(condLabel)
        if !hasReturned then
          val c = genExpr(cond)
          val ct = exprType(cond)
          val cBool = newReg()
          emit(s"  $cBool = icmp ne $ct $c, 0")
          emit(s"  br i1 $cBool, label %$bodyLabel, label %$endLabel")
        emitLabel(endLabel)
        hasReturned = savedHR
        breakLabels.pop()
        continueLabels.pop()
        loopScopeSnapshots.pop()

      case TBreakStmt =>
        emitScopeCleanup(loopScopeSnapshots.top)
        emit(s"  br label %${breakLabels.top}")
        hasReturned = true // stop emitting after unconditional branch

      case TContinueStmt =>
        emitScopeCleanup(loopScopeSnapshots.top)
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
          case SyslType.RefType(SyslType.ArrayType(elem, _)) => elem
          case SyslType.PtrType(elem) => elem
          case _ => SyslType.IntType(8) // fallback for string indexing
        // For RefType(SliceType(_)) elements, store as inline %struct.slice (24 bytes)
        val elt = elemType match
          case SyslType.RefType(_: SyslType.SliceType) => "%struct.slice"
          case _ => llvmType(elemType)
        // Compute element pointer — use GEP for arrays and pointers (LLVM calculates stride),
        // manual byte arithmetic only for slices (untyped i8* data pointer).
        val typedPtr: String = array.typ match
          case SyslType.ArrayType(_, size) =>
            val arrType = s"[$size x $elt]"
            val gep = newReg()
            emit(s"  $gep = getelementptr $arrType, $arrType* $base, i32 0, i32 $idx")
            gep
          case SyslType.PtrType(_) =>
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            val gep = newReg()
            emit(s"  $gep = getelementptr $elt, $elt* $base, i64 $idx64")
            gep
          case _ =>
            // Slice or ref-slice: use byte arithmetic on data pointer
            val elemSize = elemType match
              case SyslType.RefType(_: SyslType.SliceType) => 24L
              case _ => llvmSizeOf(elemType)
            val dataPtr = array.typ match
              case SyslType.SliceType(_) =>
                val ptrGep = newReg()
                emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 0")
                val ptr = newReg()
                emit(s"  $ptr = load i8*, i8** $ptrGep")
                ptr
              case _ => base
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            val offset = newReg()
            emit(s"  $offset = mul i64 $idx64, $elemSize")
            val elemAddr = newReg()
            emit(s"  $elemAddr = getelementptr i8, i8* $dataPtr, i64 $offset")
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $elemAddr to $elt*")
            cast
        val treatAsAggregate = isAggregate(elemType) || (elemType match
          case SyslType.RefType(_: SyslType.SliceType) => true
          case _ => false)
        if treatAsAggregate then
          val loaded = newReg()
          emit(s"  $loaded = load $elt, $elt* $v")
          emit(s"  store $elt $loaded, $elt* $typedPtr")
        else
          // Widen or truncate if value width differs from element width
          val vLt = llvmType(value.typ)
          val storeVal = if vLt != elt then emitSextIfNeeded(v, vLt, elt, value.typ.isSigned) else v
          emit(s"  store $elt $storeVal, $elt* $typedPtr")

      case TDerefAssignStmt(pointer, value) =>
        val ptr = genExpr(pointer)
        var v = genExpr(value)
        val pointeeType = pointer.typ match
          case SyslType.PtrType(inner) => inner
          case SyslType.RefType(inner) => inner
          case _ => SyslType.IntType(64)
        val pt = llvmType(pointeeType)
        val typedPtr = newReg()
        emit(s"  $typedPtr = bitcast i8* $ptr to $pt*")
        if isAggregate(pointeeType) then
          val loaded = newReg()
          emit(s"  $loaded = load $pt, $pt* $v")
          emit(s"  store $pt $loaded, $pt* $typedPtr")
        else
          // Truncate wider value to narrower pointee type (e.g., i32 → i8 for byte stores)
          val vt = exprType(value)
          if vt != pt && pt == "i8" && (vt == "i32" || vt == "i64") then
            val trunc = newReg()
            emit(s"  $trunc = trunc $vt $v to $pt")
            v = trunc
          else if vt != pt && pt == "i16" && (vt == "i32" || vt == "i64") then
            val trunc = newReg()
            emit(s"  $trunc = trunc $vt $v to $pt")
            v = trunc
          emit(s"  store $pt $v, $pt* $typedPtr")

      case TFieldAssignStmt(obj, fieldIndex, value) =>
        val (st, structLt, addr) = obj.typ match
          case pt: SyslType.PtrType =>
            val inner = canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType])
            val slt = llvmType(inner)
            val ptr = genExpr(obj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (inner, slt, cast)
          case st: SyslType.StructType =>
            (canonicalStruct(st), llvmType(obj.typ), genStructAddr(obj))
          case other =>
            throw new RuntimeException(s"TFieldAssignStmt on non-struct type: $other")
        val ft = st.fields(fieldIndex)._2
        val fieldType = llvmType(ft)
        // addr already resolved above
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        // Slice field: decrement old backref before overwrite
        if isSliceType(ft) then emitSliceBackrefDecr(gep, ft)
        // String field: decrement old buffer refcount before overwrite
        if isStringType(ft) then emitStringDescrDecr(gep)
        val v = genExpr(value)
        val vol = if st.volatileFields.contains(fieldIndex) then " volatile" else ""
        if isAggregate(ft) then
          val loaded = newReg()
          emit(s"  $loaded = load $fieldType, $fieldType* $v")
          emit(s"  store$vol $fieldType $loaded, $fieldType* $gep")
          // Slice field: increment new backref if not owned
          if isSliceType(ft) && !isSliceOwned(value) then emitSliceBackrefIncr(gep)
          // String field: increment new buffer refcount if not owned
          if isStringType(ft) && !isOwnedString(value) then emitStringDescrIncr(gep)
        else
          emit(s"  store$vol $fieldType $v, $fieldType* $gep")

      case TCompoundAssignStmt(target, op, value) =>
        val (varType, varReg) = if locals.contains(target) then
          val local = locals(target)
          (local.typ, local.reg)
        else
          // Global variable
          (globalVarTypes(target), s"@$target")
        val lt = llvmType(varType)
        val cur = newReg()
        emit(s"  $cur = load $lt, $lt* $varReg")
        val v = genExpr(value)
        val vt = exprType(value)
        val rv = emitSextIfNeeded(v, vt, lt, value.typ.isSigned)
        val isFloat = varType.isFloat
        val isUnsigned = varType.isUnsigned
        val result = newReg()
        op match
          case "+" => emit(s"  $result = ${if isFloat then "fadd" else "add"} $lt $cur, $rv")
          case "-" => emit(s"  $result = ${if isFloat then "fsub" else "sub"} $lt $cur, $rv")
          case "*" => emit(s"  $result = ${if isFloat then "fmul" else "mul"} $lt $cur, $rv")
          case "/" => emit(s"  $result = ${if isFloat then "fdiv" else if isUnsigned then "udiv" else "sdiv"} $lt $cur, $rv")
          case "%" => emit(s"  $result = ${if isFloat then "frem" else if isUnsigned then "urem" else "srem"} $lt $cur, $rv")
          case "&" => emit(s"  $result = and $lt $cur, $rv")
          case "|" => emit(s"  $result = or $lt $cur, $rv")
          case "^" => emit(s"  $result = xor $lt $cur, $rv")
          case "<<" => emit(s"  $result = shl $lt $cur, $rv")
          case ">>" => emit(s"  $result = ${if isUnsigned then "lshr" else "ashr"} $lt $cur, $rv")
        emit(s"  store $lt $result, $lt* $varReg")

      case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
        val (st, structLt, addr) = obj.typ match
          case pt: SyslType.PtrType =>
            val inner = canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType])
            val slt = llvmType(inner)
            val ptr = genExpr(obj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (inner, slt, cast)
          case st: SyslType.StructType =>
            (canonicalStruct(st), llvmType(obj.typ), genStructAddr(obj))
          case other =>
            throw new RuntimeException(s"TFieldCompoundAssignStmt on non-struct type: $other")
        val ft = st.fields(fieldIndex)._2
        val fieldType = llvmType(ft)
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        val cur = newReg()
        emit(s"  $cur = load $fieldType, $fieldType* $gep")
        val v = genExpr(value)
        val vt = exprType(value)
        val rv = emitSextIfNeeded(v, vt, fieldType, value.typ.isSigned)
        val isFloat = ft.isFloat
        val isUnsigned = ft.isUnsigned
        val result = newReg()
        op match
          case "+" => emit(s"  $result = ${if isFloat then "fadd" else "add"} $fieldType $cur, $rv")
          case "-" => emit(s"  $result = ${if isFloat then "fsub" else "sub"} $fieldType $cur, $rv")
          case "*" => emit(s"  $result = ${if isFloat then "fmul" else "mul"} $fieldType $cur, $rv")
          case "/" => emit(s"  $result = ${if isFloat then "fdiv" else if isUnsigned then "udiv" else "sdiv"} $fieldType $cur, $rv")
          case "%" => emit(s"  $result = ${if isFloat then "frem" else if isUnsigned then "urem" else "srem"} $fieldType $cur, $rv")
          case "&" => emit(s"  $result = and $fieldType $cur, $rv")
          case "|" => emit(s"  $result = or $fieldType $cur, $rv")
          case "^" => emit(s"  $result = xor $fieldType $cur, $rv")
          case "<<" => emit(s"  $result = shl $fieldType $cur, $rv")
          case ">>" => emit(s"  $result = ${if isUnsigned then "lshr" else "ashr"} $fieldType $cur, $rv")
        emit(s"  store $fieldType $result, $fieldType* $gep")

      case TDestructureStmt(names, types, init) =>
        val tuplePtr = genExpr(init) // returns alloca pointer to struct/tuple
        val st = init.typ.asInstanceOf[SyslType.StructType]
        val structLt = llvmType(init.typ)
        for (name, i) <- names.zipWithIndex do
          val ft = types(i)
          val flt = llvmType(ft)
          val gep = newReg()
          emit(s"  $gep = getelementptr $structLt, $structLt* $tuplePtr, i32 0, i32 $i")
          if isSliceType(ft) then
            // Copy slice into entry-block alloca so it dominates scope cleanup
            val alloca = deferAlloca(flt)
            val loaded = newReg()
            emit(s"  $loaded = load $flt, $flt* $gep")
            emit(s"  store $flt $loaded, $flt* $alloca")
            locals(name) = LocalVar(name, alloca, ft)
            emitSliceBackrefIncr(alloca)
          else if isAggregate(ft) then
            locals(name) = LocalVar(name, gep, ft)
          else
            val alloca = deferAlloca(flt)
            val loaded = newReg()
            emit(s"  $loaded = load $flt, $flt* $gep")
            emit(s"  store $flt $loaded, $flt* $alloca")
            locals(name) = LocalVar(name, alloca, ft)

      case TDestructureAssignStmt(names, types, init) =>
        val tuplePtr = genExpr(init)
        val st = init.typ.asInstanceOf[SyslType.StructType]
        val structLt = llvmType(init.typ)
        for (name, i) <- names.zipWithIndex do
          val ft = types(i)
          val flt = llvmType(ft)
          val gep = newReg()
          emit(s"  $gep = getelementptr $structLt, $structLt* $tuplePtr, i32 0, i32 $i")
          if isSliceType(ft) then
            val alloca = deferAlloca(flt)
            val loaded = newReg()
            emit(s"  $loaded = load $flt, $flt* $gep")
            emit(s"  store $flt $loaded, $flt* $alloca")
            locals(name) = LocalVar(name, alloca, ft)
            emitSliceBackrefIncr(alloca)
          else if isAggregate(ft) then
            locals(name) = LocalVar(name, gep, ft)
          else
            if locals.contains(name) then
              val local = locals(name)
              val loaded = newReg()
              emit(s"  $loaded = load $flt, $flt* $gep")
              emit(s"  store $flt $loaded, $flt* ${local.reg}")
            else
              val alloca = deferAlloca(flt)
              val loaded = newReg()
              emit(s"  $loaded = load $flt, $flt* $gep")
              emit(s"  store $flt $loaded, $flt* $alloca")
              locals(name) = LocalVar(name, alloca, ft)

      case _ =>
        emit(s"  ; TODO: ${stmt.getClass.getSimpleName}")

  private def genExpr(expr: TExpr): String =
    val t = exprType(expr)
    expr match
      case TIntLit(n, typ) =>
        // Pointer-typed integer literals (typically zero-init of *T) must use "null"
        typ match
          case _: SyslType.PtrType | _: SyslType.RefType if n == 0 => "null"
          case _ => n.toString
      case TFloatLit(d, _) =>
        // Use LLVM hex format for exact representation
        val bits = java.lang.Double.doubleToRawLongBits(d)
        s"0x${bits.toHexString.toUpperCase}"
      case TBoolLit(true, _) => "1"
      case TBoolLit(false, _) => "0"
      case TSizeof(size, _) => size.toString

      case TStringLit(s, _) =>
        val (label, byteLen) = internString(s)
        val strLen = byteLen - 1 // exclude null terminator for fat string length
        val alloca = deferAlloca("%struct.string")
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $alloca, i32 0, i32 0")
        val dataPtr = newReg()
        emit(s"  $dataPtr = getelementptr <{ i64, [$byteLen x i8] }>, <{ i64, [$byteLen x i8] }>* $label, i32 0, i32 1, i32 0")
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
            val vol = if local.isVolatile then " volatile" else ""
            emit(s"  $r = load$vol $lt, $lt* ${local.reg}")
            r
        else if isAggregate(typ) then
          s"@$name" // aggregate global: return address, don't load
        else
          val r = newReg()
          val vol = if volatileGlobals.contains(name) then " volatile" else ""
          emit(s"  $r = load$vol $t, $t* @$name")
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
        // Total length and allocate header+data (refcount=1)
        val totalLen = newReg()
        emit(s"  $totalLen = add i32 $lLen, $rLen")
        val totalLen64 = newReg()
        emit(s"  $totalLen64 = sext i32 $totalLen to i64")
        val buf = emitStringBufferAlloc(totalLen64)
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
        emitMakeString(buf, totalLen)

      case TBinary(left, op @ ("==" | "!="), right, _) if left.typ == SyslType.StringType =>
        val lp = genExpr(left)
        val rp = genExpr(right)
        // Extract len from both
        val lLenGep = newReg()
        emit(s"  $lLenGep = getelementptr %struct.string, %struct.string* $lp, i32 0, i32 1")
        val lLen = newReg()
        emit(s"  $lLen = load i32, i32* $lLenGep")
        val rLenGep = newReg()
        emit(s"  $rLenGep = getelementptr %struct.string, %struct.string* $rp, i32 0, i32 1")
        val rLen = newReg()
        emit(s"  $rLen = load i32, i32* $rLenGep")
        // Compare lengths
        val lenEq = newReg()
        emit(s"  $lenEq = icmp eq i32 $lLen, $rLen")
        val lenCheckBlock = currentBlock
        val lenMatchLabel = newLabel("str_len_match")
        val strCmpDone = newLabel("str_cmp_done")
        emit(s"  br i1 $lenEq, label %$lenMatchLabel, label %$strCmpDone")
        // Lengths match — compare bytes
        emitLabel(lenMatchLabel)
        val lPtrGep = newReg()
        emit(s"  $lPtrGep = getelementptr %struct.string, %struct.string* $lp, i32 0, i32 0")
        val lPtr = newReg()
        emit(s"  $lPtr = load i8*, i8** $lPtrGep")
        val rPtrGep = newReg()
        emit(s"  $rPtrGep = getelementptr %struct.string, %struct.string* $rp, i32 0, i32 0")
        val rPtr = newReg()
        emit(s"  $rPtr = load i8*, i8** $rPtrGep")
        val len64 = newReg()
        emit(s"  $len64 = sext i32 $lLen to i64")
        val cmpResult = newReg()
        emit(s"  $cmpResult = call i32 @memcmp(i8* $lPtr, i8* $rPtr, i64 $len64)")
        val bytesEq = newReg()
        emit(s"  $bytesEq = icmp eq i32 $cmpResult, 0")
        val lenMatchExit = currentBlock
        emit(s"  br label %$strCmpDone")
        // Merge
        emitLabel(strCmpDone)
        val eq = newReg()
        emit(s"  $eq = phi i1 [ false, %$lenCheckBlock ], [ $bytesEq, %$lenMatchExit ]")
        val result = newReg()
        val t = llvmType(SyslType.BoolType)
        if op == "==" then
          emit(s"  $result = zext i1 $eq to $t")
        else
          val neq = newReg()
          emit(s"  $neq = xor i1 $eq, true")
          emit(s"  $result = zext i1 $neq to $t")
        result

      case TBinary(left, op, right, _) if (op == "+" || op == "-") && (left.typ.isInstanceOf[SyslType.PtrType] || right.typ.isInstanceOf[SyslType.PtrType] || left.typ.isInstanceOf[SyslType.ArrayType] || right.typ.isInstanceOf[SyslType.ArrayType]) =>
        // Pointer arithmetic: ptr + int or int + ptr → getelementptr
        // Also handles array + int (array-to-pointer decay for arithmetic)
        val (ptrExpr, idxExpr, isSub) = (left.typ, right.typ) match
          case (_: SyslType.PtrType, _)  => (left, right, op == "-")
          case (_, _: SyslType.PtrType)  => (right, left, op == "-")
          case (_: SyslType.ArrayType, _) => (left, right, op == "-")
          case (_, _: SyslType.ArrayType) => (right, left, op == "-")
          case _ => throw new RuntimeException(s"Pointer arithmetic: unexpected types ${left.typ}, ${right.typ}")
        var ptrVal = genExpr(ptrExpr)
        val idxVal = genExpr(idxExpr)
        val (pointeeType, elemSize) = ptrExpr.typ match
          case SyslType.PtrType(p) => (p, llvmSizeOf(p))
          case SyslType.ArrayType(elem, _) =>
            // Array decay: bitcast [N x T]* to i8*
            val arrLt = llvmType(ptrExpr.typ)
            val bc = newReg()
            emit(s"  $bc = bitcast $arrLt* $ptrVal to i8*")
            ptrVal = bc
            (elem, llvmSizeOf(elem))
          case _ => throw new RuntimeException(s"Pointer arithmetic: unexpected type ${ptrExpr.typ}")
        val idxLt = exprType(idxExpr)
        val idx64 = if idxLt == "i64" then idxVal
        else
          val r = newReg()
          emit(s"  $r = sext $idxLt $idxVal to i64")
          r
        val byteOff = newReg()
        if isSub then emit(s"  $byteOff = mul i64 $idx64, -$elemSize")
        else emit(s"  $byteOff = mul i64 $idx64, $elemSize")
        val result = newReg()
        emit(s"  $result = getelementptr i8, i8* $ptrVal, i64 $byteOff")
        result

      case TBinary(left, op, right, _) =>
        var l = genExpr(left)
        var r = genExpr(right)
        val leftLt = exprType(left)
        val rightLt = exprType(right)
        // Reconcile operand widths: extend narrower int to wider
        val lt = if leftLt != rightLt && left.typ.isIntegral && right.typ.isIntegral then
          val lw = leftLt.stripPrefix("i").toInt
          val rw = rightLt.stripPrefix("i").toInt
          if lw < rw then
            val ext = newReg()
            if left.typ.isSigned then emit(s"  $ext = sext $leftLt $l to $rightLt")
            else emit(s"  $ext = zext $leftLt $l to $rightLt")
            l = ext
            rightLt
          else if rw < lw then
            val ext = newReg()
            if right.typ.isSigned then emit(s"  $ext = sext $rightLt $r to $leftLt")
            else emit(s"  $ext = zext $rightLt $r to $leftLt")
            r = ext
            leftLt
          else leftLt
        else leftLt
        val isFloat = left.typ.isFloat
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
        if operand.typ.isFloat then emit(s"  $result = fneg $vt $v")
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
            val v0 = genExpr(arg)
            val vt0 = exprType(arg)
            val (fmtName, fmtLen, vt, v) = arg.typ match
              case _: SyslType.FloatType =>
                val vd = if arg.typ == SyslType.F64 then v0 else
                  val r = newReg()
                  emit(s"  $r = fpext $vt0 $v0 to double")
                  r
                ("@.fmt_f", 3, "double", vd)
              case _ => ("@.fmt_d", 3, vt0, v0)
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
            val v0 = genExpr(arg)
            val vt0 = exprType(arg)
            val (fmtName, fmtLen, vt, v) = arg.typ match
              case _: SyslType.FloatType =>
                val vd = if arg.typ == SyslType.F64 then v0 else
                  val r = newReg()
                  emit(s"  $r = fpext $vt0 $v0 to double")
                  r
                ("@.fmt_fn", 4, "double", vd)
              case _ => ("@.fmt_dn", 4, vt0, v0)
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
        val declaredParams = funcParamTypes.getOrElse(name, Nil)
        val argVals = args.zipWithIndex.map { (a, i) =>
          val v = genExpr(a)
          val vt = exprType(a)
          val expectedType = if i < declaredParams.length then declaredParams(i) else vt
          // For aggregate types, genExpr returns a pointer — load for pass-by-value,
          // but if the parameter expects a pointer, pass the address instead
          if isAggregate(a.typ) then
            if expectedType.endsWith("*") then
              // Parameter expects a pointer — bitcast the alloca address
              val cast = newReg()
              emit(s"  $cast = bitcast $vt* $v to $expectedType")
              (cast, expectedType)
            else
              val loaded = newReg()
              emit(s"  $loaded = load $vt, $vt* $v")
              (loaded, vt)
          else
            // Widen scalar arguments to match declared parameter type (e.g., i8 → i32 for char)
            val widened = emitSextIfNeeded(v, vt, expectedType, a.typ.isSigned)
            (widened, expectedType)
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
            val alloca = deferAlloca(retType)
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
        val aggResult = if isAggregate(typ) && t != "void" then
          Some(deferAlloca(t))
        else None
        emit(s"  br i1 $cBool, label %$thenLabel, label %$elseLabel")

        emitLabel(thenLabel)
        val savedHasReturned = hasReturned
        hasReturned = false
        val preThenLocals = locals.keySet.toSet
        var thenVal = "0"
        for s <- thenBody.init do genStmt(s)
        if !hasReturned then
          thenBody.lastOption match
            case Some(TExprStmt(e)) =>
              val v = genExpr(e)
              aggResult match
                case Some(alloca) =>
                  val loaded = newReg()
                  emit(s"  $loaded = load $t, $t* $v")
                  emit(s"  store $t $loaded, $t* $alloca")
                  // Slice: increment aggResult copy (source will be cleaned up by scope cleanup)
                  if isSliceType(typ) then emitSliceBackrefIncr(alloca)
                  if isStringType(typ) then emitStringDescrIncr(alloca)
                  typ match
                    case st: SyslType.StructType if structHasStringFields(st) =>
                      emitStructStringFieldsIncr(alloca, st)
                    case _ =>
                case None =>
                  val vt = exprType(e)
                  thenVal = if vt != t && e.typ.isIntegral then emitSextIfNeeded(v, vt, t, e.typ.isSigned) else v
            case Some(other) => genStmt(other)
            case None =>
        val thenReturned = hasReturned
        if !thenReturned then
          emitScopeCleanup(preThenLocals)
        else
          // Remove locals introduced in the returning branch — their allocas are uninitialized on the other path
          for key <- locals.keySet.toList if !preThenLocals.contains(key) do
            locals.remove(key)
        // Capture exit block AFTER cleanup so phi predecessors match — emitScopeCleanup
        // emits null-check branches that change currentBlock.
        val thenExitBlock = currentBlock
        if !thenReturned then
          emit(s"  br label %$mergeLabel")
        hasReturned = savedHasReturned

        emitLabel(elseLabel)
        var elseVal = "0"
        hasReturned = false
        val preElseLocals = locals.keySet.toSet
        elseBody.foreach { stmts =>
          for s <- stmts.init do genStmt(s)
          if !hasReturned then
            stmts.lastOption match
              case Some(TExprStmt(e)) =>
                val v = genExpr(e)
                aggResult match
                  case Some(alloca) =>
                    val loaded = newReg()
                    emit(s"  $loaded = load $t, $t* $v")
                    emit(s"  store $t $loaded, $t* $alloca")
                    if isSliceType(typ) then emitSliceBackrefIncr(alloca)
                    if isStringType(typ) then emitStringDescrIncr(alloca)
                    typ match
                      case st: SyslType.StructType if structHasStringFields(st) =>
                        emitStructStringFieldsIncr(alloca, st)
                      case _ =>
                  case None =>
                    val vt = exprType(e)
                    elseVal = if vt != t && e.typ.isIntegral then emitSextIfNeeded(v, vt, t, e.typ.isSigned) else v
              case Some(other) => genStmt(other)
              case None =>
        }
        val elseReturned = hasReturned
        if !elseReturned then
          emitScopeCleanup(preElseLocals)
        val elseExitBlock = currentBlock
        if !elseReturned then
          emit(s"  br label %$mergeLabel")
        else
          for key <- locals.keySet.toList if !preElseLocals.contains(key) do
            locals.remove(key)
        hasReturned = savedHasReturned

        emitLabel(mergeLabel)
        aggResult match
          case Some(alloca) => alloca // return pointer for aggregate types
          case None =>
            if !thenReturned && !elseReturned && t != "void" then
              val phi = newReg()
              emit(s"  $phi = phi $t [ $thenVal, %$thenExitBlock ], [ $elseVal, %$elseExitBlock ]")
              phi
            else "0"

      case TStructConstruct(st, args) =>
        // Alloca, zero-init, then fill fields
        val lt = llvmType(st)
        val alloca = deferAlloca(lt)
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
          // Slice field in struct: increment backref for the copy
          if isSliceType(fieldSyslType) && !isSliceOwned(arg) then
            emitSliceBackrefIncr(gep)
          // String field in struct: increment buffer refcount for the copy
          if isStringType(fieldSyslType) && !isOwnedString(arg) then
            emitStringDescrIncr(gep)
          // Nested value-struct field: recursively increment string fields if borrowed
          fieldSyslType match
            case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
              emitStructStringFieldsIncr(gep, nested)
            case _ =>
        alloca

      case TStructLit(st @ SyslType.StructType(_, _, _)) =>
        val lt = llvmType(st)
        val alloca = deferAlloca(lt)
        emit(s"  store $lt zeroinitializer, $lt* $alloca")
        alloca

      case TFieldAccess(obj, fieldIndex, fieldType) =>
        val (st, structLt, addr) = obj.typ match
          case pt: SyslType.PtrType =>
            val inner = canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType])
            val slt = llvmType(inner)
            // Dereference pointer to struct
            val ptr = genExpr(obj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (inner, slt, cast)
          case st: SyslType.StructType =>
            (canonicalStruct(st), llvmType(obj.typ), genStructAddr(obj))
          case other =>
            throw new RuntimeException(s"TFieldAccess on non-struct type: $other")
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        if isAggregate(fieldType) then
          gep // return address for aggregate fields
        else
          val ft = llvmType(fieldType)
          val r = newReg()
          val vol = if st.volatileFields.contains(fieldIndex) then " volatile" else ""
          emit(s"  $r = load$vol $ft, $ft* $gep")
          r

      // ===== Arrays =====

      case TArrayLit(elements, SyslType.ArrayType(elemType, size)) =>
        val elt = llvmType(elemType)
        val arrType = s"[$size x $elt]"
        val alloca = deferAlloca(arrType)
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
        val alloca = deferAlloca(arrType)
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
            // For RefType(SliceType) array type from aggregate TIndex, base is %struct.slice*
            // — extract data pointer from field 0. Otherwise use emitSliceDataPtr.
            val dataPtr = (array.typ, array) match
              case (SyslType.RefType(SyslType.SliceType(_)), _: TIndex) =>
                // base is %struct.slice* from inline slice element
                val ptrGep = newReg()
                emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 0")
                val ptr = newReg()
                emit(s"  $ptr = load i8*, i8** $ptrGep")
                ptr
              case _ =>
                emitSliceDataPtr(base, array.typ)
            // For RefType(SliceType(_)) elements, treat as inline %struct.slice (24 bytes)
            val (elt, eSize, asAggregate) = elemType match
              case SyslType.RefType(_: SyslType.SliceType) => ("%struct.slice", 24L, true)
              case _ => (llvmType(elemType), llvmSizeOf(elemType), isAggregate(elemType))
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            val offset = newReg()
            emit(s"  $offset = mul i64 $idx64, $eSize")
            val elemAddr = newReg()
            emit(s"  $elemAddr = getelementptr i8, i8* $dataPtr, i64 $offset")
            val typedPtr = newReg()
            emit(s"  $typedPtr = bitcast i8* $elemAddr to $elt*")
            if asAggregate then typedPtr
            else
              val r = newReg()
              emit(s"  $r = load $elt, $elt* $typedPtr")
              r

      case TIntrinsicCall(name, args, typ) =>
        val a = genExpr(args(0))
        val b = genExpr(args(1))
        val lt = llvmType(typ)
        val width = typ.bitWidth
        val signed = typ.isSigned
        val r = newReg()
        name match
          case "wrapping_add" => emit(s"  $r = add $lt $a, $b")
          case "wrapping_sub" => emit(s"  $r = sub $lt $a, $b")
          case "wrapping_mul" => emit(s"  $r = mul $lt $a, $b")
          case "saturating_add" =>
            val intr = if signed then "sadd.sat" else "uadd.sat"
            emit(s"  $r = call $lt @llvm.$intr.i$width($lt $a, $lt $b)")
          case "saturating_sub" =>
            val intr = if signed then "ssub.sat" else "usub.sat"
            emit(s"  $r = call $lt @llvm.$intr.i$width($lt $a, $lt $b)")
          case "saturating_mul" =>
            // No native llvm.smul.sat; emulate via with.overflow + clamp.
            val intr = if signed then "smul.with.overflow" else "umul.with.overflow"
            val pair = newReg()
            emit(s"  $pair = call {$lt, i1} @llvm.$intr.i$width($lt $a, $lt $b)")
            val v = newReg()
            val ov = newReg()
            emit(s"  $v = extractvalue {$lt, i1} $pair, 0")
            emit(s"  $ov = extractvalue {$lt, i1} $pair, 1")
            val sat: String =
              if !signed then
                if width == 64 then "-1" else ((1L << width) - 1).toString
              else
                // signed: pick MIN (sign bits differ) or MAX (sign bits same)
                val xor = newReg()
                emit(s"  $xor = xor $lt $a, $b")
                val isNeg = newReg()
                emit(s"  $isNeg = icmp slt $lt $xor, 0")
                val sel = newReg()
                val minStr = (-(1L << (width - 1))).toString
                val maxStr = ((1L << (width - 1)) - 1).toString
                emit(s"  $sel = select i1 $isNeg, $lt $minStr, $lt $maxStr")
                sel
            emit(s"  $r = select i1 $ov, $lt $sat, $lt $v")
          case other => throw new RuntimeException(s"unknown intrinsic: $other")
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
          case SyslType.RefType(_: SyslType.SliceType) =>
            array match
              case _: TIndex =>
                // base is %struct.slice* (aggregate from TIndex into []&[]T)
                val base = genExpr(array)
                val lenGep = newReg()
                emit(s"  $lenGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 1")
                val len32 = newReg()
                emit(s"  $len32 = load i32, i32* $lenGep")
                len32
              case _ =>
                // base is i8* data pointer (from ref variable) — length at offset -8
                val base = genExpr(array)
                val lenAddr = newReg()
                emit(s"  $lenAddr = getelementptr i8, i8* $base, i64 -8")
                val lenPtr = newReg()
                emit(s"  $lenPtr = bitcast i8* $lenAddr to i32*")
                val len32 = newReg()
                emit(s"  $len32 = load i32, i32* $lenPtr")
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
          case SyslType.RefType(_: SyslType.SliceType) =>
            array match
              case _: TIndex =>
                val base = genExpr(array)
                val capGep = newReg()
                emit(s"  $capGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 2")
                val cap32 = newReg()
                emit(s"  $cap32 = load i32, i32* $capGep")
                cap32
              case _ =>
                val base = genExpr(array)
                val capAddr = newReg()
                emit(s"  $capAddr = getelementptr i8, i8* $base, i64 -4")
                val capPtr = newReg()
                emit(s"  $capPtr = bitcast i8* $capAddr to i32*")
                val cap32 = newReg()
                emit(s"  $cap32 = load i32, i32* $capPtr")
                cap32
          case _ =>
            emit(s"  ; TODO: cap for ${array.typ}")
            "0"

      // ===== Pointers =====

      case TAddrOf(name, _) =>
        if locals.contains(name) then locals(name).reg
        else s"@$name"

      case TAddrOfField(obj, fieldIndex, _) =>
        // Get a pointer to a struct field — used for method calls on nested struct fields
        val (st, structLt, addr) = obj.typ match
          case pt: SyslType.PtrType =>
            val inner = canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType])
            val slt = llvmType(inner)
            val ptr = genExpr(obj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (inner, slt, cast)
          case st: SyslType.StructType =>
            (canonicalStruct(st), llvmType(obj.typ), genStructAddr(obj))
          case other =>
            throw new RuntimeException(s"TAddrOfField on non-struct type: $other")
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        val cast = newReg()
        emit(s"  $cast = bitcast ${llvmType(st.fields(fieldIndex)._2)}* $gep to i8*")
        cast

      case TAddrOfIndex(array, index, _) =>
        // Get a pointer to an array/slice element — used for method calls on indexed elements
        val idx = genExpr(index)
        array.typ match
          case SyslType.ArrayType(elemType, _) =>
            // Stack array: GEP into the array directly
            val base = genExpr(array) // returns alloca pointer for aggregate
            val arrLt = llvmType(array.typ)
            val gep = newReg()
            emit(s"  $gep = getelementptr $arrLt, $arrLt* $base, i32 0, i32 $idx")
            val cast = newReg()
            emit(s"  $cast = bitcast ${llvmType(elemType)}* $gep to i8*")
            cast
          case SyslType.SliceType(elemType) =>
            val base = genExpr(array)
            val elemSize = llvmSizeOf(elemType)
            val dataPtr = emitSliceDataPtr(base, array.typ)
            val byteOff = newReg()
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            emit(s"  $byteOff = mul i64 $idx64, $elemSize")
            val elemPtr = newReg()
            emit(s"  $elemPtr = getelementptr i8, i8* $dataPtr, i64 $byteOff")
            elemPtr
          case SyslType.RefType(SyslType.SliceType(elemType)) =>
            val base = genExpr(array)
            val elemSize = llvmSizeOf(elemType)
            val dataPtr = emitSliceDataPtr(base, array.typ)
            val byteOff = newReg()
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            emit(s"  $byteOff = mul i64 $idx64, $elemSize")
            val elemPtr = newReg()
            emit(s"  $elemPtr = getelementptr i8, i8* $dataPtr, i64 $byteOff")
            elemPtr
          case SyslType.PtrType(elemType) =>
            val base = genExpr(array)
            val elt = llvmType(elemType)
            val idx64 = newReg()
            emit(s"  $idx64 = sext i32 $idx to i64")
            val gep = newReg()
            emit(s"  $gep = getelementptr $elt, $elt* $base, i64 $idx64")
            val cast = newReg()
            emit(s"  $cast = bitcast $elt* $gep to i8*")
            cast
          case other =>
            throw new RuntimeException(s"TAddrOfIndex on unsupported type: $other")

      case TTempAddr(inner, _) =>
        // Evaluate expression, store into a temporary alloca, return pointer
        val v = genExpr(inner)
        val lt = llvmType(inner.typ)
        if isAggregate(inner.typ) then
          // genExpr already returned an alloca pointer for aggregates
          val cast = newReg()
          emit(s"  $cast = bitcast $lt* $v to i8*")
          cast
        else
          val alloca = deferAlloca(lt)
          emit(s"  store $lt $v, $lt* $alloca")
          val cast = newReg()
          emit(s"  $cast = bitcast $lt* $alloca to i8*")
          cast

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

      case TSliceExpr(array, low, high, SyslType.StringType) =>
        // Substring s[lo:hi]: allocate new rc'd buffer, memcpy hi-lo bytes.
        val sp = genExpr(array)
        val srcPtrGep = newReg()
        emit(s"  $srcPtrGep = getelementptr %struct.string, %struct.string* $sp, i32 0, i32 0")
        val srcPtr = newReg()
        emit(s"  $srcPtr = load i8*, i8** $srcPtrGep")
        val srcLenGep = newReg()
        emit(s"  $srcLenGep = getelementptr %struct.string, %struct.string* $sp, i32 0, i32 1")
        val srcLen = newReg()
        emit(s"  $srcLen = load i32, i32* $srcLenGep")
        val lo = low.map(genExpr).getOrElse("0")
        val hi = high.map(genExpr).getOrElse(srcLen)
        // Bounds check: 0 <= lo <= hi <= src_len → branch to abort otherwise
        val okLabel = newLabel("substr_ok")
        val failLabel = newLabel("substr_fail")
        val loCheck = newReg()
        emit(s"  $loCheck = icmp slt i32 $lo, 0")
        val checkHi = newLabel("substr_chk_hi")
        emit(s"  br i1 $loCheck, label %$failLabel, label %$checkHi")
        emitLabel(checkHi)
        val hiLoCheck = newReg()
        emit(s"  $hiLoCheck = icmp slt i32 $hi, $lo")
        val checkLen = newLabel("substr_chk_len")
        emit(s"  br i1 $hiLoCheck, label %$failLabel, label %$checkLen")
        emitLabel(checkLen)
        val hiLenCheck = newReg()
        emit(s"  $hiLenCheck = icmp sgt i32 $hi, $srcLen")
        emit(s"  br i1 $hiLenCheck, label %$failLabel, label %$okLabel")
        emitLabel(failLabel)
        emit(s"  call void @abort()")
        emit(s"  unreachable")
        emitLabel(okLabel)
        // Compute new_len = hi - lo, allocate buffer, memcpy
        val newLen = newReg()
        emit(s"  $newLen = sub i32 $hi, $lo")
        val newLen64 = newReg()
        emit(s"  $newLen64 = sext i32 $newLen to i64")
        val dataPtr = emitStringBufferAlloc(newLen64)
        val lo64 = newReg()
        emit(s"  $lo64 = sext i32 $lo to i64")
        val srcStart = newReg()
        emit(s"  $srcStart = getelementptr i8, i8* $srcPtr, i64 $lo64")
        val cp = newReg()
        emit(s"  $cp = call i8* @memcpy(i8* $dataPtr, i8* $srcStart, i64 $newLen64)")
        emitMakeString(dataPtr, newLen)

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
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // Ref-to-slice: base is data pointer, length is at offset -8 (i.e., header offset 8)
            val lenAddr = newReg()
            emit(s"  $lenAddr = getelementptr i8, i8* $base, i64 -8")
            val lenTyped = newReg()
            emit(s"  $lenTyped = bitcast i8* $lenAddr to i32*")
            val len = newReg()
            emit(s"  $len = load i32, i32* $lenTyped")
            (base, len)
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
        val alloca = deferAlloca("%struct.slice")
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 0")
        emit(s"  store i8* $newPtr, i8** $ptrGep")
        val lenGep2 = newReg()
        emit(s"  $lenGep2 = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 1")
        emit(s"  store i32 $newLen, i32* $lenGep2")
        val capGep2 = newReg()
        emit(s"  $capGep2 = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 2")
        emit(s"  store i32 $newLen, i32* $capGep2") // cap = len for slicing
        // Backref field (index 3): set to allocation base for ref-backed slices
        val brGep = newReg()
        emit(s"  $brGep = getelementptr %struct.slice, %struct.slice* $alloca, i32 0, i32 3")
        array.typ match
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // backref = dataPtr - 16 (allocation base with refcount header)
            val allocBase = newReg()
            emit(s"  $allocBase = getelementptr i8, i8* $base, i64 -16")
            emit(s"  store i8* $allocBase, i8** $brGep")
            // Increment refcount — slice now borrows the ref
            val rcPtr = newReg()
            emit(s"  $rcPtr = bitcast i8* $allocBase to i64*")
            val rc = newReg()
            emit(s"  $rc = load i64, i64* $rcPtr")
            val newRc = newReg()
            emit(s"  $newRc = add i64 $rc, 1")
            emit(s"  store i64 $newRc, i64* $rcPtr")
          case SyslType.SliceType(_) =>
            // Inherit backref from source slice
            val srcBrGep = newReg()
            emit(s"  $srcBrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 3")
            val srcBr = newReg()
            emit(s"  $srcBr = load i8*, i8** $srcBrGep")
            emit(s"  store i8* $srcBr, i8** $brGep")
            // Increment backref — new slice holds its own reference to the backing store
            emitSliceBackrefIncr(alloca)
          case _ =>
            emit(s"  store i8* null, i8** $brGep")
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
        // Load input slice's backref (preserved in no-grow path)
        val inputBrGep = newReg()
        emit(s"  $inputBrGep = getelementptr %struct.slice, %struct.slice* $base, i32 0, i32 3")
        val inputBr = newReg()
        emit(s"  $inputBr = load i8*, i8** $inputBrGep")
        // Check if we need to grow
        val needGrow = newReg()
        emit(s"  $needGrow = icmp eq i32 $curLen, $curCap")
        val growLabel = newLabel("append_grow")
        val noGrowLabel = newLabel("append_nogrow")
        val contLabel = newLabel("append_cont")
        emit(s"  br i1 $needGrow, label %$growLabel, label %$noGrowLabel")
        // Grow path
        emitLabel(growLabel)
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
        emitLabel(noGrowLabel)
        emit(s"  br label %$contLabel")
        // Continue — phi for ptr and cap
        emitLabel(contLabel)
        val finalPtr = newReg()
        emit(s"  $finalPtr = phi i8* [ $newBuf, %$growLabel ], [ $curPtr, %$noGrowLabel ]")
        val finalCap = newReg()
        emit(s"  $finalCap = phi i32 [ $newCap, %$growLabel ], [ $curCap, %$noGrowLabel ]")
        val finalBr = newReg()
        emit(s"  $finalBr = phi i8* [ null, %$growLabel ], [ $inputBr, %$noGrowLabel ]")
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
        if isAggregate(elemType) then
          val loaded = newReg()
          emit(s"  $loaded = load $elt, $elt* $v")
          emit(s"  store $elt $loaded, $elt* $typedElemPtr")
        else
          emit(s"  store $elt $v, $elt* $typedElemPtr")
        // Build result slice
        val newLen = newReg()
        emit(s"  $newLen = add i32 $curLen, 1")
        val result = deferAlloca("%struct.slice")
        val rPtrGep = newReg()
        emit(s"  $rPtrGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 0")
        emit(s"  store i8* $finalPtr, i8** $rPtrGep")
        val rLenGep = newReg()
        emit(s"  $rLenGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 1")
        emit(s"  store i32 $newLen, i32* $rLenGep")
        val rCapGep = newReg()
        emit(s"  $rCapGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 2")
        emit(s"  store i32 $finalCap, i32* $rCapGep")
        // Backref: inherit from input in no-grow path, null in grow path
        val rBrGep = newReg()
        emit(s"  $rBrGep = getelementptr %struct.slice, %struct.slice* $result, i32 0, i32 3")
        emit(s"  store i8* $finalBr, i8** $rBrGep")
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
          val rawFieldType = st.fields(i)._2
          val fieldType = llvmType(rawFieldType)
          val gep = newReg()
          emit(s"  $gep = getelementptr $structLt, $structLt* $typedData, i32 0, i32 $i")
          val storeVal = if isAggregate(rawFieldType) then
            val loaded = newReg()
            emit(s"  $loaded = load $fieldType, $fieldType* $v")
            loaded
          else v
          emit(s"  store $fieldType $storeVal, $fieldType* $gep")
          // Borrowed string field: incr the buffer (caller still owns its copy)
          if isStringType(rawFieldType) && !isOwnedString(arg) then
            emitStringDescrIncr(gep)
          rawFieldType match
            case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
              emitStructStringFieldsIncr(gep, nested)
            case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
              emitEnumStringFieldsIncr(gep, nested)
            case _ =>
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

      case TNewEnum(et, variantIndex, args) =>
        // Heap-allocated ref-counted enum: [refcount_i64 | tag_i32 | padding | variant_data...].
        // Layout matches TEnumConstruct's value form, but in malloc'd memory with an
        // 8-byte rc header. Returns a data pointer (past the rc header) — same convention
        // as TNew. RefType(EnumType) deinit walks the active variant before free.
        val dataSize = et.sizeOf
        val totalSize = dataSize + 8
        val buf = newReg()
        emit(s"  $buf = call i8* @malloc(i64 $totalSize)")
        val rcPtr = newReg()
        emit(s"  $rcPtr = bitcast i8* $buf to i64*")
        emit(s"  store i64 1, i64* $rcPtr")
        val dataPtr = newReg()
        emit(s"  $dataPtr = getelementptr i8, i8* $buf, i64 8")
        emit(s"  call void @llvm.memset.p0i8.i64(i8* $dataPtr, i8 0, i64 $dataSize, i1 false)")
        // Store tag at data offset 0
        val tagPtr = newReg()
        emit(s"  $tagPtr = bitcast i8* $dataPtr to i32*")
        emit(s"  store i32 $variantIndex, i32* $tagPtr")
        // Store variant fields at data + dataOffset
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
            emit(s"  $fieldAddr = getelementptr i8, i8* $dataPtr, i64 ${dataOffset + fieldOffset}")
            val typedAddr = newReg()
            emit(s"  $typedAddr = bitcast i8* $fieldAddr to $ft*")
            val storeVal = if isAggregate(fieldType) then
              val loaded = newReg()
              emit(s"  $loaded = load $ft, $ft* $v")
              loaded
            else v
            emit(s"  store $ft $storeVal, $ft* $typedAddr")
            // Borrowed string field: incr buffer (caller still owns its copy)
            if isStringType(fieldType) && !isOwnedString(arg) then
              emitStringDescrIncr(typedAddr)
            fieldType match
              case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
                emitStructStringFieldsIncr(typedAddr, nested)
              case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
                emitEnumStringFieldsIncr(typedAddr, nested)
              case _ =>
            fieldOffset += fieldType.sizeOf
        dataPtr // return ptr to data (past rc header) — same convention as TNew

      case TEnumConstruct(et, variantIndex, args) =>
        val totalSize = et.sizeOf
        val lt = llvmType(et)
        val alloca = deferAlloca(lt)
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
            // Borrowed string field: incr buffer (caller still owns its copy)
            if isStringType(fieldType) && !isOwnedString(arg) then
              emitStringDescrIncr(typedAddr)
            // Borrowed nested struct/enum with strings: walk + incr
            fieldType match
              case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
                emitStructStringFieldsIncr(typedAddr, nested)
              case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStruct(arg) =>
                emitEnumStringFieldsIncr(typedAddr, nested)
              case _ =>
            fieldOffset += fieldType.sizeOf
        alloca

      case TMatchExpr(scrutinee, arms, default, typ) =>
        val scrut = genExpr(scrutinee)
        val endLabel = newLabel("match_end")
        val resultAlloca = newReg()
        // Match type may be VoidType when one arm diverges (panic). Use function return type as fallback.
        val effectiveType = if typ == SyslType.VoidType && currentFunction != null && currentFunction.returnType != SyslType.VoidType then
          currentFunction.returnType
        else typ
        val resultLt = llvmType(effectiveType)
        if resultLt != "void" then
          deferredAllocas += ((resultAlloca, resultLt))
        // For each arm, generate: check pattern, if match -> execute body, store result, br to end
        val armLabels = arms.indices.map(_ => newLabel("match_arm"))
        val nextLabels = arms.indices.map(_ => newLabel("match_next"))
        val defaultLabel = newLabel("match_default")
        // Branch to first arm check
        emit(s"  br label %${if arms.nonEmpty then nextLabels(0) else defaultLabel}")
        for (arm, i) <- arms.zipWithIndex do
          emitLabel(nextLabels(i))
          // Check patterns (OR — any pattern matching is enough)
          val matched = arm.patterns.map { pat =>
            pat match
              case TValuePattern(expr) =>
                val patVal = genExpr(expr)
                if scrutinee.typ == SyslType.StringType then
                  // String comparison: check lengths, then memcmp
                  val lLenGep = newReg()
                  emit(s"  $lLenGep = getelementptr %struct.string, %struct.string* $scrut, i32 0, i32 1")
                  val lLen = newReg()
                  emit(s"  $lLen = load i32, i32* $lLenGep")
                  val rLenGep = newReg()
                  emit(s"  $rLenGep = getelementptr %struct.string, %struct.string* $patVal, i32 0, i32 1")
                  val rLen = newReg()
                  emit(s"  $rLen = load i32, i32* $rLenGep")
                  val lenEq = newReg()
                  emit(s"  $lenEq = icmp eq i32 $lLen, $rLen")
                  val lenBlock = currentBlock
                  val lenMatchLbl = newLabel("match_str_len")
                  val strDoneLbl = newLabel("match_str_done")
                  emit(s"  br i1 $lenEq, label %$lenMatchLbl, label %$strDoneLbl")
                  emitLabel(lenMatchLbl)
                  val lPtrGep = newReg()
                  emit(s"  $lPtrGep = getelementptr %struct.string, %struct.string* $scrut, i32 0, i32 0")
                  val lPtr = newReg()
                  emit(s"  $lPtr = load i8*, i8** $lPtrGep")
                  val rPtrGep = newReg()
                  emit(s"  $rPtrGep = getelementptr %struct.string, %struct.string* $patVal, i32 0, i32 0")
                  val rPtr = newReg()
                  emit(s"  $rPtr = load i8*, i8** $rPtrGep")
                  val len64 = newReg()
                  emit(s"  $len64 = sext i32 $lLen to i64")
                  val cmpResult = newReg()
                  emit(s"  $cmpResult = call i32 @memcmp(i8* $lPtr, i8* $rPtr, i64 $len64)")
                  val bytesEq = newReg()
                  emit(s"  $bytesEq = icmp eq i32 $cmpResult, 0")
                  val matchBlock = currentBlock
                  emit(s"  br label %$strDoneLbl")
                  emitLabel(strDoneLbl)
                  val cmp = newReg()
                  emit(s"  $cmp = phi i1 [ false, %$lenBlock ], [ $bytesEq, %$matchBlock ]")
                  cmp
                else
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
              case _: TDestructurePattern =>
                "true" // destructure always matches
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
              emitLabel(guardLabel)
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
          emitLabel(armLabels(i))
          // Snapshot pre-arm locals BEFORE binding variant fields, so the bindings
          // are treated as arm-scoped and removed when the arm exits.
          val preArmLocals = locals.keySet.toSet
          // Bind variant/destructure fields if this is a binding pattern
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
                    val alloc = deferAlloca(flt)
                    val loaded = newReg()
                    emit(s"  $loaded = load $flt, $flt* $typedFAddr")
                    emit(s"  store $flt $loaded, $flt* $alloc")
                    locals(bName) = LocalVar(bName, alloc, ft)
                }
                fOffset += ft.sizeOf
            case Some(TDestructurePattern(st, bindings, fieldTypes)) =>
              val cst = canonicalStruct(st)
              val structLt = llvmType(cst)
              for (binding, j) <- bindings.zipWithIndex do
                val ft = fieldTypes(j)
                binding.foreach { bName =>
                  val flt = llvmType(ft)
                  val fAddr = newReg()
                  emit(s"  $fAddr = getelementptr $structLt, $structLt* $scrut, i32 0, i32 $j")
                  if isAggregate(ft) then
                    locals(bName) = LocalVar(bName, fAddr, ft)
                  else
                    val alloc = deferAlloca(flt)
                    val loaded = newReg()
                    emit(s"  $loaded = load $flt, $flt* $fAddr")
                    emit(s"  store $flt $loaded, $flt* $alloc")
                    locals(bName) = LocalVar(bName, alloc, ft)
                }
            case _ => // no bindings needed
          val savedHR = hasReturned
          hasReturned = false
          if arm.body.nonEmpty then
            for s <- arm.body.init do if !hasReturned then genStmt(s)
            if !hasReturned then
              arm.body.last match
                case TExprStmt(e) =>
                  val v = genExpr(e)
                  if resultLt != "void" && exprType(e) != "void" then
                    if isAggregate(effectiveType) then
                      val loaded = newReg()
                      emit(s"  $loaded = load $resultLt, $resultLt* $v")
                      emit(s"  store $resultLt $loaded, $resultLt* $resultAlloca")
                      if isSliceType(effectiveType) then emitSliceBackrefIncr(resultAlloca)
                      if isStringType(effectiveType) then emitStringDescrIncr(resultAlloca)
                      effectiveType match
                        case st: SyslType.StructType if structHasStringFields(st) =>
                          emitStructStringFieldsIncr(resultAlloca, st)
                        case _ =>
                    else emit(s"  store $resultLt $v, $resultLt* $resultAlloca")
                case other => genStmt(other)
          if !hasReturned then
            emitScopeCleanup(preArmLocals)
            emit(s"  br label %$endLabel")
          // Variant bindings are arm-scoped and reference SSA values defined in the arm's
          // basic block — they must not persist into function-exit cleanup (which runs in
          // a later block where those defs are not in scope).
          for key <- locals.keySet.toList if !preArmLocals.contains(key) do
            locals.remove(key)
          hasReturned = savedHR
        // Default
        emitLabel(defaultLabel)
        val preDefaultLocals = locals.keySet.toSet
        default match
          case Some(stmts) if stmts.nonEmpty =>
            val savedHR = hasReturned
            hasReturned = false
            for s <- stmts.init do if !hasReturned then genStmt(s)
            if !hasReturned then
              stmts.last match
                case TExprStmt(e) =>
                  val v = genExpr(e)
                  if resultLt != "void" && exprType(e) != "void" then
                    if isAggregate(effectiveType) then
                      val loaded = newReg()
                      emit(s"  $loaded = load $resultLt, $resultLt* $v")
                      emit(s"  store $resultLt $loaded, $resultLt* $resultAlloca")
                      if isSliceType(effectiveType) then emitSliceBackrefIncr(resultAlloca)
                      if isStringType(effectiveType) then emitStringDescrIncr(resultAlloca)
                      effectiveType match
                        case st: SyslType.StructType if structHasStringFields(st) =>
                          emitStructStringFieldsIncr(resultAlloca, st)
                        case _ =>
                    else emit(s"  store $resultLt $v, $resultLt* $resultAlloca")
                case other => genStmt(other)
            if !hasReturned then
              emitScopeCleanup(preDefaultLocals)
              emit(s"  br label %$endLabel")
            hasReturned = savedHR
          case _ =>
            if resultLt != "void" then
              if isAggregate(effectiveType) then emit(s"  store $resultLt zeroinitializer, $resultLt* $resultAlloca")
              else emit(s"  store $resultLt 0, $resultLt* $resultAlloca")
            emit(s"  br label %$endLabel")
        emitLabel(endLabel)
        if resultLt != "void" then
          if isAggregate(effectiveType) then resultAlloca // return pointer for aggregate types
          else
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
            case SyslType.FuncType(params, retType, _) =>
              pendingWrappers += ((wn, name, params, retType))
            case _ =>
          wn
        })
        val alloca = deferAlloca("%struct.closure")
        // Store func ptr
        val fpGep = newReg()
        emit(s"  $fpGep = getelementptr %struct.closure, %struct.closure* $alloca, i32 0, i32 0")
        val fpCast = newReg()
        typ match
          case SyslType.FuncType(params, retType, _) =>
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
        // Three env paths:
        //   NullEnv  → env_ptr = null (no captures)
        //   StackEnv → alloca'd in caller's frame (no header, no rc, no free).
        //              Used for non-escaping non-rc-bearing captures.
        //   HeapEnv  → malloc'd with [rc:i64@-16 | deinit_ptr:i8*@-8 | data] header.
        val envSize = c.captures.map((_, t) => llvmSizeOf(t)).sum
        val kind = closureKindOf(c)
        val envPtr = kind match
          case FuncKind.NullEnv => "null"
          case FuncKind.StackEnv =>
            // Stack env: alloca on the function's frame (deferred to entry block
            // so it's stable across the construction expression's IR shape).
            val rawAlloca = deferAlloca(s"[$envSize x i8]")
            val ep = newReg()
            emit(s"  $ep = bitcast [$envSize x i8]* $rawAlloca to i8*")
            // Store captures (no incr — StackEnv has no rc-bearing captures).
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
          case FuncKind.HeapEnv =>
            val totalSize = envSize + 16
            val deinitOpt = closureEnvDeinitFor(closureName, c)
            // malloc(totalSize); base = result
            val base = newReg()
            emit(s"  $base = call i8* @malloc(i64 $totalSize)")
            // Write rc=1 at base+0
            val rcPtr = newReg()
            emit(s"  $rcPtr = bitcast i8* $base to i64*")
            emit(s"  store i64 1, i64* $rcPtr")
            // Write deinit_ptr at base+8
            val deinitGep = newReg()
            emit(s"  $deinitGep = getelementptr i8, i8* $base, i64 8")
            val deinitPtrPtr = newReg()
            emit(s"  $deinitPtrPtr = bitcast i8* $deinitGep to i8**")
            deinitOpt match
              case Some(deinitName) =>
                val fnCast = newReg()
                emit(s"  $fnCast = bitcast i32 (i8*)* @$deinitName to i8*")
                emit(s"  store i8* $fnCast, i8** $deinitPtrPtr")
              case None =>
                emit(s"  store i8* null, i8** $deinitPtrPtr")
            // env_ptr = base + 16 (data area)
            val ep = newReg()
            emit(s"  $ep = getelementptr i8, i8* $base, i64 16")
            // Store captures + Phase A: incr borrowed rc-bearing captures.
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
              if structHasStringFields(capType) then
                emitValueRC(typedEnvPtr, capType, incr = true)
              offset += llvmSizeOf(capType)
            ep
        // Build %struct.closure
        val alloca = deferAlloca("%struct.closure")
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
          case SyslType.FuncType(params, retType, _) =>
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
                val ra = deferAlloca(retLt)
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
          case _: SyslType.FloatType =>
            val v0 = genExpr(inner)
            // Always promote to double for snprintf %g
            val v = if inner.typ == SyslType.F64 then v0 else
              val r = newReg()
              emit(s"  $r = fpext ${llvmType(inner.typ)} $v0 to double")
              r
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
            emit(s"  $dataPtr = getelementptr <{ i64, [$byteLen x i8] }>, <{ i64, [$byteLen x i8] }>* $label, i32 0, i32 1, i32 0")
            emitMakeString(dataPtr, s"${byteLen - 1}")

      case TFmtStr(inner, spec) =>
        val v = genExpr(inner)
        // Build printf format string from FmtSpec
        val fmt = new StringBuilder("%")
        if spec.leftAlign then fmt += '-'
        if spec.zeroPad then fmt += '0'
        if spec.showSign then fmt += '+'
        if spec.width > 0 then fmt ++= spec.width.toString
        inner.typ match
          case t if t.isIntegral =>
            val verb = if spec.upperCase then spec.verb.toUpper else spec.verb
            verb match
              case 'x' | 'X' => fmt ++= (if t == SyslType.IntType(64) then "lx" else "x")
              case 'o' => fmt ++= (if t == SyslType.IntType(64) then "lo" else "o")
              case _ => fmt ++= (if t == SyslType.IntType(64) then "ld" else "d")
            if spec.upperCase && (spec.verb == 'x') then
              // snprintf %X handles uppercase directly
              val fmtStr2 = fmt.toString.replace("x", "X").replace("lx", "lX")
              val (label, byteLen) = internCString(fmtStr2)
              val vt = llvmType(inner.typ)
              val arg = if vt == "i64" then s"i64 $v"
                else if vt == "i32" then s"i32 $v"
                else
                  val ext = newReg()
                  if t.isSigned then emit(s"  $ext = sext $vt $v to i32")
                  else emit(s"  $ext = zext $vt $v to i32")
                  s"i32 $ext"
              emitSnprintfToString(label, byteLen, arg)
            else
              val fmtString = fmt.toString
              val (label, byteLen) = internCString(fmtString)
              val vt = llvmType(inner.typ)
              val arg = if vt == "i64" then s"i64 $v"
                else if vt == "i32" then s"i32 $v"
                else
                  val ext = newReg()
                  if t.isSigned then emit(s"  $ext = sext $vt $v to i32")
                  else emit(s"  $ext = zext $vt $v to i32")
                  s"i32 $ext"
              emitSnprintfToString(label, byteLen, arg)
          case _: SyslType.FloatType =>
            fmt += 'g'
            val fmtString = fmt.toString
            val (label, byteLen) = internCString(fmtString)
            val vd = if inner.typ == SyslType.F64 then v else
              val r = newReg()
              emit(s"  $r = fpext ${llvmType(inner.typ)} $v to double")
              r
            emitSnprintfToString(label, byteLen, s"double $vd")
          case SyslType.StringType =>
            // For string verb with width padding, use snprintf with %s
            fmt += 's'
            val fmtString = fmt.toString
            val (label, byteLen) = internCString(fmtString)
            // Extract ptr from fat string
            val ptr = newReg()
            emit(s"  $ptr = getelementptr %struct.string, %struct.string* $v, i32 0, i32 0")
            val sPtr = newReg()
            emit(s"  $sPtr = load i8*, i8** $ptr")
            emitSnprintfToString(label, byteLen, s"i8* $sPtr")
          case _ =>
            // Fallback: treat as TStr
            genExpr(TStr(inner))

      case TRangeCheck(inner, range, aliasName, targetType) =>
        val v = genExpr(inner)
        val innerLt = llvmType(inner.typ)
        val targetLt = llvmType(targetType)
        val lowOk = newReg()
        val highOk = newReg()
        val ok = newReg()
        val failLbl = s"range_fail_${labelCounter}"
        val passLbl = s"range_pass_${labelCounter}"
        labelCounter += 1
        val isFloat = inner.typ.underlying.isFloat
        val isUnsigned = inner.typ.underlying.isUnsigned
        // Format numeric bound as an LLVM literal (decimal for int, hex for double)
        def intLit(n: Long): String = n.toString
        def floatLit(d: Double): String = s"0x${java.lang.Double.doubleToRawLongBits(d).toHexString.toUpperCase}"
        range match
          case IntRange(lo, hi, excl) =>
            val (low, high) = (intLit(lo), intLit(hi))
            val (lowOp, highOp) =
              if isUnsigned then ("uge", if excl then "ult" else "ule")
              else ("sge", if excl then "slt" else "sle")
            emit(s"  $lowOk = icmp $lowOp $innerLt $v, $low")
            emit(s"  $highOk = icmp $highOp $innerLt $v, $high")
          case FloatRange(lo, hi, excl) =>
            val (low, high) = (floatLit(lo), floatLit(hi))
            emit(s"  $lowOk = fcmp oge $innerLt $v, $low")
            emit(s"  $highOk = fcmp ${if excl then "olt" else "ole"} $innerLt $v, $high")
        emit(s"  $ok = and i1 $lowOk, $highOk")
        emit(s"  br i1 $ok, label %$passLbl, label %$failLbl")
        emit(s"$failLbl:")
        val (nameLbl, nameLen) = internCString(aliasName)
        emit(s"  %${failLbl}_name = getelementptr [$nameLen x i8], [$nameLen x i8]* $nameLbl, i32 0, i32 0")
        emit(s"  call void @__range_fail(i8* %${failLbl}_name, i64 ${nameLen - 1})")
        emit(s"  unreachable")
        emit(s"$passLbl:")
        currentBlock = passLbl
        // Value passes through — relabel to target if needed (NamedType is same LLVM type as base)
        v

      case TCast(inner, targetType) =>
        val v = genExpr(inner)
        val fromLt = llvmType(inner.typ)
        val toLt = llvmType(targetType)
        if fromLt == toLt then v
        else if inner.isInstanceOf[TIntLit] && targetType.isIntegral then v // literal at target width directly
        else
          val result = newReg()
          (inner.typ, targetType) match
            case (_: SyslType.IntType, _: SyslType.FloatType) | (_: SyslType.UIntType, _: SyslType.FloatType) =>
              if inner.typ.isSigned then emit(s"  $result = sitofp $fromLt $v to $toLt")
              else emit(s"  $result = uitofp $fromLt $v to $toLt")
            case (_: SyslType.FloatType, _: SyslType.IntType) =>
              emit(s"  $result = fptosi $fromLt $v to $toLt")
            case (_: SyslType.FloatType, _: SyslType.UIntType) =>
              emit(s"  $result = fptoui $fromLt $v to $toLt")
            case (SyslType.FloatType(a), SyslType.FloatType(b)) =>
              if b > a then emit(s"  $result = fpext $fromLt $v to $toLt")
              else emit(s"  $result = fptrunc $fromLt $v to $toLt")
            case _ if inner.typ.isIntegral && targetType.isIntegral =>
              val fromWidth = fromLt.stripPrefix("i").toInt
              val toWidth = toLt.stripPrefix("i").toInt
              if toWidth > fromWidth then
                if inner.typ.isSigned then emit(s"  $result = sext $fromLt $v to $toLt")
                else emit(s"  $result = zext $fromLt $v to $toLt")
              else
                emit(s"  $result = trunc $fromLt $v to $toLt")
            case _ if inner.typ.isIntegral && (targetType.isInstanceOf[SyslType.PtrType] || targetType.isInstanceOf[SyslType.RefType]) =>
              emit(s"  $result = inttoptr $fromLt $v to $toLt")
            case _ if (inner.typ.isInstanceOf[SyslType.PtrType] || inner.typ.isInstanceOf[SyslType.RefType]) && targetType.isIntegral =>
              emit(s"  $result = ptrtoint $fromLt $v to $toLt")
            case (_: SyslType.FuncType, _) if targetType.isIntegral =>
              // Closure-to-integer: extract function pointer and convert to int.
              // The closure is a %struct.closure* alloca; load the func_ptr field.
              val fpGep = newReg()
              emit(s"  $fpGep = getelementptr %struct.closure, %struct.closure* $v, i32 0, i32 0")
              val fp = newReg()
              emit(s"  $fp = load i8*, i8** $fpGep")
              emit(s"  $result = ptrtoint i8* $fp to $toLt")
            case (SyslType.StringType, _: SyslType.PtrType) =>
              // String-to-pointer: extract the data pointer field (field 0)
              val ptrGep = newReg()
              emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $v, i32 0, i32 0")
              val ptr = newReg()
              emit(s"  $ptr = load i8*, i8** $ptrGep")
              emit(s"  $result = bitcast i8* $ptr to $toLt")
            case (SyslType.ArrayType(_, _), _: SyslType.PtrType) =>
              // Array decay to pointer: GEP to element 0, then bitcast
              val arrLt = llvmType(inner.typ)
              val gep = newReg()
              emit(s"  $gep = getelementptr $arrLt, $arrLt* $v, i32 0, i32 0")
              emit(s"  $result = bitcast ${llvmType(inner.typ.asInstanceOf[SyslType.ArrayType].elem)}* $gep to $toLt")
            case (SyslType.ArrayType(_, _), _) if targetType.isIntegral =>
              // Array decay to integer: GEP to element 0, then ptrtoint
              val arrLt = llvmType(inner.typ)
              val gep = newReg()
              emit(s"  $gep = getelementptr $arrLt, $arrLt* $v, i32 0, i32 0")
              emit(s"  $result = ptrtoint ${llvmType(inner.typ.asInstanceOf[SyslType.ArrayType].elem)}* $gep to $toLt")
            case _ if isAggregate(inner.typ) =>
              // Aggregate types: genExpr returns a pointer, so bitcast the pointer
              emit(s"  $result = bitcast $fromLt* $v to $toLt")
            case _ =>
              emit(s"  $result = bitcast $fromLt $v to $toLt")
          result

      case TPreInc(name, typ) =>
        val lt = llvmType(typ)
        val local = locals(name)
        val oldVal = newReg()
        emit(s"  $oldVal = load $lt, $lt* ${local.reg}")
        val newVal = newReg()
        if typ.isFloat then
          emit(s"  $newVal = fadd $lt $oldVal, 1.0")
        else
          emit(s"  $newVal = add $lt $oldVal, 1")
        emit(s"  store $lt $newVal, $lt* ${local.reg}")
        newVal // pre-increment returns the NEW value

      case TPreDec(name, typ) =>
        val lt = llvmType(typ)
        val local = locals(name)
        val oldVal = newReg()
        emit(s"  $oldVal = load $lt, $lt* ${local.reg}")
        val newVal = newReg()
        if typ.isFloat then
          emit(s"  $newVal = fsub $lt $oldVal, 1.0")
        else
          emit(s"  $newVal = sub $lt $oldVal, 1")
        emit(s"  store $lt $newVal, $lt* ${local.reg}")
        newVal // pre-decrement returns the NEW value

      case TPostInc(name, typ) =>
        val lt = llvmType(typ)
        val local = locals(name)
        val oldVal = newReg()
        emit(s"  $oldVal = load $lt, $lt* ${local.reg}")
        val newVal = newReg()
        if typ.isFloat then
          emit(s"  $newVal = fadd $lt $oldVal, 1.0")
        else
          emit(s"  $newVal = add $lt $oldVal, 1")
        emit(s"  store $lt $newVal, $lt* ${local.reg}")
        oldVal

      case TPostDec(name, typ) =>
        val lt = llvmType(typ)
        val local = locals(name)
        val oldVal = newReg()
        emit(s"  $oldVal = load $lt, $lt* ${local.reg}")
        val newVal = newReg()
        if typ.isFloat then
          emit(s"  $newVal = fsub $lt $oldVal, 1.0")
        else
          emit(s"  $newVal = sub $lt $oldVal, 1")
        emit(s"  store $lt $newVal, $lt* ${local.reg}")
        oldVal

      case TFieldPostInc(obj, fieldIndex, typ) =>
        val (st, structLt, addr) = obj.typ match
          case pt: SyslType.PtrType =>
            val inner = canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType])
            val slt = llvmType(inner)
            val ptr = genExpr(obj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (inner, slt, cast)
          case st: SyslType.StructType =>
            (canonicalStruct(st), llvmType(obj.typ), genStructAddr(obj))
          case other =>
            throw new RuntimeException(s"TFieldPostInc on non-struct type: $other")
        val ft = st.fields(fieldIndex)._2
        val fieldType = llvmType(ft)
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        val oldVal = newReg()
        emit(s"  $oldVal = load $fieldType, $fieldType* $gep")
        val newVal = newReg()
        if ft.isFloat then
          emit(s"  $newVal = fadd $fieldType $oldVal, 1.0")
        else
          emit(s"  $newVal = add $fieldType $oldVal, 1")
        emit(s"  store $fieldType $newVal, $fieldType* $gep")
        oldVal

      case TFieldPostDec(obj, fieldIndex, typ) =>
        val (st, structLt, addr) = obj.typ match
          case pt: SyslType.PtrType =>
            val inner = canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType])
            val slt = llvmType(inner)
            val ptr = genExpr(obj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (inner, slt, cast)
          case st: SyslType.StructType =>
            (canonicalStruct(st), llvmType(obj.typ), genStructAddr(obj))
          case other =>
            throw new RuntimeException(s"TFieldPostDec on non-struct type: $other")
        val ft = st.fields(fieldIndex)._2
        val fieldType = llvmType(ft)
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $addr, i32 0, i32 $fieldIndex")
        val oldVal = newReg()
        emit(s"  $oldVal = load $fieldType, $fieldType* $gep")
        val newVal = newReg()
        if ft.isFloat then
          emit(s"  $newVal = fsub $fieldType $oldVal, 1.0")
        else
          emit(s"  $newVal = sub $fieldType $oldVal, 1")
        emit(s"  store $fieldType $newVal, $fieldType* $gep")
        oldVal

      case TStringFromSlice(sliceExpr, _) =>
        val sp = genExpr(sliceExpr)
        // Extract ptr and len from slice
        val ptrGep = newReg()
        emit(s"  $ptrGep = getelementptr %struct.slice, %struct.slice* $sp, i32 0, i32 0")
        val srcPtr = newReg()
        emit(s"  $srcPtr = load i8*, i8** $ptrGep")
        val lenGep = newReg()
        emit(s"  $lenGep = getelementptr %struct.slice, %struct.slice* $sp, i32 0, i32 1")
        val len = newReg()
        emit(s"  $len = load i32, i32* $lenGep")
        // Allocate header+data and copy bytes (refcount=1)
        val len64 = newReg()
        emit(s"  $len64 = sext i32 $len to i64")
        val dataPtr = emitStringBufferAlloc(len64)
        val cp = newReg()
        emit(s"  $cp = call i8* @memcpy(i8* $dataPtr, i8* $srcPtr, i64 $len64)")
        emitMakeString(dataPtr, len)

      case TStringFromPtr(ptrExpr, lenExpr, _) =>
        val rawPtr = genExpr(ptrExpr)
        val len = genExpr(lenExpr)
        // If ptrExpr is an array, decay to i8* via GEP + bitcast
        val srcPtr = ptrExpr.typ match
          case SyslType.ArrayType(elem, _) =>
            val arrLt = llvmType(ptrExpr.typ)
            val gep = newReg()
            emit(s"  $gep = getelementptr $arrLt, $arrLt* $rawPtr, i32 0, i32 0")
            val bc = newReg()
            emit(s"  $bc = bitcast ${llvmType(elem)}* $gep to i8*")
            bc
          case _ => rawPtr
        // Per spec, string(ptr, len) COPIES the bytes into an owned heap buffer with refcount=1
        val len64 = newReg()
        emit(s"  $len64 = sext i32 $len to i64")
        val dataPtr = emitStringBufferAlloc(len64)
        val cp = newReg()
        emit(s"  $cp = call i8* @memcpy(i8* $dataPtr, i8* $srcPtr, i64 $len64)")
        emitMakeString(dataPtr, len)

      case TAsmExpr(code, typ) =>
        // Inline asm expression — returns a value via the asm block
        val escaped = code.replace("\\n", "\n").replace("\"", "\\22")
        val retLt = llvmType(typ)
        val result = newReg()
        emit(s"""  $result = call $retLt asm sideeffect "$escaped", "=r"()""")
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
        val (structLt, addr) = innerObj.typ match
          case pt: SyslType.PtrType =>
            val slt = llvmType(canonicalStruct(pt.pointee.asInstanceOf[SyslType.StructType]))
            val ptr = genExpr(innerObj)
            val cast = newReg()
            emit(s"  $cast = bitcast i8* $ptr to $slt*")
            (slt, cast)
          case _ =>
            (llvmType(innerObj.typ), genStructAddr(innerObj))
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
    val alloca = deferAlloca("%struct.string")
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
    // Allocate header + data + 1 (for snprintf's required null terminator slot)
    val sizeWithNull = newReg()
    emit(s"  $sizeWithNull = add i64 $len64, 1")
    val buf = emitStringBufferAlloc(sizeWithNull)
    val snprintfSize = newReg()
    emit(s"  $snprintfSize = add i64 $len64, 1")
    val ignored = newReg()
    emit(s"  $ignored = call i32 (i8*, i64, i8*, ...) @snprintf(i8* $buf, i64 $snprintfSize, i8* $fmtPtr, $typedArg)")
    emitMakeString(buf, len)

  // Emit widening/narrowing cast when fromType != toType; return the (possibly cast) register.
  // signed: whether the SOURCE value is signed — controls sext vs zext for integer widening.
  private def emitSextIfNeeded(value: String, fromType: String, toType: String, signed: Boolean = true): String =
    if fromType == toType then value
    else if toType == "void" then value // discarded — no cast needed
    else
      val fromIsPtr = fromType.endsWith("*")
      val toIsPtr = toType.endsWith("*")
      val fromIsFloat = fromType == "float" || fromType == "double"
      val toIsFloat = toType == "float" || toType == "double"
      val cast = newReg()
      if !fromIsPtr && toIsPtr then
        emit(s"  $cast = inttoptr $fromType $value to $toType")
      else if fromIsPtr && !toIsPtr then
        emit(s"  $cast = ptrtoint $fromType $value to $toType")
      else if fromIsFloat && toIsFloat then
        if fromType == "float" && toType == "double" then
          emit(s"  $cast = fpext float $value to double")
        else
          emit(s"  $cast = fptrunc double $value to float")
      else
        val fromW = fromType.stripPrefix("i").toIntOption.getOrElse(0)
        val toW = toType.stripPrefix("i").toIntOption.getOrElse(0)
        if fromW > toW && toW > 0 then
          emit(s"  $cast = trunc $fromType $value to $toType")
        else if signed then
          emit(s"  $cast = sext $fromType $value to $toType")
        else
          emit(s"  $cast = zext $fromType $value to $toType")
      cast

  private def llvmType(t: SyslType): String = t match
    case SyslType.IntType(w) => s"i$w"
    case SyslType.UIntType(w) => s"i$w"
    case SyslType.BoolType => "i8"
    case SyslType.FloatType(32) => "float"
    case SyslType.FloatType(64) => "double"
    case SyslType.FloatType(w) => throw new RuntimeException(s"unsupported float width: $w")
    case SyslType.VoidType => "void"
    case SyslType.StringType => "%struct.string"
    case SyslType.StructType(name, fields, _) =>
      // Auto-register struct types encountered in signatures (e.g., built-in tuples)
      if !structTypes.contains(name) && fields.nonEmpty then
        structTypes(name) = SyslType.StructType(name, fields)
      s"%struct.$name"
    case SyslType.ArrayType(elem, size) => s"[$size x ${llvmType(elem)}]"
    case et: SyslType.EnumType => s"[${et.sizeOf} x i8]" // opaque byte array for tagged union
    case SyslType.SliceType(_) => "%struct.slice"
    case SyslType.PtrType(_) => "i8*"
    case SyslType.RefType(_) => "i8*"
    case _: SyslType.FuncType => "%struct.closure"
    // Named/derived types are erased to their base at the LLVM layer.
    case SyslType.NamedType(_, base, _, _) => llvmType(base)
    case _ => "i64"

  // LLVM-side size in bytes (may differ from Sysl's sizeOf for types like strings)
  private def llvmSizeOf(t: SyslType): Long = t match
    case SyslType.StringType => 16  // {i8*, i32} — matches Sysl's sizeOf
    case SyslType.PtrType(_) => 8
    case SyslType.RefType(_: SyslType.SliceType) => 24  // inline %struct.slice
    case SyslType.RefType(_) => 8
    case _: SyslType.FuncType => 16  // {i8*, i8*}
    case SyslType.BoolType => 1
    case SyslType.SliceType(_) => 24  // {i8*, i32, i32, i8*}
    case SyslType.StructType(name, fields, _) =>
      // Use LLVM's struct layout rules: each field aligned to its natural alignment,
      // and the struct's total size rounded up to its alignment (max of all field alignments).
      val resolved = canonicalStruct(SyslType.StructType(name, fields))
      var offset = 0L
      var maxAlign = 1L
      for (_, ft) <- resolved.fields do
        val fieldSize = llvmSizeOf(ft)
        val fieldAlign = llvmAlignOf(ft)
        maxAlign = math.max(maxAlign, fieldAlign)
        offset = (offset + fieldAlign - 1) / fieldAlign * fieldAlign // align
        offset += fieldSize
      // Round up to struct alignment
      if maxAlign > 1 then offset = (offset + maxAlign - 1) / maxAlign * maxAlign
      offset
    case SyslType.ArrayType(elem, size) => llvmSizeOf(elem) * size
    case other => other.sizeOf

  /** Alignment of a type in bytes, matching LLVM's natural alignment rules. */
  private def llvmAlignOf(t: SyslType): Long = t match
    case SyslType.PtrType(_) | SyslType.RefType(_) => 8
    case SyslType.IntType(w) => math.min(w / 8, 8).toLong
    case SyslType.UIntType(w) => math.min(w / 8, 8).toLong
    case SyslType.BoolType => 1
    case SyslType.FloatType(w) => math.min(w / 8, 8).toLong
    case SyslType.StringType => 8  // contains pointer
    case SyslType.SliceType(_) => 8  // contains pointer
    case _: SyslType.FuncType => 8  // contains pointer
    case SyslType.StructType(name, fields, _) =>
      val resolved = canonicalStruct(SyslType.StructType(name, fields))
      if resolved.fields.isEmpty then 1 else resolved.fields.map((_, ft) => llvmAlignOf(ft)).max
    case SyslType.ArrayType(elem, _) => llvmAlignOf(elem)
    case SyslType.NamedType(_, base, _, _) => llvmAlignOf(base)
    case _ => 8

  // Types that are passed by pointer (alloca) rather than by value
  private def isAggregate(t: SyslType): Boolean = t match
    case _: SyslType.StructType | _: SyslType.ArrayType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | SyslType.StringType => true
    case SyslType.NamedType(_, base, _, _) => isAggregate(base)
    case _ => false

  // ===== Refcounting helpers =====

  private def isRef(t: SyslType): Boolean = t match
    case _: SyslType.RefType => true
    case _ => false

  private def isSliceType(t: SyslType): Boolean = t match
    case _: SyslType.SliceType => true
    case _ => false

  private def isStringType(t: SyslType): Boolean = t == SyslType.StringType

  /** True if a value type (recursively) contains any string buffers needing rc.
    * Recurses through value-struct fields, value-array elements, and enum
    * variant fields. Stops at refs/pointers/slices (handled separately). */
  private def structHasStringFields(t: SyslType): Boolean = t match
    case _ if isStringType(t) => true
    case st: SyslType.StructType =>
      val resolved = canonicalStruct(st)
      resolved.fields.exists((_, ft) => structHasStringFields(ft))
    case SyslType.ArrayType(elem, _) => structHasStringFields(elem)
    case et: SyslType.EnumType =>
      et.variants.exists((_, fields) => fields.exists((_, ft) => structHasStringFields(ft)))
    case _ => false

  /** True if any field of an enum variant carries string content. */
  private def variantHasStringFields(fields: List[(String, SyslType)]): Boolean =
    fields.exists((_, ft) => structHasStringFields(ft))

  /** Decrement rc of every string buffer (recursively into nested value-struct fields
    * and value-array elements) at `structAddr`. */
  private def emitStructStringFieldsDecr(structAddr: String, st: SyslType.StructType): Unit =
    val resolved = canonicalStruct(st)
    val structLt = llvmType(resolved)
    for case ((_, ft), i) <- resolved.fields.zipWithIndex do
      if structHasStringFields(ft) then
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $structAddr, i32 0, i32 $i")
        emitValueRC(gep, ft, incr = false)

  /** Increment rc of every string buffer (recursively) inside a value struct.
    * Used when copying a borrowed struct so the destination owns its share. */
  private def emitStructStringFieldsIncr(structAddr: String, st: SyslType.StructType): Unit =
    val resolved = canonicalStruct(st)
    val structLt = llvmType(resolved)
    for case ((_, ft), i) <- resolved.fields.zipWithIndex do
      if structHasStringFields(ft) then
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $structAddr, i32 0, i32 $i")
        emitValueRC(gep, ft, incr = true)

  /** Null out every string descriptor's data pointer (recursively) so a later cleanup
    * pass sees an already-released field and skips the decrement (null check). */
  private def emitStructStringFieldsNull(structAddr: String, st: SyslType.StructType): Unit =
    val resolved = canonicalStruct(st)
    val structLt = llvmType(resolved)
    for case ((_, ft), i) <- resolved.fields.zipWithIndex do
      if structHasStringFields(ft) then
        val gep = newReg()
        emit(s"  $gep = getelementptr $structLt, $structLt* $structAddr, i32 0, i32 $i")
        emitValueNull(gep, ft)

  /** Emit a per-elem-type slice deinit function. Called when freeing a `&[]T`
    * whose elements carry rc content. Slice block layout:
    * [rc:8 | len:8 | data...]. The function receives the data pointer (past the
    * 16-byte header), reads len from data-8, and decrs every element. Length
    * field is 8 bytes (i64) — the layout matches what TNewArray emits.
    *
    * Returns i32 (matches deinitFunctions ABI) but the value is unused. */
  private def emitSliceDeinit(name: String, elem: SyslType): Unit =
    val elemLt = llvmType(elem)
    // Fresh register/label namespace for this synthesized function
    regCounter = 0
    labelCounter = 0
    emit(s"; slice element deinit: $name")
    emit(s"define i32 @$name(i8* %data) {")
    emit("entry:")
    // Slice block layout (per TNewArray): rc:i64@0, len:i32@8, cap:i32@12, data@16.
    // We're called with data ptr, so length is at data-8 as i32.
    val lenAddr = newReg()
    emit(s"  $lenAddr = getelementptr i8, i8* %data, i64 -8")
    val lenAddrPtr = newReg()
    emit(s"  $lenAddrPtr = bitcast i8* $lenAddr to i32*")
    val len32 = newReg()
    emit(s"  $len32 = load i32, i32* $lenAddrPtr")
    val len = newReg()
    emit(s"  $len = sext i32 $len32 to i64")
    // Cast data ptr to elem ptr for indexing
    val basePtr = newReg()
    emit(s"  $basePtr = bitcast i8* %data to $elemLt*")
    // Use an alloca for the counter — emitValueRC may create sub-blocks
    // (for null-checks etc.) so the back-edge predecessor isn't a single
    // known block, defeating a simple phi.
    val iSlot = newReg()
    emit(s"  $iSlot = alloca i64")
    emit(s"  store i64 0, i64* $iSlot")
    val loopEntry = newLabel(s"sd_${name}_loop")
    val loopBody = newLabel(s"sd_${name}_body")
    val loopExit = newLabel(s"sd_${name}_done")
    emit(s"  br label %$loopEntry")
    emitLabel(loopEntry)
    val iVal = newReg()
    emit(s"  $iVal = load i64, i64* $iSlot")
    val cond = newReg()
    emit(s"  $cond = icmp ult i64 $iVal, $len")
    emit(s"  br i1 $cond, label %$loopBody, label %$loopExit")
    emitLabel(loopBody)
    val elemPtr = newReg()
    emit(s"  $elemPtr = getelementptr $elemLt, $elemLt* $basePtr, i64 $iVal")
    emitValueRC(elemPtr, elem, incr = false)
    val nextI = newReg()
    emit(s"  $nextI = add i64 $iVal, 1")
    emit(s"  store i64 $nextI, i64* $iSlot")
    emit(s"  br label %$loopEntry")
    emitLabel(loopExit)
    emit("  ret i32 0")
    emit("}")
    emit("")

  /** Auto-synthesized per-struct-type deinit. Called when freeing a `&MyStruct`
    * whose fields carry rc content and the user hasn't defined
    * `TypeName.deinit`. Receives the struct's data ptr (past the rc header)
    * and decrs each string-bearing field. Buffer itself is freed by the
    * caller's emitRefDecr after this returns. */
  private def emitStructDeinit(name: String, st: SyslType.StructType): Unit =
    val structLt = llvmType(st)
    regCounter = 0
    labelCounter = 0
    emit(s"; struct deinit: $name")
    emit(s"define i32 @$name(i8* %data) {")
    emit("entry:")
    val typed = newReg()
    emit(s"  $typed = bitcast i8* %data to $structLt*")
    emitStructStringFieldsDecr(typed, st)
    emit("  ret i32 0")
    emit("}")
    emit("")

  /** Per-enum-type deinit. Called when freeing a `&MyEnum` whose active variant
    * carries rc content. Receives the enum's data ptr (past the rc header) and
    * walks the active variant's strings via emitEnumStringFieldsDecr. The buffer
    * itself is freed by the caller's emitRefDecr after this returns. */
  private def emitEnumDeinit(name: String, et: SyslType.EnumType): Unit =
    val enumLt = llvmType(et)
    regCounter = 0
    labelCounter = 0
    emit(s"; enum deinit: $name")
    emit(s"define i32 @$name(i8* %data) {")
    emit("entry:")
    val typed = newReg()
    emit(s"  $typed = bitcast i8* %data to $enumLt*")
    emitEnumStringFieldsDecr(typed, et)
    emit("  ret i32 0")
    emit("}")
    emit("")

  /** Generalized rc walker for any value type at `addr`. Handles strings,
    * value structs (recurses), value arrays (loops over elements), and value
    * enums (runtime tag-dispatch). No-op for any type without string content. */
  private def emitValueRC(addr: String, t: SyslType, incr: Boolean): Unit =
    if !structHasStringFields(t) then return
    if isStringType(t) then
      if incr then emitStringDescrIncr(addr) else emitStringDescrDecr(addr)
    else t match
      case st: SyslType.StructType =>
        if incr then emitStructStringFieldsIncr(addr, st) else emitStructStringFieldsDecr(addr, st)
      case SyslType.ArrayType(elem, count) =>
        val elemLt = llvmType(elem)
        for i <- 0 until count do
          val gep = newReg()
          emit(s"  $gep = getelementptr [$count x $elemLt], [$count x $elemLt]* $addr, i32 0, i32 $i")
          emitValueRC(gep, elem, incr)
      case et: SyslType.EnumType =>
        if incr then emitEnumStringFieldsIncr(addr, et) else emitEnumStringFieldsDecr(addr, et)
      case _ =>

  /** Null out string descriptor data ptrs in a value type (recurses through structs/arrays/enums). */
  private def emitValueNull(addr: String, t: SyslType): Unit =
    if !structHasStringFields(t) then return
    if isStringType(t) then
      val pgep = newReg()
      emit(s"  $pgep = getelementptr %struct.string, %struct.string* $addr, i32 0, i32 0")
      emit(s"  store i8* null, i8** $pgep")
    else t match
      case st: SyslType.StructType =>
        emitStructStringFieldsNull(addr, st)
      case SyslType.ArrayType(elem, count) =>
        val elemLt = llvmType(elem)
        for i <- 0 until count do
          val gep = newReg()
          emit(s"  $gep = getelementptr [$count x $elemLt], [$count x $elemLt]* $addr, i32 0, i32 $i")
          emitValueNull(gep, elem)
      case et: SyslType.EnumType =>
        emitEnumStringFieldsNull(addr, et)
      case _ =>

  /** Decrement rc of every string in the active variant of an enum at `addr`.
    * Loads tag (i32 @ 0), then chained icmp/br per variant carrying string
    * content; variants with no strings are skipped. Each match block walks
    * the variant's fields (computing offsets via stride) and branches to end. */
  private def emitEnumStringFieldsDecr(addr: String, et: SyslType.EnumType): Unit =
    emitEnumStringFieldsWalk(addr, et, mode = "decr")

  /** Increment rc of every string in the active variant. Used when copying a
    * borrowed enum (e.g., field of a struct constructed from a borrowed source). */
  private def emitEnumStringFieldsIncr(addr: String, et: SyslType.EnumType): Unit =
    emitEnumStringFieldsWalk(addr, et, mode = "incr")

  /** Null out string descriptor ptrs in the active variant. Used after scope
    * cleanup decr to prevent double-decrement when an outer scope re-walks. */
  private def emitEnumStringFieldsNull(addr: String, et: SyslType.EnumType): Unit =
    emitEnumStringFieldsWalk(addr, et, mode = "null")

  private def emitEnumStringFieldsWalk(addr: String, et: SyslType.EnumType, mode: String): Unit =
    val variantsWithStrings = et.variants.zipWithIndex.collect {
      case ((_, fields), idx) if variantHasStringFields(fields) => (fields, idx)
    }
    if variantsWithStrings.isEmpty then return
    val lt = llvmType(et)
    val byteAddr = newReg()
    emit(s"  $byteAddr = bitcast $lt* $addr to i8*")
    val tagPtr = newReg()
    emit(s"  $tagPtr = bitcast i8* $byteAddr to i32*")
    val tag = newReg()
    emit(s"  $tag = load i32, i32* $tagPtr")
    val endLabel = newLabel("enum_rc_end")
    val dataOff = et.dataOffset
    for (fields, idx) <- variantsWithStrings do
      val matchLbl = newLabel("enum_rc_match")
      val nextLbl = newLabel("enum_rc_next")
      val cmp = newReg()
      emit(s"  $cmp = icmp eq i32 $tag, $idx")
      emit(s"  br i1 $cmp, label %$matchLbl, label %$nextLbl")
      emitLabel(matchLbl)
      var fieldOffset = 0L
      for (_, ft) <- fields do
        val align = ft.alignOf
        fieldOffset = ((fieldOffset + align - 1) / align) * align
        if structHasStringFields(ft) then
          val fieldByteAddr = newReg()
          emit(s"  $fieldByteAddr = getelementptr i8, i8* $byteAddr, i64 ${dataOff + fieldOffset}")
          val flt = llvmType(ft)
          val fieldTypedAddr = newReg()
          emit(s"  $fieldTypedAddr = bitcast i8* $fieldByteAddr to $flt*")
          mode match
            case "decr" => emitValueRC(fieldTypedAddr, ft, incr = false)
            case "incr" => emitValueRC(fieldTypedAddr, ft, incr = true)
            case "null" => emitValueNull(fieldTypedAddr, ft)
        fieldOffset += ft.sizeOf
      emit(s"  br label %$endLabel")
      emitLabel(nextLbl)
    emit(s"  br label %$endLabel")
    emitLabel(endLabel)

  /** True for expressions that produce a freshly-owned string buffer (refcount = 1 or immortal).
    * No incr is needed when binding such a value to a fresh local. */
  private def isOwnedString(expr: TExpr): Boolean = expr match
    case _: TStringLit => true                          // immortal sentinel — incr is a no-op anyway
    case _: TStringFromPtr | _: TStringFromSlice => true
    case TBinary(_, "+", _, SyslType.StringType) => true
    case TSliceExpr(_, _, _, SyslType.StringType) => true
    case _: TCall | _: TIndirectCall => true            // ownership transferred from callee
    case _: TIfExpr | _: TMatchExpr => true             // branches handle their own RC
    case _ => false

  /** True for expressions that produce a freshly-constructed/transferred-ownership
    * value struct or enum. No incr is needed when binding such a value to a fresh local. */
  private def isOwnedStruct(expr: TExpr): Boolean = expr match
    case _: TStructConstruct => true
    case _: TEnumConstruct => true
    case _: TCall | _: TIndirectCall => true
    case _: TIfExpr | _: TMatchExpr => true
    case _ => false

  /** Returns true if the expression produces a slice with a fresh/null backref (no increment needed).
    * Returns false if the expression borrows a backref from elsewhere (increment needed on copy). */
  private def isSliceOwned(expr: TExpr): Boolean = expr match
    case _: TAppend => false                             // no-grow inherits input backref
    case _: TSliceExpr => true                           // all TSliceExpr paths produce owned backrefs
    case _: TCall | _: TIndirectCall => true             // ownership transferred from callee
    case _: TIfExpr | _: TMatchExpr => true              // branches handle their own RC
    case _ => false

  /** Identify all local slice/string allocas that should NOT be cleaned up because they
    * are part of the returned expression (either directly or embedded in a struct).
    * Returns a set of alloca registers to skip during emitReleaseRefs. */
  private def returnedSliceAllocas(expr: TExpr): Set[String] =
    if locals == null then return Set.empty
    expr match
      case TVarRef(name, typ) if isSliceType(typ) && locals.contains(name) =>
        Set(locals(name).reg)
      case TVarRef(name, typ) if isStringType(typ) && locals.contains(name) =>
        Set(locals(name).reg)
      case TVarRef(name, typ) if structHasStringFields(typ) && locals.contains(name) =>
        Set(locals(name).reg)
      case TVarRef(name, _) if derivedFromSlice != null && derivedFromSlice.contains(name) =>
        val sliceName = derivedFromSlice(name)
        if locals.contains(sliceName) then Set(locals(sliceName).reg) else Set.empty
      case TStructConstruct(_, args) =>
        // Struct being returned — skip cleanup for any slice/string args that are local VarRefs
        args.flatMap(returnedSliceAllocas).toSet
      case TAppend(slice, _, _) =>
        // Append may inherit input's backref — protect the source slice
        returnedSliceAllocas(slice)
      case TCall(_, args, typ) if isSliceType(typ) =>
        // Slice-returning call may share backref with a slice argument — protect all slice args
        args.flatMap(returnedSliceAllocas).toSet
      case TIndirectCall(_, args, typ) if isSliceType(typ) =>
        args.flatMap(returnedSliceAllocas).toSet
      case _ => Set.empty


  /** Header offset: bytes from data pointer back to refcount field. */
  private def refHeaderOffset(t: SyslType): Int = t match
    case SyslType.RefType(SyslType.SliceType(_)) => 16  // refcount(8) + len(4) + cap(4)
    case SyslType.RefType(_) => 8                        // refcount(8) only
    case SyslType.StringType => 8                        // i64 refcount
    case _ => 8

  /** Allocate a string buffer with i64 refcount header initialized to 1.
    * dataLen64 is an i64 register/literal for the data byte length.
    * Returns the data pointer (i8*) past the header. */
  private def emitStringBufferAlloc(dataLen64: String): String =
    val totalSize = newReg()
    emit(s"  $totalSize = add i64 $dataLen64, 8")
    val base = newReg()
    emit(s"  $base = call i8* @malloc(i64 $totalSize)")
    val rcPtr = newReg()
    emit(s"  $rcPtr = bitcast i8* $base to i64*")
    emit(s"  store i64 1, i64* $rcPtr")
    val dataPtr = newReg()
    emit(s"  $dataPtr = getelementptr i8, i8* $base, i64 8")
    dataPtr

  /** Increment refcount of the buffer referenced by a string descriptor alloca. */
  private def emitStringDescrIncr(descrAlloca: String): Unit =
    val ptrGep = newReg()
    emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $descrAlloca, i32 0, i32 0")
    val ptr = newReg()
    emit(s"  $ptr = load i8*, i8** $ptrGep")
    emitRefIncr(ptr, 8)

  /** Decrement refcount of the buffer referenced by a string descriptor alloca; frees on 0. */
  private def emitStringDescrDecr(descrAlloca: String): Unit =
    val ptrGep = newReg()
    emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* $descrAlloca, i32 0, i32 0")
    val ptr = newReg()
    emit(s"  $ptr = load i8*, i8** $ptrGep")
    emitRefDecr(ptr, 8)

  /** Emit inline refcount increment. ptr is the data pointer (past header). */
  private def emitRefIncr(ptr: String, headerOffset: Int): Unit =
    val skip = newLabel("rc_skip")
    val doIncr = newLabel("rc_incr")
    // Null check
    val isNull = newReg()
    emit(s"  $isNull = icmp eq i8* $ptr, null")
    emit(s"  br i1 $isNull, label %$skip, label %$doIncr")
    emitLabel(doIncr)
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
    emitLabel(doStore)
    val newRc = newReg()
    emit(s"  $newRc = add i64 $rc, 1")
    emit(s"  store i64 $newRc, i64* $rcPtr")
    emit(s"  br label %$skip")
    emitLabel(skip)

  /** Look up deinit function name for a RefType's inner type. */
  private def deinitFor(typ: SyslType): Option[String] = typ match
    case SyslType.RefType(st @ SyslType.StructType(name, _, _)) =>
      deinitFunctions.get(name).orElse(structDeinitFor(st))
    case SyslType.RefType(SyslType.SliceType(elem)) => sliceDeinitFor(elem)
    case SyslType.RefType(et: SyslType.EnumType) => enumDeinitFor(et)
    case _ => None

  /** Mangled name for a per-enum-type deinit function. Registers the type so
    * the function body is emitted at the end of generate(). Returns None if no
    * variant carries rc content (no walk needed; just free). */
  private def enumDeinitFor(et: SyslType.EnumType): Option[String] =
    if !structHasStringFields(et) then None
    else
      val name = s"__enum_deinit_${et.name}"
      enumDeinitsNeeded(name) = et
      Some(name)

  /** Auto-synthesized struct deinit: walks string fields before free. Only
    * registered for structs with rc-bearing fields and no user-defined
    * `TypeName.deinit` (which takes precedence via `deinitFunctions`). */
  private def structDeinitFor(st: SyslType.StructType): Option[String] =
    if !structHasStringFields(st) then None
    else
      val name = s"__struct_deinit_${st.name}"
      structDeinitsNeeded(name) = st
      Some(name)

  /** True if a slice element type carries refcounted content that must be
    * decr'd before the backing buffer is freed. */
  private def sliceElemNeedsDeinit(elem: SyslType): Boolean = elem match
    case _ if isStringType(elem) => true
    case _: SyslType.RefType => true
    case st: SyslType.StructType => structHasStringFields(st)
    case et: SyslType.EnumType => structHasStringFields(et)
    case SyslType.ArrayType(e, _) => sliceElemNeedsDeinit(e)
    case _ => false

  /** Mangled name for a per-elem-type slice deinit function (no `@` prefix —
    * call sites add it themselves, matching deinitFunctions convention).
    * Registers the type so the function body is emitted at the end of generate().
    * Returns None for elem types that hold no rc content. */
  private def sliceDeinitFor(elem: SyslType): Option[String] =
    if !sliceElemNeedsDeinit(elem) then None
    else
      val tag = mangleType(elem)
      val name = s"__slice_deinit_$tag"
      sliceElemDeinitsNeeded(name) = elem
      Some(name)

  /** Stable, llvm-safe tag for a type, used to mangle slice deinit names. */
  private def mangleType(t: SyslType): String = t match
    case _ if isStringType(t) => "string"
    case SyslType.RefType(inner) => s"ref_${mangleType(inner)}"
    case SyslType.StructType(n, _, _) => s"struct_$n"
    case SyslType.EnumType(n, _) => s"enum_$n"
    case SyslType.ArrayType(e, n) => s"arr${n}_${mangleType(e)}"
    case other => other.getClass.getSimpleName.toLowerCase

  /** Closure env layout (heap):
    *   [rc:i64 @ -16 | deinit_ptr:i8* @ -8 | data...]
    * env_ptr is an i8* pointing to data. envSize+16 is malloc'd; rc=1 written;
    * deinit_ptr is the per-closure-id deinit (or null if no rc captures).
    *
    * __closure_env_dispatch is a thin shim called when env's rc hits zero. It
    * loads the runtime deinit_ptr from env-8 and tail-calls it. Lets emitRefDecr
    * handle envs uniformly without per-closure-id static deinit linkage at scope
    * cleanup sites.
    */
  private def emitClosureEnvDispatch(): Unit =
    emit("define i32 @__closure_env_dispatch(i8* %env) {")
    emitLabel("entry")
    val deinitGep = newReg()
    emit(s"  $deinitGep = getelementptr i8, i8* %env, i64 -8")
    val deinitPtrPtr = newReg()
    emit(s"  $deinitPtrPtr = bitcast i8* $deinitGep to i8**")
    val deinitPtr = newReg()
    emit(s"  $deinitPtr = load i8*, i8** $deinitPtrPtr")
    val isNull = newReg()
    emit(s"  $isNull = icmp eq i8* $deinitPtr, null")
    val callLbl = newLabel("dispatch_call")
    val skipLbl = newLabel("dispatch_skip")
    emit(s"  br i1 $isNull, label %$skipLbl, label %$callLbl")
    emitLabel(callLbl)
    val typedFn = newReg()
    emit(s"  $typedFn = bitcast i8* $deinitPtr to i32 (i8*)*")
    val r = newReg()
    emit(s"  $r = call i32 $typedFn(i8* %env)")
    emit(s"  br label %$skipLbl")
    emitLabel(skipLbl)
    emit("  ret i32 0")
    emit("}")
    emit("")

  /** Per-closure-id env deinit: walks the closure's captures (known layout) and
    * decr's rc-bearing entries via emitValueRC. Receives `i8* %env` (data ptr).
    * Only registered for closures with at least one rc-bearing capture; closures
    * with none use deinit_ptr=null and the dispatch shim no-ops.
    */
  private def emitClosureEnvDeinit(name: String, closure: TClosure): Unit =
    emit(s"define i32 @$name(i8* %env) {")
    emitLabel("entry")
    var offset = 0L
    for (_, capType) <- closure.captures do
      if structHasStringFields(capType) then
        val byteAddr = newReg()
        emit(s"  $byteAddr = getelementptr i8, i8* %env, i64 $offset")
        val lt = llvmType(capType)
        val typedAddr = newReg()
        emit(s"  $typedAddr = bitcast i8* $byteAddr to $lt*")
        emitValueRC(typedAddr, capType, incr = false)
      offset += llvmSizeOf(capType)
    emit("  ret i32 0")
    emit("}")
    emit("")

  /** Register a per-closure-id env deinit if the closure has rc-bearing
    * captures. Returns the deinit function name, or None for scalar-only captures. */
  private def closureEnvDeinitFor(name: String, closure: TClosure): Option[String] =
    val hasRcCaptures = closure.captures.exists((_, t) => structHasStringFields(t) || t.isInstanceOf[SyslType.RefType])
    if !hasRcCaptures then None
    else
      val deinitName = s"__closure_env_deinit_$name"
      closureEnvDeinitsNeeded(deinitName) = closure
      Some(deinitName)

  /** Decrement env's rc via the descriptor's env_ptr (field 1 in %struct.closure).
    * Loads env_ptr; if non-null, calls emitRefDecr with headerOff=16 and the
    * generic dispatch deinit. */
  private def emitClosureDescrDecr(descrAlloca: String): Unit =
    closureEnvDispatchNeeded = true
    val envGep = newReg()
    emit(s"  $envGep = getelementptr %struct.closure, %struct.closure* $descrAlloca, i32 0, i32 1")
    val envPtr = newReg()
    emit(s"  $envPtr = load i8*, i8** $envGep")
    emitRefDecr(envPtr, 16, Some("__closure_env_dispatch"))

  /** Increment env's rc via descriptor's env_ptr. Used when copying a
    * descriptor (var g = f) so both descriptors share the env. */
  private def emitClosureDescrIncr(descrAlloca: String): Unit =
    val envGep = newReg()
    emit(s"  $envGep = getelementptr %struct.closure, %struct.closure* $descrAlloca, i32 0, i32 1")
    val envPtr = newReg()
    emit(s"  $envPtr = load i8*, i8** $envGep")
    emitRefIncr(envPtr, 16)

  /** When a function returns a borrowed FuncType local (param OR HeapEnv local),
    * incr the env so the caller's TCall=HeapEnv decr balances. `descAlloca` is
    * the descriptor pointer (genExpr's result for an aggregate). No-op for any
    * other expression. */
  private def emitFuncReturnIncrIfBorrowed(value: TExpr, descAlloca: String): Unit =
    value match
      case TVarRef(_, t) if t.isInstanceOf[SyslType.FuncType]
        && funcKindOfExpr(value) == FuncKind.HeapEnv =>
        emitClosureDescrIncr(descAlloca)
      case _ =>

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
    emitLabel(doDecr)
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
    emitLabel(doStore)
    val newRc = newReg()
    emit(s"  $newRc = sub i64 $rc, 1")
    emit(s"  store i64 $newRc, i64* $rcPtr")
    val isZero = newReg()
    emit(s"  $isZero = icmp eq i64 $newRc, 0")
    val doFree = newLabel("rcd_free")
    emit(s"  br i1 $isZero, label %$doFree, label %$skip")
    emitLabel(doFree)
    // Set refcount to IMMORTAL (-1) to prevent re-entrant deinit
    emit(s"  store i64 -1, i64* $rcPtr")
    // Call deinit if present (passes data pointer, not base)
    deinit.foreach { name =>
      emit(s"  call i32 @$name(i8* $ptr)")
    }
    // Free the base allocation
    emit(s"  call void @free(i8* $base)")
    emit(s"  br label %$skip")
    emitLabel(skip)

  /** Decrement refcounts for all ref-typed locals before function exit. */
  /** Emit backref increment for a slice: load backref field, if non-null increment refcount. */
  private def emitSliceBackrefIncr(slicePtr: String): Unit =
    val brGep = newReg()
    emit(s"  $brGep = getelementptr %struct.slice, %struct.slice* $slicePtr, i32 0, i32 3")
    val br = newReg()
    emit(s"  $br = load i8*, i8** $brGep")
    val isNull = newReg()
    emit(s"  $isNull = icmp eq i8* $br, null")
    val incrLabel = newLabel("br_incr")
    val skipLabel = newLabel("br_skip")
    emit(s"  br i1 $isNull, label %$skipLabel, label %$incrLabel")
    emitLabel(incrLabel)
    val rcPtr = newReg()
    emit(s"  $rcPtr = bitcast i8* $br to i64*")
    val rc = newReg()
    emit(s"  $rc = load i64, i64* $rcPtr")
    val newRc = newReg()
    emit(s"  $newRc = add i64 $rc, 1")
    emit(s"  store i64 $newRc, i64* $rcPtr")
    emit(s"  br label %$skipLabel")
    emitLabel(skipLabel)

  /** Emit backref decrement for a slice: load backref field, if non-null decrement refcount,
    * call elem deinit (if any), free at zero. `sliceType` is the slice's value type
    * (`SyslType.SliceType(elem)`); needed to dispatch the per-elem-type deinit. */
  private def emitSliceBackrefDecr(slicePtr: String, sliceType: SyslType = SyslType.VoidType): Unit =
    val deinit = sliceType match
      case SyslType.SliceType(elem) => sliceDeinitFor(elem)
      case _ => None
    val brGep = newReg()
    emit(s"  $brGep = getelementptr %struct.slice, %struct.slice* $slicePtr, i32 0, i32 3")
    val br = newReg()
    emit(s"  $br = load i8*, i8** $brGep")
    val isNull = newReg()
    emit(s"  $isNull = icmp eq i8* $br, null")
    val decrLabel = newLabel("br_decr")
    val skipLabel = newLabel("br_skip")
    emit(s"  br i1 $isNull, label %$skipLabel, label %$decrLabel")
    emitLabel(decrLabel)
    val rcPtr = newReg()
    emit(s"  $rcPtr = bitcast i8* $br to i64*")
    val rc = newReg()
    emit(s"  $rc = load i64, i64* $rcPtr")
    // Check immortal
    val isImmortal = newReg()
    emit(s"  $isImmortal = icmp eq i64 $rc, -1")
    val doDecr = newLabel("br_do_decr")
    emit(s"  br i1 $isImmortal, label %$skipLabel, label %$doDecr")
    emitLabel(doDecr)
    val newRc = newReg()
    emit(s"  $newRc = sub i64 $rc, 1")
    emit(s"  store i64 $newRc, i64* $rcPtr")
    val isZero = newReg()
    emit(s"  $isZero = icmp eq i64 $newRc, 0")
    val freeLabel = newLabel("br_free")
    emit(s"  br i1 $isZero, label %$freeLabel, label %$skipLabel")
    emitLabel(freeLabel)
    emit(s"  store i64 -1, i64* $rcPtr") // mark immortal before free to prevent double-free
    deinit.foreach { name =>
      // Call elem deinit with data ptr (= br + 16, past the rc/len header)
      val dataPtr = newReg()
      emit(s"  $dataPtr = getelementptr i8, i8* $br, i64 16")
      emit(s"  call i32 @$name(i8* $dataPtr)")
    }
    emit(s"  call void @free(i8* $br)")
    emit(s"  br label %$skipLabel")
    emitLabel(skipLabel)

  private def emitReleaseRefs(skipSliceRegs: Set[String] = Set.empty): Unit =
    val hasRefs = locals.exists((_, l) => isRef(l.typ))
    if hasRefs then
      // Flush stdout before deinit functions might write to it
      val flushIgnored = newReg()
      emit(s"  $flushIgnored = call i32 @fflush(i8* null)")
    for (name, local) <- locals if isRef(local.typ) do
      if !captureBorrows.contains(name) then
        val hoff = refHeaderOffset(local.typ)
        val ptr = newReg()
        emit(s"  $ptr = load i8*, i8** ${local.reg}")
        emitRefDecr(ptr, hoff, deinitFor(local.typ))
    // Slice backref cleanup: decrement all slice locals except those being returned
    for (name, local) <- locals if isSliceType(local.typ) do
      if !skipSliceRegs.contains(local.reg) && !captureBorrows.contains(name) then
        emitSliceBackrefDecr(local.reg, local.typ)
    // String buffer cleanup: decrement all string locals except those being returned
    for (name, local) <- locals if isStringType(local.typ) do
      if !skipSliceRegs.contains(local.reg) && !captureBorrows.contains(name) then
        emitStringDescrDecr(local.reg)
    // Value-struct string-field cleanup: decrement string fields of struct locals
    for (name, local) <- locals do
      if !captureBorrows.contains(name) then
        local.typ match
          case st: SyslType.StructType if structHasStringFields(st) && !skipSliceRegs.contains(local.reg) =>
            emitStructStringFieldsDecr(local.reg, st)
          case SyslType.ArrayType(elem, _) if structHasStringFields(elem) && !skipSliceRegs.contains(local.reg) =>
            emitValueRC(local.reg, local.typ, incr = false)
          case et: SyslType.EnumType if structHasStringFields(et) && !skipSliceRegs.contains(local.reg) =>
            emitEnumStringFieldsDecr(local.reg, et)
          case _: SyslType.FuncType
              if !skipSliceRegs.contains(local.reg)
              && !funcBorrowParams.contains(name)
              && closureLocalKind.get(name).contains(FuncKind.HeapEnv) =>
            emitClosureDescrDecr(local.reg)
          case _ =>

  /** Clean up locals introduced since a scope snapshot.
    * Decrements slice backrefs then nulls them out to prevent double-decrement
    * when outer scopes or function exit re-process the same local. */
  private def emitScopeCleanup(preLocals: Set[String]): Unit =
    for (name, local) <- locals if !preLocals.contains(name) do
      if captureBorrows.contains(name) then ()
      else
        if isSliceType(local.typ) then
          emitSliceBackrefDecr(local.reg, local.typ)
          // Null out backref so it's not decremented again by outer scope or function exit
          val brGep = newReg()
          emit(s"  $brGep = getelementptr %struct.slice, %struct.slice* ${local.reg}, i32 0, i32 3")
          emit(s"  store i8* null, i8** $brGep")
        if isStringType(local.typ) then
          emitStringDescrDecr(local.reg)
          // Null out the data pointer so the buffer is not decremented again by outer scope or function exit
          val ptrGep = newReg()
          emit(s"  $ptrGep = getelementptr %struct.string, %struct.string* ${local.reg}, i32 0, i32 0")
          emit(s"  store i8* null, i8** $ptrGep")
        local.typ match
          case st: SyslType.StructType if structHasStringFields(st) =>
            emitStructStringFieldsDecr(local.reg, st)
            emitStructStringFieldsNull(local.reg, st)
          case SyslType.ArrayType(elem, _) if structHasStringFields(elem) =>
            emitValueRC(local.reg, local.typ, incr = false)
            emitValueNull(local.reg, local.typ)
          case et: SyslType.EnumType if structHasStringFields(et) =>
            emitEnumStringFieldsDecr(local.reg, et)
            emitEnumStringFieldsNull(local.reg, et)
          case _: SyslType.FuncType
              if !funcBorrowParams.contains(name)
              && closureLocalKind.get(name).contains(FuncKind.HeapEnv) =>
            emitClosureDescrDecr(local.reg)
            // Null out env_ptr so the descr decr is not re-run if outer scope re-processes
            val envGep = newReg()
            emit(s"  $envGep = getelementptr %struct.closure, %struct.closure* ${local.reg}, i32 0, i32 1")
            emit(s"  store i8* null, i8** $envGep")
          case _ =>


  /** Check if an expression is a TNew/TNewArray (already owns the ref, no incr needed). */
  private def isOwnedNew(expr: TExpr): Boolean = expr match
    case _: TNew | _: TNewArray => true
    case _ => false

  /** Convert a TExpr to an LLVM constant initializer for global variables. */
  private def constValue(expr: TExpr, typ: SyslType): String = expr match
    case TIntLit(0, _) if isAggregate(typ) || typ == SyslType.StringType => "zeroinitializer"
    case TIntLit(0, _) if typ.isInstanceOf[SyslType.PtrType] => "null"
    case TIntLit(v, _) => v.toString
    case TUnary("-", TIntLit(v, _), _) => (-v).toString
    case TFloatLit(v, _) =>
      val bits = java.lang.Double.doubleToRawLongBits(v)
      s"0x${bits.toHexString.toUpperCase}"
    case TBoolLit(v, _) => if v then "1" else "0"
    case TStringLit(s, _) =>
      val (label, byteLen) = internString(s)
      val strLen = byteLen - 1
      s"{ i8* ${gepStringDataConst(label, byteLen)}, i32 $strLen }"
    case TArrayLit(elems, SyslType.ArrayType(SyslType.IntType(8) | SyslType.UIntType(8), size)) if elems.forall(_.isInstanceOf[TIntLit]) =>
      // Byte array literal: emit as c"..." constant
      val bytes = elems.map { case TIntLit(v, _) => (v & 0xff).toByte }
      val escaped = bytes.map(b => f"\\${b & 0xff}%02X").mkString
      s"""c"$escaped""""
    case TArrayLit(elems, at @ SyslType.ArrayType(elemType, _)) =>
      // General array literal with constant elements
      val elt = llvmType(elemType)
      val vals = elems.map(e => s"$elt ${constValue(e, elemType)}")
      s"[${vals.mkString(", ")}]"
    case _ =>
      typ match
        case SyslType.StringType => "{ i8* null, i32 0 }"
        case _ if isAggregate(typ) => "zeroinitializer"
        case _: SyslType.PtrType => "null"
        case _ => "0"

  private def emit(line: String): Unit =
    activeOut ++= line
    activeOut += '\n'

  /** Emit a return instruction, handling void vs value returns. */
  private def emitRet(retType: String, value: String = "0"): Unit =
    if retType == "void" then emit("  ret void")
    else emit(s"  ret $retType $value")

  /** Emit a basic block label and track it as the current block. */
  private def emitLabel(label: String): Unit =
    emit(s"$label:")
    currentBlock = label
