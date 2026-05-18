package io.github.edadma.trisc

import scala.collection.mutable

class SyslSVMCodegen extends SyslSVMCodegenStatements, SyslSVMCodegenExpressions:
  val out = new StringBuilder
  var labelCounter = 0
  var modulePrefix = ""
  val stringLiterals = new mutable.ListBuffer[(String, String)]

  // Local variable tracking — maps name → local index
  case class LocalInfo(index: Int, typ: SyslType)
  var locals: mutable.LinkedHashMap[String, LocalInfo] = null
  var nextLocalIndex: Int = 0
  // Names of scalar locals whose address is taken at some point in the body.
  // These are stored on the memory stack instead of in SVM local slots so
  // that &local and writes-through-pointer observe the same storage.
  var addressedLocals: mutable.HashSet[String] = null

  // Globals
  val globals = new mutable.LinkedHashMap[String, SyslType]
  val globalConstants = new mutable.LinkedHashMap[String, Long]

  // Canonical struct types (name -> field-populated StructType). Placeholders
  // (StructType(_, Nil)) can leak into expression types; this map resolves them.
  val structTypes = new mutable.HashMap[String, SyslType.StructType]

  // Interface itables encountered during codegen. Key = itable symbol name,
  // value = (iface type, concrete struct name). Emitted in rodata at EOF.
  val itables = new mutable.LinkedHashMap[String, (SyslType.InterfaceType, String)]

  // Set of function names defined in this module (for method name resolution).
  val definedFuncNames = new mutable.HashSet[String]
  def canonicalStruct(st: SyslType.StructType): SyslType.StructType =
    if st.fields.isEmpty then structTypes.getOrElse(st.name, st) else st

  /** Extract the canonical StructType from any expression's type (handles
    * NamedType / RefType / PtrType wrappers and empty placeholder structs). */
  def structOf(t: SyslType): SyslType.StructType = t.underlying match
    case s: SyslType.StructType => canonicalStruct(s)
    case SyslType.RefType(s) => s.underlying match
      case ss: SyslType.StructType => canonicalStruct(ss)
      case _ => sys.error(s"not a struct type: $t")
    case SyslType.PtrType(s) => s.underlying match
      case ss: SyslType.StructType => canonicalStruct(ss)
      case _ => sys.error(s"not a struct type: $t")
    case _ => sys.error(s"not a struct type: $t")

  // Loop labels for break/continue
  val breakLabels = new mutable.Stack[String]
  val continueLabels = new mutable.Stack[String]
  // User-supplied loop labels (None for unlabeled loops). Parallel to break/continue stacks.
  val loopNameStack = new mutable.Stack[Option[String]]

  /** Find stack index of loop matching `label` (0 = innermost). None → innermost. */
  def resolveLoopIdx(label: Option[String]): Int = label match
    case None => 0
    case Some(name) =>
      val idx = loopNameStack.indexWhere(_.contains(name))
      if idx < 0 then throw new RuntimeException(s"SVM: no enclosing loop with label '$name'")
      idx

  // Per-defer-site state. Each lexical `defer S` in a fn body owns one i64
  // counter local; the counter is bumped at the defer-statement site and the
  // body is replayed at every fn-exit path inside `while counter > 0`. This
  // gives correct dynamic semantics — skipped branches see counter=0 (no
  // fire), loop iterations bump the counter to N (fires N times) — without
  // requiring a runtime defer queue.
  val deferSiteSlot = new mutable.LinkedHashMap[TStmt, Int]
  val deferBodies = new mutable.ArrayBuffer[TStmt]

  // Current function
  var currentFunction: TFunDecl = null
  var needsSpExtern: Boolean = false
  var needsStrConcat: Boolean = false
  var needsStrEq: Boolean = false
  var needsStrCmp: Boolean = false
  var needsNewSlice: Boolean = false
  var needsStrFromI64: Boolean = false
  var needsStrFromBool: Boolean = false
  var needsStrFmtI64: Boolean = false
  var needsStrFmtStr: Boolean = false
  var needsStrFromF64: Boolean = false

  // Map: function name → parameter types (for arg-coercion at call sites).
  val funcParamTypes = new mutable.HashMap[String, List[SyslType]]

  // Map: canonical struct name → user-defined deinit function name. Populated
  // by scanning all TFunDecls whose name ends in "_deinit". When a `&T` ref's
  // refcount drops to zero, emitRefDecr calls this fn (if present) with the
  // struct address.
  val deinitFunctions = new mutable.HashMap[String, String]

  // Per-function list of &T local indexes paired with their struct types.
  // Populated as `var v: &T = ...` / `val v: &T = ...` are lowered. At every
  // return (and the implicit end-of-function), emitFunctionExitRefDecrs
  // decrements each so that scope-exit fires deinit when the refcount hits
  // zero. Cleared at the start of every function.
  val refLocals = new mutable.ListBuffer[(Int, SyslType.StructType)]
  /** True for `&T` where T is a struct — the SVM-deinit machinery handles
    * exactly this shape today. Slice/closure refs use their own paths. */
  def isStructRef(t: SyslType): Option[SyslType.StructType] = t.underlying match
    case SyslType.RefType(inner) => inner.underlying match
      case st: SyslType.StructType => Some(canonicalStruct(st))
      case _ => None
    case _ => None

  /** Mirror of LLVM's isOwnedStruct: true when the expression produces a
    * freshly-owned ref (refcount already 1 from `new`/call). Other expressions
    * (TVarRef, TFieldAccess) are borrowed — caller needs an incr. */
  def isOwnedRefExpr(e: TExpr): Boolean = e match
    case _: TNew | _: TNewArray => true
    case _: TCall | _: TIndirectCall => true
    case _: TIfExpr | _: TMatchExpr => true
    case _ => false

  // Closures: hoisted bodies generated alongside regular functions. The hoisted
  // function's first param is a hidden env_ptr (stored in local 0).
  var closureCounter = 0
  val pendingClosures = new mutable.ListBuffer[(String, TClosure)]
  // While compiling a hoisted closure body: capture name → (env offset, type).
  // TVarRef checks this first.
  var closureCaptures: Map[String, (Long, SyslType)] = Map.empty
  // Per-function shims: ignore env_ptr and forward to plain function.
  val emittedShims = new mutable.HashSet[String]
  val pendingShims = new mutable.ListBuffer[(String, String, List[SyslType], SyslType)]
  // (shimName, targetName, paramTypes, returnType)
  def shimNameFor(target: String): String = s"__shim__$target"

  def emit(s: String): Unit = out ++= s + "\n"
  def newLabel(prefix: String): String =
    labelCounter += 1
    if modulePrefix.nonEmpty then s".${prefix}_${modulePrefix}_$labelCounter"
    else s".${prefix}_$labelCounter"

  // Pre-count locals needed for a function body
  def countLocals(body: TFunBody): Int =
    var count = 0
    val seen = new mutable.HashSet[String]
    def scanStmts(stmts: List[TStmt]): Unit = stmts.foreach(scanStmt)
    def scanExpr(e: TExpr): Unit = e match
      case TMatchExpr(scr, arms, default, _) =>
        count += 1 // scrutinee slot
        scanExpr(scr)
        for arm <- arms do
          for pat <- arm.patterns do
            pat match
              case TDestructurePattern(_, bindings, _, _) => count += bindings.count(_.isDefined)
              case TVariantPattern(_, _, bindings, _, _) => count += bindings.count(_.isDefined)
              case TValuePattern(v) => scanExpr(v)
              case TRangePattern(lo, hi) => scanExpr(lo); scanExpr(hi)
              case TBindPattern(_, _) => count += 1
              case _ =>
          arm.guard.foreach(scanExpr)
          arm.body.foreach(scanStmt)
        default.foreach(_.foreach(scanStmt))
      case TIfExpr(cond, thenBody, elseBody, _) =>
        scanExpr(cond)
        thenBody.foreach(scanStmt)
        elseBody.foreach(_.foreach(scanStmt))
      case TSliceExpr(arr, lo, hi, _) =>
        // TSliceExpr allocates 5 anonymous slots (base, len, lo, hi, struct)
        count += 5
        scanExpr(arr); lo.foreach(scanExpr); hi.foreach(scanExpr)
      case TNewArray(_, sz) => scanExpr(sz)
      case TNew(_, args) => args.foreach(scanExpr)
      case TNewEnum(_, _, args) => args.foreach(scanExpr)
      case TAppend(sl, el, _) =>
        count += 6 // slice, oldPtr, oldLen, new, dst, rem
        scanExpr(sl); scanExpr(el)
      case TInterfaceBox(inner, _, _) => count += 2; scanExpr(inner)
      case TInterfaceDispatch(v, _, args, _) => count += 1; scanExpr(v); args.foreach(scanExpr)
      case TBinary(l, _, r, _) => scanExpr(l); scanExpr(r)
      case TUnary(_, o, _) => scanExpr(o)
      case TCast(inner, _) => scanExpr(inner)
      case TStringFromSlice(s, _) => count += 6; scanExpr(s)
      case TStringFromPtr(p, l, _) => count += 5; scanExpr(p); scanExpr(l)
      case TCall(_, args, _) => args.foreach(scanExpr)
      case TTempAddr(e, _) => scanExpr(e)
      case TIndirectCall(c, args, _) =>
        // The FuncType branch of TIndirectCall's codegen allocates one
        // anonymous local (descrIdx) to hold the 16-byte closure-descriptor
        // address across arg evaluation. countLocals must reserve a slot,
        // or nested calls (g(f(x))) trip an out-of-bounds local access at
        // runtime when the second TIndirectCall's local_set lands past the
        // declared frame size.
        if c.typ.isInstanceOf[SyslType.FuncType] then count += 1
        scanExpr(c); args.foreach(scanExpr)
      case _: TClosure =>
        // genClosureExpr allocates one anonymous local (envIdx) per
        // construction site to hold the env pointer across descriptor
        // wiring. The slot is never released, so two closure literals in
        // the same outer body each need their own — without this
        // reservation the second `local_set` lands past the declared
        // frame and silently overwrites adjacent locals (a TVarStmt's
        // slot, etc.). The closure body itself is hoisted to its own
        // function with its own `frame` directive, so we do not scan
        // it from the outer fn's countLocals.
        count += 1
      case TIndex(a, i, _) => scanExpr(a); scanExpr(i)
      case TFieldAccess(o, _, _) => scanExpr(o)
      case TDeref(p, _) => scanExpr(p)
      case TAddrOfIndex(a, i, _) => scanExpr(a); scanExpr(i)
      case TAddrOfField(o, _, _) => scanExpr(o)
      case TStructConstruct(_, args) => args.foreach(scanExpr)
      case TEnumConstruct(_, _, args) => args.foreach(scanExpr)
      case TArrayLit(elements, _) => elements.foreach(scanExpr)
      case TRangeCheck(inner, _, _, _) => scanExpr(inner)
      case TLen(inner, _) => scanExpr(inner)
      case TCap(inner, _) => scanExpr(inner)
      case _ =>
    def scanStmt(s: TStmt): Unit = s match
      case TVarStmt(name, _, init, _, _) => seen += name; count += 1; scanExpr(init)
      case TAssignStmt(target, value) =>
        scanExpr(value)
        if !seen.contains(target) && !globals.contains(target) then
          seen += target; count += 1
      case TCompoundAssignStmt(_, _, value) => scanExpr(value)
      case TDerefAssignStmt(p, v) => scanExpr(p); scanExpr(v)
      case TIndexAssignStmt(a, i, v) => scanExpr(a); scanExpr(i); scanExpr(v)
      case TFieldAssignStmt(o, _, v) => scanExpr(o); scanExpr(v)
      case TFieldCompoundAssignStmt(o, _, _, v) => scanExpr(o); scanExpr(v)
      case TWhileStmt(cond, body, _) => scanExpr(cond); scanStmts(body)
      case TForStmt(init, cond, update, body, _) =>
        scanStmt(init); scanExpr(cond); scanStmt(update); scanStmts(body)
      case TDoWhileStmt(cond, body, _) => scanExpr(cond); scanStmts(body)
      case TLoopStmt(body, _) => scanStmts(body)
      case TIfExpr(cond, thenBody, elseBody, _) =>
        // if-as-statement: scan bodies directly (no scanExpr — would double-count)
        scanExpr(cond); scanStmts(thenBody); elseBody.foreach(scanStmts)
      case TExprStmt(e) => scanExpr(e)
      case TDestructureStmt(names, _, init) => count += names.count(_ != "_"); scanExpr(init)
      case TDestructureAssignStmt(names, _, init) =>
        // Conservative: each name may be new or existing. Over-count is harmless.
        count += names.count(_ != "_"); scanExpr(init)
      case TReturnStmt(Some(e)) => scanExpr(e)
      case TMultiStmt(children) => children.foreach(scanStmt)
      case TContractCheck(_, e, _) => scanExpr(e)
      case TDeferStmt(b) =>
        // One i64 counter slot per textual defer-site, plus whatever locals
        // the body itself needs (the body is replayed at fn-exit, so its
        // local-allocating constructs run in the per-site cleanup loop).
        count += 1
        scanStmt(b)
      case _ =>
    body match
      case TExprBody(e) => scanExpr(e)
      case TBlockBody(stmts) => scanStmts(stmts)
    count

  // Determine smallest push instruction for an integer
  def emitPushInt(n: Long): Unit =
    n match
      case 0 => emit("  push_0")
      case 1 => emit("  push_1")
      case 2 => emit("  push_2")
      case -1 => emit("  push_m1")
      case v if v >= -128 && v <= 127 => emit(s"  push_i8 $v")
      case v if v >= 0 && v <= 255 => emit(s"  push_u8 $v")
      case v if v >= -32768 && v <= 32767 => emit(s"  push_i16 $v")
      case v if v >= -2147483648L && v <= 2147483647L => emit(s"  push_i32 $v")
      case Long.MinValue =>
        // -9223372036854775808 can't be parsed as unary-minus literal; emit via hex.
        emit("  push_i64 0x8000000000000000")
      case v => emit(s"  push_i64 $v")

  def isUnsigned(t: SyslType): Boolean = t.isInstanceOf[SyslType.UIntType]
  def isFloat(t: SyslType): Boolean = t.isFloat

  /** True if this type needs memory allocation (can't fit in a single 64-bit local slot). */
  def needsMemAlloc(t: SyslType): Boolean = t match
    case _: SyslType.ArrayType => true
    case _: SyslType.StructType => true
    case _: SyslType.EnumType => true
    case _: SyslType.FuncType => true     // 16-byte {func_ptr, env_ptr} closure descriptor
    case _ => false

  /** Wrap a scalar on TOS into an EnumType-shaped buffer on the memory stack.
   *
   *  Background: a function declared `f() -> SimpleEnum = SimpleEnum.Variant`
   *  has its body lowered to `TIntLit(variantOrdinal, I32)`. Without this
   *  wrap, the return site emits a bare scalar that the caller dereferences
   *  as an enum address — garbage. Mirror of `coerceScalarToEnumReturn` in
   *  the LLVM backend (fixed 2026-05-16 at sysl@99c79bfcb).
   *
   *  Stack in : ( ..., scalar )
   *  Stack out: ( ..., enum-buf-addr )
   */
  def coerceScalarToEnumReturn(et: SyslType.EnumType): Unit =
    val size = et.sizeOf
    emitMemAlloc(size)                                       // ( scalar, addr )
    val aligned = ((size + 7) / 8 * 8).toInt
    for i <- 0 until aligned by 8 do
      emit("  dup")                                          // ( scalar, addr, addr )
      if i > 0 then { emitPushInt(i); emit("  add") }
      emit("  push_0")
      emit("  swap")
      emit("  store64")                                      // ( scalar, addr )
    emit("  swap")                                           // ( addr, scalar )
    emit("  over")                                           // ( addr, scalar, addr )
    emit("  store32")                                        // ( addr )

  /** If the current function's declared return type is an `EnumType` but the
   *  expression being returned was lowered to a scalar, wrap it. No-op otherwise.
   */
  def maybeCoerceReturnToEnum(exprTyp: SyslType): Unit =
    if currentFunction != null then
      (currentFunction.returnType, exprTyp) match
        case (et: SyslType.EnumType, t) if !needsMemAlloc(t) && t != SyslType.StringType && !t.isInstanceOf[SyslType.SliceType] =>
          coerceScalarToEnumReturn(et)
        case _ => ()

  /** Emit code to allocate `size` bytes on the memory stack. Leaves address on data stack. */
  def emitMemAlloc(size: Long): Unit =
    // __sp -= size (aligned to 8); push __sp
    emit("  push_i64 __sp")
    emit("  dup")
    emit("  load64")          // ( &__sp old_sp )
    emitPushInt((size + 7) & ~7) // align to 8
    emit("  sub")             // ( &__sp new_sp )
    emit("  dup")             // ( &__sp new_sp new_sp )
    emit("  rot")             // ( new_sp new_sp &__sp )
    emit("  store64")         // write new_sp to __sp; ( new_sp ) remains
    needsSpExtern = true

  /** Allocate a fresh `len` bytes (top-of-stack i64) on the memory stack,
    * 8-aligned. Leaves the base address on TOS. Used when the size isn't
    * known at codegen time (e.g. `string(ptr, len)` byte-copy). */
  def emitMemAllocDyn(): Unit =
    // align len to 8: aligned = (len + 7) & ~7
    emitPushInt(7)
    emit("  add")                // ( aligned_plus_partial )
    emit("  push_i64 -8")
    emit("  and")                // ( aligned )
    emit("  push_i64 __sp")      // ( aligned &__sp )
    emit("  dup")                // ( aligned &__sp &__sp )
    emit("  load64")             // ( aligned &__sp old_sp )
    emit("  rot")                // ( &__sp old_sp aligned )
    emit("  sub")                // ( &__sp new_sp )
    emit("  dup")                // ( &__sp new_sp new_sp )
    emit("  rot")                // ( new_sp new_sp &__sp )
    emit("  store64")            // write new_sp to __sp; ( new_sp )
    needsSpExtern = true

  /** Allocate a fresh buffer holding `lenLocal` bytes copied from `srcLocal`,
    * and return the buffer's local index. Used by string-from-bytes
    * constructors to give the new string an independent backing store. */
  def emitDynByteAllocAndCopy(srcLocal: Int, lenLocal: Int): Int =
    emit(s"  local_get $lenLocal")
    emitMemAllocDyn()
    val bufIdx = nextLocalIndex; nextLocalIndex += 1
    emit(s"  local_set $bufIdx")
    // byte-copy loop: for i in 0..<len: buf[i] = src[i]
    val iIdx = nextLocalIndex; nextLocalIndex += 1
    emit("  push_0")
    emit(s"  local_set $iIdx")
    val loopLbl = newLabel("strcpy_loop")
    val doneLbl = newLabel("strcpy_done")
    emit(s"$loopLbl:")
    emit(s"  local_get $iIdx")
    emit(s"  local_get $lenLocal")
    emit("  ltu")
    emit(s"  jumpz $doneLbl")
    emit(s"  local_get $srcLocal")
    emit(s"  local_get $iIdx")
    emit("  add")
    emit("  load8")
    emit(s"  local_get $bufIdx")
    emit(s"  local_get $iIdx")
    emit("  add")
    emit("  store8")
    emit(s"  local_get $iIdx")
    emit("  inc")
    emit(s"  local_set $iIdx")
    emit(s"  jump $loopLbl")
    emit(s"$doneLbl:")
    bufIdx

  // ============================================================================
  // Refcount machinery for `&T` references
  // ============================================================================
  // A `&T` allocation has an 8-byte refcount header immediately before the data:
  //
  //     | rc: i64 |    ...struct data...    |
  //     ^         ^
  //     header    data pointer (= what the user code holds)
  //
  // The header is initialized to 1 by emitNewRefAlloc. emitRefIncr increments
  // the rc; emitRefDecr decrements it and, when it hits zero, calls the user-
  // defined deinit (if any) — passing the data pointer as the first arg.
  //
  // SVM's memory stack is one-way (no individual free), so a refcount-zero
  // event still leaves the memory in place. That's a known SVM trade-off; the
  // *observable* deinit semantics (deinit body runs exactly once at rc=0) are
  // preserved, which is what user code can detect.

  /** Allocate (header + size) bytes; init header to refcount=1; leave the
    * DATA pointer (header + 8) on TOS. */
  def emitNewRefAlloc(size: Long): Unit =
    val total = size + 8
    emitMemAlloc(total)         // ( base )           base is the start of the allocation
    emit("  dup")                // ( base, base )
    emit("  push_1")             // ( base, base, 1 )
    emit("  swap")               // ( base, 1, base )
    emit("  store64")            // *base = 1 (refcount); ( base )
    emit("  push_i8 8")
    emit("  add")                // ( data )         data = base + 8

  /** Stack ( ptr ) → ( ptr ). Increment refcount at ptr-8 unless the header
    * holds the immortal sentinel (-1, used by static string literals). */
  def emitRefIncr(): Unit =
    val skipLabel = newLabel("rc_incr_skip")
    val doIncrLabel = newLabel("rc_incr_do")
    emit("  dup")                       // ( ptr, ptr )
    emit("  push_i8 -8")
    emit("  add")                       // ( ptr, ptr-8 )
    emit("  dup")                       // ( ptr, hdr, hdr )
    emit("  load64")                    // ( ptr, hdr, rc )
    emit("  push_m1")
    emit("  neq")                       // ( ptr, hdr, rc != -1 )
    emit(s"  jumpz $skipLabel")         // if rc == -1, skip
    // ( ptr, hdr )
    emit("  dup")                       // ( ptr, hdr, hdr )
    emit("  load64")                    // ( ptr, hdr, rc )
    emit("  inc")                       // ( ptr, hdr, rc+1 )
    emit("  swap")                      // ( ptr, rc+1, hdr )
    emit("  store64")                   // ( ptr )
    emit(s"  jump ${skipLabel}_end")
    emit(s"$skipLabel:")
    emit("  drop")                      // drop hdr, leave ptr
    emit(s"${skipLabel}_end:")

  /** Stack ( ptr ) → ( ). Decrement refcount at ptr-8 unless the header
    * holds the immortal sentinel. When rc drops to zero, call the type's
    * deinit fn (if any) passing ptr. SVM can't release the memory itself,
    * but the deinit's observable side effects fire — that's the user-visible
    * contract. */
  def emitRefDecr(st: SyslType.StructType): Unit =
    val immortal = newLabel("rc_decr_immortal")
    val nonzero  = newLabel("rc_decr_nonzero")
    val endLabel = newLabel("rc_decr_end")
    val deinitName = deinitFunctions.get(canonicalStruct(st).name)
    emit("  dup")                       // ( ptr, ptr )
    emit("  push_i8 -8")
    emit("  add")                       // ( ptr, hdr )
    emit("  dup")                       // ( ptr, hdr, hdr )
    emit("  load64")                    // ( ptr, hdr, rc )
    emit("  push_m1")
    emit("  neq")                       // ( ptr, hdr, rc != -1 )
    emit(s"  jumpz $immortal")          // rc == -1 → immortal path
    // Not immortal: ( ptr, hdr )
    emit("  dup")                       // ( ptr, hdr, hdr )
    emit("  load64")                    // ( ptr, hdr, rc )
    emit("  dec")                       // ( ptr, hdr, new_rc )
    emit("  dup")                       // ( ptr, hdr, new_rc, new_rc )
    emit("  rot")                       // ( ptr, new_rc, new_rc, hdr )
    emit("  store64")                   // store new_rc at hdr → ( ptr, new_rc )
    emit("  eqz")                       // ( ptr, new_rc == 0 )
    emit(s"  jumpz $nonzero")           // non-zero → skip deinit, drop ptr
    // rc == 0: ( ptr ) — call deinit if defined, else just drop ptr.
    deinitName match
      case Some(fn) =>
        emit(s"  call $fn")             // consumes ptr; unit return leaves nothing
      case None =>
        emit("  drop")                  // ( )
    emit(s"  jump $endLabel")
    emit(s"$nonzero:")
    emit("  drop")                      // drop ptr → ( )
    emit(s"  jump $endLabel")
    emit(s"$immortal:")
    emit("  drop")                      // drop hdr → ( ptr )
    emit("  drop")                      // drop ptr → ( )
    emit(s"$endLabel:")

  /** Emit refcount decr for each tracked &T local. Called at every return
    * site and at the implicit end-of-function. */
  def emitFunctionExitRefDecrs(): Unit =
    for (idx, st) <- refLocals do
      emit(s"  local_get $idx")
      emitRefDecr(st)

  // Materialize a fixed [N]T array as a slice struct {ptr, len, cap, backref}
  // on the memory stack. Leaves the struct address on TOS.
  def emitArrayToSlice(arg: TExpr, size: Long): Unit =
    emitMemAlloc(24)                 // allocate slice struct, TOS = sliceAddr
    emit("  dup")                    // [..., sliceAddr, sliceAddr]
    genExpr(arg)                     // [..., sliceAddr, sliceAddr, arrAddr]
    emit("  swap")                   // [..., sliceAddr, arrAddr, sliceAddr]
    emit("  store64")                // write ptr field; [..., sliceAddr]
    emit("  dup")                    // [..., sliceAddr, sliceAddr]
    emitPushInt(8)
    emit("  add")                    // [..., sliceAddr, sliceAddr+8]
    emitPushInt(size)
    emit("  swap")                   // [..., sliceAddr, len, sliceAddr+8]
    emit("  store32")                // write len (i32); [..., sliceAddr]
    emit("  dup")                    // [..., sliceAddr, sliceAddr]
    emitPushInt(12)
    emit("  add")                    // [..., sliceAddr, sliceAddr+12]
    emitPushInt(size)
    emit("  swap")                   // [..., sliceAddr, cap, sliceAddr+12]
    emit("  store32")                // write cap (i32); [..., sliceAddr]
    emit("  dup")                    // [..., sliceAddr, sliceAddr]
    emitPushInt(16)
    emit("  add")                    // [..., sliceAddr, sliceAddr+16]
    emit("  push_0")
    emit("  swap")                   // [..., sliceAddr, 0, sliceAddr+16]
    emit("  store64")                // write backref=0; [..., sliceAddr]

  def allocLocal(name: String, typ: SyslType): Int =
    val idx = nextLocalIndex
    locals(name) = LocalInfo(idx, typ)
    nextLocalIndex += 1
    idx

  def constEval(e: TExpr): Option[Long] = e match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(v, _) => Some(if v then 1 else 0)
    case TUnitLit(_) => Some(0)
    case TVarRef(name, _) => globalConstants.get(name)
    case TUnary("-", operand, _) => constEval(operand).map(-_)
    case TUnary("~", operand, _) => constEval(operand).map(~_)
    case TBinary(left, "+", right, _) => for l <- constEval(left); r <- constEval(right) yield l + r
    case TBinary(left, "-", right, _) => for l <- constEval(left); r <- constEval(right) yield l - r
    case TBinary(left, "*", right, _) => for l <- constEval(left); r <- constEval(right) yield l * r
    case TBinary(left, "/", right, _) => for l <- constEval(left); r <- constEval(right) if r != 0 yield l / r
    case TBinary(left, "%", right, _) => for l <- constEval(left); r <- constEval(right) if r != 0 yield l % r
    case TBinary(left, "|", right, _) => for l <- constEval(left); r <- constEval(right) yield l | r
    case TBinary(left, "&", right, _) => for l <- constEval(left); r <- constEval(right) yield l & r
    case TBinary(left, "^", right, _) => for l <- constEval(left); r <- constEval(right) yield l ^ r
    case TBinary(left, "<<", right, _) => for l <- constEval(left); r <- constEval(right) yield l << r.toInt
    case TBinary(left, ">>", right, _) => for l <- constEval(left); r <- constEval(right) yield l >> r.toInt
    case TCast(inner, _) => constEval(inner)
    case _ => None

  def isZeroInit(typ: SyslType, init: TExpr): Boolean =
    constEval(init).contains(0L) || init.isInstanceOf[TArrayDecl] || init.isInstanceOf[TStructLit]

  // ========================================================================
  // generate — top-level entry point
  // ========================================================================
  def generate(program: TProgram): String =
    out.clear()
    labelCounter = 0
    stringLiterals.clear()
    globals.clear()
    globalConstants.clear()
    structTypes.clear()
    itables.clear()
    definedFuncNames.clear()
    needsSpExtern = false
    needsStrConcat = false
    needsStrEq = false
    needsStrCmp = false
    needsNewSlice = false
    needsStrFromI64 = false
    needsStrFromBool = false
    needsStrFmtI64 = false
    needsStrFmtStr = false
    needsStrFromF64 = false
    deinitFunctions.clear()

    // Register canonical struct types so stale placeholder StructType(_, Nil)
    // values in expression types can be resolved back to their real fields.
    def registerType(t: SyslType): Unit = t match
      case st: SyslType.StructType if st.fields.nonEmpty => structTypes(st.name) = st
      case _ =>
    for decl <- program.decls do decl match
      case TStructDecl(name, fields, _) =>
        structTypes(name) = SyslType.StructType(name, fields)
      case _ =>
    for decl <- program.decls do decl match
      case f: TFunDecl =>
        definedFuncNames += f.name
        f.params.foreach(p => registerType(p.typ))
        registerType(f.returnType)
        funcParamTypes(f.name) = f.params.map(_.typ).toList
        // Register user-defined deinit fn for its struct type. The analyzer
        // lowers `Node.deinit() -> unit` to a TFunDecl whose name ends in
        // "_deinit" and whose first/only param is `*Node` (the implicit self).
        if f.name.endsWith("_deinit") then
          f.params.headOption.flatMap(p => p.typ.underlying match
            case SyslType.PtrType(inner) => inner.underlying match
              case st: SyslType.StructType => Some(canonicalStruct(st).name)
              case _ => None
            case _ => None
          ).foreach { structName => deinitFunctions(structName) = f.name }
      case _ =>

    modulePrefix = program.decls.collectFirst { case TModuleDecl(path) => path.mkString("_") }.getOrElse("")

    // Emit entry + globals from module metadata
    val meta = ModuleMeta.fromProgram(program)
    val hasMain = meta.symbols.exists(s => s.name == "main" && s.typ.isInstanceOf[SymbolMeta.Kind.Func])
    if hasMain then emit("entry main")
    // Deduplicate globals/externs by symbol name so repeated monomorphized
    // generics (e.g. is_err_i64_Error used in two sibling modules' test
    // scopes) don't produce duplicate 'global' directives.
    val asmGlobalLines = meta.toAsmGlobals.linesIterator.toList
    val emittedNames = new mutable.HashSet[String]
    for line <- asmGlobalLines do
      val trimmed = line.trim
      val name =
        if trimmed.startsWith("global ") then trimmed.stripPrefix("global ").takeWhile(c => c != ',' && !c.isWhitespace)
        else if trimmed.startsWith("extern ") then trimmed.stripPrefix("extern ").takeWhile(c => c != ',' && !c.isWhitespace)
        else ""
      if name.nonEmpty && emittedNames.add(name) then out ++= line + "\n"
      else if name.isEmpty then out ++= line + "\n"

    // Collect globals
    val dataGlobals = new mutable.ListBuffer[TDecl]
    val bssGlobals = new mutable.ListBuffer[TDecl]

    for decl <- program.decls do decl match
      case v @ TVarDecl(_, typ, init, _, _, _, _) =>
        globals(v.name) = typ
        constEval(init).foreach(n => globalConstants(v.name) = n)
        if isZeroInit(typ, init) then bssGlobals += v
        else dataGlobals += v
      case _ =>

    // Emit code segment — functions (deduplicated by name so repeated
    // generic monomorphizations across sibling units don't duplicate labels)
    emit("segment code")
    val emittedFuncs = new mutable.HashSet[String]
    for decl <- program.decls do decl match
      case f: TFunDecl if emittedFuncs.add(f.name) => genFunction(f)
      case _ =>

    // Emit hoisted closure bodies (collected during gen of expression-context closures).
    // Each one is a regular function with a hidden env_ptr first param at local 0.
    while pendingClosures.nonEmpty do
      val (name, c) = pendingClosures.remove(0)
      genHoistedClosure(name, c)

    // Emit per-function shims for plain function pointers taken via TFuncRef.
    for (shim, target, paramTypes, retType) <- pendingShims do
      genShim(shim, target, paramTypes, retType)

    // Pre-register string globals so their rodata labels are emitted in the
    // rodata segment before the data segment references them.
    val stringGlobalLabels = new mutable.HashMap[String, String]
    // For arrays of strings: per-element labels, indexed by (arrayName, i).
    val stringArrayElemLabels = new mutable.HashMap[(String, Int), String]
    for decl <- dataGlobals do decl match
      case TVarDecl(name, SyslType.StringType, TStringLit(s, _), _, _, _, _) =>
        labelCounter += 1
        val lbl = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_${labelCounter}__g_$name"
                  else s"__str_${labelCounter}__g_$name"
        stringLiterals += ((lbl, s))
        stringGlobalLabels(name) = lbl
      case TVarDecl(name, SyslType.ArrayType(SyslType.StringType, _), TArrayLit(elements, _), _, _, _, _) =>
        for (e, idx) <- elements.zipWithIndex do e match
          case TStringLit(s, _) =>
            labelCounter += 1
            val lbl = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_${labelCounter}__g_${name}_$idx"
                      else s"__str_${labelCounter}__g_${name}_$idx"
            stringLiterals += ((lbl, s))
            stringArrayElemLabels((name, idx)) = lbl
          case _ =>
      case _ =>

    // Emit rodata segment — string literals + interface itables
    if stringLiterals.nonEmpty || itables.nonEmpty then
      emit("segment rodata")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("ISO-8859-1")
        emit(s"global $label, data, ${bytes.length + 9}")
      for (iname, (iface, _)) <- itables do
        emit(s"global $iname, data, ${iface.methods.length * 8}")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("ISO-8859-1")
        emit(s"  dl -1") // immortal refcount header
        emit(s"$label:")
        for b <- bytes do emit(s"  db ${b & 0xff}")
        emit("  db 0")
      // Each itable: array of function pointers for the interface's methods
      for (iname, (iface, structName)) <- itables do
        emit(s"  align 8")
        emit(s"$iname:")
        for ((mName, _, _, _) <- iface.methods) do
          val shortName = s"${structName}_$mName"
          val fnName =
            if definedFuncNames.contains(shortName) then shortName
            else definedFuncNames.find(_.endsWith(s"__$shortName")).getOrElse(shortName)
          emit(s"  dl $fnName")

    // Emit data segment
    if dataGlobals.nonEmpty then
      emit("segment data")
      for decl <- dataGlobals do decl match
        case TVarDecl(name, typ, _, _, _, _, _) =>
          emit(s"global $name, data, ${typ.sizeOf.max(8)}")
        case _ =>
      for decl <- dataGlobals do decl match
        case TVarDecl(name, typ, init, _, _, _, _) =>
          emit(s"  align 8")
          emit(s"$name:")
          typ match
            case SyslType.StringType =>
              // Static string literal: inline 16-byte {ptr, len}. Label was
              // pre-registered before rodata emission.
              init match
                case TStringLit(s, _) =>
                  val bytes = s.getBytes("ISO-8859-1")
                  emit(s"  dl ${stringGlobalLabels(name)}")
                  emit(s"  dl ${bytes.length}")
                case _ =>
                  emit(s"  dl 0"); emit(s"  dl 0")
            case SyslType.ArrayType(SyslType.StringType, arrSize) =>
              // Array of strings: emit inline {ptr, len} per element.
              init match
                case TArrayLit(elements, _) =>
                  for (e, idx) <- elements.zipWithIndex do e match
                    case TStringLit(s, _) =>
                      val bytes = s.getBytes("ISO-8859-1")
                      emit(s"  dl ${stringArrayElemLabels((name, idx))}")
                      emit(s"  dl ${bytes.length}")
                    case _ =>
                      emit(s"  dl 0"); emit(s"  dl 0")
                  for _ <- elements.length until arrSize.toInt do
                    emit("  dl 0"); emit("  dl 0")
                case _ =>
                  for _ <- 0 until arrSize.toInt do
                    emit("  dl 0"); emit("  dl 0")
            case SyslType.ArrayType(SyslType.UIntType(8) | SyslType.IntType(8) | SyslType.BoolType, arrSize) =>
              // Byte array with string-literal / array-literal init.
              init match
                case TStringLit(s, _) =>
                  val bytes = s.getBytes("ISO-8859-1")
                  for b <- bytes do emit(s"  db ${b & 0xff}")
                  for _ <- bytes.length until arrSize.toInt do emit("  db 0")
                case TArrayLit(elements, _) =>
                  for e <- elements do
                    val n = constEval(e).getOrElse(0L) & 0xff
                    emit(s"  db $n")
                  for _ <- elements.length until arrSize.toInt do emit("  db 0")
                case _ =>
                  for _ <- 0 until arrSize.toInt do emit("  db 0")
            case SyslType.ArrayType(elem, arrSize) =>
              val elemBytes = elem.sizeOf.toInt
              val directive = elemBytes match
                case 1 => "db"
                case 2 => "ds"
                case 4 => "dw"
                case _ => "dl"
              val mask = elemBytes match
                case 1 => 0xffL
                case 2 => 0xffffL
                case 4 => 0xffffffffL
                case _ => -1L
              init match
                case TArrayLit(elements, _) =>
                  for e <- elements do
                    val n = e match
                      case TFloatLit(d, _) if elemBytes == 8 =>
                        java.lang.Double.doubleToLongBits(d)
                      case _ => constEval(e).getOrElse(0L) & mask
                    emit(s"  $directive $n")
                  for _ <- elements.length until arrSize.toInt do
                    emit(s"  $directive 0")
                case _ =>
                  for _ <- 0 until arrSize.toInt do
                    emit(s"  $directive 0")
            case _ =>
              val sizeSlots = (typ.sizeOf.max(8) / 8).toInt
              init match
                case TFloatLit(d, _) =>
                  emit(s"  dl ${java.lang.Double.doubleToLongBits(d)}")
                  for _ <- 1 until sizeSlots do emit("  dl 0")
                case _ =>
                  constEval(init) match
                    case Some(n) =>
                      emit(s"  dl $n")
                      for _ <- 1 until sizeSlots do emit("  dl 0")
                    case None =>
                      for _ <- 0 until sizeSlots do emit("  dl 0")
        case _ =>

    // Emit bss segment
    if bssGlobals.nonEmpty then
      emit("segment bss")
      for decl <- bssGlobals do decl match
        case TVarDecl(name, typ, _, _, _, _, _) =>
          emit(s"global $name, data, ${typ.sizeOf.max(8)}")
        case _ =>
      for decl <- bssGlobals do decl match
        case TVarDecl(name, typ, _, _, _, _, _) =>
          emit(s"  align 8")
          emit(s"$name:")
          val size = typ.sizeOf.max(8)
          emit(s"  rl ${((size + 7) / 8).toInt}")
        case _ =>

    // Emit extern declarations
    val generated = out.toString
    val definedSymbols = program.decls.flatMap {
      case TFunDecl(name, _, _, _, _, _, _, _, _, _) => Some(name)
      case TVarDecl(name, _, _, _, _, _, _) => Some(name)
      case _ => None
    }.toSet
    val metaSymbols = meta.symbols.map(_.name).toSet

    for decl <- program.decls do decl match
      case TExternFuncDecl(name, _, _) if !definedSymbols.contains(name) && !metaSymbols.contains(name) =>
        emit(s"extern $name")
      case TExternVarDecl(name, _) if !definedSymbols.contains(name) && !metaSymbols.contains(name) =>
        emit(s"extern $name")
      case _ =>

    // Emit __sp extern if memory stack was used
    if needsSpExtern && !definedSymbols.contains("__sp") then
      emit("extern __sp")
    if needsStrConcat && !definedSymbols.contains("__svm_str_concat") then
      emit("extern __svm_str_concat")
    if needsStrEq && !definedSymbols.contains("__svm_str_eq") then
      emit("extern __svm_str_eq")
    if needsStrCmp && !definedSymbols.contains("__svm_str_cmp") then
      emit("extern __svm_str_cmp")
    if needsStrFromI64 && !definedSymbols.contains("__svm_str_from_i64") then
      emit("extern __svm_str_from_i64")
    if needsStrFromBool && !definedSymbols.contains("__svm_str_from_bool") then
      emit("extern __svm_str_from_bool")
    if needsStrFmtI64 && !definedSymbols.contains("__svm_str_fmt_i64") then
      emit("extern __svm_str_fmt_i64")
    if needsStrFmtStr && !definedSymbols.contains("__svm_str_fmt_str") then
      emit("extern __svm_str_fmt_str")
    if needsStrFromF64 && !definedSymbols.contains("__svm_str_from_f64") then
      emit("extern __svm_str_from_f64")
    if needsNewSlice && !definedSymbols.contains("__svm_new_slice") then
      emit("extern __svm_new_slice")

    out.toString

  // ========================================================================
  // genFunction
  // ========================================================================
  def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    nextLocalIndex = 0
    deferSiteSlot.clear()
    deferBodies.clear()
    addressedLocals = new mutable.HashSet[String]
    // Pre-scan body for any TAddrOf(name) — those names need to be stored
    // on the memory stack so the pointer and the local refer to the same cell.
    def scanAddrOfE(e: TExpr): Unit = e match
      case TAddrOf(name, _) => addressedLocals += name
      case TBinary(l, _, r, _) => scanAddrOfE(l); scanAddrOfE(r)
      case TUnary(_, o, _) => scanAddrOfE(o)
      case TCall(_, args, _) => args.foreach(scanAddrOfE)
      case TIndirectCall(c, args, _) => scanAddrOfE(c); args.foreach(scanAddrOfE)
      case TCast(i, _) => scanAddrOfE(i)
      case TIndex(a, i, _) => scanAddrOfE(a); scanAddrOfE(i)
      case TDeref(p, _) => scanAddrOfE(p)
      case TAddrOfIndex(a, i, _) => scanAddrOfE(a); scanAddrOfE(i)
      case TAddrOfField(o, _, _) => scanAddrOfE(o)
      case TFieldAccess(o, _, _) => scanAddrOfE(o)
      case TIfExpr(c, tb, eb, _) => scanAddrOfE(c); tb.foreach(scanAddrOfS); eb.foreach(_.foreach(scanAddrOfS))
      case TMatchExpr(s, arms, d, _) =>
        scanAddrOfE(s)
        for a <- arms do
          a.body.foreach(scanAddrOfS)
          a.guard.foreach(scanAddrOfE)
        d.foreach(_.foreach(scanAddrOfS))
      case TStructConstruct(_, args) => args.foreach(scanAddrOfE)
      case TEnumConstruct(_, _, args) => args.foreach(scanAddrOfE)
      case TNew(_, args) => args.foreach(scanAddrOfE)
      case TNewEnum(_, _, args) => args.foreach(scanAddrOfE)
      case TNewArray(_, s) => scanAddrOfE(s)
      case TAppend(s, el, _) => scanAddrOfE(s); scanAddrOfE(el)
      case TSliceExpr(a, lo, hi, _) => scanAddrOfE(a); lo.foreach(scanAddrOfE); hi.foreach(scanAddrOfE)
      case TArrayLit(es, _) => es.foreach(scanAddrOfE)
      case TLen(i, _) => scanAddrOfE(i)
      case TCap(i, _) => scanAddrOfE(i)
      case TTempAddr(i, _) => scanAddrOfE(i)
      case TStringFromSlice(s, _) => scanAddrOfE(s)
      case TStringFromPtr(p, l, _) => scanAddrOfE(p); scanAddrOfE(l)
      case TInterfaceBox(i, _, _) => scanAddrOfE(i)
      case TInterfaceDispatch(v, _, args, _) => scanAddrOfE(v); args.foreach(scanAddrOfE)
      case TRangeCheck(i, _, _, _) => scanAddrOfE(i)
      case TStr(i) => scanAddrOfE(i)
      case _ =>
    def scanAddrOfS(s: TStmt): Unit = s match
      case TVarStmt(_, _, i, _, _) => scanAddrOfE(i)
      case TAssignStmt(_, v) => scanAddrOfE(v)
      case TCompoundAssignStmt(_, _, v) => scanAddrOfE(v)
      case TDerefAssignStmt(p, v) => scanAddrOfE(p); scanAddrOfE(v)
      case TIndexAssignStmt(a, i, v) => scanAddrOfE(a); scanAddrOfE(i); scanAddrOfE(v)
      case TFieldAssignStmt(o, _, v) => scanAddrOfE(o); scanAddrOfE(v)
      case TFieldCompoundAssignStmt(o, _, _, v) => scanAddrOfE(o); scanAddrOfE(v)
      case TWhileStmt(c, b, _) => scanAddrOfE(c); b.foreach(scanAddrOfS)
      case TForStmt(i, c, u, b, _) => scanAddrOfS(i); scanAddrOfE(c); scanAddrOfS(u); b.foreach(scanAddrOfS)
      case TDoWhileStmt(c, b, _) => scanAddrOfE(c); b.foreach(scanAddrOfS)
      case TLoopStmt(b, _) => b.foreach(scanAddrOfS)
      case TIfExpr(c, tb, eb, _) => scanAddrOfE(c); tb.foreach(scanAddrOfS); eb.foreach(_.foreach(scanAddrOfS))
      case TExprStmt(e) => scanAddrOfE(e)
      case TReturnStmt(Some(e)) => scanAddrOfE(e)
      case TDestructureStmt(_, _, i) => scanAddrOfE(i)
      case TDestructureAssignStmt(_, _, i) => scanAddrOfE(i)
      case TMultiStmt(c) => c.foreach(scanAddrOfS)
      case TContractCheck(_, e, _) => scanAddrOfE(e)
      case TDeferStmt(b) => scanAddrOfS(b)
      case _ =>
    fun.body match
      case TExprBody(e) => scanAddrOfE(e)
      case TBlockBody(stmts) => stmts.foreach(scanAddrOfS)

    val nParams = fun.params.length
    val nBodyLocals = countLocals(fun.body)
    val totalLocals = nParams + nBodyLocals

    emit(s"${fun.name}:")
    // Always emit a frame so RET unwinds this function's call frame rather
    // than the caller's. Even 0-local functions need it.
    emit(s"  frame $totalLocals")

    // Reset per-function refcount tracking.
    refLocals.clear()

    // Pop args from stack into locals.
    // Caller pushes args left-to-right, so TOS = last arg pushed.
    // We need to pop in reverse param order.
    for i <- (0 until nParams).reverse do
      locals(fun.params(i).name) = LocalInfo(i, fun.params(i).typ)
    // Pop: TOS is last param (index nParams-1), next is nParams-2, etc.
    for i <- (nParams - 1) to 0 by -1 do
      emit(s"  local_set $i")
    nextLocalIndex = nParams

    // For each &T struct-ref param: incr at entry (caller's share + callee's
    // share, both balanced at exit) and track for scope-exit decr.
    for i <- 0 until nParams do
      val p = fun.params(i)
      isStructRef(p.typ) match
        case Some(st) =>
          emit(s"  local_get $i")
          emitRefIncr()                // incr at ptr-8; leaves ptr on stack
          emit("  drop")
          refLocals += ((i, st))
        case None =>
    // The function whose name is `*_deinit` is itself the deinit fn — its
    // self param shouldn't be tracked or incremented, because (a) the
    // caller already decremented to zero before calling it, and (b)
    // re-incrementing inside the deinit would prevent it from completing.
    if fun.name.endsWith("_deinit") then
      refLocals.clear()

    // For any scalar param whose address is taken, promote it to a memory-
    // stack cell and replace the local's value (raw value) with the cell's
    // address so the addressed-local code paths observe the same storage.
    for i <- 0 until nParams do
      val p = fun.params(i)
      if addressedLocals.contains(p.name) && !needsMemAlloc(p.typ)
         && p.typ != SyslType.StringType && !p.typ.isInstanceOf[SyslType.SliceType] then
        emitMemAlloc(8)
        emit("  dup")         // (cell, cell)
        emit(s"  local_get $i")
        emit("  swap")         // (cell, value, cell)
        emitStore(p.typ)
        emit(s"  local_set $i")

    // Value-struct (and array / data-enum) params are pass-by-value per the
    // language reference. The caller hands us the source's address; we must
    // allocate a fresh defensive local copy so writes inside the body don't
    // leak back to the caller. Skips refs/slices/strings/closures — those
    // carry their own sharing semantics and the caller intends sharing.
    for i <- 0 until nParams do
      val p = fun.params(i)
      p.typ match
        case _: SyslType.StructType | _: SyslType.ArrayType | _: SyslType.EnumType =>
          val size = p.typ.sizeOf
          emitMemAlloc(size)         // ( new )
          emit("  dup")              // ( new, new )
          emit(s"  local_get $i")    // ( new, new, src )
          emit("  swap")             // ( new, src, new )  -- (src, dest) on top
          emitStore(p.typ)           // emitStore-aggregate consumes BOTH src and dest → ( new )
          emit(s"  local_set $i")    // local[i] = new
        case _ => // scalars/refs/slices/strings/closures: keep caller's value/share

    fun.body match
      case TExprBody(expr) =>
        genExpr(expr)
        maybeCoerceReturnToEnum(expr.typ)
        emitDefers()
        emitFunctionExitRefDecrs()
        emit("  ret")
      case TBlockBody(stmts) =>
        if stmts.isEmpty then
          emitDefers()
          emitFunctionExitRefDecrs()
          emit("  ret")
        else if fun.returnType != SyslType.UnitType then
          genStmtsAsExpr(stmts)
          emitDefers()
          emitFunctionExitRefDecrs()
          emit("  ret")
        else
          genStmts(stmts)
          if !stmts.lastOption.exists(_.isInstanceOf[TReturnStmt]) then
            emitDefers()
            emitFunctionExitRefDecrs()
            emit("  ret")

  // ========================================================================
  // Closure layout helpers
  // ========================================================================
  /** Compute env layout: list of (name, offset, type) and total size. */
  def envLayout(captures: List[(String, SyslType)]): (List[(String, Long, SyslType)], Long) =
    var off: Long = 0L
    val items = captures.map { (n, t) =>
      val align = t.alignOf.max(1)
      off = (off + align - 1) / align * align
      val item = (n, off, t)
      off += t.sizeOf
      item
    }
    (items, off)

  /** Emit code at the construction site to build a 16-byte closure descriptor
    * on the memory stack and leave its address on TOS. */
  def genClosureExpr(c: TClosure): Unit =
    closureCounter += 1
    val cName = s"__closure_${closureCounter}"
    pendingClosures += ((cName, c))
    val (layout, envSize) = envLayout(c.captures)
    // Allocate env (or use null when no captures).
    val envIdx = nextLocalIndex
    nextLocalIndex += 1
    if c.captures.isEmpty then
      emit("  push_0")
      emit(s"  local_set $envIdx")
    else
      emitMemAlloc(envSize)
      emit(s"  local_set $envIdx")
      // Store each capture into env at its offset. The self-name slot (if any) is
      // skipped here and wired below after the descriptor exists — its value is
      // the descriptor address itself, which doesn't exist yet at this point.
      for (capName, off, capTyp) <- layout if !c.selfName.contains(capName) do
        emit(s"  local_get $envIdx")
        if off > 0 then { emitPushInt(off); emit("  add") }
        // Read the captured value from caller's local/global, then store into env.
        genExpr(TVarRef(capName, capTyp))
        // For aggregates the genExpr returned an address; we want to copy bytes.
        // For scalars we want to store the loaded value.
        emit("  swap")
        emitStore(capTyp)
    // Allocate descriptor (16 bytes).
    emitMemAlloc(16)
    emit("  dup")
    emit(s"  push_i64 $cName")
    emit("  swap")
    emit("  store64")          // descr[0] = func_ptr
    emit("  dup")
    emitPushInt(8)
    emit("  add")
    emit(s"  local_get $envIdx")
    emit("  swap")
    emit("  store64")          // descr[8] = env_ptr
    // Inner-def self-recursion: backfill env[self_offset] with a copy of the 16-byte
    // descriptor we just built. Aggregate captures are stored inline by value (the
    // env slot for a FuncType holds the full {func_ptr, env_ptr} struct, not a
    // pointer to it), so the closure body's standard TVarRef→env-lookup path
    // returns the address of env+self_offset and treats it as the descriptor.
    for selfName <- c.selfName do
      val selfOffOpt = layout.find(_._1 == selfName).map(_._2)
      for selfOff <- selfOffOpt do
        // Stack: [d]   d = freshly-built descriptor address
        emit("  dup")                                    // [d, d]   keep d for return
        emit(s"  local_get $envIdx")                     // [d, d, env]
        if selfOff > 0 then { emitPushInt(selfOff); emit("  add") } // [d, d, env+off]
        // emitStore for FuncType copies 16 bytes from src to dest and pops both.
        emitStore(SyslType.FuncType(Nil, SyslType.UnitType))
        // Stack after emitStore: [d]

  /** Emit a hoisted closure body as a regular function. The first param is a
    * hidden env_ptr (local 0); explicit params follow. Captures are accessed
    * via env_ptr+offset using `closureCaptures`. */
  def genHoistedClosure(name: String, c: TClosure): Unit =
    emit(s"global $name, func")
    val (layout, _) = envLayout(c.captures)
    val captureMap = layout.map { case (n, off, t) => (n, (off, t)) }.toMap

    // Build a synthetic TFunDecl-like context. We'll call genFunction-style
    // logic but with closureCaptures populated.
    val savedCaptures = closureCaptures
    val savedLocals = locals
    val savedNextIdx = nextLocalIndex
    val savedAddressed = addressedLocals
    val savedDeferSiteSlot = deferSiteSlot.toList
    val savedDeferBodies = deferBodies.toList
    val savedFunc = currentFunction

    closureCaptures = captureMap
    locals = new mutable.LinkedHashMap
    nextLocalIndex = 0
    addressedLocals = new mutable.HashSet[String]
    deferSiteSlot.clear()
    deferBodies.clear()

    // Pre-scan body for &x on locals (skip captures — they're not addressable
    // through this scan since they live in env).
    def scanAddrOfE(e: TExpr): Unit = e match
      case TAddrOf(n, _) if !captureMap.contains(n) => addressedLocals += n
      case TBinary(l, _, r, _) => scanAddrOfE(l); scanAddrOfE(r)
      case TUnary(_, o, _) => scanAddrOfE(o)
      case TCall(_, args, _) => args.foreach(scanAddrOfE)
      case TIndirectCall(cc, args, _) => scanAddrOfE(cc); args.foreach(scanAddrOfE)
      case TCast(i, _) => scanAddrOfE(i)
      case TIndex(a, i, _) => scanAddrOfE(a); scanAddrOfE(i)
      case TDeref(p, _) => scanAddrOfE(p)
      case TFieldAccess(o, _, _) => scanAddrOfE(o)
      case TIfExpr(cc, tb, eb, _) => scanAddrOfE(cc); tb.foreach(scanAddrOfS); eb.foreach(_.foreach(scanAddrOfS))
      case _ =>
    def scanAddrOfS(s: TStmt): Unit = s match
      case TVarStmt(_, _, i, _, _) => scanAddrOfE(i)
      case TAssignStmt(_, v) => scanAddrOfE(v)
      case TExprStmt(e) => scanAddrOfE(e)
      case TReturnStmt(Some(e)) => scanAddrOfE(e)
      case TWhileStmt(cc, b, _) => scanAddrOfE(cc); b.foreach(scanAddrOfS)
      case TForStmt(i, cc, u, b, _) => scanAddrOfS(i); scanAddrOfE(cc); scanAddrOfS(u); b.foreach(scanAddrOfS)
      case _ =>
    c.body match
      case TExprBody(e) => scanAddrOfE(e)
      case TBlockBody(stmts) => stmts.foreach(scanAddrOfS)

    // Reserve local 0 for env_ptr. Explicit params start at local 1.
    val nParams = c.params.length
    val nBodyLocals = countLocals(c.body)
    val totalLocals = 1 + nParams + nBodyLocals

    emit(s"$name:")
    emit(s"  frame $totalLocals")
    // Pop args+env: stack is (env, p0, p1, ..., pN-1) with pN-1 on top.
    // Reverse-pop: pN-1 → local nParams, ..., p0 → local 1, env → local 0.
    for i <- (nParams - 1) to 0 by -1 do
      locals(c.params(i).name) = LocalInfo(i + 1, c.params(i).typ)
      emit(s"  local_set ${i + 1}")
    emit(s"  local_set 0")          // env_ptr → local 0
    nextLocalIndex = 1 + nParams

    // Body
    c.body match
      case TExprBody(e) =>
        genExpr(e)
        emitDefers()
        emit("  ret")
      case TBlockBody(stmts) =>
        if stmts.isEmpty then
          emitDefers()
          emit("  ret")
        else if c.returnType != SyslType.UnitType then
          genStmtsAsExpr(stmts)
          emitDefers()
          emit("  ret")
        else
          genStmts(stmts)
          if !stmts.lastOption.exists(_.isInstanceOf[TReturnStmt]) then
            emitDefers()
            emit("  ret")

    // Restore
    closureCaptures = savedCaptures
    locals = savedLocals
    nextLocalIndex = savedNextIdx
    addressedLocals = savedAddressed
    deferSiteSlot.clear()
    for (b, s) <- savedDeferSiteSlot do deferSiteSlot(b) = s
    deferBodies.clear()
    deferBodies ++= savedDeferBodies
    currentFunction = savedFunc

  /** Fallback for TStr on types we can't render: emit "???" string. */
  def emitStrPlaceholder(inner: TExpr): Unit =
    labelCounter += 1
    val lbl = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_${labelCounter}__qqq"
              else s"__str_${labelCounter}__qqq"
    stringLiterals += ((lbl, "???"))
    genExpr(inner)
    inner.typ.underlying match
      case _: SyslType.StructType | _: SyslType.EnumType
         | _: SyslType.SliceType | _: SyslType.ArrayType
         | SyslType.StringType => () // no scalar value on stack to drop
      case _ => emit("  drop")
    emitMemAlloc(16)
    emit("  dup")
    emit(s"  push_i64 $lbl")
    emit("  swap")
    emit("  store64")
    emit("  dup")
    emitPushInt(8)
    emit("  add")
    emitPushInt(3)
    emit("  swap")
    emit("  store64")

  /** Emit a per-function shim: takes (env_ptr, ...args), tail-calls target
    * with (...args). Used so plain function pointers (TFuncRef) work uniformly
    * with the closure indirect-call convention. */
  def genShim(shim: String, target: String, paramTypes: List[SyslType], retType: SyslType): Unit =
    val nParams = paramTypes.length
    val totalLocals = 1 + nParams
    emit(s"global $shim, func")
    emit(s"$shim:")
    emit(s"  frame $totalLocals")
    // Stack at entry: (env, arg1, ..., argN), argN on top.
    for i <- (nParams - 1) to 0 by -1 do
      emit(s"  local_set ${i + 1}")
    emit(s"  local_set 0")        // env (discarded)
    // Push args back in order, then tail-call.
    for i <- 0 until nParams do
      emit(s"  local_get ${i + 1}")
    emit(s"  call $target")
    emit(s"  ret")

  // ========================================================================
  // Helpers
  // ========================================================================

  def emitBinaryOp(op: String, operandType: SyslType): Unit =
    val f = isFloat(operandType)
    val u = isUnsigned(operandType)
    op match
      case "+" => emit(if f then "  fadd" else "  add"); truncateForNarrow(operandType)
      case "-" => emit(if f then "  fsub" else "  sub"); truncateForNarrow(operandType)
      case "*" => emit(if f then "  fmul" else "  mul"); truncateForNarrow(operandType)
      case "/" => emit(if f then "  fdiv" else if u then "  divu" else "  div")
      case "%" => emit(if f then "  fmod" else if u then "  modu" else "  mod")
      case "==" => emit(if f then "  feq" else "  eq")
      case "!=" => emit(if f then "  fneq" else "  neq")
      case "<" => emit(if f then "  flt" else if u then "  ltu" else "  lt")
      case ">" => emit(if f then "  fgt" else if u then "  gtu" else "  gt")
      case "<=" => emit(if f then "  fle" else if u then "  leu" else "  le")
      case ">=" => emit(if f then "  fge" else if u then "  geu" else "  ge")
      case "&" => emit("  and")
      case "|" => emit("  or")
      case "^" => emit("  xor")
      case "<<" => emit("  shl"); truncateForNarrow(operandType)
      case ">>" => emit(if u then "  shr" else "  sar")
      case _ => sys.error(s"unsupported binary operator: $op")

  /** Mask / sign-extend the 64-bit TOS back to the narrow-int range so
    * overflow in (u)i{8,16,32} arithmetic matches the source-level type. */
  def truncateForNarrow(t: SyslType): Unit = t.underlying match
    case SyslType.UIntType(8)  => emitPushInt(0xff); emit("  and")
    case SyslType.UIntType(16) => emitPushInt(0xffff); emit("  and")
    case SyslType.UIntType(32) => emit("  push_i64 4294967295"); emit("  and")
    case SyslType.IntType(8)   => emitPushInt(56); emit("  shl"); emitPushInt(56); emit("  sar")
    case SyslType.IntType(16)  => emitPushInt(48); emit("  shl"); emitPushInt(48); emit("  sar")
    case SyslType.IntType(32)  => emitPushInt(32); emit("  shl"); emitPushInt(32); emit("  sar")
    case _ =>

  def emitStore(typ: SyslType): Unit = typ match
    case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType => emit("  store8")
    case SyslType.IntType(16) | SyslType.UIntType(16) => emit("  store16")
    case SyslType.IntType(32) | SyslType.UIntType(32) => emit("  store32")
    case _: SyslType.StructType | _: SyslType.EnumType | SyslType.StringType | _: SyslType.SliceType
       | _: SyslType.FuncType | _: SyslType.ArrayType =>
      // Inline aggregate: stack has ( src_addr dest_addr ). Copy EXACTLY sizeOf
      // bytes — never round up. Rounding up to 8 would overwrite the slot after
      // the dst element (e.g. for 12-byte structs in a tight slice, clobbering
      // element[i+1]'s first field).
      val total = typ.sizeOf.toInt
      var off = 0
      while off + 8 <= total do
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  load64")
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  store64")
        off += 8
      while off + 4 <= total do
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  load32")
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  store32")
        off += 4
      while off + 2 <= total do
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  load16")
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  store16")
        off += 2
      while off < total do
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  load8")
        emit("  over")
        if off > 0 then { emitPushInt(off); emit("  add") }
        emit("  store8")
        off += 1
      emit("  drop")
      emit("  drop")
    case _ => emit("  store64")

  def emitLoad(typ: SyslType): Unit = typ.underlying match
    case SyslType.IntType(8) => emit("  load8s")
    case SyslType.UIntType(8) | SyslType.BoolType => emit("  load8")
    case SyslType.IntType(16) => emit("  load16s")
    case SyslType.UIntType(16) => emit("  load16")
    case SyslType.IntType(32) => emit("  load32s")
    case SyslType.UIntType(32) => emit("  load32")
    // Inline aggregates are address-represented — the 'load' is a no-op,
    // leaving the field/slot address on the stack.
    case _: SyslType.StructType | _: SyslType.EnumType | SyslType.StringType
       | _: SyslType.SliceType | _: SyslType.ArrayType | _: SyslType.FuncType =>
      ()
    case _ => emit("  load64")

  def emitCast(from: SyslType, to: SyslType): Unit =
    import SyslType.*
    val srcFloat = from.isFloat
    val tgtFloat = to.isFloat
    if srcFloat && !tgtFloat then emit("  f2i")
    else if !srcFloat && tgtFloat then emit("  i2f")
    else (from.underlying, to.underlying) match
      // String / slice to raw pointer: deref the struct to get the data ptr.
      // (Arrays are already data-addressed, so array→ptr is a no-op.)
      case (StringType | _: SliceType, _: PtrType) =>
        emit("  load64")
      // Enum → integral: the enum value on the stack is an address pointing at
      // the enum buffer; the tag (variant ordinal) lives as i32 at offset 0.
      // Synthesised by the analyzer at `Type::Image(c)` / `Type::Pos(c)` /
      // pattern-match scrutinee coercions whenever the static type is
      // EnumType but the consumer expects i32. Without this case the address
      // itself was reaching the consumer (giant random ordinal → "?" in Image
      // lookups), which combined with the broken return-site lowering to
      // produce the surface bug catalogued in feedback_sysl_simple_enum_fn_return.
      case (_: EnumType, t) if t.isIntegral =>
        emit("  load32s")
      case _ => to match
        case IntType(8) =>
          emitPushInt(56); emit("  shl"); emitPushInt(56); emit("  sar")
        case IntType(16) =>
          emitPushInt(48); emit("  shl"); emitPushInt(48); emit("  sar")
        case IntType(32) =>
          emitPushInt(32); emit("  shl"); emitPushInt(32); emit("  sar")
        case UIntType(8) =>
          emitPushInt(0xff); emit("  and")
        case UIntType(16) =>
          emitPushInt(0xffff); emit("  and")
        case UIntType(32) =>
          emit("  push_i64 4294967295"); emit("  and")
        case _ => // no-op for same-width or i64/u64/ptr

  // Recursively emit a discriminator check for a (possibly nested) match
  // pattern. The outer scrutinee value's address is in local `scrIdx`.
  // `absOff` is the offset from the scrutinee's address where this nested
  // sub-value lives. For variant patterns, loads the tag at the field
  // address (i32 at offset 0 of the nested enum) and `jumpz`-es to
  // `failLabel` on mismatch; recurses for any deeper nested patterns.
  // For struct destructure patterns, recurses without a discriminator
  // check. Other pattern shapes (TWildcard / primitives) act as
  // wildcards in nested position.
  def emitNestedPatternCheckSVM(
      pat: TMatchPattern,
      fieldType: SyslType,
      scrIdx: Int,
      absOff: Int,
      failLabel: String,
  ): Unit = pat match
    case TWildcard => ()
    case TVariantPattern(et, variantIndex, _, _, deeperNested) =>
      // Push field address (= scrutinee addr + absOff)
      emit(s"  local_get $scrIdx")
      if absOff != 0 then { emitPushInt(absOff); emit("  add") }
      // Load tag (i32 at offset 0 of the nested enum)
      emit("  load32")
      emitPushInt(variantIndex)
      emit("  eq")
      emit(s"  jumpz $failLabel")
      // Recurse into deeper nested
      val variantFields = et.variants(variantIndex)._2
      val dataOff = et.dataOffset.toInt
      var fieldOff = 0
      for ((deeperOpt, i) <- deeperNested.zipWithIndex) do
        val (_, deeperFieldType) = variantFields(i)
        val align = deeperFieldType.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        deeperOpt.foreach { deeper =>
          emitNestedPatternCheckSVM(deeper, deeperFieldType, scrIdx, absOff + dataOff + fieldOff, failLabel)
        }
        fieldOff += deeperFieldType.sizeOf.toInt
    case TDestructurePattern(st, _, _, deeperNested) =>
      for ((deeperOpt, i) <- deeperNested.zipWithIndex) do
        deeperOpt.foreach { deeper =>
          val deeperFieldType = st.fields(i)._2
          val off = fieldOffset(st, i).toInt
          emitNestedPatternCheckSVM(deeper, deeperFieldType, scrIdx, absOff + off, failLabel)
        }
    case _ => () // primitive nested patterns — treat as wildcard

  // Recursively emit name bindings for a (possibly nested) match pattern.
  // The outer scrutinee value's address is in local `scrIdx`. `absOff`
  // is the offset from the scrutinee where this nested sub-value lives.
  // Each named binding inside the nested pattern allocates a new local
  // and copies the field value (loaded relative to scrutinee + absOff +
  // local field offset).
  def emitNestedPatternBindingsSVM(
      pat: TMatchPattern,
      fieldType: SyslType,
      scrIdx: Int,
      absOff: Int,
  ): Unit = pat match
    case TVariantPattern(et, variantIndex, bindings, fieldTypes, deeperNested) =>
      val variantFields = et.variants(variantIndex)._2
      val dataOff = et.dataOffset.toInt
      var fieldOff = 0
      for (((binding, ft), i) <- bindings.zip(fieldTypes).zipWithIndex) do
        val align = ft.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        binding.foreach { name =>
          val localIdx = nextLocalIndex
          nextLocalIndex += 1
          locals(name) = LocalInfo(localIdx, ft)
          emit(s"  local_get $scrIdx")
          val totalOff = absOff + dataOff + fieldOff
          if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
          emitLoad(ft)
          emit(s"  local_set $localIdx")
        }
        if i < deeperNested.length then deeperNested(i).foreach { deeper =>
          emitNestedPatternBindingsSVM(deeper, ft, scrIdx, absOff + dataOff + fieldOff)
        }
        fieldOff += ft.sizeOf.toInt
    case TDestructurePattern(st, bindings, fieldTypes, deeperNested) =>
      for (((binding, ft), i) <- bindings.zip(fieldTypes).zipWithIndex) do
        val off = fieldOffset(st, i).toInt
        binding.foreach { name =>
          val localIdx = nextLocalIndex
          nextLocalIndex += 1
          locals(name) = LocalInfo(localIdx, ft)
          emit(s"  local_get $scrIdx")
          val totalOff = absOff + off
          if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
          emitLoad(ft)
          emit(s"  local_set $localIdx")
        }
        if i < deeperNested.length then deeperNested(i).foreach { deeper =>
          emitNestedPatternBindingsSVM(deeper, ft, scrIdx, absOff + off)
        }
    case _ => ()

  def fieldOffset(st: SyslType.StructType, fieldIndex: Int): Long =
    if fieldIndex >= st.fields.length then
      sys.error(s"fieldOffset: index $fieldIndex out of range for struct '${st.name}' with ${st.fields.length} fields")
    var offset = 0L
    for i <- 0 until fieldIndex do
      val fType = st.fields(i)._2
      val align = fType.alignOf.max(1)
      offset = ((offset + align - 1) / align) * align
      offset += fType.sizeOf
    val targetType = st.fields(fieldIndex)._2
    val align = targetType.alignOf.max(1)
    ((offset + align - 1) / align) * align

  /** Generate a match expression. If asExpr, each body leaves a value on the stack. */
  def genMatch(scrutinee: TExpr, arms: List[TMatchArm], default: Option[List[TStmt]], matchTyp: SyslType, asExpr: Boolean): Unit =
    val scrIdx = nextLocalIndex
    nextLocalIndex += 1
    genExpr(scrutinee)
    emit(s"  local_set $scrIdx")
    val endLabel = newLabel("match_end")
    for arm <- arms do
      val hitLabel = newLabel("match_hit")
      val nextArm = newLabel("match_next")
      for pat <- arm.patterns do pat match
        case TWildcard =>
          emit(s"  jump $hitLabel")
        case TBindPattern(_, _) =>
          // Binding pattern matches anything; the actual name->slot
          // wiring happens after the hit label below.
          emit(s"  jump $hitLabel")
        case TValuePattern(v) =>
          genExpr(v)
          emit(s"  local_get $scrIdx")
          if scrutinee.typ == SyslType.StringType then
            // Strings are 16-byte fat pointers; the generic `eq` opcode
            // compares only the descriptor addresses (each TStringLit
            // allocates a fresh descriptor, so two equal-content strings
            // never compare equal under raw eq). Route through the
            // dedicated byte-wise __svm_str_eq helper.
            emit("  call __svm_str_eq")
            needsStrEq = true
          else
            scrutinee.typ.underlying match
              case et: SyslType.EnumType if et.variants.forall(_._2.isEmpty) =>
                // Simple-enum scrutinee is stored as a pointer to an enum
                // buffer (tag at offset 0). Pattern compares against the
                // variant's i32 value, so deref the tag first.
                emit("  load32")
              case _ =>
            emit("  eq")
          emit(s"  jumpnz $hitLabel")
        case TRangePattern(lo, hi) =>
          val rangeNext = newLabel("match_rng")
          emit(s"  local_get $scrIdx")
          genExpr(lo)
          emit(if scrutinee.typ.isUnsigned then "  geu" else "  ge")
          emit(s"  jumpz $rangeNext")
          emit(s"  local_get $scrIdx")
          genExpr(hi)
          emit(if scrutinee.typ.isUnsigned then "  leu" else "  le")
          emit(s"  jumpnz $hitLabel")
          emit(s"$rangeNext:")
        case TDestructurePattern(st, _, _, nested) =>
          if nested.forall(_.isEmpty) then
            emit(s"  jump $hitLabel")
          else
            val patFail = newLabel("pat_fail")
            for ((subOpt, i) <- nested.zipWithIndex) do subOpt.foreach { sub =>
              val off = fieldOffset(st, i)
              emitNestedPatternCheckSVM(sub, st.fields(i)._2, scrIdx, off.toInt, patFail)
            }
            emit(s"  jump $hitLabel")
            emit(s"$patFail:")
        case TVariantPattern(et, variantIndex, _, _, nested) =>
          // Load tag (i32 at offset 0 of enum), compare with variant index
          emit(s"  local_get $scrIdx")
          emit("  load32")
          emitPushInt(variantIndex)
          emit("  eq")
          if nested.forall(_.isEmpty) then
            emit(s"  jumpnz $hitLabel")
          else
            val patFail = newLabel("pat_fail")
            emit(s"  jumpz $patFail")
            val variantFields = et.variants(variantIndex)._2
            val dataOff = et.dataOffset.toInt
            var fieldOff = 0
            for ((subOpt, i) <- nested.zipWithIndex) do
              val (_, fieldType) = variantFields(i)
              val align = fieldType.alignOf.toInt.max(1)
              fieldOff = ((fieldOff + align - 1) / align) * align
              subOpt.foreach { sub =>
                emitNestedPatternCheckSVM(sub, fieldType, scrIdx, dataOff + fieldOff, patFail)
              }
              fieldOff += fieldType.sizeOf.toInt
            emit(s"  jump $hitLabel")
            emit(s"$patFail:")
      emit(s"  jump $nextArm")
      emit(s"$hitLabel:")
      // Bind destructure/variant pattern fields to locals before guard
      for pat <- arm.patterns do pat match
        case TBindPattern(name, typ) =>
          // Top-level binding: alias the user's name to the scrutinee slot.
          // No copy needed — arm body won't mutate the synthetic slot.
          locals(name) = LocalInfo(scrIdx, typ)
        case TVariantPattern(et, variantIndex, bindings, _, nested) =>
          val dataOff = et.dataOffset.toInt
          val variantFields = et.variants(variantIndex)._2
          var fieldOff = 0
          for (binding, i) <- bindings.zipWithIndex do
            val (_, fieldType) = variantFields(i)
            val align = fieldType.alignOf.toInt.max(1)
            fieldOff = ((fieldOff + align - 1) / align) * align
            binding.foreach { name =>
              val localIdx = nextLocalIndex
              nextLocalIndex += 1
              locals(name) = LocalInfo(localIdx, fieldType)
              emit(s"  local_get $scrIdx")
              val totalOff = dataOff + fieldOff
              if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
              emitLoad(fieldType)
              emit(s"  local_set $localIdx")
            }
            if i < nested.length then nested(i).foreach { sub =>
              emitNestedPatternBindingsSVM(sub, fieldType, scrIdx, dataOff + fieldOff)
            }
            fieldOff += fieldType.sizeOf.toInt
        case TDestructurePattern(st, bindings, _, nested) =>
          for (binding, i) <- bindings.zipWithIndex do
            val fieldType = st.fields(i)._2
            binding.foreach { name =>
              val localIdx = nextLocalIndex
              nextLocalIndex += 1
              locals(name) = LocalInfo(localIdx, fieldType)
              val off = fieldOffset(st, i)
              emit(s"  local_get $scrIdx")
              if off != 0 then { emitPushInt(off); emit("  add") }
              emitLoad(fieldType)
              emit(s"  local_set $localIdx")
            }
            if i < nested.length then nested(i).foreach { sub =>
              emitNestedPatternBindingsSVM(sub, fieldType, scrIdx, fieldOffset(st, i).toInt)
            }
        case _ =>
      arm.guard.foreach { g =>
        genExpr(g)
        emit(s"  jumpz $nextArm")
      }
      if asExpr then genStmtsAsExpr(arm.body)
      else genStmts(arm.body)
      emit(s"  jump $endLabel")
      emit(s"$nextArm:")
    default match
      case Some(stmts) =>
        if asExpr then genStmtsAsExpr(stmts)
        else genStmts(stmts)
      case None =>
        if asExpr then emitPushInt(0)
    emit(s"$endLabel:")

  def genStructAddr(obj: TExpr): Unit = obj match
    case TDeref(ptr, _) =>
      genExpr(ptr) // pointer dereference yields the address
    case TFieldAccess(innerObj, fieldIndex, typ) if needsMemAlloc(typ) =>
      // Nested field access on aggregate: compute parent addr + field offset
      val st = innerObj.typ match
        case s: SyslType.StructType => s
        case SyslType.RefType(s: SyslType.StructType) => s
        case SyslType.PtrType(s: SyslType.StructType) => s
        case _ => sys.error(s"nested field access on non-struct: ${innerObj.typ}")
      genStructAddr(innerObj)
      val off = fieldOffset(st, fieldIndex)
      if off != 0 then { emitPushInt(off); emit("  add") }
    case _ =>
      obj.typ match
        case _: SyslType.PtrType | _: SyslType.RefType =>
          genExpr(obj) // pointer/ref: evaluates to address
        case _ =>
          genExpr(obj) // local holding memory address for aggregates
