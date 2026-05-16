package io.github.edadma.trisc

import scala.collection.mutable

class SyslTriscCodegen(addresses: Int = 4, peepholeEnabled: Boolean = true):
  // Output is accumulated as structured Line values (parsed once at emit time) so
  // the peephole optimizer can run over typed instructions without a string round-trip.
  // Stringified once at the end of generate().
  private val out = mutable.ArrayBuffer.empty[TriscPeephole.Line]
  private var labelCounter = 0
  private var modulePrefix = "" // unique prefix for this compilation unit
  private val stringLiterals = new mutable.ListBuffer[(String, String)]() // (label, value)
  private var needsAllocExtern = false // set when codegen emits malloc/free references
  private var needsFreeExtern = false // set when codegen emits free references
  private var needsStrInt = false // set when codegen needs __str_int helper
  private var needsStrFloat = false // set when codegen needs __str_float helper
  private var needsStrFmtI64 = false // set when codegen needs __str_fmt_i64 helper (f"..." on i64)

  private def newLabel(prefix: String): String =
    labelCounter += 1
    if modulePrefix.nonEmpty then s".${prefix}_${modulePrefix}_$labelCounter"
    else s".${prefix}_$labelCounter"

  // Struct types that have a deinit method (populated during generate)
  private val deinitFunctions = new mutable.HashMap[String, String] // struct name → deinit function name

  // Slice deinit functions to emit (for &[]T where T contains rc data). Generated
  // on demand when a slice with rc-content elements is freed; emitted after all
  // user functions. Each takes r1 = base of slice block (rc header at +0, len at
  // +8, data at +16) and decrs each element before returning.
  private val sliceElemDeinitsNeeded = new mutable.LinkedHashMap[String, SyslType] // deinit name → elem type

  // Enum deinit functions to emit (for &MyEnum where the enum has string-bearing
  // variants). Generated on demand when an enum ref is freed; emitted after all
  // user functions. Each takes r1 = data ptr (past the rc header) and walks the
  // active variant's strings before returning.
  private val enumDeinitsNeeded = new mutable.LinkedHashMap[String, SyslType.EnumType] // deinit name → enum type

  // Auto-synthesized struct deinit functions (for &MyStruct where the struct has
  // string-bearing fields AND the user hasn't defined `TypeName.deinit`). Called
  // by emitRefDecr at rc=0 before free(). Takes r1 = data ptr (past rc header)
  // and walks string fields to decr them.
  private val structDeinitsNeeded = new mutable.LinkedHashMap[String, SyslType.StructType] // deinit name → struct type

  // Pending closure functions to generate after all regular functions
  private var closureCounter = 0
  private val pendingClosures = new mutable.ListBuffer[(String, TClosure)]

  // Per-closure-id env deinit functions to emit (only for closures whose captures carry rc content).
  // Maps closure name → its TClosure (for capture layout). The deinit walks captures decr'ing each.
  private val closureEnvDeinitsNeeded = new mutable.LinkedHashMap[String, TClosure]
  // True if any closure has captures (need __closure_env_dispatch + needsFreeExtern)
  private var closureEnvDispatchNeeded = false
  // Borrowed-capture local names in a closure body (skip rc cleanup — env owns the buffers)
  private var captureBorrows: Set[String] = Set.empty

  /** Kind of env backing a closure descriptor:
    *  - NullEnv: env_ptr is null (no captures, or TFuncRef). Decr is no-op.
    *  - StackEnv: env lives in caller's stack frame (no header, no rc, no free).
    *    Used when captures are all non-rc-bearing AND the closure does not escape.
    *  - HeapEnv: env malloc'd with [rc:8 | deinit_ptr:8 | data] header. Scope-exit
    *    decr's env's rc; deinit walks rc-bearing captures; free reclaims env.
    */
  private enum FuncKind:
    case NullEnv, StackEnv, HeapEnv

  /** Per-local kind tracking for FuncType locals. Set at TVarStmt time based on
    * RHS analysis; consulted at scope-exit to decide whether to emit
    * emitClosureDescrDecr. Locals not in the map default to NullEnv (skip decr —
    * safe for uninitialized var f: (...) -> T whose env_ptr starts as zero).
    * Reset at the start of each genFunction / genClosureFunction. */
  private val closureLocalKind = new mutable.HashMap[String, FuncKind]

  /** A capture type counts as "rc-bearing" if its value-flow needs an incr/decr —
    * any RefType, or any value type containing a String (directly or nested). */
  private def captureNeedsRc(t: SyslType): Boolean =
    t.isInstanceOf[SyslType.RefType] || structHasStringFields(t)

  /** Compute the env data area size for a closure, with each capture placed at
   *  its natural alignment (so mixed int+FuncType captures don't violate the
   *  CPU's 8-byte-aligned ldd/std requirement). Mirrors the layout used by the
   *  env-store, env-unpack and env-deinit walkers. */
  private def closureEnvSize(captures: List[(String, SyslType)]): Int =
    var off = 0
    for (_, t) <- captures do
      val a = stackAlign(t)
      off = ((off + a - 1) / a) * a
      off += stackSize(t).toInt
    off

  /** Decide the env kind for a freshly-constructed TClosure based on capture
    * analysis. Stack env is only used for non-escaping closures whose captures
    * are all non-rc-bearing — in that case no rc header / malloc / free is
    * needed and the env can live on the constructing function's stack frame. */
  private def closureKindOf(c: TClosure): FuncKind =
    if c.captures.isEmpty then FuncKind.NullEnv
    else if c.escapes || c.captures.exists((_, t) => captureNeedsRc(t)) then FuncKind.HeapEnv
    else FuncKind.StackEnv

  private def funcKindOfExpr(e: TExpr): FuncKind = e match
    case c: TClosure => closureKindOf(c)
    case _: TFuncRef => FuncKind.NullEnv
    case TVarRef(name, _) if funcBorrowParams.contains(name) =>
      // FuncType params are borrowed from the caller — treat as HeapEnv (could
      // be NullEnv at runtime; dispatch's null-check handles that). Lets the
      // copy/return paths emit the right incr to balance shared ownership.
      FuncKind.HeapEnv
    case TVarRef(name, _) => closureLocalKind.getOrElse(name, FuncKind.NullEnv)
    case _: TCall | _: TIndirectCall | _: TInterfaceDispatch =>
      if needsAllocExtern then FuncKind.HeapEnv else FuncKind.NullEnv
    case _: TIfExpr | _: TMatchExpr =>
      if needsAllocExtern then FuncKind.HeapEnv else FuncKind.NullEnv
    case _ => FuncKind.NullEnv

  // Interface tables: (struct, interface) → itable label + method function names
  // Collected during genExpr when TInterfaceBox is encountered, emitted in rodata
  private val itables = new mutable.LinkedHashMap[String, List[String]] // itable label → list of function names
  private var declaredFunctions = Set.empty[String] // all function names in this compilation unit

  def generate(program0: TProgram): String =
    // Dedupe decls by name across compilation units. The test runner merges
    // multiple units into one TProgram via `flatMap(_.typed.decls)`; if two
    // units both instantiated the same generic (e.g. `is_err[i64, Error]`)
    // we'd emit two `global is_err_i64_Error, ...` lines and two
    // `is_err_i64_Error:` labels, and the asm assembler rejects the duplicate
    // symbol. Keep the first occurrence per name; subsequent duplicates are
    // identical re-instantiations of the same template and can be skipped.
    val program: TProgram =
      val seen = mutable.Set.empty[String]
      def keep(name: String): Boolean =
        if seen.contains(name) then false else { seen += name; true }
      val deduped = program0.decls.filter {
        case TFunDecl(name, _, _, _, _, _, _, _, _, _) => keep(name)
        case TVarDecl(name, _, _, _, _, _, _) => keep(name)
        case TExternFuncDecl(name, _, _) => keep(name)
        case TExternVarDecl(name, _) => keep(name)
        case _ => true
      }
      TProgram(deduped)

    out.clear()
    labelCounter = 0
    stringLiterals.clear()
    deinitFunctions.clear()
    sliceElemDeinitsNeeded.clear()
    enumDeinitsNeeded.clear()
    structDeinitsNeeded.clear()
    needsAllocExtern = scanNeedsAlloc(program)  // pre-scan so rc-bracket gates are correct from the start
    needsFreeExtern = false
    // Extract module prefix for unique symbol names across compilation units
    modulePrefix = program.decls.collectFirst { case TModuleDecl(path) => path.mkString("_") }.getOrElse("")
    needsStrInt = false
    needsStrFloat = false
    needsStrFmtI64 = false
    globalConstants.clear()
    itables.clear()

    // Extract module path for unique label prefixing
    modulePrefix = program.decls.collectFirst { case TModuleDecl(path) => path.mkString("_") }.getOrElse("")

    // Collect all declared function names for itable resolution
    declaredFunctions = program.decls.collect { case TFunDecl(name, _, _, _, _, _, _, _, _, _) => name }.toSet

    // Scan for deinit methods: functions named TypeName_deinit
    for decl <- program.decls do
      decl match
        case TFunDecl(name, _, _, _, _, _, _, _, _, _) if name.endsWith("_deinit") =>
          val structName = name.indexOf("__") match
            case -1 => name.dropRight(7)
            case i  => name.substring(i + 2).dropRight(7)
          deinitFunctions(structName) = name
        case _ =>

    // Emit entry point and global directives from module metadata
    val meta = ModuleMeta.fromProgram(program)
    val hasMain = meta.symbols.exists(s => s.name == "main" && s.typ.isInstanceOf[SymbolMeta.Kind.Func])
    if hasMain then emit("entry main")
    // Parse the multi-line string from ModuleMeta into structured Lines.
    for line <- meta.toAsmGlobals.linesIterator do emit(line)

    // Collect globals into data (initialized) and bss (zero-initialized) lists
    val dataGlobals = new mutable.ListBuffer[TDecl]
    val bssGlobals = new mutable.ListBuffer[TDecl]

    for decl <- program.decls do
      decl match
        case v @ TVarDecl(_, typ, init, _, _, _, _) =>
          globals(v.name) = typ
          // Track constant values for cross-reference in other global initializers
          constEval(init).foreach(n => globalConstants(v.name) = n)
          floatConstEval(init).foreach(d => globalConstants(v.name) = java.lang.Double.doubleToRawLongBits(d))
          if isZeroInit(typ, init) then bssGlobals += v
          else dataGlobals += v
        case _ => // functions, externs, types — handled below

    // Emit code segment — functions
    emit("segment code")
    pendingClosures.clear()
    closureEnvDeinitsNeeded.clear()
    closureEnvDispatchNeeded = false
    closureCounter = 0
    for decl <- program.decls do
      decl match
        case f: TFunDecl => genFunction(f)
        case _ => // skip

    // Emit closure functions (generated during genExpr for TClosure nodes)
    while pendingClosures.nonEmpty do
      val batch = pendingClosures.toList
      pendingClosures.clear()
      for (name, closure) <- batch do
        genClosureFunction(name, closure)

    // Emit slice element deinit functions (registered when freeing slices of
    // rc-content elements). Iterating may register more types via emitValueRC
    // (e.g. nested slices), so drain a worklist.
    val emittedSliceDeinits = mutable.Set.empty[String]
    while sliceElemDeinitsNeeded.exists((n, _) => !emittedSliceDeinits.contains(n)) do
      val pending = sliceElemDeinitsNeeded.filterNot((n, _) => emittedSliceDeinits.contains(n)).toList
      for (name, elem) <- pending do
        emittedSliceDeinits += name
        emitSliceDeinit(name, elem)

    // Emit per-enum-type deinit functions (registered when freeing an enum ref
    // whose active variant carries rc content).
    val emittedEnumDeinits = mutable.Set.empty[String]
    while enumDeinitsNeeded.exists((n, _) => !emittedEnumDeinits.contains(n)) do
      val pending = enumDeinitsNeeded.filterNot((n, _) => emittedEnumDeinits.contains(n)).toList
      for (name, et) <- pending do
        emittedEnumDeinits += name
        emitEnumDeinit(name, et)

    // Emit auto-synthesized struct deinit functions (registered when freeing a
    // struct ref whose fields carry rc content and no user deinit exists).
    val emittedStructDeinits = mutable.Set.empty[String]
    while structDeinitsNeeded.exists((n, _) => !emittedStructDeinits.contains(n)) do
      val pending = structDeinitsNeeded.filterNot((n, _) => emittedStructDeinits.contains(n)).toList
      for (name, st) <- pending do
        emittedStructDeinits += name
        emitStructDeinit(name, st)

    // Emit per-closure-id env deinit functions (walk captures to decr rc content)
    for (name, closure) <- closureEnvDeinitsNeeded do
      emitClosureEnvDeinit(name, closure)

    // Emit __closure_env_dispatch (loads runtime deinit_ptr from env-8 and calls it)
    if closureEnvDispatchNeeded then emitClosureEnvDispatch()

    // Emit __str_int helper if needed (integer to string conversion)
    if needsStrInt then emitStrIntHelper()

    // Emit __str_float helper if needed (float to string conversion)
    if needsStrFloat then emitStrFloatHelper()

    // Emit __str_fmt_i64 helper if needed (formatted integer interpolation, audit #14)
    if needsStrFmtI64 then emitStrFmtI64Helper()

    // Pre-walk dataGlobals to intern any string-literal initializers (scalar or
    // array elements). This must run before rodata emission so the bodies land
    // in rodata; the data segment loop below then re-uses the precomputed
    // (label, len) pairs without re-interning.
    val dataGlobalStringLabels = new mutable.HashMap[Int, List[(String, Int)]]
    for (decl, idx) <- dataGlobals.toList.zipWithIndex do
      decl match
        case TVarDecl(_, typ, init, _, _, _, _) =>
          init match
            case TArrayLit(elements, _) =>
              val isStringArray = typ match
                case SyslType.ArrayType(e, _) => e.underlying == SyslType.StringType
                case _ => false
              if isStringArray then
                dataGlobalStringLabels(idx) = elements.map {
                  case TStringLit(value, _) => internStringLiteral(value)
                  case _ => ("0", 0)
                }.toList
            case TStringLit(value, _) =>
              dataGlobalStringLabels(idx) = List(internStringLiteral(value))
            case _ =>
        case _ =>

    // Emit rodata segment — string literals and interface tables
    if stringLiterals.nonEmpty || itables.nonEmpty then
      emit("segment rodata")

    // Interface tables: each is an array of function pointers
    for (label, funcNames) <- itables do
      emit(s"global $label, data, ${funcNames.length * 8}")
    for (label, funcNames) <- itables do
      emit(s"  align 8")
      emit(s"$label:")
      for fn <- funcNames do
        emit(s"  dl $fn")

    // String literals with immortal refcount headers
    if stringLiterals.nonEmpty then
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("ISO-8859-1")
        // Total: 8 (refcount) + bytes + 1 (null terminator)
        emit(s"global $label, data, ${bytes.length + 9}")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("ISO-8859-1")
        emit(s"  dl -1") // immortal refcount header (assembler auto-aligns dl)
        emit(s"$label:")
        for b <- bytes do emit(s"  db ${b & 0xff}")
        emit("  db 0") // null terminator for *i8 decay compatibility

    // Emit data segment — initialized globals
    if dataGlobals.nonEmpty then
      emit("segment data")
      for (decl, idx) <- dataGlobals.toList.zipWithIndex do
        decl match
          case TVarDecl(name, typ, init, _, _, _, _) =>
            val align = stackAlign(typ)
            if align > 1 then emit(s"  align $align")
            emit(s"# global: $name")
            emit(s"$name:")
            init match
              case TArrayLit(elements, _) =>
                val declElemType = typ match
                  case SyslType.ArrayType(e, _) => e
                  case other => throw new RuntimeException(s"global array literal: expected ArrayType, got $other")
                declElemType.underlying match
                  case SyslType.StringType =>
                    // Each element is a 16-byte {ptr, len} descriptor pointing at
                    // an interned string blob in rodata (interned in the pre-walk).
                    val labels = dataGlobalStringLabels(idx)
                    for (label, lenBytes) <- labels do
                      if label == "0" then
                        emit("  dl 0")
                        emit("  dl 0")
                      else
                        emit(s"  dl $label")
                        emit(s"  dl $lenBytes")
                  case _ =>
                    val elemDir = emitDataDirective(declElemType)
                    for elem <- elements do
                      constEval(elem) match
                        case Some(n) => emit(s"  $elemDir $n")
                        case None => emit(s"  $elemDir 0")
              case TStringLit(_, _) =>
                // Module-level scalar string init: emit a 16-byte {ptr, len} descriptor
                // pointing at the interned blob.
                val (label, lenBytes) = dataGlobalStringLabels(idx).head
                emit(s"  dl $label")
                emit(s"  dl $lenBytes")
              case _ =>
                val directive = emitDataDirective(typ)
                floatConstEval(init) match
                  case Some(d) => emit(s"  $directive $d")
                  case None =>
                    constEval(init) match
                      case Some(n) => emit(s"  $directive $n")
                      case None => emit(s"  $directive 0")
          case _ =>

    // Emit bss segment — zero-initialized globals (arrays, structs, uninitialized)
    if bssGlobals.nonEmpty then
      emit("segment bss")
      for decl <- bssGlobals do
        decl match
          case TVarDecl(name, typ, _, _, _, _, _) =>
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

    // Emit extern declarations for malloc/free based on actual references in generated code.
    // Scan the structured Instr array directly — no string formatting needed.
    val definedSymbols = (for decl <- program.decls yield decl match
      case TFunDecl(name, _, _, _, _, _, _, _, _, _) => Some(name)
      case TVarDecl(name, _, _, _, _, _, _) => Some(name)
      case _ => None).flatten.toSet
    def referencesSymbol(sym: String): Boolean =
      out.exists {
        case TriscPeephole.Instr("movi", List(_, `sym`)) => true
        case _ => false
      }
    if referencesSymbol("malloc") && !definedSymbols.contains("malloc") then emit("extern malloc")
    if referencesSymbol("free") && !definedSymbols.contains("free") then emit("extern free")

    // Run the peephole optimizer over the structured output, then render to asm.
    if peepholeEnabled then
      val (optimized, _) = TriscPeephole.optimize(out)
      TriscPeephole.render(optimized)
    else
      TriscPeephole.render(out)

  private case class LocalVar(name: String, offset: Int, typ: SyslType)

  private val globals = new mutable.LinkedHashMap[String, SyslType]
  private var locals: mutable.LinkedHashMap[String, LocalVar] = null
  private var refParams: mutable.LinkedHashMap[String, SyslType.RefType] = null // ref-typed params for cleanup
  /** String parameter names — borrowed from caller; do not decref in emitRefCleanup (caller incr/decr around the call). */
  private var stringBorrowParams: Set[String] = Set.empty
  /** FuncType/InterfaceType parameter names — borrowed; caller still owns the descriptor's env. */
  private var funcBorrowParams: Set[String] = Set.empty
  private var stackOffset: Int = 0
  private val savedScopes = new mutable.Stack[(Map[String, LocalVar], Int)]
  private val loopScopeOffsets = new mutable.Stack[Int]

  private def enterScope(): Unit =
    savedScopes.push((locals.toMap, stackOffset))

  private def leaveScope(): Unit =
    val (savedLocals, savedOffset) = savedScopes.pop()
    // Decrement refcounts for ref-typed and string locals leaving scope
    // Skip params (positive offsets) — they are borrowed, not owned
    for (name, local) <- locals if !savedLocals.contains(name) && local.offset < 0 do
      // Captured-into-body locals are borrowed; env owns the buffers.
      if captureBorrows.contains(name) then ()
      else local.typ match
        case rt: SyslType.RefType =>
          val hoff = refHeaderOffset(rt)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")
          emitRefDecr(1, hoff, deinitFor(rt))
          emit("  popd r1")
        case SyslType.StringType if needsAllocExtern =>
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")       // r1 = ptr field
          emitRefDecr(1, 8)
          emit("  popd r1")
        case SyslType.SliceType(elem) =>
          // Decrement backref if non-null. If elements need deinit, supply a
          // per-elem-type deinit function. Normalize the deinit's convention
          // to r1 = data ptr (past 16-byte rc/len header) by adding 16 and
          // passing headerOff=16 (so rc/free still target the original base).
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset + 16)
          emit("  ldd r1, r1, r0")       // r1 = backref (= base)
          val deinit = sliceDeinitFor(elem)
          if deinit.isDefined then
            emitAddImm(1, 1, 16)         // r1 = data ptr
            emitRefDecr(1, 16, deinit)
          else
            emitRefDecr(1, 0)
          emit("  popd r1")
        case st: SyslType.StructType if structHasStringFields(st) =>
          emitStructStringFieldsRC(5, local.offset, st, incr = false)
        case SyslType.ArrayType(elem, _) if structHasStringFields(elem) =>
          emitValueRC(5, local.offset, local.typ, incr = false)
        case et: SyslType.EnumType if structHasStringFields(et) =>
          emitEnumStringFieldsRC(5, local.offset, et, incr = false)
        case _: SyslType.FuncType
            if !funcBorrowParams.contains(name)
            && closureLocalKind.get(name).contains(FuncKind.HeapEnv) =>
          emitClosureDescrDecr(5, local.offset)
        case _ =>
    locals.clear()
    locals ++= savedLocals
    if stackOffset != savedOffset then
      emitAddImm(7, 7, savedOffset - stackOffset)
      stackOffset = savedOffset
  private var currentFunction: TFunDecl = null
  // True while generating a closure body. Closure prologues push an extra slot for
  // the env pointer (r3), so the epilogue needs to skip 8 more bytes than a regular
  // function. Used by emitEpilogue (called from TReturnStmt) so early-return paths
  // pop the same number of bytes as the implicit-return path through emitClosureEpilogue.
  private var inClosureBody: Boolean = false

  // Determine if a global variable should go in bss (zero-initialized) vs data
  private def isZeroInit(typ: SyslType, init: TExpr): Boolean =
    init match
      case TArrayLit(_, _) => false // array literal has explicit values → data
      case TStringLit("", _) => true  // empty string descriptor is {ptr=0, len=0} → bss
      case TStringLit(_, _) => false  // non-empty string literal needs interned data → data
      case _ =>
        floatConstEval(init) match
          case Some(0.0) => true  // explicit zero float → bss
          case Some(_) => false   // nonzero float constant → data
          case None =>
            typ match
              case SyslType.ArrayType(_, _) => true // uninitialized array → bss
              case _: SyslType.StructType => true // struct → bss
              case _ =>
                constEval(init) match
                  case Some(0) => true // explicitly zero → bss
                  case None => true // no initializer → bss
                  case _ => false // nonzero constant → data

  // Compile-time evaluate a float expression (handles literal and unary minus).
  private def floatConstEval(expr: TExpr): Option[Double] = expr match
    case TFloatLit(d, _) => Some(d)
    case TUnary("-", operand, _) => floatConstEval(operand).map(-_)
    case _ => None

  // Does this return type require a caller-allocated return slot?
  private def returnsViaPointer(typ0: SyslType): Boolean =
    val typ = typ0.underlying
    typ.isInstanceOf[SyslType.StructType] || typ == SyslType.StringType || typ.isInstanceOf[SyslType.SliceType] || typ.isInstanceOf[SyslType.EnumType] || typ.isInstanceOf[SyslType.FuncType] || typ.isInstanceOf[SyslType.InterfaceType]

  // Method-on-temporary receiver where the inner expression is a call or
  // struct/enum constructor that produces a stack-resident temp. genExpr
  // leaves r1 pointing INTO that temp, so a generic stack-rewind would
  // reclaim the temp before its address is consumed (corrupting the data).
  // TTempAddr wrapping a TDeref (e.g. an `inout` param's auto-deref) just
  // yields the dereferenced pointer and uses the existing else-branch path.
  private def isStructLikeTempAddr(arg: TExpr): Boolean = arg match
    case TTempAddr(inner, _) =>
      val innerTyp = inner.typ
      val isStructLike = innerTyp.isInstanceOf[SyslType.StructType] || innerTyp.isInstanceOf[SyslType.EnumType]
      val producesStackTemp = inner match
        case _: TCall | _: TIndirectCall | _: TInterfaceDispatch | _: TStructConstruct => true
        case _ => false
      isStructLike && producesStackTemp
    case _ => false

  // Size of a type on the stack in bytes, rounded up to alignment
  private def stackSize(typ: SyslType): Int =
    val raw = typ.sizeOf.toInt
    val align = stackAlign(typ)
    ((raw + align - 1) / align) * align

  // Natural alignment for a type. NamedType (derived/constrained) delegates to its base.
  private def stackAlign(typ: SyslType): Int = typ.underlying match
    case SyslType.IntType(w) => (w / 8).min(8)
    case SyslType.UIntType(w) => (w / 8).min(8)
    case SyslType.BoolType => 1
    case SyslType.PtrType(_) => 8
    case _: SyslType.FuncType => 8
    case SyslType.ArrayType(elem, _) => stackAlign(elem)
    case SyslType.StructType(_, fields, _) => if fields.isEmpty then 1 else fields.map(f => stackAlign(f._2)).max
    case SyslType.EnumType(_, variants) =>
      val fieldAligns = variants.flatMap(_._2.map(f => stackAlign(f._2)))
      if fieldAligns.isEmpty then 4 else fieldAligns.max.max(4)
    case SyslType.StringType => 8    // contains a pointer
    case SyslType.SliceType(_) => 8  // contains a pointer
    case _ => 8

  // Map of global constant names to their evaluated values (populated during global processing)
  private val globalConstants = new mutable.LinkedHashMap[String, Long]

  // Try to evaluate a constant expression at compile time.
  // Returns Some(value) for integer constants, None otherwise.
  // Resolves TVarRef to previously evaluated global constants.
  private def constEval(expr: TExpr): Option[Long] = expr match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(b, _) => Some(if b then 1 else 0)
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
    case TCast(inner, _) => constEval(inner) // pointer casts like *byte(0xC000)
    case _ => None

  // Emit load from [rBase + 0] into rDest, using width-appropriate instruction.
  // The CPU's ldb/lds/ldw already sign-extend via Int→Long in Register.write,
  // so no explicit sext is needed for signed types.
  // For unsigned types, load + zero-extend to clear sign-extended bits.
  private def emitLoad(destReg: Int, addrReg: Int, typ: SyslType): Unit =
    typ.underlying match
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
      case SyslType.IntType(64) | SyslType.UIntType(64) | SyslType.FloatType(64) |
           _: SyslType.PtrType | _: SyslType.RefType =>
        emit(s"  ldd r$destReg, r$addrReg, r0")
      case SyslType.FloatType(32) =>
        // f32 stored as 4 bytes; widen to f64 in register so arithmetic uses native fadd/fsub/etc.
        emit(s"  ldw r$destReg, r$addrReg, r0")
        emit(s"  f32tof64 r$destReg, r$destReg")
      case other =>
        throw new RuntimeException(s"emitLoad: unexpected type $other")

  // Emit store from rSrc to [rBase + 0], using width-appropriate instruction
  /** Copy `size` bytes from [srcReg] to [addrReg], using either 8-byte or
    *  4-byte memory ops based on `align`. Required because TRISC `std`/`ldd`
    *  fault on misaligned addresses, and an aggregate's natural alignment
    *  determines the worst-case alignment of its in-memory address. */
  private def emitAggregateCopy(srcReg: Int, addrReg: Int, size: Int, align: Int): Unit =
    // The loop uses r3 and r4 as scratch (r4 = loaded value, r3 = dest addr).
    // If srcReg is 3 or 4, the inner loop would clobber the source-base
    // pointer between iterations (e.g. `addi r4, r4, i; ldw r4, r4, r0`
    // computes from the previously-loaded VALUE instead of the source ADDR).
    // Same for addrReg in {3, 4}. Copy any conflicting reg to r2 / r1 first.
    // When picking srcBase, avoid the register that addrReg occupies — a naive
    // "always pick r2 when srcReg conflicts" caused a use-after-free for the
    // by-name forwarding case (srcReg=r3, addrReg=r2) where srcBase=2
    // overwrote env_ptr with &src and the whole copy became src→src.
    val srcBase =
      if srcReg == 3 || srcReg == 4 then
        if addrReg == 1 then 2 else 1
      else srcReg
    if srcBase != srcReg then emit(s"  mov r$srcBase, r$srcReg")
    val destBase =
      if addrReg == 3 || addrReg == 4 then
        if srcBase == 1 then 2 else 1
      else addrReg
    if destBase != addrReg then emit(s"  mov r$destBase, r$addrReg")
    if align >= 8 then
      for i <- 0 until size by 8 do
        emitAddImm(4, srcBase, i)
        emit("  ldd r4, r4, r0")
        emitAddImm(3, destBase, i)
        emit("  std r4, r3, r0")
    else
      for i <- 0 until size by 4 do
        emitAddImm(4, srcBase, i)
        emit("  ldw r4, r4, r0")
        emitAddImm(3, destBase, i)
        emit("  stw r4, r3, r0")

  private def emitStore(srcReg: Int, addrReg: Int, typ: SyslType): Unit =
    typ.underlying match
      case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType =>
        emit(s"  stb r$srcReg, r$addrReg, r0")
      case SyslType.IntType(16) | SyslType.UIntType(16) =>
        emit(s"  sts r$srcReg, r$addrReg, r0")
      case SyslType.IntType(32) | SyslType.UIntType(32) =>
        emit(s"  stw r$srcReg, r$addrReg, r0")
      case SyslType.StringType | (_: SyslType.FuncType) | _: SyslType.InterfaceType =>
        // 16-byte copy. srcReg may be one of our scratches (r3 or r4); pick
        // a load order that doesn't clobber srcReg before its second use.
        // Bug history: the previous order loaded src[0] into r4 first, then
        // re-derived src+8 from srcReg — which produced garbage when
        // srcReg == r4 (e.g. the append-grow path for `[]string`,
        // where the elem was loaded into r4). Now: read both source words
        // *before* any store, into r3 (lo) and r4 (hi), with the pair
        // ordered so the load that clobbers srcReg comes last.
        if srcReg == 3 then
          // srcReg already in r3; compute src+8 (clobbers r4 only) and load
          // src[8] first, then load src[0] last (clobbering r3 = srcReg).
          emitAddImm(4, srcReg, 8)
          emit("  ldd r4, r4, r0")            // r4 = src[8]
          emit(s"  ldd r3, r$srcReg, r0")     // r3 = src[0]  (srcReg dies here)
        else
          // srcReg is r1/r2/r4 — load src[0] into r3 (no srcReg clobber unless
          // srcReg=3, handled above), then derive src+8 into r4 (clobbers
          // srcReg if srcReg=4, fine because src[0] is already saved).
          emit(s"  ldd r3, r$srcReg, r0")     // r3 = src[0]
          emitAddImm(4, srcReg, 8)
          emit("  ldd r4, r4, r0")            // r4 = src[8]
        emit(s"  std r3, r$addrReg, r0")      // dst[0] = src[0]
        emitAddImm(3, addrReg, 8)
        emit("  std r4, r3, r0")              // dst[8] = src[8]
      case SyslType.SliceType(_) =>
        // 24-byte copy: {ptr(8), len+cap(8), backref(8)}
        for i <- 0 until 24 by 8 do
          emitAddImm(4, srcReg, i)
          emit("  ldd r4, r4, r0")
          emitAddImm(3, addrReg, i)
          emit("  std r4, r3, r0")
      case st: SyslType.StructType =>
        // Struct copy: srcReg = source address, addrReg = dest address.
        // If the struct's natural alignment is < 8, the destination is only
        // 4-aligned (e.g. a struct field at a non-8-aligned offset), so we
        // must use 4-byte loads/stores. Otherwise 8-byte ops are fine.
        emitAggregateCopy(srcReg, addrReg, stackSize(st), stackAlign(st))
      case at: SyslType.ArrayType =>
        // Fixed-size array copy: srcReg = source address, addrReg = dest address.
        // Used when an array is a struct field (e.g. `buf: [1024]u8` in std/bufio).
        emitAggregateCopy(srcReg, addrReg, stackSize(at), stackAlign(at))
      case et: SyslType.EnumType =>
        // Enum copy: srcReg = source address, addrReg = dest address.
        // Same alignment story as struct — enums whose payloads are only
        // 4-aligned (e.g. an enum carrying `int`-only variants embedded in
        // another enum) sit at 4-aligned offsets and must not be copied
        // with `std`. See audit item #32 (TRISC enum-match misalignment,
        // surfaced by item #19's runner).
        emitAggregateCopy(srcReg, addrReg, stackSize(et), stackAlign(et))
      case SyslType.IntType(64) | SyslType.UIntType(64) | SyslType.FloatType(64) |
           _: SyslType.PtrType | _: SyslType.RefType =>
        emit(s"  std r$srcReg, r$addrReg, r0")
      case SyslType.FloatType(32) =>
        // Source register holds f64; narrow to f32 bit pattern (lower 32 bits) and store 4 bytes.
        // Use r4 as scratch to preserve srcReg.
        emit(s"  f64tof32 r4, r$srcReg")
        emit(s"  stw r4, r$addrReg, r0")
      case other =>
        throw new RuntimeException(s"emitStore: unexpected type $other")

  // Recursively emit a discriminator check for a NESTED match pattern.
  // The outer scrutinee value's address is loaded from
  // `(fp + scrutineeOffset)` (where scrutineeOffset is fp-relative).
  // `absOffset` is the offset INSIDE that scrutinee where this nested
  // sub-value lives (variant data offset + outer field offset chains).
  // For variant patterns, loads the tag at the field address and branches
  // to `failLabel` on mismatch; recurses for any deeper nested patterns.
  // For struct destructure patterns, recurses without a discriminator
  // check (struct destructure always matches at the outer level). Other
  // pattern shapes (TWildcard / TValuePattern / TRangePattern) currently
  // act as wildcards in nested position — full nested-primitive support
  // can layer on later.
  private def emitNestedPatternCheck(
      pat: TMatchPattern,
      fieldType: SyslType,
      scrutineeOffset: Int,
      absOffset: Long,
      failLabel: String,
  ): Unit = pat match
    case TWildcard => () // always matches
    case TVariantPattern(et, variantIndex, _, _, deeperNested) =>
      // r1 = address of this nested enum value
      emitAddImm(1, 5, scrutineeOffset)
      emit("  ldd r1, r1, r0")
      if absOffset != 0 then emitAddImm(1, 1, absOffset.toInt)
      // Load tag from offset 0
      emit("  ldw r1, r1, r0")
      emitLoadImm(2, variantIndex)
      emit(s"  bne r1, r2, $failLabel")
      // Recurse into deeper nested patterns
      val variantFields = et.variants(variantIndex)._2
      val dataOff = et.dataOffset.toInt
      var fieldOff = 0
      for ((deeperOpt, i) <- deeperNested.zipWithIndex) do
        val (_, deeperFieldType) = variantFields(i)
        val align = stackAlign(deeperFieldType)
        fieldOff = ((fieldOff + align - 1) / align) * align
        deeperOpt.foreach { deeper =>
          emitNestedPatternCheck(deeper, deeperFieldType, scrutineeOffset, absOffset + dataOff + fieldOff, failLabel)
        }
        fieldOff += deeperFieldType.sizeOf.toInt
    case TDestructurePattern(st, _, _, deeperNested) =>
      for ((deeperOpt, i) <- deeperNested.zipWithIndex) do
        deeperOpt.foreach { deeper =>
          val deeperFieldType = st.fields(i)._2
          val off = fieldOffset(st, i)
          emitNestedPatternCheck(deeper, deeperFieldType, scrutineeOffset, absOffset + off, failLabel)
        }
    case _ => () // primitive nested patterns — treat as wildcard (analyzer guards this)

  // Recursively emit bindings for a nested match pattern. The outer
  // scrutinee value's address is loaded from `(fp + scrutineeOffset)`.
  // `absOffset` is the offset INSIDE that scrutinee where this nested
  // sub-value lives. For each named binding inside the nested pattern,
  // alloc a local and copy the corresponding field value (with refcount
  // increment for refcounted aggregates, matching the outer-binding
  // refcount discipline).
  private def emitNestedPatternBindings(
      pat: TMatchPattern,
      fieldType: SyslType,
      scrutineeOffset: Int,
      absOffset: Long,
  ): Unit = pat match
    case TVariantPattern(et, variantIndex, bindings, fieldTypes, deeperNested) =>
      val variantFields = et.variants(variantIndex)._2
      val dataOff = et.dataOffset.toInt
      var fieldOff = 0
      for (((binding, ft), i) <- bindings.zip(fieldTypes).zipWithIndex) do
        val align = stackAlign(ft)
        fieldOff = ((fieldOff + align - 1) / align) * align
        binding.foreach { name =>
          val local = allocLocal(name, ft)
          emitAddImm(1, 5, scrutineeOffset)
          emit("  ldd r1, r1, r0")        // r1 = scrutinee enum address
          val totalOff = absOffset + dataOff + fieldOff
          if totalOff != 0 then emitAddImm(1, 1, totalOff.toInt)
          ft match
            case SyslType.StringType | _: SyslType.StructType | _: SyslType.EnumType
              | _: SyslType.SliceType | _: SyslType.ArrayType
              | _: SyslType.FuncType | _: SyslType.InterfaceType =>
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, ft)
              ft match
                case SyslType.StringType if needsAllocExtern =>
                  emitAddImm(1, 5, local.offset)
                  emit("  ldd r1, r1, r0")
                  emitRefIncr(1, 8)
                case st2: SyslType.StructType if structHasStringFields(st2) =>
                  emitStructStringFieldsRC(5, local.offset, st2, incr = true)
                case et2: SyslType.EnumType if structHasStringFields(et2) =>
                  emitEnumStringFieldsRC(5, local.offset, et2, incr = true)
                case _ =>
            case _ =>
              emitLoad(1, 1, ft)
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, ft)
        }
        // Recurse into deeper nested for THIS field
        if i < deeperNested.length then deeperNested(i).foreach { deeper =>
          emitNestedPatternBindings(deeper, ft, scrutineeOffset, absOffset + dataOff + fieldOff)
        }
        fieldOff += ft.sizeOf.toInt
    case TDestructurePattern(st, bindings, fieldTypes, deeperNested) =>
      for (((binding, ft), i) <- bindings.zip(fieldTypes).zipWithIndex) do
        val off = fieldOffset(st, i)
        binding.foreach { name =>
          val local = allocLocal(name, ft)
          emitAddImm(1, 5, scrutineeOffset)
          emit("  ldd r1, r1, r0")        // r1 = scrutinee struct address
          val totalOff = absOffset + off
          if totalOff != 0 then emitAddImm(1, 1, totalOff.toInt)
          ft match
            case SyslType.StringType | _: SyslType.StructType | _: SyslType.EnumType
              | _: SyslType.SliceType | _: SyslType.ArrayType
              | _: SyslType.FuncType | _: SyslType.InterfaceType =>
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, ft)
              ft match
                case SyslType.StringType if needsAllocExtern =>
                  emitAddImm(1, 5, local.offset)
                  emit("  ldd r1, r1, r0")
                  emitRefIncr(1, 8)
                case st2: SyslType.StructType if structHasStringFields(st2) =>
                  emitStructStringFieldsRC(5, local.offset, st2, incr = true)
                case et2: SyslType.EnumType if structHasStringFields(et2) =>
                  emitEnumStringFieldsRC(5, local.offset, et2, incr = true)
                case _ =>
            case _ =>
              emitLoad(1, 1, ft)
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, ft)
        }
        if i < deeperNested.length then deeperNested(i).foreach { deeper =>
          emitNestedPatternBindings(deeper, ft, scrutineeOffset, absOffset + off)
        }
    case _ => () // wildcards / primitives — nothing to bind

  // Refcount header offset: refcount is at [ptr - headerOffset]
  // Structs: 8 (just refcount), Slices: 16 (refcount + length), Strings: 8 (just refcount, length in fat pointer)
  private def refHeaderOffset(typ: SyslType): Int = typ match
    case SyslType.RefType(SyslType.SliceType(_)) => 16
    case SyslType.StringType => 8
    case _ => 8

  // Get deinit function name for a ref type, if one exists
  private def deinitFor(typ: SyslType): Option[String] = typ match
    case SyslType.RefType(st @ SyslType.StructType(name, _, _)) if deinitFunctions.contains(name) =>
      Some(deinitFunctions(name))
    case SyslType.RefType(st: SyslType.StructType) =>
      structDeinitFor(st)
    case SyslType.RefType(SyslType.SliceType(elem)) =>
      sliceDeinitFor(elem)
    case SyslType.RefType(et: SyslType.EnumType) =>
      enumDeinitFor(et)
    case _ => None

  // Mangled name for a per-enum-type deinit function. Registers the type so the
  // function body is emitted at the end of generate(). Returns None if no
  // variant carries rc content (no walk needed; the buffer is just freed).
  private def enumDeinitFor(et: SyslType.EnumType): Option[String] =
    if !structHasStringFields(et) then None
    else
      val name = s"__enum_deinit_${modulePrefix}_${et.name}".replace("__", "_")
      enumDeinitsNeeded(name) = et
      Some(name)

  // Auto-synthesized struct deinit: walks string fields before free. Only
  // registered for structs with no user-defined deinit; if the user writes
  // `TypeName.deinit(...)` that takes precedence via `deinitFunctions`.
  private def structDeinitFor(st: SyslType.StructType): Option[String] =
    if !structHasStringFields(st) then None
    else
      val name = s"__struct_deinit_${modulePrefix}_${st.name}".replace("__", "_")
      structDeinitsNeeded(name) = st
      Some(name)

  // True if a slice element type carries refcounted content that must be decr'd
  // before the backing buffer is freed.
  private def sliceElemNeedsDeinit(elem: SyslType): Boolean = elem match
    case SyslType.StringType => true
    case _: SyslType.RefType => true
    case st: SyslType.StructType => structHasStringFields(st)
    case et: SyslType.EnumType => structHasStringFields(et)
    case SyslType.ArrayType(e, _) => sliceElemNeedsDeinit(e)
    case _ => false

  // Mangled name for a per-elem-type slice deinit function. Registers the type so
  // the function body is emitted at the end of generate(). Returns None for elem
  // types that hold no rc content.
  private def sliceDeinitFor(elem: SyslType): Option[String] =
    if !sliceElemNeedsDeinit(elem) then None
    else
      val tag = mangleType(elem)
      val name = s"__slice_deinit_${modulePrefix}_$tag".replace("__", "_")
      sliceElemDeinitsNeeded(name) = elem
      Some(name)

  // Stable, asm-safe tag for a type. Used to mangle slice deinit names.
  private def mangleType(t: SyslType): String = t match
    case SyslType.StringType => "string"
    case SyslType.RefType(inner) => s"ref_${mangleType(inner)}"
    case SyslType.StructType(n, _, _) => s"struct_$n"
    case SyslType.EnumType(n, _) => s"enum_$n"
    case SyslType.ArrayType(e, n) => s"arr${n}_${mangleType(e)}"
    case other => other.getClass.getSimpleName.toLowerCase

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
    needsFreeExtern = true
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
    // refcount == 0 → write -1 sentinel, call deinit, free(base). Writing -1
    // BEFORE deinit prevents a self-referential cycle: e.g., a closure-env's
    // self-cell capture stores a copy of the descriptor whose env_ptr is the
    // same env. When the deinit walks that capture and tries to decr the env
    // again, the sentinel check (refcount == -1 → skip) makes it a no-op.
    emit("  addi r4, r0, -1")         // r4 = -1 (sentinel)
    emit("  std r4, r3, r0")          // rc = -1 (mark as being freed)
    emit("  pshd r1")                 // save r1
    deinitFunc.foreach { name =>
      // Call deinit(dataPtr) — dataPtr is ptrReg (past header). r3 holds the
      // base ptr we'll need for free(); the deinit may clobber r3, so save it.
      emit("  pshd r3")
      emit(s"  mov r1, r$ptrReg")     // r1 = data pointer (self)
      emit(s"  movi r4, $name")
      emit("  jalr r6, r4")
      emit("  popd r3")
    }
    emit("  mov r1, r3")              // r1 = base pointer (for free)
    emit("  movi r4, free")
    emit("  jalr r6, r4")
    emit("  popd r1")                 // restore r1
    emit(s"$noFree")
    emit(s"$skip")

  /** Scan the program for any expression that would require heap allocation at
    * runtime (string concat, string(ptr,len), TStr/TFmtStr, new, escaping closure).
    * Used to set needsAllocExtern upfront so rc-bracket gates are correct from
    * the start of codegen — otherwise a leak: an assignment that needs decr-old
    * runs before the value's expression flips needsAllocExtern, so the decr is
    * skipped. Lazily-set flags + sequential codegen don't compose. */
  private def scanNeedsAlloc(program: TProgram): Boolean =
    // Names of functions declared in this unit — calls to anything NOT in this
    // set are external (different compilation unit) and may return a heap-env
    // closure we can't inspect. Used to flag TCall-returning-FuncType
    // conservatively without over-pulling extern free for purely-local programs.
    val localFuncs = program.decls.collect { case TFunDecl(n, _, _, _, _, _, _, _, _, _) => n }.toSet
    def scanE(e: TExpr): Boolean = e match
      case TBinary(_, "+", _, SyslType.StringType) => true
      case _: TStringFromPtr | _: TStringFromSlice | _: TStr | _: TFmtStr => true
      case _: TNew | _: TNewArray | _: TNewEnum => true
      case c: TClosure =>
        // Only a heap-env closure pulls in malloc/free. Stack-env closures
        // (non-escaping, non-rc-bearing captures) live on the caller's frame.
        val needsHeap = c.escapes || c.captures.exists((_, t) =>
          t.isInstanceOf[SyslType.RefType] || structHasStringFields(t))
        needsHeap || (c.body match
          case TExprBody(ex) => scanE(ex)
          case TBlockBody(ss) => ss.exists(scanS))
      case TBinary(l, _, r, _) => scanE(l) || scanE(r)
      case TUnary(_, op, _) => scanE(op)
      // Cross-unit call returning FuncType: callee may be producing a heap-env
      // closure we can't inspect. Without needsAllocExtern=true here, the caller
      // would skip the scope-exit decr and leak the env. Local calls don't need
      // this — if a local function returns a heap closure, scanNeedsAlloc already
      // visits its body through the TFunDecl walk below.
      case TCall(name, args, retType) =>
        (retType.isInstanceOf[SyslType.FuncType] && !localFuncs.contains(name)) ||
          args.exists(scanE)
      // Indirect and interface calls always go through an opaque function pointer
      // whose body we can't inspect — treat FuncType return as possibly heap-env.
      case TIndirectCall(callee, args, retType) =>
        retType.isInstanceOf[SyslType.FuncType] || scanE(callee) || args.exists(scanE)
      case TInterfaceDispatch(obj, _, args, retType) =>
        retType.isInstanceOf[SyslType.FuncType] || scanE(obj) || args.exists(scanE)
      case TInterfaceBox(inner, _) => scanE(inner)
      case TIntrinsicCall(_, args, _) => args.exists(scanE)
      case TIfExpr(c, t, e, _) => scanE(c) || t.exists(scanS) || e.exists(_.exists(scanS))
      case TQuantifier(_, _, _, lo, hi, _, pred, _) => scanE(lo) || scanE(hi) || scanE(pred)
      case TMatchExpr(scr, arms, default, _) =>
        scanE(scr) || arms.exists(a => a.guard.exists(scanE) || a.body.exists(scanS)) ||
          default.exists(_.exists(scanS))
      case TStructConstruct(_, args) => args.exists(scanE)
      case TEnumConstruct(_, _, args) => args.exists(scanE)
      case TArrayLit(elems, _) => elems.exists(scanE)
      case TFieldAccess(obj, _, _) => scanE(obj)
      case TFieldPreInc(obj, _, _) => scanE(obj)
      case TFieldPreDec(obj, _, _) => scanE(obj)
      case TFieldPostInc(obj, _, _) => scanE(obj)
      case TFieldPostDec(obj, _, _) => scanE(obj)
      case TIndex(arr, idx, _) => scanE(arr) || scanE(idx)
      case TSliceExpr(_, _, _, SyslType.StringType) => true   // substring allocates new buffer
      case TSliceExpr(arr, lo, hi, _) => scanE(arr) || lo.exists(scanE) || hi.exists(scanE)
      case TAppend(slice, elem, _) => scanE(slice) || scanE(elem)
      case TDeref(p, _) => scanE(p)
      case TCast(inner, _) => scanE(inner)
      case TLen(a, _) => scanE(a)
      case TCap(a, _) => scanE(a)
      case TTempAddr(inner, _) => scanE(inner)
      case TAddrOfIndex(arr, idx, _) => scanE(arr) || scanE(idx)
      case TAddrOfField(obj, _, _) => scanE(obj)
      case _ => false

    def scanS(s: TStmt): Boolean = s match
      case TVarStmt(_, _, init, _, _) => scanE(init)
      case TAssignStmt(_, value) => scanE(value)
      case TFieldAssignStmt(obj, _, value) => scanE(obj) || scanE(value)
      case TIndexAssignStmt(arr, idx, value) => scanE(arr) || scanE(idx) || scanE(value)
      case TDerefAssignStmt(ptr, value) => scanE(ptr) || scanE(value)
      case TCompoundAssignStmt(_, _, value) => scanE(value)
      case TFieldCompoundAssignStmt(obj, _, _, value) => scanE(obj) || scanE(value)
      case TExprStmt(e) => scanE(e)
      case TReturnStmt(Some(e)) => scanE(e)
      case TWhileStmt(c, body, _) => scanE(c) || body.exists(scanS)
      case TDoWhileStmt(c, body, _) => scanE(c) || body.exists(scanS)
      case TForStmt(init, c, upd, body, _) => scanS(init) || scanE(c) || scanS(upd) || body.exists(scanS)
      case TLoopStmt(body, _) => body.exists(scanS)
      case TDeferStmt(stmt) => scanS(stmt)
      case TDestructureStmt(_, _, init) => scanE(init)
      case TDestructureAssignStmt(_, _, init) => scanE(init)
      case _ => false

    program.decls.exists {
      case TFunDecl(_, _, _, body, _, _, _, _, _, _) => body match
        case TExprBody(e) => scanE(e)
        case TBlockBody(stmts) => stmts.exists(scanS)
      case TVarDecl(_, _, init, _, _, _, _) => scanE(init)
      case _ => false
    }

  // True if a value type (recursively) holds any rc-bearing content: string buffers
  // or closure descriptors with heap envs. Stops at refs/pointers/slices (handled by
  // their own paths). Recurses through value-struct fields, value-array elements,
  // and enum variant fields. FuncType is included because closure descriptors carry
  // an env_ptr that may point to a heap-allocated env (rc-tracked); the runtime
  // null-check in emitRefDecr makes always-decr safe for NullEnv descriptors too.
  private def structHasStringFields(t: SyslType): Boolean = t match
    case SyslType.StringType => true
    case _: SyslType.FuncType => true
    case st: SyslType.StructType =>
      st.fields.exists((_, ft) => structHasStringFields(ft))
    case SyslType.ArrayType(elem, _) => structHasStringFields(elem)
    case et: SyslType.EnumType =>
      et.variants.exists((_, fields) => fields.exists((_, ft) => structHasStringFields(ft)))
    case _ => false

  // True if an enum variant's field list carries any string content.
  private def variantHasStringFields(fields: List[(String, SyslType)]): Boolean =
    fields.exists((_, ft) => structHasStringFields(ft))

  // Expressions that produce a freshly-owned string buffer (rc=1 or immortal).
  private def isOwnedStringExpr(expr: TExpr): Boolean = expr match
    case _: TStringLit => true
    case TBinary(_, "+", _, SyslType.StringType) => true
    case TSliceExpr(_, _, _, SyslType.StringType) => true
    case _: TStringFromPtr | _: TStringFromSlice => true
    case _: TCall | _: TIndirectCall => true
    case _: TIfExpr | _: TMatchExpr => true
    case _ => false

  // Expressions that produce a freshly-constructed value struct or enum (string fields
  // already owned by the new aggregate — no copy-incr needed).
  private def isOwnedStructExpr(expr: TExpr): Boolean = expr match
    case _: TStructConstruct => true
    case _: TEnumConstruct => true
    case _: TCall | _: TIndirectCall => true
    case _: TIfExpr | _: TMatchExpr => true
    case _ => false

  // Expressions that produce a freshly-owned closure descriptor (caller has the only
  // share of the env — no copy-incr needed when storing into a struct/enum field).
  // TVarRef and TFieldAccess fall through as "borrowed" — incr needed.
  private def isOwnedClosureExpr(expr: TExpr): Boolean = expr match
    case _: TClosure => true       // fresh env (rc=1 from malloc) or NullEnv
    case _: TFuncRef => true       // NullEnv (no env to track)
    case _: TCall | _: TIndirectCall | _: TInterfaceDispatch => true
    case _: TIfExpr | _: TMatchExpr => true
    case _ => false

  // Increment or decrement the RC of every string field (recursively into nested
  // value-struct fields and value-array elements) inside a struct at [r{baseReg} + baseOff].
  // Clobbers r3, r4 (via emitRefIncr/Decr) and uses r1 for the field ptr. baseReg
  // is preserved if it is not r1 (callers should use r5/fp for locals).
  private def emitStructStringFieldsRC(baseReg: Int, baseOff: Int, st: SyslType.StructType, incr: Boolean): Unit =
    if !needsAllocExtern then return
    for case ((_, ft), i) <- st.fields.zipWithIndex do
      val foff = baseOff + fieldOffset(st, i)
      emitValueRC(baseReg, foff, ft, incr)

  // Generalized RC walker for any value type at [r{baseReg} + baseOff]. Handles
  // strings, value structs (recurses), value arrays (loops over elements), and
  // value enums (runtime tag-dispatch). No-op for any type without string content.
  private def emitValueRC(baseReg: Int, baseOff: Int, t: SyslType, incr: Boolean): Unit =
    if !needsAllocExtern then return
    t match
      case SyslType.StringType =>
        emit("  pshd r1")
        emitAddImm(1, baseReg, baseOff)
        emit("  ldd r1, r1, r0")
        if incr then emitRefIncr(1, 8) else emitRefDecr(1, 8)
        emit("  popd r1")
      case _: SyslType.FuncType =>
        // Closure descriptor at [baseReg+baseOff] is 16 bytes {func_ptr, env_ptr}.
        // env_ptr is null for NullEnv closures (descr just literal func ptr) — the
        // null-check inside emitRefDecr makes always-walk safe. emitClosureDescr*
        // operates on the descriptor's address.
        if incr then emitClosureDescrIncr(baseReg, baseOff)
        else emitClosureDescrDecr(baseReg, baseOff)
      case nested: SyslType.StructType if structHasStringFields(nested) =>
        emitStructStringFieldsRC(baseReg, baseOff, nested, incr)
      case SyslType.ArrayType(elem, count) if structHasStringFields(elem) =>
        val es = stackSize(elem)
        for i <- 0 until count do
          emitValueRC(baseReg, baseOff + i * es, elem, incr)
      case et: SyslType.EnumType if structHasStringFields(et) =>
        emitEnumStringFieldsRC(baseReg, baseOff, et, incr)
      case _ =>

  // Walk the active variant's string-bearing fields of an enum at
  // [r{baseReg} + baseOff], inc/dec each via emitValueRC. Loads tag (i32 @ 0)
  // then chained compare-branches per variant that carries strings; variants
  // with no string content are skipped entirely. Uses r3 (tag) and r4 (cmp)
  // as scratch — preserved across emitValueRC inner calls because we reload
  // nothing after the per-variant branch is taken (each match falls through
  // straight to the variant walk and then jumps to end).
  private def emitEnumStringFieldsRC(baseReg: Int, baseOff: Int, et: SyslType.EnumType, incr: Boolean): Unit =
    if !needsAllocExtern then return
    val variantsWithStrings = et.variants.zipWithIndex.collect {
      case ((_, fields), idx) if variantHasStringFields(fields) => (fields, idx)
    }
    if variantsWithStrings.isEmpty then return
    val dataOff = et.dataOffset.toInt
    val endLabel = newLabel("enum_rc_end")
    // r3 = tag (sign-extended i32 load is fine; we only compare against small idx)
    emitAddImm(3, baseReg, baseOff)
    emit("  ldw r3, r3, r0")
    for (fields, idx) <- variantsWithStrings do
      val nextLabel = newLabel("enum_rc_next")
      emitLoadImm(4, idx)
      emit(s"  bne r3, r4, $nextLabel")
      // Walk variant fields at correct offsets within et.dataOffset
      var fieldOff = 0
      for (_, fieldType) <- fields do
        val align = stackAlign(fieldType)
        fieldOff = ((fieldOff + align - 1) / align) * align
        if structHasStringFields(fieldType) then
          emitValueRC(baseReg, baseOff + dataOff + fieldOff, fieldType, incr)
        fieldOff += fieldType.sizeOf.toInt
      emit(s"  bra $endLabel")
      emit(s"$nextLabel")
    emit(s"$endLabel")

  /** Emit a per-elem-type slice deinit function. Called when freeing a
    * `&[]T` whose elements carry rc content. Slice block layout:
    * [rc:8 | len:8 | data...]. The function receives r1 = data ptr (past the
    * 16-byte header), decrs every element, then returns. Length is read from
    * [r1-8]; element[i] sits at [r1 + i*elemSize].
    *
    * r2 holds the current element address across iterations and is preserved
    * by emitValueRC. r3 (loop counter) is clobbered by emitRefDecr's free path
    * so it gets pushed/popped around the per-element decr.
    */
  private def emitSliceDeinit(name: String, elem: SyslType): Unit =
    val es = stackSize(elem)
    val loop = newLabel("slice_deinit_loop")
    val done = newLabel("slice_deinit_done")
    emit(s"# slice element deinit: $name")
    emit(s"global $name, func, 1 i64 i64")
    emit(s"$name:")
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    // r2 = current element addr = data ptr = r1
    emit("  mov r2, r1")
    // r3 = remaining count = *(r1 - 8)
    emitAddImm(3, 1, -8)
    emit("  ldd r3, r3, r0")
    emit(s"$loop")
    emit(s"  beq r3, r0, $done")
    emit("  pshd r3")              // save count (emitRefDecr clobbers r3)
    emit("  pshd r2")              // save addr
    emitValueRC(2, 0, elem, incr = false)
    emit("  popd r2")
    emit("  popd r3")
    emitAddImm(2, 2, es)
    emit("  addi r3, r3, -1")
    emit(s"  bra $loop")
    emit(s"$done")
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emit("  jalr r0, r6")

  /** Auto-synthesized per-struct-type deinit. Called when freeing a `&MyStruct`
    * whose fields carry rc content and the user hasn't defined a custom
    * `TypeName.deinit`. r1 = data ptr (past the 8-byte rc header). Walks string
    * fields via emitStructStringFieldsRC and returns. Buffer itself is freed by
    * emitRefDecr's free() call after this returns.
    */
  private def emitStructDeinit(name: String, st: SyslType.StructType): Unit =
    emit(s"# struct deinit: $name")
    emit(s"global $name, func, 1 i64 i64")
    emit(s"$name:")
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    // Save data ptr at fp-8; reload before the walk so inner free's don't
    // clobber it (same protection as the enum deinit uses).
    emit("  pshd r1")
    emitAddImm(1, 5, -8)
    emit("  ldd r1, r1, r0")
    emitStructStringFieldsRC(1, 0, st, incr = false)
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emit("  jalr r0, r6")

  /** Per-enum-type deinit function. Called when freeing a `&MyEnum` whose
    * active variant carries rc content (string fields, nested struct/enum/array
    * with strings). r1 = data ptr (past the 8-byte rc header). Walks the active
    * variant via emitEnumStringFieldsRC (tag-dispatched), then returns. Buffer
    * itself is freed by emitRefDecr's free() call after this returns.
    */
  private def emitEnumDeinit(name: String, et: SyslType.EnumType): Unit =
    emit(s"# enum deinit: $name")
    emit(s"global $name, func, 1 i64 i64")
    emit(s"$name:")
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    // r1 = data ptr (input). emitEnumStringFieldsRC(baseReg=1, baseOff=0)
    // reads tag from [r1] and walks the matching variant. emitValueRC saves
    // and restores r1 around inner decr's, so r1 is preserved across inner
    // calls — but emitRefDecr's free path may clobber it after the popd.
    // Save data ptr at fp-8 and use that copy to keep things simple.
    emit("  pshd r1")                    // save data ptr at fp-8 (r5-8)
    // Reload r1 from saved slot — emitEnumStringFieldsRC will pshd/popd r1
    // around each inner decr, but emitRefDecr inside that decr does not
    // touch r1's saved slot at fp-8.
    emitAddImm(1, 5, -8)
    emit("  ldd r1, r1, r0")             // r1 = data ptr (refreshed)
    emitEnumStringFieldsRC(1, 0, et, incr = false)
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emit("  jalr r0, r6")

  /** Closure env layout (heap):
    *   [rc:8 @ base+0 | deinit_ptr:8 @ base+8 | capture_data...]
    *   env_ptr = base + 16
    * So env_ptr-16 is &rc and base for free; env_ptr-8 is &deinit_ptr.
    *
    * __closure_env_dispatch: thin shim called when env's rc hits zero. Loads
    * the runtime deinit_ptr from env_ptr-8 and tail-calls it. Lets emitRefDecr
    * handle envs uniformly (one named deinit) without per-closure-id static
    * deinit linkage on the call site. r1 = env_ptr (data ptr) on entry.
    */
  private def emitClosureEnvDispatch(): Unit =
    emit("# closure env dispatch (loads deinit_ptr from env-8 and calls it)")
    emit("global __closure_env_dispatch, func, 1 i64 i64")
    emit("__closure_env_dispatch:")
    val noCall = newLabel("dispatch_no_call")
    emit("  pshd r6")                  // save caller's return addr
    emit("  addi r3, r1, -8")
    emit("  ldd r3, r3, r0")           // r3 = deinit_ptr
    emit(s"  beq r3, r0, $noCall")
    emit("  jalr r6, r3")              // call deinit(env_ptr); r1 already env_ptr
    emit(s"$noCall")
    emit("  popd r6")
    emit("  jalr r0, r6")

  /** Per-closure-id env deinit: walks the closure's captures (known layout) and
    * decr's any rc-bearing entry via emitValueRC. r1 = env_ptr (data ptr; env-8
    * may still hold this fn's address — irrelevant). Only registered for
    * closures with at least one rc-bearing capture; closures with none use
    * deinit_ptr=null and the dispatch shim no-ops.
    *
    * env_ptr is saved at fp-8 and reloaded into r2 BEFORE each per-capture
    * emitValueRC call, because emitValueRC may transitively call free which
    * clobbers r2 (caller-saved). Without the reload, the second capture's base
    * address would be garbage, corrupting the heap.
    */
  private def emitClosureEnvDeinit(name: String, closure: TClosure): Unit =
    emit(s"# closure env deinit: $name")
    emit(s"global $name, func, 1 i64 i64")
    emit(s"$name:")
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    emit("  pshd r1")                  // save env_ptr at fp-8 (r5-8)
    // Walk captures at their natural alignment so offsets match the env-store layout.
    var envOffset = 0
    for (_, capType) <- closure.captures do
      val capAlign = stackAlign(capType)
      envOffset = ((envOffset + capAlign - 1) / capAlign) * capAlign
      if structHasStringFields(capType) then
        emitAddImm(2, 5, -8)            // r2 = &saved env_ptr
        emit("  ldd r2, r2, r0")        // r2 = env_ptr
        emitValueRC(2, envOffset, capType, incr = false)
      envOffset += stackSize(capType).toInt
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emit("  jalr r0, r6")

  /** Register a per-closure-id env deinit if the closure has rc-bearing
    * captures. Returns the deinit function name, or None if all captures are
    * scalar/raw (no walk needed).
    */
  private def closureEnvDeinitFor(name: String, closure: TClosure): Option[String] =
    val hasRcCaptures = closure.captures.exists((_, t) => structHasStringFields(t) || t.isInstanceOf[SyslType.RefType])
    if !hasRcCaptures then None
    else
      val deinitName = s"__closure_env_deinit_$name"
      closureEnvDeinitsNeeded(deinitName) = closure
      Some(deinitName)

  /** Decrement env's rc via the descriptor's env_ptr (offset +8 in descriptor).
    * descrAddr is the descriptor's address (16 bytes: fp at +0, env_ptr at +8).
    * Loads env_ptr; if non-null, calls emitRefDecr with headerOff=16 and the
    * generic dispatch deinit.
    */
  private def emitClosureDescrDecr(descBaseReg: Int, descBaseOff: Int): Unit =
    closureEnvDispatchNeeded = true
    needsFreeExtern = true
    emit("  pshd r1")
    emitAddImm(1, descBaseReg, descBaseOff + 8)  // r1 = &env_ptr
    emit("  ldd r1, r1, r0")                     // r1 = env_ptr
    emitRefDecr(1, 16, Some("__closure_env_dispatch"))
    emit("  popd r1")

  /** Increment env's rc via descriptor's env_ptr. Used when copying a
    * descriptor (var g = f) so both descriptors share the env. */
  private def emitClosureDescrIncr(descBaseReg: Int, descBaseOff: Int): Unit =
    emit("  pshd r1")
    emitAddImm(1, descBaseReg, descBaseOff + 8)
    emit("  ldd r1, r1, r0")
    emitRefIncr(1, 16)
    emit("  popd r1")

  /** Evaluate `arg` and push its full byte representation onto the stack as a
    * call argument. Handles ref/string borrow incr; appends to `stringPtrOffsets`
    * the fp-relative offset of each pushed string ptr that needs a post-call decr.
    *
    * Per-type byte sizes pushed:
    *   string:   16  ({ptr, len})
    *   slice:    24  ({ptr, len+cap, backref})
    *   func/iface: 16  ({fn, env})
    *   struct/enum: aligned(stackSize)  (full byte copy — true pass-by-value)
    *   scalar:   8
    *
    * Note: struct args don't need caller incr/decr brackets — the callee copies
    * bytes into its own local at function entry and incr's string fields there. */
  private def evalAndPushArg(arg: TExpr, stringPtrOffsets: mutable.ListBuffer[Int]): Unit =
    val preOffset = stackOffset
    arg match
      case TAddrLit(off) => emitAddImm(1, 5, off)
      case _ => genExpr(arg)
    // Borrow-incr for ref/string args (caller decr's after the call)
    arg.typ match
      case rt: SyslType.RefType => arg match
        case _: TNew | _: TNewArray | _: TNewEnum =>
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
      if needsAllocExtern && !arg.isInstanceOf[TBinary] then
        stringPtrOffsets += stackOffset
    else if arg.typ.isInstanceOf[SyslType.SliceType] then
      emit("  ldd r2, r1, r0")
      emit("  addi r3, r1, 8")
      emit("  ldd r3, r3, r0")
      emit("  addi r4, r1, 16")
      emit("  ldd r4, r4, r0")
      val extra = preOffset - stackOffset
      if extra > 0 then
        emitAddImm(7, 7, extra)
        stackOffset = preOffset
      emit("  pshd r4")
      emit("  pshd r3")
      emit("  pshd r2")
      stackOffset -= 24
    else if arg.typ.isInstanceOf[SyslType.FuncType] || arg.typ.isInstanceOf[SyslType.InterfaceType] then
      emit("  ldd r2, r1, r0")
      emit("  addi r3, r1, 8")
      emit("  ldd r3, r3, r0")
      // Do NOT rewind the temp region (`extra = preOffset - stackOffset`)
      // before pushing. For an inline-constructed iface arg
      // (`use_shape(Square(9))`), `data_ptr` (r3) points INTO the temp
      // region — and the subsequent `pshd r3; pshd r2` would overwrite
      // the source struct data before the callee dereferences
      // `data_ptr`. Same hazard for stack-env closures passed as FuncType
      // args. Leak the temp until the function epilogue restores r7.
      // The TCall cleanup adds it all back in one go via
      // `argsAllocated = cleanupTo - stackOffset`, so accounting stays
      // correct.
      emit("  pshd r3")
      emit("  pshd r2")
      stackOffset -= 16
    else if arg.typ.isInstanceOf[SyslType.EnumType] || arg.typ.isInstanceOf[SyslType.StructType] then
      val aligned = (stackSize(arg.typ) + 7) & ~7
      // Compensate for any extra stack used by genExpr (e.g., a struct
      // constructor that materialised the value into a temporary). Without
      // this, subsequent args end up at the wrong offsets and the callee
      // reads garbage. Same pattern slice/string/scalar branches use.
      // r1 still points to the source bytes — they live in the just-popped
      // region until our copy reads them, and nothing in this loop writes
      // there before we ldd from it.
      val extra = preOffset - stackOffset
      if extra > 0 then
        emitAddImm(7, 7, extra)
        stackOffset = preOffset
      emitAddImm(7, 7, -aligned)
      stackOffset -= aligned
      for off <- 0 until aligned by 8 do
        emitAddImm(3, 1, off)
        emit("  ldd r3, r3, r0")
        emitAddImm(4, 7, off)
        emit("  std r3, r4, r0")
    else if isStructLikeTempAddr(arg) then
      // Method-on-temporary receiver passed as a stack arg (e.g. `outer(inner())`
      // where `inner()` returns a struct by value, and the outer takes the
      // struct's address). genExpr leaves r1 pointing INTO the inner call's
      // hidden return slot (which lives on the stack just above us). The
      // generic rewind below would reclaim the inner ret slot before the pshd,
      // and pshd would then write the pointer value INTO the freed slot it
      // points to — corrupting the struct data. Leak the inner ret slot until
      // the outer's final cleanup; the post-call `argsAllocated = cleanupTo -
      // stackOffset` reclaims it then.
      emit("  pshd r1")
      stackOffset -= 8
    else
      val extra = preOffset - stackOffset
      if extra > 0 then
        emitAddImm(7, 7, extra)
        stackOffset = preOffset
      emit("  pshd r1")
      stackOffset -= 8

  /** After a call returns, decrement the borrowed-string rcs whose ptrs were tracked
    * during arg push. Uses r1 for the ptr; preserves the call's return value. */
  private def emitStringArgDecr(stringPtrOffsets: List[Int]): Unit =
    for off <- stringPtrOffsets do
      emit("  pshd r1")            // save return value
      emitAddImm(1, 5, off)        // r1 = &ptr on stack (fp-relative)
      emit("  ldd r1, r1, r0")     // r1 = ptr
      emitRefDecr(1, 8)
      emit("  popd r1")            // restore return value

  // Decrement refcounts for all ref-typed and string-typed locals and params
  private def emitRefCleanup(): Unit =
    // Decrement owned locals (negative fp offsets)
    for (name, local) <- locals if local.offset < 0 do
      // Captured-into-body locals are borrowed views into the env — skip rc work.
      if captureBorrows.contains(name) then ()
      else local.typ match
        case rt: SyslType.RefType =>
          val hoff = refHeaderOffset(rt)
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset)
          emit("  ldd r1, r1, r0")
          emitRefDecr(1, hoff, deinitFor(rt))
          emit("  popd r1")
        case SyslType.StringType if needsAllocExtern =>
          // String params are borrowed (caller emitRefIncr before call, emitRefDecr after).
          if stringBorrowParams.contains(name) then ()
          else
            emit("  pshd r1")
            emitAddImm(1, 5, local.offset)
            emit("  ldd r1, r1, r0")       // r1 = ptr field
            emitRefDecr(1, 8)
            emit("  popd r1")
        case SyslType.SliceType(elem) =>
          // Decrement backref (at slice offset +16) if non-null. See the
          // matching block in leaveScope for the convention details.
          emit("  pshd r1")
          emitAddImm(1, 5, local.offset + 16)
          emit("  ldd r1, r1, r0")       // r1 = backref (= base)
          val deinit = sliceDeinitFor(elem)
          if deinit.isDefined then
            emitAddImm(1, 1, 16)         // r1 = data ptr
            emitRefDecr(1, 16, deinit)
          else
            emitRefDecr(1, 0)
          emit("  popd r1")
        case st: SyslType.StructType if structHasStringFields(st) =>
          emitStructStringFieldsRC(5, local.offset, st, incr = false)
        case SyslType.ArrayType(elem, _) if structHasStringFields(elem) =>
          emitValueRC(5, local.offset, local.typ, incr = false)
        case et: SyslType.EnumType if structHasStringFields(et) =>
          emitEnumStringFieldsRC(5, local.offset, et, incr = false)
        case _: SyslType.FuncType
            if !funcBorrowParams.contains(name)
            && closureLocalKind.get(name).contains(FuncKind.HeapEnv) =>
          // Closure descriptor with heap-allocated env: decr env's rc.
          // Stack-env / null-env / unknown-kind locals skip — only HeapEnv needs free.
          emitClosureDescrDecr(5, local.offset)
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
    // String params: skipped in locals loop via stringBorrowParams (borrowed, not owned).

  // Allocate a local variable on the stack, return its offset from fp.
  // The variable is aligned to the greater of its natural alignment and 8
  // (pshd/popd require SP to stay 8-byte aligned).
  private def allocLocal(name: String, typ: SyslType): LocalVar =
    allocLocal(name, typ, stackSize(typ))

  private def allocLocal(name: String, typ: SyslType, size: Int): LocalVar =
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
    stringBorrowParams = fun.params.collect { case p if p.typ == SyslType.StringType => p.name }.toSet
    funcBorrowParams = fun.params.collect {
      case p if p.typ.isInstanceOf[SyslType.FuncType] || p.typ.isInstanceOf[SyslType.InterfaceType] => p.name
    }.toSet
    closureLocalKind.clear()
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

    // Map user params to their stack locations.
    // Scalar params use I64 because the ABI passes all scalars as 8-byte values
    // (via pshd/register), and TRISC is big-endian so ldw at the slot base would
    // read the wrong half. The 8-byte slot matches the 8-byte load.
    val userParamRegStart = if structReturn then 1 else 0
    val userRegParams = fun.params.length.min(1 - userParamRegStart)
    for (param, i) <- fun.params.take(userRegParams).zipWithIndex do
      val regIndex = userParamRegStart + i
      val callerOffset = 16 + (nRegPushed - 1 - regIndex) * 8
      locals(param.name) = LocalVar(param.name, callerOffset, SyslType.I64)
    // Stack params: those beyond register capacity
    // String params take 16 bytes, slice params 24 bytes, struct/enum params take their
    // aligned size (caller pushed full bytes), others 8.
    val nUserStackStart = 1 - userParamRegStart
    var stackParamOffset = 16 + nRegPushed * 8
    for param <- fun.params.drop(nUserStackStart) do
      if param.typ == SyslType.StringType then
        locals(param.name) = LocalVar(param.name, stackParamOffset, SyslType.StringType)
        stackParamOffset += 16
      else if param.typ.isInstanceOf[SyslType.SliceType] then
        locals(param.name) = LocalVar(param.name, stackParamOffset, param.typ)
        stackParamOffset += 24
      else if param.typ.isInstanceOf[SyslType.StructType] || param.typ.isInstanceOf[SyslType.EnumType] then
        val aligned = (stackSize(param.typ) + 7) & ~7
        locals(param.name) = LocalVar(param.name, stackParamOffset, param.typ)
        stackParamOffset += aligned
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

    // Copy FuncType/InterfaceType params into local 16-byte slots (same as strings).
    for (param, i) <- fun.params.zipWithIndex if param.typ.isInstanceOf[SyslType.FuncType] || param.typ.isInstanceOf[SyslType.InterfaceType] do
      val srcLocal = locals(param.name)
      val isRegParam = i < (1 - userParamRegStart)
      emitAddImm(7, 7, -16)
      stackOffset -= 16
      val funcLocal = LocalVar(param.name, stackOffset, param.typ)
      if isRegParam then
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r1, r1, r0")        // r1 = caller's func pair address
        emit("  ldd r2, r1, r0")        // r2 = func_ptr
        emit("  std r2, r7, r0")        // store func_ptr at local+0
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")        // r2 = env_ptr
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")        // store env_ptr at local+8
      else
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r2, r1, r0")        // r2 = func_ptr
        emit("  std r2, r7, r0")
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")        // r2 = env_ptr
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
      locals(param.name) = funcLocal

    // Copy struct/enum params into local frame slots so mutation by the callee is
    // contained and string-field rcs can be cleaned up uniformly via emitRefCleanup.
    // Register struct params: srcLocal holds an 8-byte address → dereference and copy.
    // Stack struct params: bytes are already at fp+srcLocal.offset → copy directly.
    for (param, i) <- fun.params.zipWithIndex if param.typ.isInstanceOf[SyslType.StructType] || param.typ.isInstanceOf[SyslType.EnumType] do
      val srcLocal = locals(param.name)
      val isRegParam = i < (1 - userParamRegStart)
      val aligned = (stackSize(param.typ) + 7) & ~7
      emitAddImm(7, 7, -aligned)
      stackOffset -= aligned
      val newLocal = LocalVar(param.name, stackOffset, param.typ)
      if isRegParam then
        // Register param: srcLocal holds an 8-byte address → load it into r1 first
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r1, r1, r0")        // r1 = caller's struct address
        for off <- 0 until aligned by 8 do
          emitAddImm(2, 1, off)
          emit("  ldd r2, r2, r0")
          emitAddImm(3, 7, off)
          emit("  std r2, r3, r0")
      else
        // Stack param: bytes directly at fp+srcLocal.offset
        for off <- 0 until aligned by 8 do
          emitAddImm(2, 5, srcLocal.offset + off)
          emit("  ldd r2, r2, r0")
          emitAddImm(3, 7, off)
          emit("  std r2, r3, r0")
      locals(param.name) = newLocal
      // Incr string fields of the local copy (callee owns its share of each field's buffer)
      param.typ match
        case st: SyslType.StructType if structHasStringFields(st) =>
          emitStructStringFieldsRC(5, newLocal.offset, st, incr = true)
        case _ =>

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        genExpr(expr) // result in r1
        emitFuncReturnIncrIfBorrowed(expr)
        if structReturn then emitStructReturn()
        emitDefers()
        emitRefCleanup()
        emitEpilogue()
      case TBlockBody(stmts) =>
        genBlock(stmts)

    locals = null
    currentFunction = null
    stringBorrowParams = Set.empty
    funcBorrowParams = Set.empty

  /** When the function returns a borrowed FuncType local (param OR HeapEnv
    * local), incr the env so the caller's TCall=HeapEnv decr balances. r1 must
    * hold the descriptor address at call time. No-op for any other expression. */
  private def emitFuncReturnIncrIfBorrowed(value: TExpr): Unit =
    value match
      case TVarRef(_, t) if t.isInstanceOf[SyslType.FuncType]
        && funcKindOfExpr(value) == FuncKind.HeapEnv =>
        emitClosureDescrIncr(1, 0)
      case _ =>

  private def genClosureFunction(name: String, closure: TClosure): Unit =
    // Create a TFunDecl for the closure so we can reuse epilogue/return machinery
    val fun = TFunDecl(name, closure.params, closure.returnType, closure.body, isPrivate = false)
    currentFunction = fun
    inClosureBody = true
    locals = new mutable.LinkedHashMap
    refParams = new mutable.LinkedHashMap
    stringBorrowParams = fun.params.collect { case p if p.typ == SyslType.StringType => p.name }.toSet
    funcBorrowParams = fun.params.collect {
      case p if p.typ.isInstanceOf[SyslType.FuncType] || p.typ.isInstanceOf[SyslType.InterfaceType] => p.name
    }.toSet
    closureLocalKind.clear()
    captureBorrows = closure.captures.map(_._1).toSet
    stackOffset = 0
    deferStack.clear()

    val structReturn = returnsViaPointer(fun.returnType)

    emit(s"global $name")
    emit(s"# closure: $name")
    emit(s"$name:")

    // ABI: same as regular function — r1 = first arg (or hidden return ptr), r3 = env_ptr
    val allRegSlots = if structReturn then 1 + fun.params.length else fun.params.length
    val nRegPushed = allRegSlots.min(1)
    for i <- 0 until nRegPushed do
      emit(s"  pshd r${i + 1}")

    // Save env_ptr (r3) before prologue clobbers it
    emit("  pshd r3")

    // Prologue
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")

    // Map hidden return pointer
    // Stack layout from fp: [saved_r5] [saved_r6] [saved_r3(env)] [pushed_r1(if any)] [caller stack args...]
    val retPtrOffset = if structReturn then
      val off = 16 + 8 + (nRegPushed - 1) * 8  // +8 for the saved r3
      locals("_ret_ptr") = LocalVar("_ret_ptr", off, SyslType.PtrType(fun.returnType))
      off
    else -1

    // Map user params — register params are above saved lr/fp/env on the stack
    val userParamRegStart = if structReturn then 1 else 0
    val userRegParams = fun.params.length.min(1 - userParamRegStart)
    for (param, i) <- fun.params.take(userRegParams).zipWithIndex do
      val regIndex = userParamRegStart + i
      val callerOffset = 16 + 8 + (nRegPushed - 1 - regIndex) * 8
      locals(param.name) = LocalVar(param.name, callerOffset, SyslType.I64)

    // Stack params
    val nUserStackStart = 1 - userParamRegStart
    var stackParamOffset = 16 + 8 + nRegPushed * 8
    for param <- fun.params.drop(nUserStackStart) do
      if param.typ == SyslType.StringType then
        locals(param.name) = LocalVar(param.name, stackParamOffset, SyslType.StringType)
        stackParamOffset += 16
      else if param.typ.isInstanceOf[SyslType.SliceType] then
        locals(param.name) = LocalVar(param.name, stackParamOffset, param.typ)
        stackParamOffset += 24
      else if param.typ.isInstanceOf[SyslType.FuncType] || param.typ.isInstanceOf[SyslType.InterfaceType] then
        locals(param.name) = LocalVar(param.name, stackParamOffset, param.typ)
        stackParamOffset += 16
      else if param.typ.isInstanceOf[SyslType.StructType] || param.typ.isInstanceOf[SyslType.EnumType] then
        val aligned = (stackSize(param.typ) + 7) & ~7
        locals(param.name) = LocalVar(param.name, stackParamOffset, param.typ)
        stackParamOffset += aligned
      else
        locals(param.name) = LocalVar(param.name, stackParamOffset, SyslType.I64)
        stackParamOffset += 8

    // Copy string params into local slots (same as genFunction)
    for (param, i) <- fun.params.zipWithIndex if param.typ == SyslType.StringType do
      val srcLocal = locals(param.name)
      val isRegParam = i < (1 - userParamRegStart)
      emitAddImm(7, 7, -16)
      stackOffset -= 16
      val strLocal = LocalVar(param.name, stackOffset, SyslType.StringType)
      if isRegParam then
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r1, r1, r0")
        emit("  ldd r2, r1, r0")
        emit("  std r2, r7, r0")
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
      else
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r2, r1, r0")
        emit("  std r2, r7, r0")
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
      locals(param.name) = strLocal

    // Copy FuncType/InterfaceType params into local slots
    for (param, i) <- fun.params.zipWithIndex if param.typ.isInstanceOf[SyslType.FuncType] || param.typ.isInstanceOf[SyslType.InterfaceType] do
      val srcLocal = locals(param.name)
      val isRegParam = i < (1 - userParamRegStart)
      emitAddImm(7, 7, -16)
      stackOffset -= 16
      val funcLocal = LocalVar(param.name, stackOffset, param.typ)
      if isRegParam then
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r1, r1, r0")
        emit("  ldd r2, r1, r0")
        emit("  std r2, r7, r0")
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
      else
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r2, r1, r0")
        emit("  std r2, r7, r0")
        emit("  addi r1, r1, 8")
        emit("  ldd r2, r1, r0")
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
      locals(param.name) = funcLocal

    // Copy struct/enum params into local frame slots (mirrors genFunction).
    for (param, i) <- fun.params.zipWithIndex if param.typ.isInstanceOf[SyslType.StructType] || param.typ.isInstanceOf[SyslType.EnumType] do
      val srcLocal = locals(param.name)
      val isRegParam = i < (1 - userParamRegStart)
      val aligned = (stackSize(param.typ) + 7) & ~7
      emitAddImm(7, 7, -aligned)
      stackOffset -= aligned
      val newLocal = LocalVar(param.name, stackOffset, param.typ)
      if isRegParam then
        emitAddImm(1, 5, srcLocal.offset)
        emit("  ldd r1, r1, r0")
        for off <- 0 until aligned by 8 do
          emitAddImm(2, 1, off)
          emit("  ldd r2, r2, r0")
          emitAddImm(3, 7, off)
          emit("  std r2, r3, r0")
      else
        for off <- 0 until aligned by 8 do
          emitAddImm(2, 5, srcLocal.offset + off)
          emit("  ldd r2, r2, r0")
          emitAddImm(3, 7, off)
          emit("  std r2, r3, r0")
      locals(param.name) = newLocal
      param.typ match
        case st: SyslType.StructType if structHasStringFields(st) =>
          emitStructStringFieldsRC(5, newLocal.offset, st, incr = true)
        case _ =>

    // Load captured variables from env into locals.
    // Mirror the env-store layout: each capture lives at its natural alignment in
    // env (otherwise mixed-alignment captures hit the CPU's alignment check).
    if closure.captures.nonEmpty then
      // env_ptr is saved at [fp+16] (above saved r6 and r5)
      emitAddImm(1, 5, 16)       // r1 = fp+16
      emit("  ldd r1, r1, r0")   // r1 = env_ptr
      var envOffset = 0
      for (capName, capType) <- closure.captures do
        val size = stackSize(capType).toInt
        val capAlign = stackAlign(capType)
        envOffset = ((envOffset + capAlign - 1) / capAlign) * capAlign
        capType match
          case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType |
               _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
            // Aggregate: copy size bytes into a local slot
            val aligned = ((size + 7) & ~7)
            emitAddImm(7, 7, -aligned)
            stackOffset -= aligned
            for i <- 0 until aligned by 8 do
              emitAddImm(2, 1, envOffset + i)
              emit("  ldd r2, r2, r0")
              emitAddImm(3, 7, i)
              emit("  std r2, r3, r0")
            locals(capName) = LocalVar(capName, stackOffset, capType)
          case _ =>
            // Scalar: load value into a local slot
            emitAddImm(7, 7, -8)
            stackOffset -= 8
            emitAddImm(2, 1, envOffset)
            emitLoad(2, 2, capType)
            emit("  mov r3, r7")
            emitStore(2, 3, capType)
            locals(capName) = LocalVar(capName, stackOffset, capType)
        envOffset += size

    // Generate body
    fun.body match
      case TExprBody(expr) =>
        genExpr(expr)
        emitFuncReturnIncrIfBorrowed(expr)
        if structReturn then emitStructReturn()
        emitDefers()
        emitRefCleanup()
        emitClosureEpilogue(nRegPushed)
      case TBlockBody(stmts) =>
        genClosureBlock(stmts, structReturn, nRegPushed)

    locals = null
    currentFunction = null
    inClosureBody = false
    stringBorrowParams = Set.empty
    funcBorrowParams = Set.empty
    captureBorrows = Set.empty

  private def emitClosureEpilogue(nRegPushed: Int): Unit =
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    // Skip saved r3 (env) + pushed register params
    val skip = 8 + (if nRegPushed > 0 then nRegPushed * 8 else 0)
    emitAddImm(7, 7, skip)
    emit("  jalr r0, r6")

  private def genClosureBlock(stmts: List[TStmt], sr: Boolean, nRegPushed: Int): Unit =
    if stmts.nonEmpty then
      for stmt <- stmts.init do genStmt(stmt)
      stmts.last match
        case TExprStmt(expr) =>
          genExpr(expr)
          if sr then emitStructReturn()
          emitDefers()
          emitRefCleanup()
          emitClosureEpilogue(nRegPushed)
        case TAsmStmt(code) =>
          for line <- code.split("\\\\n|\\n") do
            emit(s"  ${line.trim}")
          emitDefers()
          emitRefCleanup()
          emitClosureEpilogue(nRegPushed)
        case TReturnStmt(Some(expr)) =>
          genExpr(expr)
          if sr then emitStructReturn()
          emitDefers()
          emitRefCleanup()
          emitClosureEpilogue(nRegPushed)
        case TReturnStmt(None) =>
          emitDefers()
          emitRefCleanup()
          emitClosureEpilogue(nRegPushed)
        case other =>
          genStmt(other)
          emitDefers()
          emitRefCleanup()
          emitClosureEpilogue(nRegPushed)
    else
      emitDefers()
      emitRefCleanup()
      emitClosureEpilogue(nRegPushed)

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
        case TAsmStmt(code) =>
          // asm as last statement: emit assembly, assume r1 is set
          for line <- code.split("\\\\n|\\n") do
            emit(s"  ${line.trim}")
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

  // Emit a divide-by-zero check on `divisorReg`. If the divisor is zero,
  // trap with error code 5 before the div/divu issues — TRISC `div` on
  // zero is hardware-undefined, so we must fault deterministically.
  private def emitDivByZeroCheck(divisorReg: String): Unit =
    val ok = newLabel("div_ok")
    emit(s"  bne $divisorReg, r0, $ok")
    emit("  ldi r1, 5")           // error code: 5 = divide-by-zero
    emit("  trap 1")
    emit(s"$ok")

  // Emit binary operation: r1 = r1 op r3
  private def emitBinOp(op: String): Unit =
    op match
      case "+"  => emit("  add r1, r1, r3")
      case "-"  => emit("  sub r1, r1, r3")
      case "*"  => emit("  mul r1, r1, r3")
      case "/"  => emitDivByZeroCheck("r3"); emit("  div r1, r1, r3")
      case "%"  => emitDivByZeroCheck("r3"); emit("  rem r1, r3")
      case "&"  => emit("  and r1, r1, r3")
      case "|"  => emit("  or r1, r1, r3")
      case "^"  => emit("  xor r1, r1, r3")
      case "<<" => emit("  lsl r1, r1, r3")
      case ">>" => emit("  asr r1, r1, r3")

  // Copy multi-word value from src address (r1) to _ret_ptr, then set r1 = _ret_ptr
  // Works for both StructType and StringType (16 bytes)
  private def emitStructReturn(): Unit =
    val (size, align) = currentFunction.returnType match
      case st: SyslType.StructType => (stackSize(st), stackAlign(st))
      case et: SyslType.EnumType => (stackSize(et), stackAlign(et))
      case SyslType.StringType | _: SyslType.FuncType | _: SyslType.InterfaceType => (16, 8)
      case _: SyslType.SliceType => (24, 8)
      case _ => (8, 8)
    val retLocal = locals("_ret_ptr")
    // r1 = source address; load _ret_ptr into r2
    emit("  pshd r1")                       // save source
    emitAddImm(2, 5, retLocal.offset)
    emit("  ldd r2, r2, r0")               // r2 = _ret_ptr (destination)
    emit("  popd r3")                       // r3 = source
    // Copy size bytes from r3 to r2 using width-appropriate ops. See
    // `emitAggregateCopy` for the alignment story (audit item #32).
    if align >= 8 then
      for i <- 0 until size by 8 do
        emitAddImm(4, 3, i)
        emit("  ldd r4, r4, r0")
        emitAddImm(1, 2, i)
        emit("  std r4, r1, r0")
    else
      for i <- 0 until size by 4 do
        emitAddImm(4, 3, i)
        emit("  ldw r4, r4, r0")
        emitAddImm(1, 2, i)
        emit("  stw r4, r1, r0")
    // r1 = _ret_ptr (for the caller)
    emit("  mov r1, r2")
    // Increment string fields in the destination so the source local can be
    // safely freed by emitRefCleanup. r2 holds destination address; emitRefIncr
    // only clobbers r3/r4 so r2 is preserved across iterations.
    currentFunction.returnType match
      case st: SyslType.StructType if structHasStringFields(st) =>
        emitStructStringFieldsRC(2, 0, st, incr = true)
      case _ =>

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
    // Skip past pre-prologue pushed register params (including hidden return ptr if any).
    // Closure bodies additionally push the env pointer (r3) before r6/r5, so when an
    // early `return` triggers this epilogue we need to skip that slot too — otherwise
    // the stack ends up 8 bytes off and the caller's frame is corrupted.
    val nRegPushed = if currentFunction != null then
      val sr = returnsViaPointer(currentFunction.returnType)
      val allSlots = (if sr then 1 else 0) + currentFunction.params.length
      allSlots.min(1)
    else 0
    val extraEnvSkip = if inClosureBody then 8 else 0
    val skip = nRegPushed * 8 + extraEnvSkip
    if skip > 0 then
      emitAddImm(7, 7, skip)
    emit("  jalr r0, r6")

  // Break/continue label stacks
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]
  // User-supplied loop labels (None for unlabeled loops). Parallel to break/continue stacks.
  private val loopNameStack = new mutable.Stack[Option[String]]

  /** Find stack index of loop matching `label` (0 = innermost). None → innermost. */
  private def resolveLoopIdx(label: Option[String]): Int = label match
    case None => 0
    case Some(name) =>
      val idx = loopNameStack.indexWhere(_.contains(name))
      if idx < 0 then throw new RuntimeException(s"no enclosing loop with label '$name'")
      idx

  // Defer stack — deferred statements executed in LIFO order before return/epilogue
  private val deferStack = new mutable.ArrayBuffer[TStmt]

  private def genStmt(stmt: TStmt): Unit =
    stmt match
      case TVarStmt(name, typ, init, _, _) =>
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
          case TStructLit(st @ SyslType.StructType(_, _, _)) =>
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
              // Borrowed string field: incr the buffer (caller still owns its copy)
              fieldType match
                case SyslType.StringType if needsAllocExtern && !isOwnedStringExpr(arg) =>
                  emit("  pshd r1")
                  emitAddImm(1, 5, local.offset + off)
                  emit("  ldd r1, r1, r0")
                  emitRefIncr(1, 8)
                  emit("  popd r1")
                case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
                  emitStructStringFieldsRC(5, local.offset + off, nested, incr = true)
                case _: SyslType.FuncType if needsAllocExtern && !isOwnedClosureExpr(arg) =>
                  emitClosureDescrIncr(5, local.offset + off)
                case _ =>
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
              // Borrowed string/struct/enum/closure field: incr (caller still owns its copy)
              fieldType match
                case SyslType.StringType if needsAllocExtern && !isOwnedStringExpr(arg) =>
                  emit("  pshd r1")
                  emitAddImm(1, 5, local.offset + dataOff + fieldOff)
                  emit("  ldd r1, r1, r0")
                  emitRefIncr(1, 8)
                  emit("  popd r1")
                case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
                  emitStructStringFieldsRC(5, local.offset + dataOff + fieldOff, nested, incr = true)
                case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
                  emitEnumStringFieldsRC(5, local.offset + dataOff + fieldOff, nested, incr = true)
                case _: SyslType.FuncType if needsAllocExtern && !isOwnedClosureExpr(arg) =>
                  emitClosureDescrIncr(5, local.offset + dataOff + fieldOff)
                case _ =>
              fieldOff += fieldType.sizeOf.toInt
          case call @ TCall(_, _, retType) if returnsViaPointer(retType) =>
            // Function returns struct via caller-allocated slot.
            // genExpr allocates the return slot and returns its address in r1.
            // The slot is already on our stack at the current stackOffset after genExpr.
            genExpr(call)
            // r1 = address of return slot. Register the local at the slot's stack position.
            // The return slot was the last thing allocated, so it's at stackOffset.
            locals(name) = LocalVar(name, stackOffset, typ)
            // FuncType returned via structReturn from a call: same default as
            // funcKindOfExpr's TCall case — HeapEnv if the program already pulls
            // the heap allocator (decr's null-check skips NullEnv returns).
            if typ.isInstanceOf[SyslType.FuncType] then
              closureLocalKind(name) = funcKindOfExpr(call)
          case _ if typ.isInstanceOf[SyslType.FuncType] =>
            // Pre-allocate __env_N for stack-env TClosure RHS so env survives
            // the expression's frame and lives at function scope.
            init match
              case c: TClosure if closureKindOf(c) == FuncKind.StackEnv =>
                val envSize = closureEnvSize(c.captures)
                val alignedEnvSize = (envSize + 7) & ~7
                allocLocal(s"__env_$closureCounter", SyslType.IntType(64), alignedEnvSize)
              case _ =>
            genExpr(init) // r1 = descriptor address (16 bytes on expr stack)
            val local = allocLocal(name, typ)
            emitAddImm(2, 5, local.offset)
            emitStore(1, 2, typ)
            // Record kind for scope-cleanup gating.
            val rhsKind = funcKindOfExpr(init)
            closureLocalKind(name) = rhsKind
            // Descriptor copy (var g = f) shares env_ptr with source — incr env
            // rc so each descriptor's scope-exit decr is balanced.
            init match
              case _: TVarRef if rhsKind == FuncKind.HeapEnv =>
                emitClosureDescrIncr(5, local.offset)
              case _ =>
          case _ =>
            genExpr(init) // result in r1
            // Increment refcount for copies (not for new — TNew already sets refcount=1)
            (typ, init) match
              case (rt: SyslType.RefType, _: TNew | _: TNewArray | _: TNewEnum) => // owned, no incr needed
              case (rt: SyslType.RefType, _) => emitRefIncr(1, refHeaderOffset(rt))
              case (SyslType.StringType, e) if isOwnedStringExpr(e) => // owned (concat/substring/literal)
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
        // Extract each field into a new local (skip `_` discards)
        for ((name, fieldType), i) <- names.zip(types).zipWithIndex if name != "_" do
          val off = fieldOffset(st, i)
          emitAddImm(1, 5, tmpLocal.offset)
          emit("  ldd r1, r1, r0")  // r1 = tuple address
          if off != 0 then emitAddImm(1, 1, off)
          // Aggregates use address-as-value; emitStore→emitAggregateCopy will
          // do the byte-copy. Scalars need an actual load before the store.
          fieldType.underlying match
            case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType
              | _: SyslType.SliceType | _: SyslType.EnumType
              | _: SyslType.FuncType | _: SyslType.InterfaceType => ()
            case _ => emitLoad(1, 1, fieldType)
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
        // Extract each field and store to existing variable (skip `_` discards)
        for ((name, fieldType), i) <- names.zip(types).zipWithIndex if name != "_" do
          val off = fieldOffset(st, i)
          emitAddImm(1, 5, tmpLocal.offset)
          emit("  ldd r1, r1, r0")  // r1 = tuple address
          if off != 0 then emitAddImm(1, 1, off)
          fieldType.underlying match
            case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType
              | _: SyslType.SliceType | _: SyslType.EnumType
              | _: SyslType.FuncType | _: SyslType.InterfaceType => ()
            case _ => emitLoad(1, 1, fieldType)
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
                case _: TNew | _: TNewArray | _: TNewEnum => // owned, no incr needed
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
              // Increment new string's refcount (skip for owned: concat/substring/literal)
              if needsAllocExtern && !isOwnedStringExpr(value) then
                emit("  pshd r1")
                emit("  ldd r1, r1, r0")
                emitRefIncr(1, 8)
                emit("  popd r1")
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)
            case st: SyslType.StructType if structHasStringFields(st) =>
              // Decrement old struct's string fields before overwrite
              emitStructStringFieldsRC(5, local.offset, st, incr = false)
              genExpr(value)              // r1 = source struct address
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)  // copy struct bytes
              // Incr new struct's string fields if borrowed (owned source already at rc=1)
              if !isOwnedStructExpr(value) then
                emitStructStringFieldsRC(5, local.offset, st, incr = true)
            case et: SyslType.EnumType if structHasStringFields(et) =>
              // Decrement old enum's active variant string fields before overwrite
              emitEnumStringFieldsRC(5, local.offset, et, incr = false)
              genExpr(value)              // r1 = source enum address
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)  // copy enum bytes (incl new tag)
              // Incr new enum's variant strings if borrowed
              if !isOwnedStructExpr(value) then
                emitEnumStringFieldsRC(5, local.offset, et, incr = true)
            case _: SyslType.FuncType =>
              // Decr old env if HeapEnv; store new bytes; incr new env if RHS is
              // a borrowed descriptor copy (TVarRef whose kind is HeapEnv).
              if closureLocalKind.get(target).contains(FuncKind.HeapEnv) then
                emitClosureDescrDecr(5, local.offset)
              genExpr(value)
              emitAddImm(2, 5, local.offset)
              emitStore(1, 2, local.typ)
              val rhsKind = funcKindOfExpr(value)
              closureLocalKind(target) = rhsKind
              value match
                case _: TVarRef if rhsKind == FuncKind.HeapEnv =>
                  emitClosureDescrIncr(5, local.offset)
                case _ =>
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
                case _: TNew | _: TNewArray | _: TNewEnum =>
                case _ => emitRefIncr(1, hoff)
              emit("  pshd r1")
              emit(s"  movi r1, $target")
              emit("  popd r2")
              emitStore(2, 1, gtyp)
            case SyslType.StringType =>
              if needsAllocExtern then
                // Decr old buffer
                emit(s"  movi r1, $target")
                emit("  ldd r1, r1, r0")    // r1 = old ptr
                emitRefDecr(1, 8)
              genExpr(value)                // r1 = new descriptor address
              emit(s"  movi r2, $target")   // r2 = global address
              emitStore(1, 2, gtyp)         // copy 16 bytes
              if needsAllocExtern && !isOwnedStringExpr(value) then
                emit(s"  movi r1, $target")
                emit("  ldd r1, r1, r0")  // r1 = new ptr (just stored)
                emitRefIncr(1, 8)
            case st: SyslType.StructType if structHasStringFields(st) && needsAllocExtern =>
              // Decr old struct's string fields (baseReg=1 = global address; preserved
              // across emitRefDecr via the helper's pshd/popd r1)
              emit(s"  movi r1, $target")
              emitStructStringFieldsRC(1, 0, st, incr = false)
              genExpr(value)                // r1 = source struct address
              emit(s"  movi r2, $target")   // r2 = global address
              emitStore(1, 2, gtyp)         // copy struct bytes
              if !isOwnedStructExpr(value) then
                emit(s"  movi r1, $target")
                emitStructStringFieldsRC(1, 0, st, incr = true)
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
            case TStructLit(st @ SyslType.StructType(_, _, _)) =>
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
              emit("  mul r1, r1, r3") // r1 = offset * elemSize
          case _ =>
        emit("  pshd r1") // save scaled operand
        if locals != null && locals.contains(target) then
          val local = locals(target)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit("  popd r3")
          emitBinOp(op)
          emitNarrow(1, local.typ)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else
          val gtyp = globals.getOrElse(target, value.typ)
          emit(s"  movi r2, $target")
          emitLoad(1, 2, gtyp)
          emit("  popd r3")
          emitBinOp(op)
          emitNarrow(1, gtyp)
          emit(s"  movi r2, $target")
          emitStore(1, 2, gtyp)

      case TReturnStmt(Some(value)) =>
        genExpr(value) // result in r1
        emitFuncReturnIncrIfBorrowed(value)
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

      case TMultiStmt(children) =>
        children.foreach(genStmt)

      case TContractCheck(_, expr, _) =>
        genExpr(expr) // r1 = cond
        val pass = newLabel("contract_pass")
        emit(s"  bne r1, r0, $pass")
        emit("  ldi r1, 6") // error code 6 = contract violation
        emit("  trap 1")
        emit(s"$pass:")

      case TExprStmt(expr) =>
        genExpr(expr) // result in r1, discarded

      case TWhileStmt(cond, body, userLabel) =>
        val loopLabel = newLabel("while")
        val endLabel = newLabel("endwhile")
        breakLabels.push(endLabel)
        continueLabels.push(loopLabel)
        loopNameStack.push(userLabel)
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
        loopNameStack.pop()
        breakLabels.pop()
        continueLabels.pop()

      case TForStmt(init, cond, update, body, userLabel) =>
        val loopLabel = newLabel("for")
        val updateLabel = newLabel("forupdate")
        val endLabel = newLabel("endfor")
        enterScope()
        genStmt(init)
        breakLabels.push(endLabel)
        continueLabels.push(updateLabel)
        loopNameStack.push(userLabel)
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
        loopNameStack.pop()
        breakLabels.pop()
        continueLabels.pop()
        leaveScope()

      case TDoWhileStmt(cond, body, userLabel) =>
        val loopLabel = newLabel("dowhile")
        val condLabel = newLabel("dowhile_cond")
        val endLabel = newLabel("enddowhile")
        breakLabels.push(endLabel)
        continueLabels.push(condLabel) // continue jumps to condition, not body
        loopNameStack.push(userLabel)
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
        loopNameStack.pop()
        breakLabels.pop()
        continueLabels.pop()

      case TLoopStmt(body, userLabel) =>
        val loopLabel = newLabel("loop")
        val endLabel = newLabel("endloop")
        breakLabels.push(endLabel)
        continueLabels.push(loopLabel)
        loopNameStack.push(userLabel)
        loopScopeOffsets.push(stackOffset)
        emit(s"$loopLabel")
        enterScope()
        for stmt <- body do genStmt(stmt)
        leaveScope()
        emit(s"  bra $loopLabel")
        emit(s"$endLabel")
        loopScopeOffsets.pop()
        loopNameStack.pop()
        breakLabels.pop()
        continueLabels.pop()

      case TBreakStmt(lbl) =>
        val idx = resolveLoopIdx(lbl)
        val loopOffset = loopScopeOffsets(idx)
        if stackOffset != loopOffset then
          emitAddImm(7, 7, loopOffset - stackOffset)
        emit(s"  bra ${breakLabels(idx)}")

      case TContinueStmt(lbl) =>
        val idx = resolveLoopIdx(lbl)
        val loopOffset = loopScopeOffsets(idx)
        if stackOffset != loopOffset then
          emitAddImm(7, 7, loopOffset - stackOffset)
        emit(s"  bra ${continueLabels(idx)}")

      case TAsmStmt(code) =>
        // Emit each line of inline assembly verbatim
        for line <- code.split("\\\\n|\\n") do
          emit(s"  ${line.trim}")

      case TDerefAssignStmt(pointer, value) =>
        val pointee = pointer.typ match
          case SyslType.PtrType(p) => p
          case other => throw new RuntimeException(s"TDerefAssignStmt: expected PtrType, got $other")

        val needsRC = (pointee == SyslType.StringType && needsAllocExtern) ||
                      pointee.isInstanceOf[SyslType.RefType] ||
                      (pointee match
                        case s: SyslType.StructType => structHasStringFields(s) && needsAllocExtern
                        case _ => false)

        if !needsRC then
          genExpr(value)           // r1 = value to store
          emit("  pshd r1")       // save as 64-bit temp
          genExpr(pointer)         // r1 = address
          emit("  popd r2")        // r2 = value
          emitStore(2, 1, pointee)
        else
          // Compute destination address (r1), save in fp-relative scratch slot
          // (genExpr below may push its descriptor onto the stack, breaking pshd/popd LIFO)
          genExpr(pointer)         // r1 = address
          emit("  pshd r1")
          stackOffset -= 8
          val saveOff = stackOffset
          // Decrement old value at the destination
          pointee match
            case SyslType.StringType =>
              emit("  ldd r1, r1, r0")
              emitRefDecr(1, 8)
            case rt: SyslType.RefType =>
              emit("  ldd r1, r1, r0")
              emitRefDecr(1, refHeaderOffset(rt), deinitFor(rt))
            case s: SyslType.StructType =>
              emitStructStringFieldsRC(1, 0, s, incr = false)
            case _ =>
          // Compute new value (clobbers everything; may push descriptor)
          genExpr(value)
          // Reload destination address
          emitAddImm(2, 5, saveOff)
          emit("  ldd r2, r2, r0")
          emitStore(1, 2, pointee)
          // Increment new value if borrowed
          pointee match
            case SyslType.StringType =>
              value match
                case _: TBinary =>
                case _ =>
                  emit("  ldd r1, r2, r0")
                  emitRefIncr(1, 8)
            case rt: SyslType.RefType =>
              value match
                case _: TNew | _: TNewArray | _: TNewEnum =>
                case _ =>
                  emit("  ldd r1, r2, r0")
                  emitRefIncr(1, refHeaderOffset(rt))
            case s: SyslType.StructType =>
              if !isOwnedStructExpr(value) then
                emitStructStringFieldsRC(2, 0, s, incr = true)
            case _ =>

      case TIndexAssignStmt(array, index, value) =>
        val elemType = array.typ match
          case SyslType.ArrayType(e, _) => e
          case SyslType.PtrType(e) => e
          case SyslType.SliceType(e) => e
          case SyslType.RefType(SyslType.SliceType(e)) => e
          case other => throw new RuntimeException(s"TIndexAssignStmt: expected indexable type, got $other")
        val elemSize = stackSize(elemType)
        val isSlice = array.typ.isInstanceOf[SyslType.SliceType]

        val needsRC = (elemType == SyslType.StringType && needsAllocExtern) ||
                      elemType.isInstanceOf[SyslType.RefType] ||
                      (elemType match
                        case s: SyslType.StructType => structHasStringFields(s) && needsAllocExtern
                        case _ => false)

        if !needsRC then
          genExpr(value)           // r1 = value
          emit("  pshd r1")       // save as 64-bit temp
          genExpr(index)           // r1 = index
          emit("  pshd r1")
          genExpr(array)           // r1 = array/slice address
          if isSlice then
            emit("  ldd r1, r1, r0") // r1 = data pointer (from slice struct)
          emit("  popd r2")        // r2 = index
          emitLoadImm(3, elemSize)
          emit("  mul r2, r2, r3") // r2 = index * elemSize
          emit("  add r1, r1, r2") // r1 = base + offset
          emit("  popd r2")        // r2 = value
          emitStore(2, 1, elemType)
        else
          // Compute element address first, then bracket the store with rc decr/incr
          genExpr(index)
          emit("  pshd r1")        // save index temporarily
          stackOffset -= 8
          genExpr(array)           // r1 = array/slice address
          if isSlice then
            emit("  ldd r1, r1, r0") // r1 = data pointer
          emit("  popd r2")        // r2 = index
          stackOffset += 8
          emitLoadImm(3, elemSize)
          emit("  mul r2, r2, r3") // r2 = index * elemSize
          emit("  add r1, r1, r2") // r1 = element address
          // Save element address in fp-relative scratch (survives genExpr)
          emit("  pshd r1")
          stackOffset -= 8
          val saveOff = stackOffset
          // Decrement old element value
          elemType match
            case SyslType.StringType =>
              emit("  ldd r1, r1, r0")
              emitRefDecr(1, 8)
            case rt: SyslType.RefType =>
              emit("  ldd r1, r1, r0")
              emitRefDecr(1, refHeaderOffset(rt), deinitFor(rt))
            case s: SyslType.StructType =>
              emitStructStringFieldsRC(1, 0, s, incr = false)
            case _ =>
          // Compute new value
          genExpr(value)
          emitAddImm(2, 5, saveOff)
          emit("  ldd r2, r2, r0") // r2 = element address
          emitStore(1, 2, elemType)
          // Increment new value if borrowed
          elemType match
            case SyslType.StringType =>
              value match
                case _: TBinary =>
                case _ =>
                  emit("  ldd r1, r2, r0")
                  emitRefIncr(1, 8)
            case rt: SyslType.RefType =>
              value match
                case _: TNew | _: TNewArray | _: TNewEnum =>
                case _ =>
                  emit("  ldd r1, r2, r0")
                  emitRefIncr(1, refHeaderOffset(rt))
            case s: SyslType.StructType =>
              if !isOwnedStructExpr(value) then
                emitStructStringFieldsRC(2, 0, s, incr = true)
            case _ =>

      case TFieldAssignStmt(obj, fieldIndex, value) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2

        // Refcounted field types need decr-old/incr-new bracketing on overwrite.
        val needsRC = (fieldType == SyslType.StringType && needsAllocExtern) ||
                      fieldType.isInstanceOf[SyslType.RefType] ||
                      (fieldType match
                        case s: SyslType.StructType => structHasStringFields(s) && needsAllocExtern
                        case _: SyslType.FuncType => needsAllocExtern
                        case _ => false)

        if !needsRC then
          genExpr(value)             // r1 = value
          emit("  pshd r1")
          emitStructAddr(obj)        // r1 = struct address
          if off != 0 then emitAddImm(1, 1, off)
          emit("  popd r2")          // r2 = value
          emitStore(2, 1, fieldType)
        else
          // Refcounted assignment must evaluate RHS *before* decrementing the
          // old field. When the RHS reads the field (e.g. `t.s = t.s + "x"`),
          // a free()'d old buffer would leave the read pointing at freed memory
          // — silent on the rv/wasm bump-allocator runtimes (free is a no-op),
          // but wrong on llvm-host and corrupts state on trisc when malloc
          // recycles the block before the strcpy runs. Mirrors the fix in
          // SyslLLVMCodegen.TFieldAssignStmt.
          //
          // We reserve TWO fp-relative scratch slots up front so they survive
          // genExpr's own stack churn (a string-concat RHS pushes a 16-byte
          // descriptor whose pointer it returns in r1; we must capture that
          // pointer before any subsequent codegen perturbs r7).
          emit("  addi r7, r7, -16")
          stackOffset -= 16
          val saveOff   = stackOffset      // [r5+saveOff]   = field address
          val newValOff = stackOffset + 8  // [r5+newValOff] = new-value pointer
          // Slot 1: field address
          emitStructAddr(obj)
          if off != 0 then emitAddImm(1, 1, off)
          emitAddImm(2, 5, saveOff)
          emit("  std r1, r2, r0")
          // Slot 2: result of evaluating the RHS (read this before any other
          // codegen — RHS for a string concat leaves a descriptor at r7+0).
          genExpr(value)
          emitAddImm(2, 5, newValOff)
          emit("  std r1, r2, r0")
          // Now decrement the old field value
          emitAddImm(1, 5, saveOff)
          emit("  ldd r1, r1, r0")    // r1 = field address
          fieldType match
            case SyslType.StringType =>
              emit("  ldd r1, r1, r0")  // r1 = old ptr
              emitRefDecr(1, 8)
            case rt: SyslType.RefType =>
              emit("  ldd r1, r1, r0")  // r1 = old ref
              emitRefDecr(1, refHeaderOffset(rt), deinitFor(rt))
            case s: SyslType.StructType =>
              emitStructStringFieldsRC(1, 0, s, incr = false)
            case _: SyslType.FuncType =>
              // r1 = field address — descriptor is at [r1+0]; decr its env_ptr.
              emitClosureDescrDecr(1, 0)
            case _ =>
          // Reload new value (r1) and field address (r2); store
          emitAddImm(1, 5, newValOff)
          emit("  ldd r1, r1, r0")    // r1 = new-value pointer
          emitAddImm(2, 5, saveOff)
          emit("  ldd r2, r2, r0")    // r2 = field address
          emitStore(1, 2, fieldType)
          // Increment the new value if it's a borrowed (non-owned) reference
          fieldType match
            case SyslType.StringType =>
              if !isOwnedStringExpr(value) then
                emit("  ldd r1, r2, r0")  // r1 = new ptr just stored at field
                emitRefIncr(1, 8)
            case rt: SyslType.RefType =>
              value match
                case _: TNew | _: TNewArray | _: TNewEnum => // owned, no incr
                case _ =>
                  emit("  ldd r1, r2, r0")
                  emitRefIncr(1, refHeaderOffset(rt))
            case s: SyslType.StructType =>
              if !isOwnedStructExpr(value) then
                emitStructStringFieldsRC(2, 0, s, incr = true)
            case _: SyslType.FuncType =>
              if !isOwnedClosureExpr(value) then
                emitClosureDescrIncr(2, 0)
            case _ =>

      case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
        val st = obj.typ.asInstanceOf[SyslType.StructType]
        val off = fieldOffset(st, fieldIndex)
        val fieldType = st.fields(fieldIndex)._2
        // Step 1: compute field address and push it
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
          case "/"  => emitDivByZeroCheck("r1"); emit("  div r2, r2, r1")
          case "%"  => emitDivByZeroCheck("r1"); emit("  rem r2, r1")
          case "&"  => emit("  and r2, r2, r1")
          case "|"  => emit("  or r2, r2, r1")
          case "^"  => emit("  xor r2, r2, r1")
          case "<<" => emit("  lsl r2, r2, r1")
          case ">>" => emit("  asr r2, r2, r1")
        // Step 5: truncate narrow unsigned, then store (field address is safely on stack)
        emitNarrow(2, fieldType)
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
        else if n >= Int.MinValue && n <= 0xFFFFFFFFL then
          // movi takes a 32-bit unsigned immediate and writes it zero-extended
          // to the 64-bit register. For NEGATIVE i32 literals we mask to the low
          // 32 bits to fit movi, then `sew` (sign-extend word) to recover the
          // full 64-bit signed value. Without the sew, `var x: int = -3` lands
          // in r1 as `0x00000000FFFFFFFD` instead of `0xFFFFFFFFFFFFFFFD`, and
          // any 64-bit-wide compare (e.g. `result == x + x` inside an `ensure`,
          // where `result` was loaded via `ldw` which DOES sign-extend) reads
          // the two operands as unequal and traps. Non-negative values in
          // 0..0xFFFFFFFF skip the sew — that range covers both unsigned u32
          // values up to 0xFFFFFFFF and positive i32 values (high bit clear),
          // both of which want the zero-extended representation movi gives.
          if n < 0 then
            emit(s"  movi r1, ${n & 0xFFFFFFFFL}")
            emit("  sew r1, r1")
          else
            emit(s"  movi r1, $n")
        else
          emit(s"  ldc r1, $n")

      case TBoolLit(true, _) => emit("  ldi r1, 1")
      case TBoolLit(false, _) => emit("  ldi r1, 0")

      case TUnitLit(_) => emit("  ldi r1, 0")  // unit is 0-byte; placeholder constant

      case TVarRef(name, typ) =>
        if locals != null && locals.contains(name) then
          val local = locals(name)
          local.typ match
            case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
              emitAddImm(1, 5, local.offset) // aggregates: address, not value
            case _ =>
              emitAddImm(2, 5, local.offset)
              emitLoad(1, 2, local.typ)
        else
          emit(s"  movi r1, $name")
          val gt = globals.getOrElse(name, typ) // use AST type for cross-unit globals
          gt match
            case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
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

      case TBinary(left, op, right, _)
          if left.typ == SyslType.StringType
            && Set("==", "!=", "<", "<=", ">", ">=").contains(op) =>
        // Lexicographic byte-wise compare. Computes a signed three-way diff in
        // r1 (negative / zero / positive) by walking min(len1, len2) bytes; if
        // every byte matches, the tiebreak is len1 - len2. The diff is then
        // reduced to a 0/1 boolean via the matching zero-relative branch.
        //
        // Stack layout after setup (40 bytes total, sp+0 at the top):
        //   sp+0  : minlen (running counter)
        //   sp+8  : len2
        //   sp+16 : ptr2
        //   sp+24 : len1
        //   sp+32 : ptr1
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
        // Reserve the minlen slot (placeholder 0; rewritten below).
        emit("  pshd r0")
        stackOffset -= 8

        // Compute minlen = min(len1, len2) and store at sp+0.
        emitAddImm(1, 7, 24)
        emit("  ldd r1, r1, r0")          // r1 = len1
        emitAddImm(2, 7, 8)
        emit("  ldd r2, r2, r0")          // r2 = len2
        val useLeftLbl = newLabel("str_cmp_use_l")
        val minDoneLbl = newLabel("str_cmp_min_done")
        emit(s"  bls r1, r2, $useLeftLbl") // len1 < len2 → use len1
        emit("  std r2, r7, r0")          // minlen = len2
        emit(s"  bra $minDoneLbl")
        emit(s"$useLeftLbl")
        emit("  std r1, r7, r0")          // minlen = len1
        emit(s"$minDoneLbl")

        // r2 = lp (ptr1), r3 = rp (ptr2); both walk forward through the loop.
        emitAddImm(2, 7, 32)
        emit("  ldd r2, r2, r0")
        emitAddImm(3, 7, 16)
        emit("  ldd r3, r3, r0")

        val loopLbl = newLabel("str_cmp_loop")
        val lensTieLbl = newLabel("str_cmp_lens")
        val haveResLbl = newLabel("str_cmp_have")
        emit(s"$loopLbl")
        emit("  ldd r4, r7, r0")          // r4 = minlen
        emit(s"  beq r4, r0, $lensTieLbl")
        emit("  ldb r1, r2, r0")          // r1 = lb (unsigned 0..255)
        emit("  ldb r4, r3, r0")          // r4 = rb (clobbers counter — reloaded below)
        emit("  sub r1, r1, r4")          // r1 = lb - rb (three-way diff at this byte)
        emit(s"  bne r1, r0, $haveResLbl") // mismatch → r1 carries the diff
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, 1")
        emit("  ldd r4, r7, r0")
        emit("  addi r4, r4, -1")
        emit("  std r4, r7, r0")
        emit(s"  bra $loopLbl")

        emit(s"$lensTieLbl")
        emitAddImm(1, 7, 24)
        emit("  ldd r1, r1, r0")          // r1 = len1
        emitAddImm(4, 7, 8)
        emit("  ldd r4, r4, r0")          // r4 = len2
        emit("  sub r1, r1, r4")          // r1 = len1 - len2

        emit(s"$haveResLbl")
        // Reduce the three-way diff in r1 to a 0/1 boolean for the requested op.
        val yesLbl = newLabel("str_cmp_yes")
        val endLbl = newLabel("str_cmp_end")
        op match
          case "==" => emit(s"  beq r1, r0, $yesLbl")
          case "!=" => emit(s"  bne r1, r0, $yesLbl")
          case "<"  => emit(s"  bls r1, r0, $yesLbl")
          case "<=" => emit(s"  ble r1, r0, $yesLbl")
          case ">"  => emit(s"  bls r0, r1, $yesLbl") // 0 < r1
          case ">=" => emit(s"  bge r1, r0, $yesLbl")
        emit("  ldi r1, 0")
        emit(s"  bra $endLbl")
        emit(s"$yesLbl")
        emit("  ldi r1, 1")
        emit(s"$endLbl")

        // Reclaim 40 bytes (32 for the two descriptors + 8 for the minlen slot).
        emitAddImm(7, 7, 40)
        stackOffset += 40

      case TBinary(left, op @ ("+" | "-"), right, _) if left.typ.isPointerLike =>
        // Pointer arithmetic: ptr + int → ptr (scale by element size)
        val elemSize = left.typ match
          case SyslType.PtrType(e) => stackSize(e)
          case SyslType.ArrayType(e, _) => stackSize(e)
          case _ => 8
        genExpr(left)        // r1 = pointer
        emit("  pshd r1")
        stackOffset -= 8
        genExpr(right)       // r1 = integer offset
        emitLoadImm(3, elemSize)
        emit("  mul r1, r1, r3") // scale by element size
        emit("  popd r2")   // r2 = pointer
        stackOffset += 8
        if op == "+" then emit("  add r1, r2, r1")
        else emit("  sub r1, r2, r1")

      case TIntrinsicCall(name, args, typ) =>
        // Eval args into r1 (left) and r2 (right), same protocol as TBinary.
        genExpr(args(0))
        emit("  pshd r1")
        stackOffset -= 8
        genExpr(args(1))
        emit("  mov r2, r1")
        emit("  popd r1")
        stackOffset += 8
        val unsigned = typ.isUnsigned
        val width = typ.bitWidth
        // Helper: load 64-bit constant. movi loads 32 bits ZERO-extended (no sign extension),
        // so use ldc (constant pool) for negative values to get the correct 64-bit value.
        def loadImm(reg: Int, v: Long): Unit =
          if v >= 0 && v <= 255 then emit(s"  ldi r$reg, $v")
          else if v >= 0 && v <= 0xFFFFFFFFL then emit(s"  movi r$reg, $v")
          else emit(s"  ldc r$reg, $v")
        name match
          // wrapping_*: ordinary add/sub/mul, then narrow to width bits to discard high bits.
          case "wrapping_add" =>
            emit("  add r1, r1, r2"); emitNarrow(1, typ)
          case "wrapping_sub" =>
            emit("  sub r1, r1, r2"); emitNarrow(1, typ)
          case "wrapping_mul" =>
            emit("  mul r1, r1, r2")
            emitNarrow(1, typ)
          case "saturating_add" | "saturating_sub" | "saturating_mul" =>
            // Special case: saturating_mul on u32 — the full u64 product can exceed
            // signed i64 range (e.g. 0xFFFFFFFF * 0xFFFFFFFF = 0xFFFFFFFE_00000001),
            // so the generic signed-clamp path below would misinterpret it as
            // negative and saturate to 0. Use unsigned compare against u32 max
            // instead; no LOW clamp is needed because `mul` on unsigned operands
            // never produces a value below 0 in unsigned interpretation.
            if name == "saturating_mul" && unsigned && width == 32 then
              // u32 × u32 product fits in u64; `mul` (low 64 bits) is the full result.
              emit("  mul r1, r1, r2")     // r1 = full u64 product
              loadImm(3, (1L << 32) - 1)   // r3 = u32 max = 0xFFFFFFFF
              emit("  sltu r4, r3, r1")    // r4 = 1 if u32max < r1 unsigned
              val noHi = newLabel("sat_nohi")
              emit(s"  beq r4, r0, $noHi")
              emit("  mov r1, r3")
              emit(s"$noHi")
            else if width >= 64 then
              // 64-bit saturating arithmetic. The narrow signed-clamp path below
              // doesn't work — operands already span the full i64 range, so the
              // intermediate computation cannot be widened. Use overflow detection
              // on the wrapped result and clamp to type-specific bounds.
              (name, unsigned) match
                case ("saturating_add", true) =>
                  // u64 add: overflow iff (a + b) < a unsigned.
                  emit("  pshd r1")             // save a
                  stackOffset -= 8
                  emit("  add r1, r1, r2")      // r1 = a + b (wrapped)
                  emit("  popd r3")             // r3 = a
                  stackOffset += 8
                  emit("  sltu r4, r1, r3")     // r4 = 1 iff sum < a
                  val noOf = newLabel("sat_noof")
                  emit(s"  beq r4, r0, $noOf")
                  loadImm(1, -1L)               // MAX_U64 = 0xFFFF_FFFF_FFFF_FFFF
                  emit(s"$noOf")
                case ("saturating_sub", true) =>
                  // u64 sub: would-underflow iff a < b unsigned.
                  emit("  sltu r4, r1, r2")     // r4 = 1 iff a < b
                  emit("  sub r1, r1, r2")      // r1 = a - b (wrapped)
                  val noUn = newLabel("sat_noun")
                  emit(s"  beq r4, r0, $noUn")
                  emit("  ldi r1, 0")
                  emit(s"$noUn")
                case ("saturating_mul", true) =>
                  // u64 mul: compute high (mulhu) and low (mul) separately —
                  // mulhu is destructive on rd, so save a in r3 first.
                  // Overflow iff high half is non-zero.
                  emit("  mov r3, r1")          // r3 = a (preserve for mulhu)
                  emit("  mulhu r3, r2")        // r3 = high(a *u b)
                  emit("  mul r1, r1, r2")      // r1 = low(a * b)
                  val noOf = newLabel("sat_noof")
                  emit(s"  beq r3, r0, $noOf")
                  loadImm(1, -1L)               // MAX_U64
                  emit(s"$noOf")
                case ("saturating_add", false) =>
                  // i64 add: signed overflow iff sign(a)==sign(b) && sign(result)!=sign(a).
                  // XOR trick: ((a ^ result) & (b ^ result)) is negative ⇔ overflow.
                  emit("  pshd r1")             // save a
                  stackOffset -= 8
                  emit("  pshd r2")             // save b
                  stackOffset -= 8
                  emit("  add r1, r1, r2")      // r1 = result (wrapped)
                  emit("  popd r2")             // r2 = b
                  stackOffset += 8
                  emit("  popd r3")             // r3 = a
                  stackOffset += 8
                  emit("  xor r4, r3, r1")      // a ^ result
                  emit("  xor r5, r2, r1")      // b ^ result
                  emit("  and r4, r4, r5")
                  emit("  slt r4, r4, r0")      // r4 = 1 iff combined indicator < 0
                  val noOf = newLabel("sat_noof")
                  emit(s"  beq r4, r0, $noOf")
                  // Overflow direction: same sign as a (== sign of b).
                  emit("  slt r4, r3, r0")      // r4 = 1 iff a < 0
                  val neg = newLabel("sat_neg")
                  emit(s"  bne r4, r0, $neg")
                  loadImm(1, Long.MaxValue)
                  emit(s"  bra $noOf")
                  emit(s"$neg")
                  loadImm(1, Long.MinValue)
                  emit(s"$noOf")
                case ("saturating_sub", false) =>
                  // i64 sub: overflow iff sign(a)!=sign(b) && sign(result)!=sign(a).
                  // XOR trick: ((a ^ b) & (a ^ result)) is negative ⇔ overflow.
                  emit("  pshd r1")             // save a
                  stackOffset -= 8
                  emit("  pshd r2")             // save b
                  stackOffset -= 8
                  emit("  sub r1, r1, r2")      // r1 = result (wrapped)
                  emit("  popd r2")             // r2 = b
                  stackOffset += 8
                  emit("  popd r3")             // r3 = a
                  stackOffset += 8
                  emit("  xor r4, r3, r2")      // a ^ b
                  emit("  xor r5, r3, r1")      // a ^ result
                  emit("  and r4, r4, r5")
                  emit("  slt r4, r4, r0")      // r4 = 1 iff combined indicator < 0
                  val noOf = newLabel("sat_noof")
                  emit(s"  beq r4, r0, $noOf")
                  // Overflow direction: same sign as a.
                  emit("  slt r4, r3, r0")      // r4 = 1 iff a < 0
                  val neg = newLabel("sat_neg")
                  emit(s"  bne r4, r0, $neg")
                  loadImm(1, Long.MaxValue)
                  emit(s"  bra $noOf")
                  emit(s"$neg")
                  loadImm(1, Long.MinValue)
                  emit(s"$noOf")
                case ("saturating_mul", false) =>
                  // i64 mul: compute high (mulh, destructive) and low (mul) separately.
                  // Overflow iff high != asr(low, 63), i.e. high doesn't equal the
                  // sign-extension of the low half.
                  emit("  mov r3, r1")          // r3 = a (preserve for mulh)
                  emit("  mulh r3, r2")         // r3 = high(a *s b)
                  emit("  mul r1, r1, r2")      // r1 = low(a * b)
                  emit("  ldi r4, 63")
                  emit("  asr r4, r1, r4")      // r4 = sign-extended low (expected high)
                  val noOf = newLabel("sat_noof")
                  emit(s"  beq r4, r3, $noOf")
                  // Overflow direction: sign of actual high tells us positive vs negative.
                  emit("  slt r4, r3, r0")      // r4 = 1 iff high < 0
                  val neg = newLabel("sat_neg")
                  emit(s"  bne r4, r0, $neg")
                  loadImm(1, Long.MaxValue)
                  emit(s"  bra $noOf")
                  emit(s"$neg")
                  loadImm(1, Long.MinValue)
                  emit(s"$noOf")
                case _ =>
            else
              // Compute in 64-bit; for narrow widths the intermediate fits in signed i64.
              // Then signed-clamp to [minV, maxV]. For unsigned types maxV is set to the
              // unsigned max, but we still use signed slt because the intermediate is in signed range.
              // mul-low is identical signed/unsigned per the post-Stage-2 ISA.
              name match
                case "saturating_add" => emit("  add r1, r1, r2")
                case "saturating_sub" => emit("  sub r1, r1, r2")
                case "saturating_mul" => emit("  mul r1, r1, r2")
                case _ =>
              val (minV, maxV) =
                if unsigned then (0L, (1L << width) - 1)
                else (-(1L << (width - 1)), (1L << (width - 1)) - 1)
              // Clamp HIGH: if r1 > maxV then r1 = maxV  (signed compare)
              loadImm(3, maxV)
              emit("  slt r4, r3, r1")       // r4 = (max < r1)
              val noHi = newLabel("nohi")
              emit(s"  beq r4, r0, $noHi")
              emit("  mov r1, r3")
              emit(s"$noHi")
              // Clamp LOW: if r1 < minV then r1 = minV  (signed compare)
              loadImm(3, minV)
              emit("  slt r4, r1, r3")       // r4 = (r1 < min)
              val noLo = newLabel("nolo")
              emit(s"  beq r4, r0, $noLo")
              emit("  mov r1, r3")
              emit(s"$noLo")
          case other => throw new RuntimeException(s"unknown intrinsic: $other")

      case TBinary(left, op, right, resultType) =>
        genExpr(left)        // r1 = left
        emit("  pshd r1")   // save left on stack
        stackOffset -= 8
        genExpr(right)       // r1 = right
        emit("  mov r2, r1") // r2 = right
        emit("  popd r1")   // r1 = left
        stackOffset += 8
        val isFloat = left.typ.isFloat
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
              // Synthesize `a <= b` as `(a < b) | (a == b)` so NaN poisons
              // correctly (IEEE 754: any comparison with NaN is false; the
              // earlier `!(b < a)` approach turned NaN's false-poisoned
              // `<` into `true`). r1 = a (left), r2 = b (right).
              emit("  mov r4, r1")        // r4 = a
              emit("  fslt r1, r1, r2")   // r1 = (a < b)
              emit("  fseq r4, r4, r2")   // r4 = (a == b)
              emit("  or r1, r1, r4")     // r1 = a <= b
            case ">=" =>
              // Synthesize `a >= b` as `(b < a) | (a == b)` — same NaN-correctness
              // motivation as `<=` above.
              emit("  mov r4, r1")        // r4 = a
              emit("  fslt r1, r2, r1")   // r1 = (b < a) = (a > b)
              emit("  fseq r4, r4, r2")   // r4 = (a == b)
              emit("  or r1, r1, r4")     // r1 = a >= b
            case _ => // unsupported float op — fall through
        else
          op match
            case "+"  => emit("  add r1, r1, r2")
            case "-"  => emit("  sub r1, r1, r2")
            case "*"  => emit("  mul r1, r1, r2")  // mul-low is identical signed/unsigned post-Stage-2
            case "/"  => emitDivByZeroCheck("r2"); emit(if unsigned then "  divu r1, r1, r2" else "  div r1, r1, r2")
            case "%"  => emitDivByZeroCheck("r2"); emit(if unsigned then "  remu r1, r2" else "  rem r1, r2")
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
          // Truncate narrow unsigned results after arithmetic ops
          if !isFloat then
            op match
              case "+" | "-" | "*" | "/" | "%" | "<<" | "~" =>
                resultType match
                  case SyslType.UIntType(8)  => emit("  zeb r1, r1")
                  case SyslType.UIntType(16) => emit("  zes r1, r1")
                  case SyslType.UIntType(32) => emit("  zew r1, r1")
                  case _ =>
              case _ => // comparisons, bitwise &/|/^, >> — no truncation needed

      case TPreInc(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        if locals != null && locals.contains(name) then
          val local = locals(name)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit(s"  addi r1, r1, $step")
          emitNarrow(1, typ)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else
          emit(s"  movi r2, $name")
          emitLoad(1, 2, typ)
          emit(s"  addi r1, r1, $step")
          emitNarrow(1, typ)
          emit(s"  movi r2, $name")
          emitStore(1, 2, typ)

      case TPreDec(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        if locals != null && locals.contains(name) then
          val local = locals(name)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit(s"  addi r1, r1, -$step")
          emitNarrow(1, typ)
          emitAddImm(2, 5, local.offset)
          emitStore(1, 2, local.typ)
        else
          emit(s"  movi r2, $name")
          emitLoad(1, 2, typ)
          emit(s"  addi r1, r1, -$step")
          emitNarrow(1, typ)
          emit(s"  movi r2, $name")
          emitStore(1, 2, typ)

      case TPostInc(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        if locals != null && locals.contains(name) then
          val local = locals(name)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit(s"  addi r3, r1, $step")
          emitNarrow(3, typ)
          emitStore(3, 2, local.typ)
        else
          emit(s"  movi r2, $name")
          emitLoad(1, 2, typ)
          emit(s"  addi r3, r1, $step")
          emitNarrow(3, typ)
          emit(s"  movi r2, $name")
          emitStore(3, 2, typ)

      case TPostDec(name, typ) =>
        val step = typ match
          case SyslType.PtrType(pointee) => stackSize(pointee).max(1)
          case SyslType.ArrayType(elem, _) => stackSize(elem).max(1)
          case _ => 1
        if locals != null && locals.contains(name) then
          val local = locals(name)
          emitAddImm(2, 5, local.offset)
          emitLoad(1, 2, local.typ)
          emit(s"  addi r3, r1, -$step")
          emitNarrow(3, typ)
          emitStore(3, 2, local.typ)
        else
          emit(s"  movi r2, $name")
          emitLoad(1, 2, typ)
          emit(s"  addi r3, r1, -$step")
          emitNarrow(3, typ)
          emit(s"  movi r2, $name")
          emitStore(3, 2, typ)

      case TRangeCheck(inner, range, _, _) =>
        genExpr(inner) // value in r1
        val failLbl = newLabel("range_fail")
        val passLbl = newLabel("range_pass")
        val isFloat = inner.typ.underlying.isFloat
        val isUnsigned = inner.typ.underlying.isUnsigned
        def loadImm(reg: Int, v: Long): Unit =
          if v >= 0 && v <= 255 then emit(s"  ldi r$reg, $v")
          else if v >= 0 && v <= 0xFFFFFFFFL then emit(s"  movi r$reg, $v")
          else emit(s"  ldc r$reg, $v")
        range match
          case IntRange(lo, hi, excl) =>
            // low bound check: fail if val < lo
            loadImm(2, lo)
            emit(s"  ${if isUnsigned then "sltu" else "slt"} r3, r1, r2") // r3 = (val < lo)
            emit(s"  bne r3, r0, $failLbl")
            // high bound check
            loadImm(2, hi)
            if excl then
              // exclusive: fail if !(val < hi)
              emit(s"  ${if isUnsigned then "sltu" else "slt"} r3, r1, r2") // r3 = (val < hi)
              emit(s"  beq r3, r0, $failLbl")
            else
              // inclusive: fail if hi < val
              emit(s"  ${if isUnsigned then "sltu" else "slt"} r3, r2, r1") // r3 = (hi < val)
              emit(s"  bne r3, r0, $failLbl")
          case FloatRange(lo, hi, excl) =>
            emit(s"  ldc r2, $lo")
            emit(s"  fslt r3, r1, r2") // r3 = (val < lo)
            emit(s"  bne r3, r0, $failLbl")
            emit(s"  ldc r2, $hi")
            if excl then
              emit(s"  fslt r3, r1, r2")
              emit(s"  beq r3, r0, $failLbl")
            else
              emit(s"  fslt r3, r2, r1")
              emit(s"  bne r3, r0, $failLbl")
        emit(s"  bra $passLbl")
        emit(s"$failLbl:")
        emit("  ldi r1, 5")  // error code: range check
        emit("  trap 1")
        emit(s"$passLbl:")

      case TCast(TStringLit(value, _), target) if target.isInstanceOf[SyslType.PtrType] =>
        // String literal → *i8 decay: emit data pointer directly, no fat pointer needed
        val bytes = value.getBytes("ISO-8859-1")
        labelCounter += 1
        val strLabel = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_$labelCounter" else s"__str_$labelCounter"
        stringLiterals += ((strLabel, value))
        emit(s"  movi r1, $strLabel")  // r1 = ptr to byte data (past refcount header)

      case TCast(inner, target) if inner.typ == SyslType.StringType && target.isInstanceOf[SyslType.PtrType] =>
        // String→*i8 decay: load ptr field from the 16-byte string struct
        genExpr(inner)               // r1 = address of {ptr, len}
        emit("  ldd r1, r1, r0")    // r1 = ptr (offset 0)

      case TCast(inner, target) if inner.typ.isInstanceOf[SyslType.FuncType] && !target.isInstanceOf[SyslType.FuncType] =>
        // FuncType → i64: extract func_ptr from the {func_ptr, env_ptr} pair
        genExpr(inner)               // r1 = address of pair
        emit("  ldd r1, r1, r0")    // r1 = func_ptr (offset 0)
        // Reclaim the 16-byte pair from the stack
        emitAddImm(7, 7, 16)
        stackOffset += 16

      case TCast(inner, target) if inner.typ.isInstanceOf[SyslType.ArrayType] =>
        // Array decay: genExpr returns address of array in r1, which is address of element 0
        genExpr(inner)
        // r1 already holds the address — for both ptr and int targets this is the right value
        // Apply width truncation/extension if target is a sub-64-bit integer
        import SyslType.*
        target match
          case IntType(8) => emit("  seb r1, r1")
          case IntType(16) => emit("  ses r1, r1")
          case IntType(32) => emit("  sew r1, r1")
          case UIntType(8) => emit("  zeb r1, r1")
          case UIntType(16) => emit("  zes r1, r1")
          case UIntType(32) => emit("  zew r1, r1")
          case _ => // i64, u64, *T — no-op, r1 is already the address

      case TCast(inner, target) =>
        genExpr(inner)
        import SyslType.*
        val srcIsFloat = inner.typ.isFloat
        val tgtIsFloat = target.isFloat
        // Float → int: convert float bits to integer value first
        if srcIsFloat && !tgtIsFloat then emit("  fint r1, r1")
        // Int → float: convert integer value to float bits
        else if !srcIsFloat && tgtIsFloat then emit("  cvt r1, r1")
        // Explicit f64 → f32 rounds to single precision (then re-widens for register use).
        // f32 → f64 is a no-op since f32 values already live as f64 in registers.
        if srcIsFloat && tgtIsFloat && inner.typ == FloatType(64) && target == FloatType(32) then
          emit("  f64tof32 r1, r1")
          emit("  f32tof64 r1, r1")
        target match
          case _: FloatType => // float→float handled above; int→float handled by cvt above

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

      case TUnary("-", operand, resultType) =>
        genExpr(operand)
        if operand.typ.isFloat then emit("  fneg r1, r1")
        else
          emit("  neg r1, r1")
          emitNarrow(1, resultType)

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

      case TUnary("~", operand, resultType) =>
        genExpr(operand)
        emit("  not r1, r1")
        emitNarrow(1, resultType)

      case TFuncRef(name, _) =>
        // Build {func_ptr, env_ptr=null} pair on stack (16 bytes)
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit(s"  movi r1, $name")
        emit("  std r1, r7, r0")       // func_ptr at [sp+0]
        emitAddImm(2, 7, 8)
        emit("  std r0, r2, r0")       // env_ptr = null at [sp+8]
        emit("  mov r1, r7")           // r1 = address of the pair

      case c @ TClosure(params, returnType, body, captures, escapes, _, _) =>
        // Generate a unique name and defer the closure function body
        val closureName = if modulePrefix.nonEmpty then s"__closure_${modulePrefix}_$closureCounter"
                          else s"__closure_$closureCounter"
        closureCounter += 1
        pendingClosures += ((closureName, c))

        // Effective kind = preferred kind, but downgrade StackEnv to HeapEnv if
        // the calling context did not pre-allocate __env_<N>. Contexts that
        // pre-allocate: TCall args, TInterfaceDispatch args, TIndirectCall args,
        // TVarStmt RHS for FuncType locals. Contexts that don't pre-allocate:
        // returned-from-function, in arbitrary expressions, etc. — fallback to
        // heap so the env outlives the construction-site frame.
        val preferredKind = closureKindOf(c)
        val envLocalName = s"__env_${closureCounter - 1}"
        val effectiveKind =
          if preferredKind == FuncKind.StackEnv && !locals.contains(envLocalName)
          then FuncKind.HeapEnv
          else preferredKind
        effectiveKind match
          case FuncKind.NullEnv =>
            // No captures — same as TFuncRef with null env
            emitAddImm(7, 7, -16)
            stackOffset -= 16
            emit(s"  movi r1, $closureName")
            emit("  std r1, r7, r0")       // func_ptr
            emitAddImm(2, 7, 8)
            emit("  std r0, r2, r0")       // env_ptr = null
            emit("  mov r1, r7")

          case FuncKind.StackEnv =>
            // Non-escaping closure with all-non-rc-bearing captures: env lives on
            // the constructing function's stack frame as a pre-allocated local
            // `__env_<closureCounter-1>`. No malloc, no free, no rc header.
            val envLocal = locals(envLocalName)
            // r1 = address of env (= &__env_<n>)
            emitAddImm(1, 5, envLocal.offset)
            emit("  pshd r1")              // save env_ptr (top of stack)
            stackOffset -= 8

            // Copy captures into env. No incr (StackEnv kind has no rc-bearing
            // captures by construction — closureKindOf would have returned
            // HeapEnv otherwise). Skip the self-slot (if any) — wired below
            // from the just-built descriptor. Captures are placed at their
            // natural alignment so mixed-alignment captures don't violate the
            // CPU's strict 8-byte alignment check.
            var envOffset = 0
            var selfOffStack = -1
            for (name, typ) <- captures do
              val size = stackSize(typ).toInt
              val capAlign = stackAlign(typ)
              envOffset = ((envOffset + capAlign - 1) / capAlign) * capAlign
              if c.selfName.contains(name) then
                selfOffStack = envOffset
              else
                emit("  ldd r2, r7, r0")
                if envOffset != 0 then emitAddImm(2, 2, envOffset)
                if locals != null && locals.contains(name) then
                  val local = locals(name)
                  typ match
                    case _: SyslType.StructType | _: SyslType.ArrayType |
                         _: SyslType.SliceType | _: SyslType.EnumType |
                         _: SyslType.FuncType | _: SyslType.InterfaceType |
                         SyslType.StringType =>
                      emitAddImm(3, 5, local.offset)
                      emitAggregateCopy(3, 2, size, stackAlign(typ))
                    case _ =>
                      // Use local.typ for the load — params are stored in 8-byte
                      // I64 slots (big-endian; ldw at the slot base reads the
                      // wrong half), but emitStore uses the capture type so the
                      // env slot keeps the natural width.
                      emitAddImm(3, 5, local.offset)
                      emitLoad(3, 3, local.typ)
                      emitStore(3, 2, typ)
                else
                  emit(s"  movi r3, $name")
                  typ match
                    case _: SyslType.ArrayType | _: SyslType.StructType |
                         _: SyslType.SliceType | _: SyslType.EnumType |
                         _: SyslType.FuncType | _: SyslType.InterfaceType |
                         SyslType.StringType =>
                      emitAggregateCopy(3, 2, size, stackAlign(typ))
                    case _ =>
                      emitLoad(3, 3, typ)
                      emitStore(3, 2, typ)
              envOffset += size

            // Build {func_ptr, env_ptr} pair on stack (16 bytes)
            emit("  popd r2")              // r2 = env_ptr
            stackOffset += 8
            emitAddImm(7, 7, -16)
            stackOffset -= 16
            emit(s"  movi r1, $closureName")
            emit("  std r1, r7, r0")
            emitAddImm(3, 7, 8)
            emit("  std r2, r3, r0")
            emit("  mov r1, r7")
            // Inner-def self-recursion: copy the just-built 16-byte descriptor into
            // env[selfOff]. r2 still holds env_ptr; r7 points at the descriptor.
            if selfOffStack >= 0 then
              for i <- 0 until 16 by 8 do
                emitAddImm(4, 7, i)
                emit("  ldd r4, r4, r0")
                emitAddImm(3, 2, selfOffStack + i)
                emit("  std r4, r3, r0")

          case FuncKind.HeapEnv =>
            // Escaping closure or any rc-bearing captures: heap env with
            // [rc:8 | deinit_ptr:8 | data] header. env_ptr = base + 16.
            // Captures are laid out at their natural alignment so that mixed-alignment
            // captures (e.g. int + FuncType) don't end up at addresses that fail the
            // CPU's strict 8-byte alignment check on ldd/std.
            needsAllocExtern = true
            val deinitOpt = closureEnvDeinitFor(closureName, c)
            val envLayout = {
              var off = 0
              captures.map { (name, typ) =>
                val size = stackSize(typ).toInt
                val align = stackAlign(typ)
                off = ((off + align - 1) / align) * align
                val rec = (name, typ, off, size)
                off += size
                rec
              }
            }
            val envSize = if envLayout.isEmpty then 0
                          else envLayout.last._3 + envLayout.last._4
            val totalSize = envSize + 16

            // malloc(totalSize)
            emitLoadImm(1, totalSize)
            emit("  movi r4, malloc")
            emit("  jalr r6, r4")
            emit("  ldi r2, 1")
            emit("  std r2, r1, r0")          // rc = 1
            deinitOpt match
              case Some(deinitName) =>
                emit(s"  movi r2, $deinitName")
                emitAddImm(3, 1, 8)
                emit("  std r2, r3, r0")      // deinit_ptr
              case None =>
                emitAddImm(3, 1, 8)
                emit("  std r0, r3, r0")      // deinit_ptr = null
            emitAddImm(1, 1, 16)              // r1 = env_ptr (data)
            emit("  pshd r1")                 // save env_ptr (top of stack)
            stackOffset -= 8

            // Copy captures into env + Phase A: incr borrowed rc-bearing captures.
            // Skip the self-slot (if any) — wired below from the just-built descriptor.
            var selfOffHeap = -1
            for (name, typ, envOffset, size) <- envLayout do
              if c.selfName.contains(name) then
                selfOffHeap = envOffset
              else
                emit("  ldd r2, r7, r0")
                if envOffset != 0 then emitAddImm(2, 2, envOffset)
                if locals != null && locals.contains(name) then
                  val local = locals(name)
                  typ match
                    case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType |
                         _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
                      emitAddImm(3, 5, local.offset)
                      emitAggregateCopy(3, 2, size, stackAlign(typ))
                    case _ =>
                      // Use local.typ for the load — params are stored in 8-byte
                      // I64 slots (big-endian; ldw at the slot base reads the
                      // wrong half), but emitStore uses the capture type so the
                      // env slot keeps the natural width.
                      emitAddImm(3, 5, local.offset)
                      emitLoad(3, 3, local.typ)
                      emitStore(3, 2, typ)
                else
                  emit(s"  movi r3, $name")
                  typ match
                    case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType |
                         _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
                      emitAggregateCopy(3, 2, size, stackAlign(typ))
                    case _ =>
                      emitLoad(3, 3, typ)
                      emitStore(3, 2, typ)
                if structHasStringFields(typ) then
                  emit("  ldd r2, r7, r0")
                  emitValueRC(2, envOffset, typ, incr = true)

            emit("  popd r2")             // r2 = env_ptr
            stackOffset += 8
            emitAddImm(7, 7, -16)
            stackOffset -= 16
            emit(s"  movi r1, $closureName")
            emit("  std r1, r7, r0")
            emitAddImm(3, 7, 8)
            emit("  std r2, r3, r0")
            emit("  mov r1, r7")
            // Inner-def self-recursion: copy the just-built 16-byte descriptor into
            // env[selfOff]. r2 still holds env_ptr; r7 points at the descriptor.
            if selfOffHeap >= 0 then
              for i <- 0 until 16 by 8 do
                emitAddImm(4, 7, i)
                emit("  ldd r4, r4, r0")
                emitAddImm(3, 2, selfOffHeap + i)
                emit("  std r4, r3, r0")

      case TInterfaceBox(expr, iface) =>
        // Box a concrete value into an interface: {itable_ptr, data_ptr}
        val structName = expr.typ match
          case SyslType.StructType(name, _, _) => name
          case SyslType.PtrType(SyslType.StructType(name, _, _)) => name
          case SyslType.RefType(SyslType.StructType(name, _, _)) => name
          case other => throw new RuntimeException(s"cannot box $other into interface")

        // Resolve itable label, registering it if first time for this (struct, interface) pair
        val itableLabel = s"__itable_${structName}_${iface.name}"
        if !itables.contains(itableLabel) then
          val funcNames = iface.methods.map { (methodName, _, _, _) =>
            val shortName = s"${structName}_$methodName"
            if declaredFunctions.contains(shortName) then shortName
            else declaredFunctions.find(_.endsWith(s"__$shortName")).getOrElse(shortName)
          }
          itables(itableLabel) = funcNames

        expr.typ match
          case st: SyslType.StructType =>
            // Box by reference: data_ptr = original struct's address. No copy,
            // no malloc. Mutations through the boxed interface propagate back
            // to the source, matching direct-call semantics (`w.method()`
            // passes &w as self) and the interpreter's behavior (which wraps
            // the value in a Cell that the method writes through).
            //
            // The historical implementation heap-allocated a copy here, which
            // made interface dispatch silently lose any mutating-method side
            // effect — `w.write(buf)` through a `Writer` iface filled a heap
            // copy that was discarded on return, so `w` in the caller stayed
            // empty (entire `std/io` test_writer_interface + test_copy
            // cluster). The lifetime risk (returning a Writer of a stack
            // local now dangles) is the same risk the language already has
            // for `*T` of a stack local; it's the user's responsibility,
            // and direct dispatch already had the same shape.
            genExpr(expr)                    // r1 = address of struct data (the original)
            emit("  pshd r1")               // save data_ptr
            stackOffset -= 8
            emitAddImm(7, 7, -16)
            stackOffset -= 16
            emit(s"  movi r1, $itableLabel")
            emit("  std r1, r7, r0")         // itable_ptr at [sp+0]
            emitAddImm(2, 7, 8)
            emit("  popd r3")               // r3 = data_ptr (the saved original)
            stackOffset += 8
            emit("  std r3, r2, r0")         // data_ptr at [sp+8]
            emit("  mov r1, r7")             // r1 = address of the pair

          case _: SyslType.PtrType | _: SyslType.RefType =>
            // Pointer/ref type: data_ptr IS the pointer value itself
            genExpr(expr)                    // r1 = pointer value
            emit("  pshd r1")               // save data_ptr
            stackOffset -= 8
            emitAddImm(7, 7, -16)
            stackOffset -= 16
            emit(s"  movi r1, $itableLabel")
            emit("  std r1, r7, r0")         // itable_ptr at [sp+0]
            emitAddImm(2, 7, 8)
            emit("  popd r3")               // r3 = data_ptr (from saved)
            stackOffset += 8
            emit("  std r3, r2, r0")         // data_ptr at [sp+8]
            emit("  mov r1, r7")             // r1 = address of the pair

          case other =>
            throw new RuntimeException(s"TInterfaceBox: unsupported concrete type $other")

      case TInterfaceDispatch(ifaceVal, methodIndex, args, retType) =>
        // Dynamic dispatch: load itable + data from interface value, call method
        // Interface layout: {itable_ptr: i64, data_ptr: i64}
        // itable[methodIndex] = function pointer
        // Method ABI: r1 = self (data_ptr) — or hidden return slot ptr if method returns
        // via pointer, in which case all user args (including self) go on stack.

        val callStructReturn = returnsViaPointer(retType)
        val retSlotOffset = if callStructReturn then
          val size = retType match
            case st: SyslType.StructType => stackSize(st)
            case et: SyslType.EnumType => stackSize(et)
            case SyslType.StringType | _: SyslType.FuncType | _: SyslType.InterfaceType => 16
            case _: SyslType.SliceType => 24
            case _ => 8
          val aligned = (size + 7) & ~7
          emitAddImm(7, 7, -aligned)
          stackOffset -= aligned
          emit("  mov r1, r7")
          for i <- 0 until aligned by 8 do
            emitAddImm(2, 1, i)
            emit("  std r0, r2, r0")
          stackOffset
        else 0

        // Pre-allocate stack envs for non-escaping non-rc-bearing TClosure args.
        var envPreallocCounter = closureCounter
        for arg <- args do arg match
          case c: TClosure if closureKindOf(c) == FuncKind.StackEnv =>
            val envSize = closureEnvSize(c.captures)
            val alignedEnvSize = (envSize + 7) & ~7
            allocLocal(s"__env_$envPreallocCounter", SyslType.IntType(64), alignedEnvSize)
            envPreallocCounter += 1
          case _ =>

        val savedOffset = stackOffset
        val stringArgPtrOffsets = mutable.ListBuffer[Int]()

        // Push user args right-to-left using the byte-aware pusher
        for arg <- args.reverse do
          evalAndPushArg(arg, stringArgPtrOffsets)

        // Evaluate interface value — r1 = address of {itable_ptr, data_ptr}
        val ifaceEvalPre = stackOffset
        genExpr(ifaceVal)
        emitAddImm(2, 1, 8)
        emit("  ldd r2, r2, r0")          // r2 = data_ptr (self)
        emit("  ldd r3, r1, r0")          // r3 = itable_ptr
        val methodOff = methodIndex * 8
        if methodOff != 0 then emitAddImm(3, 3, methodOff)
        emit("  ldd r4, r3, r0")          // r4 = method function pointer
        // Reclaim ifaceVal temp (preserve r2 = self, r4 = method)
        val ifaceExtra = ifaceEvalPre - stackOffset
        if ifaceExtra > 0 then
          emitAddImm(7, 7, ifaceExtra)
          stackOffset = ifaceEvalPre
        if callStructReturn then
          // Hidden return ptr in r1, self pushed as first stack arg below user args
          // Push self onto stack (8 bytes) — methods receive self at fp+16+(stack args size)
          emit("  pshd r2")
          stackOffset -= 8
          emitAddImm(1, 5, retSlotOffset)
        else
          emit("  mov r1, r2")            // r1 = self
        emit("  jalr r6, r4")
        emitStringArgDecr(stringArgPtrOffsets.toList)
        // Clean up everything we pushed except the return slot (caller needs it)
        val cleanupTo = if callStructReturn then retSlotOffset else savedOffset
        val argsAllocated = cleanupTo - stackOffset
        if argsAllocated != 0 then
          emitAddImm(7, 7, argsAllocated)
          stackOffset = cleanupTo
        if callStructReturn then
          emitAddImm(1, 5, retSlotOffset)

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
            case SyslType.StringType | _: SyslType.FuncType | _: SyslType.InterfaceType => 16
            case _: SyslType.SliceType => 24
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

        // Pre-allocate stack envs for non-escaping non-rc-bearing TClosure args.
        // Must happen before savedOffset is captured so the env space is part of
        // the permanent frame and outlives the call's expression cleanup. The
        // TClosure construction site looks up `__env_<closureCounter>` and uses
        // it as the env address. closureCounter at this point matches what each
        // TClosure will see on its turn (counter is incremented per construction).
        var envPreallocCounter = closureCounter
        for arg <- allArgs do arg match
          case c: TClosure if closureKindOf(c) == FuncKind.StackEnv =>
            val envSize = closureEnvSize(c.captures)
            val alignedEnvSize = (envSize + 7) & ~7
            allocLocal(s"__env_$envPreallocCounter", SyslType.IntType(64), alignedEnvSize)
            envPreallocCounter += 1
          case _ =>

        val savedOffset = stackOffset
        val stringArgPtrOffsets = mutable.ListBuffer[Int]()  // fp-relative ptrs needing post-call decr

        def evalAndPush(arg: TExpr): Unit = evalAndPushArg(arg, stringArgPtrOffsets)

        // With r1-only ABI, there's at most one register arg.
        // If it's a string or slice: pre-evaluate it FIRST (pushing data above stack args),
        // then push stack args, then set r1 to address of the pre-pushed data.
        // This ensures the temp data doesn't sit between callee's saved regs and stack args.
        val regArgOpt = allArgs.headOption.filter(_ => nRegArgs > 0)
        var regAggregateDataOffset = 0 // fp-relative offset of pre-pushed aggregate data
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
            regAggregateDataOffset = stackOffset
            // Track ptr offset for post-call refcount decrement (matches refIncr conditions)
            if needsAllocExtern && !arg.isInstanceOf[TBinary] then
              stringArgPtrOffsets += stackOffset
          else if arg.typ.isInstanceOf[SyslType.SliceType] then
            // Pre-evaluate slice register arg: copy 24-byte struct to a known stack location.
            val preOffset = stackOffset
            genExpr(arg)
            // r1 = address of 24-byte slice struct
            emit("  ldd r2, r1, r0")       // r2 = ptr
            emit("  addi r3, r1, 8")
            emit("  ldd r3, r3, r0")       // r3 = len+cap (8 bytes)
            emit("  addi r4, r1, 16")
            emit("  ldd r4, r4, r0")       // r4 = backref (8 bytes)
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r4")              // push backref
            emit("  pshd r3")              // push len+cap
            emit("  pshd r2")              // push ptr
            stackOffset -= 24
            regAggregateDataOffset = stackOffset
          else if arg.typ.isInstanceOf[SyslType.FuncType] || arg.typ.isInstanceOf[SyslType.InterfaceType] then
            // Pre-evaluate 16-byte pair register arg.
            // Do NOT rewind the temp region before pushing. For an
            // inline-constructed iface arg (`use_shape(Square(9))`),
            // `data_ptr` (r3) points INTO the temp region — and the
            // subsequent `pshd r3; pshd r2` would overwrite the source
            // struct data before the callee dereferences `data_ptr`.
            // Same hazard for stack-env closures as FuncType args. Leak
            // the temp until the function epilogue restores r7. The
            // TCall cleanup adds it all back in one go via
            // `argsAllocated = cleanupTo - stackOffset`.
            genExpr(arg)
            emit("  ldd r2, r1, r0")
            emit("  addi r3, r1, 8")
            emit("  ldd r3, r3, r0")
            emit("  pshd r3")
            emit("  pshd r2")
            stackOffset -= 16
            regAggregateDataOffset = stackOffset
          else if arg.typ.isInstanceOf[SyslType.StructType] || arg.typ.isInstanceOf[SyslType.EnumType] then
            // Pre-evaluate aggregate register arg: copy bytes to stack at known offset.
            // The callee gets the address (passed in r1 below) and copies bytes into its
            // own local frame slot at function entry — true pass-by-value.
            genExpr(arg)
            val aligned = (stackSize(arg.typ) + 7) & ~7
            emitAddImm(7, 7, -aligned)
            stackOffset -= aligned
            for off <- 0 until aligned by 8 do
              emitAddImm(3, 1, off)
              emit("  ldd r3, r3, r0")
              emitAddImm(4, 7, off)
              emit("  std r3, r4, r0")
            regAggregateDataOffset = stackOffset
          else if isStructLikeTempAddr(arg) then
            // Method-on-temporary receiver where the inner expression returns
            // a struct/enum by value: genExpr leaves the inner ret slot on
            // the stack with r1 pointing into it. Copy those bytes into a
            // fresh slot below the inner ret slot so the receiver pointer
            // survives both the OUTER's "extra reclaim" and any subsequent
            // stack-arg pushes. The OUTER's final cleanup at savedOffset
            // reclaims both the copy and the now-dead inner ret slot in one
            // go after the call.
            val structType = arg.asInstanceOf[TTempAddr].expr.typ
            genExpr(arg)
            val aligned = (stackSize(structType) + 7) & ~7
            emitAddImm(7, 7, -aligned)
            stackOffset -= aligned
            for off <- 0 until aligned by 8 do
              emitAddImm(3, 1, off)
              emit("  ldd r3, r3, r0")
              emitAddImm(4, 7, off)
              emit("  std r3, r4, r0")
            regAggregateDataOffset = stackOffset
        }
        // Pre-evaluate stack args that are method-on-temporary receivers
        // into stable slots. Without this, the inner call's ret slot is
        // leaked between previously-pushed args and the receiver pointer,
        // shifting subsequent stack args off their expected offsets.
        // Mirror the regArg pre-eval pattern: copy the inner ret-slot
        // bytes into a fresh slot here, then push only the address in the
        // main loop.
        val stackArgPreEvalOffsets = mutable.Map[Int, Int]()
        for ((arg, idx) <- stackArgs.zipWithIndex) do
          if isStructLikeTempAddr(arg) then
            val structType = arg.asInstanceOf[TTempAddr].expr.typ
            genExpr(arg)
            val aligned = (stackSize(structType) + 7) & ~7
            emitAddImm(7, 7, -aligned)
            stackOffset -= aligned
            for off <- 0 until aligned by 8 do
              emitAddImm(3, 1, off)
              emit("  ldd r3, r3, r0")
              emitAddImm(4, 7, off)
              emit("  std r3, r4, r0")
            stackArgPreEvalOffsets(idx) = stackOffset
        // Push stack args (1+) right-to-left
        for ((arg, idx) <- stackArgs.zipWithIndex.reverse) do
          if stackArgPreEvalOffsets.contains(idx) then
            emitAddImm(1, 5, stackArgPreEvalOffsets(idx))
            emit("  pshd r1")
            stackOffset -= 8
          else
            evalAndPush(arg)
        // Push the register arg, then pop into r1
        regArgOpt.foreach { arg =>
          if arg.typ == SyslType.StringType then
            // Address of the pre-pushed 16-byte string data (above stack args)
            emitAddImm(1, 5, regAggregateDataOffset)
          else if arg.typ.isInstanceOf[SyslType.SliceType] || arg.typ.isInstanceOf[SyslType.FuncType] || arg.typ.isInstanceOf[SyslType.InterfaceType] then
            // Address of the pre-pushed 16-byte data (above stack args)
            emitAddImm(1, 5, regAggregateDataOffset)
          else if arg.typ.isInstanceOf[SyslType.StructType] || arg.typ.isInstanceOf[SyslType.EnumType] then
            // r1 = address of the pre-pushed struct bytes (above stack args)
            emitAddImm(1, 5, regAggregateDataOffset)
          else if isStructLikeTempAddr(arg) then
            // Method-on-temporary receiver: pre-eval already copied the inner
            // call's struct ret slot into a fresh slot at regAggregateDataOffset.
            emitAddImm(1, 5, regAggregateDataOffset)
          else
            val preOffset = stackOffset
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            arg.typ match
              case rt: SyslType.RefType => arg match
                case _: TNew | _: TNewArray | _: TNewEnum =>
                case _ => emitRefIncr(1, refHeaderOffset(rt))
              case _ =>
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
        // Decrement refcounts for string arg temporaries (caller incremented before call,
        // callee uses borrowed ref; this rebalances)
        emitStringArgDecr(stringArgPtrOffsets.toList)
        // Clean up stack args (NOT the return slot — caller needs it)
        val argsAllocated = savedOffset - stackOffset
        if argsAllocated != 0 then
          emitAddImm(7, 7, argsAllocated)
          stackOffset = savedOffset
        // For struct return, r1 = pointer to return slot (which is on our stack)

      case TIndirectCall(callee, args, retType) =>
        // ABI: r1 = arg 0 (or hidden return ptr if returning via pointer), args 1+ on stack,
        // r3 = env_ptr, r4 = func_ptr. Mirrors TCall but resolves the func ptr at runtime.
        val callStructReturn = returnsViaPointer(retType)
        val retSlotOffset = if callStructReturn then
          val size = retType match
            case st: SyslType.StructType => stackSize(st)
            case et: SyslType.EnumType => stackSize(et)
            case SyslType.StringType | _: SyslType.FuncType | _: SyslType.InterfaceType => 16
            case _: SyslType.SliceType => 24
            case _ => 8
          val aligned = (size + 7) & ~7
          emitAddImm(7, 7, -aligned)
          stackOffset -= aligned
          // Zero-init the return slot
          emit("  mov r1, r7")
          for i <- 0 until aligned by 8 do
            emitAddImm(2, 1, i)
            emit("  std r0, r2, r0")
          stackOffset
        else 0

        val allArgs = if callStructReturn then TAddrLit(retSlotOffset) :: args else args
        val nRegArgs = allArgs.length.min(1)
        val stackArgs = allArgs.drop(1)

        // Pre-allocate stack envs for non-escaping non-rc-bearing TClosure args.
        var envPreallocCounter = closureCounter
        for arg <- allArgs do arg match
          case c: TClosure if closureKindOf(c) == FuncKind.StackEnv =>
            val envSize = closureEnvSize(c.captures)
            val alignedEnvSize = (envSize + 7) & ~7
            allocLocal(s"__env_$envPreallocCounter", SyslType.IntType(64), alignedEnvSize)
            envPreallocCounter += 1
          case _ =>

        val savedOffset = stackOffset
        val stringArgPtrOffsets = mutable.ListBuffer[Int]()

        // Push stack args right-to-left using the shared byte-aware pusher
        for arg <- stackArgs.reverse do
          evalAndPushArg(arg, stringArgPtrOffsets)

        // Pre-eval register arg (arg 0) — for aggregate types, push bytes ABOVE stack args
        // and pass the address in r1 (matches TCall pattern).
        val regArgOpt = allArgs.headOption.filter(_ => nRegArgs > 0)
        var regAggregateDataOffset = 0
        regArgOpt.foreach { arg =>
          if arg.typ == SyslType.StringType then
            val preOffset = stackOffset
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            if needsAllocExtern then
              arg match
                case _: TBinary =>
                case _ =>
                  emit("  pshd r1")
                  emit("  ldd r1, r1, r0")
                  emitRefIncr(1, 8)
                  emit("  popd r1")
            emit("  addi r2, r1, 8")
            emit("  ldd r2, r2, r0")
            emit("  ldd r1, r1, r0")
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r2")
            emit("  pshd r1")
            stackOffset -= 16
            regAggregateDataOffset = stackOffset
            if needsAllocExtern && !arg.isInstanceOf[TBinary] then
              stringArgPtrOffsets += stackOffset
          else if arg.typ.isInstanceOf[SyslType.SliceType] then
            val preOffset = stackOffset
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            emit("  ldd r2, r1, r0")
            emit("  addi r3, r1, 8")
            emit("  ldd r3, r3, r0")
            emit("  addi r4, r1, 16")
            emit("  ldd r4, r4, r0")
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r4")
            emit("  pshd r3")
            emit("  pshd r2")
            stackOffset -= 24
            regAggregateDataOffset = stackOffset
          else if arg.typ.isInstanceOf[SyslType.FuncType] || arg.typ.isInstanceOf[SyslType.InterfaceType] then
            val preOffset = stackOffset
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            emit("  ldd r2, r1, r0")
            emit("  addi r3, r1, 8")
            emit("  ldd r3, r3, r0")
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r3")
            emit("  pshd r2")
            stackOffset -= 16
            regAggregateDataOffset = stackOffset
          else if arg.typ.isInstanceOf[SyslType.StructType] || arg.typ.isInstanceOf[SyslType.EnumType] then
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            val aligned = (stackSize(arg.typ) + 7) & ~7
            emitAddImm(7, 7, -aligned)
            stackOffset -= aligned
            for off <- 0 until aligned by 8 do
              emitAddImm(3, 1, off)
              emit("  ldd r3, r3, r0")
              emitAddImm(4, 7, off)
              emit("  std r3, r4, r0")
            regAggregateDataOffset = stackOffset
          else
            // Scalar register arg: push 8 bytes
            val preOffset = stackOffset
            arg match
              case TAddrLit(off) => emitAddImm(1, 5, off)
              case _ => genExpr(arg)
            arg.typ match
              case rt: SyslType.RefType => arg match
                case _: TNew | _: TNewArray | _: TNewEnum =>
                case _ => emitRefIncr(1, refHeaderOffset(rt))
              case _ =>
            val extra = preOffset - stackOffset
            if extra > 0 then
              emitAddImm(7, 7, extra)
              stackOffset = preOffset
            emit("  pshd r1")
            stackOffset -= 8
        }
        // Set r1 to register-arg value: address of pre-pushed bytes for aggregates,
        // or pop the saved 8 bytes for scalars.
        val afterArgPush = stackOffset
        // Evaluate callee — r1 = address of {func_ptr, env_ptr} pair
        genExpr(callee)
        emitAddImm(3, 1, 8)
        emit("  ldd r3, r3, r0")  // r3 = env_ptr
        emit("  ldd r4, r1, r0")  // r4 = func_ptr
        val calleeExtra = afterArgPush - stackOffset
        if calleeExtra > 0 then
          emitAddImm(7, 7, calleeExtra)
          stackOffset = afterArgPush
        // For aggregate register arg, r1 = address of pre-pushed bytes; for scalar, popd
        regArgOpt.foreach { arg =>
          if arg.typ == SyslType.StringType ||
             arg.typ.isInstanceOf[SyslType.SliceType] ||
             arg.typ.isInstanceOf[SyslType.FuncType] ||
             arg.typ.isInstanceOf[SyslType.InterfaceType] ||
             arg.typ.isInstanceOf[SyslType.StructType] ||
             arg.typ.isInstanceOf[SyslType.EnumType] then
            emitAddImm(1, 5, regAggregateDataOffset)
          else
            emit("  popd r1")
            stackOffset += 8
        }
        emit("  jalr r6, r4")
        emitStringArgDecr(stringArgPtrOffsets.toList)
        // Clean up everything we pushed except the return slot (caller needs it)
        val cleanupTo = if callStructReturn then retSlotOffset else savedOffset
        val argsAllocated = cleanupTo - stackOffset
        if argsAllocated != 0 then
          emitAddImm(7, 7, argsAllocated)
          stackOffset = cleanupTo
        // For struct return, point r1 at the return slot (which is on our stack)
        if callStructReturn then
          emitAddImm(1, 5, retSlotOffset)

      case TAddrOf(name, _) =>
        if locals != null && locals.contains(name) then
          emitLocalAddr(name, 1) // r1 = stack address of local variable
        else
          emit(s"  movi r1, $name") // r1 = address of global variable

      case TTempAddr(expr, _) =>
        // Evaluate expression, store on stack, return address
        genExpr(expr) // r1 = value (or address of struct)
        expr.typ match
          case st: SyslType.StructType =>
            // r1 already points to struct data, just use it
            ()
          case _ =>
            // Scalar: push to stack, take address
            emit("  pshd r1")
            emit("  mov r1, r7") // r1 = sp (points to the pushed value)

      case TAddrOfIndex(array, index, SyslType.PtrType(elemType)) =>
        val elemSize = stackSize(elemType)
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = array/slice base; for SliceType this is the descriptor's address
        emit("  popd r2")        // r2 = index
        // For slices the descriptor's `ptr` field (offset 0) is the data start;
        // genExpr returns the descriptor address, so we must deref to get the
        // data pointer. Arrays/pointers/RefType(SliceType) already give the
        // data pointer directly. Without this, &slot[i] resolves to the
        // address of the descriptor itself rather than the heap data, so any
        // write through the resulting pointer corrupts the caller's frame.
        if array.typ.isInstanceOf[SyslType.SliceType] then
          emit("  ldd r1, r1, r0") // r1 = slice.ptr
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
        val st = obj.typ match
          case s: SyslType.StructType => s
          case SyslType.PtrType(s: SyslType.StructType) => s
          case other => throw new RuntimeException(s"TFieldAccess: expected StructType, got $other")
        val off = fieldOffset(st, fieldIndex)
        emitStructAddr(obj)        // r1 = struct address
        if off != 0 then emitAddImm(1, 1, off)
        fieldType match
          case _: SyslType.ArrayType | _: SyslType.StructType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
            () // aggregate types: address is the value (don't dereference)
          case _ =>
            emitLoad(1, 1, fieldType) // scalar types: load the value

      case TDeref(inner, typ) =>
        genExpr(inner)           // r1 = pointer address
        typ match
          case _: SyslType.StructType | _: SyslType.EnumType | _: SyslType.ArrayType |
               _: SyslType.FuncType | _: SyslType.SliceType | SyslType.StringType =>
            () // aggregate: pointer IS the base address, don't load
          case _ =>
            emitLoad(1, 1, typ)  // scalar: load value at pointer

      case TIndex(array, index, elemType) if array.typ == SyslType.StringType =>
        // String indexing: bounds-checked byte access
        genExpr(index)           // r1 = index
        emit("  pshd r1")
        genExpr(array)           // r1 = address of {ptr, len} string struct
        emit("  popd r2")        // r2 = index
        // Bounds check: 0 <= index < len
        emit("  addi r3, r1, 8")
        emit("  ldd r3, r3, r0") // r3 = len (i64 from string fat pointer {ptr(8), len(8)})
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
        stackOffset -= 8
        val preOffset = stackOffset
        genExpr(array)           // r1 = slice struct address
        // Reclaim any stack temp genExpr left between the index slot and r7
        // (e.g. a slice-returning call's 24-byte ret slot). Without this,
        // popd r2 reads from inside that ret slot instead of the pushed
        // index. r1 still points into the descriptor; subsequent ldds for
        // ptr/len read it before anything overwrites it.
        val extra = preOffset - stackOffset
        if extra > 0 then
          emitAddImm(7, 7, extra)
          stackOffset = preOffset
        emit("  popd r2")        // r2 = index
        stackOffset += 8
        // Bounds check
        emit("  addi r3, r1, 8")
        emit("  ldw r3, r3, r0") // r3 = len (32-bit in slice struct)
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
        elemType match
          case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
            () // address is the value for aggregates
          case _ => emitLoad(1, 1, elemType)

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
          case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
            () // address is the value for aggregates
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
          case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType | _: SyslType.SliceType | _: SyslType.EnumType | _: SyslType.FuncType | _: SyslType.InterfaceType =>
            () // address is the value for aggregates
          case _ => emitLoad(1, 1, elemType) // load scalar with proper width

      case TArrayDecl(size, typ) =>
        // Allocate array on stack with proper element size, rounded up to 8
        val elemType = typ match
          case SyslType.ArrayType(e, _) => e
          case other => throw new RuntimeException(s"TArrayDecl: expected ArrayType, got $other")
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
        val bytes = value.getBytes("ISO-8859-1")
        labelCounter += 1
        val strLabel = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_$labelCounter" else s"__str_$labelCounter"
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
        // Convert value to string — dispatch on type.
        // __str_int handles integers/bool (bits interpreted as signed i64);
        // __str_float handles f64 values.
        val isFloat = inner.typ.isFloat
        genExpr(inner) // r1 = value (integer bits or f64 bits)
        if isFloat then needsStrFloat = true
        else needsStrInt = true
        needsAllocExtern = true
        // Allocate 16-byte return slot for the result string
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r0, r7, r0")
        emitAddImm(2, 7, 8)
        emit("  std r0, r2, r0")
        // Push value as stack arg
        emit("  pshd r1")
        stackOffset -= 8
        // r1 = hidden return slot ptr (just above the pushed value)
        emitAddImm(1, 7, 8)
        // Call helper
        val mp = if modulePrefix.nonEmpty then s"_$modulePrefix" else ""
        if isFloat then emit(s"  movi r4, __str_float$mp")
        else emit(s"  movi r4, __str_int$mp")
        emit("  jalr r6, r4")
        // Clean value arg
        emitAddImm(7, 7, 8)
        stackOffset += 8
        // r1 = address of return slot (which now contains {ptr, len})
        emit("  mov r1, r7")

      case TFmtStr(inner, spec) =>
        // Lower formatted-string interpolations to a runtime helper. For
        // integer verbs we route through __str_fmt_i64 with the appropriate
        // base/width/flag bits — this matches SVM's __svm_str_fmt_i64
        // (audit item #14). For %s we just pass the string through (padded
        // string verbs aren't implemented yet on any backend; matches LLVM/SVM
        // behavior). Anything else degrades to plain TStr semantics.
        val verb = spec.verb
        verb match
          case 'd' | 'x' | 'X' | 'o' | 'b' if inner.typ.isIntegral =>
            // Allocate 16-byte return slot for the result string struct
            emitAddImm(7, 7, -16)
            stackOffset -= 16
            emit("  std r0, r7, r0")
            emitAddImm(2, 7, 8)
            emit("  std r0, r2, r0")

            // Evaluate value into r1
            genExpr(inner) // r1 = value (integer bits)
            // Widen narrow ints to i64 for the runtime helper. Mirrors SVM
            // logic: signed types sext via shl/sar pair, unsigned mask off
            // the high bits.
            val t = inner.typ.underlying
            if t.bitWidth < 64 then
              if t.isSigned then
                emit(s"  sext r1, r1") // sign-extend any narrow int up to 64 bits
              else
                t.bitWidth match
                  case 8  => emit("  zeb r1, r1")
                  case 16 => emit("  zes r1, r1")
                  case 32 => emit("  zew r1, r1")
                  case _ => ()

            // Compute flag bits up-front (compile-time constants).
            var flags = 0
            if spec.zeroPad then flags |= 0x1
            if spec.leftAlign then flags |= 0x2
            if spec.showSign then flags |= 0x4
            if spec.upperCase || verb == 'X' then flags |= 0x8
            val base = verb match
              case 'd'       => 10
              case 'x' | 'X' => 16
              case 'o'       => 8
              case 'b'       => 2
              case _         => 10

            // Push stack args right-to-left so the callee sees:
            //   [fp+24] = n, [fp+32] = base, [fp+40] = width, [fp+48] = flags
            // r1 currently holds n; preserve it while loading the constants
            // through r2.
            emitLoadImm(2, flags);      emit("  pshd r2"); stackOffset -= 8 // flags  → [fp+48]
            emitLoadImm(2, spec.width); emit("  pshd r2"); stackOffset -= 8 // width  → [fp+40]
            emitLoadImm(2, base);       emit("  pshd r2"); stackOffset -= 8 // base   → [fp+32]
            emit("  pshd r1");          stackOffset -= 8                    // n      → [fp+24]

            // r1 = address of pre-allocated return slot (which sits at sp+32 — past 4 stack args).
            emitAddImm(1, 7, 32)
            val mpf = if modulePrefix.nonEmpty then s"_$modulePrefix" else ""
            emit(s"  movi r4, __str_fmt_i64$mpf")
            emit("  jalr r6, r4")
            // Clean up 4 stack args (32 bytes)
            emitAddImm(7, 7, 32)
            stackOffset += 32
            // r1 = return slot address
            emit("  mov r1, r7")
            needsStrFmtI64 = true
            needsAllocExtern = true

          case 's' if inner.typ.underlying == SyslType.StringType =>
            // Pass-through. Width/pad on string verbs isn't implemented
            // anywhere yet — matches SVM/LLVM behavior.
            genExpr(inner)

          case _ =>
            // Any other shape (bool with %d, etc.) — fall back to plain TStr
            // semantics. This is a lossy fallback: width/pad flags get dropped.
            // Matches SVM precedent.
            genExpr(TStr(inner))

      case TQuantifier(kind, name, nameType, lo, hi, inclusive, pred, _) =>
        // Lower `for all/some x in lo..hi => P(x)` as a short-circuiting counted loop
        // that accumulates a bool. `all` seeds result=1 and bails out with 0 on the first
        // counterexample; `some` seeds 0 and bails out with 1 on the first witness.
        // Using direct `iter < hi` / `iter <= hi` comparisons avoids the underflow that
        // would occur if we precomputed `end = hi - 1` for an unsigned empty range.
        enterScope()
        val resultLocal = allocLocal("__quant_result", SyslType.I64)
        val initBit = if kind == "all" then 1 else 0
        emit(s"  ldi r1, $initBit")
        emitAddImm(2, 5, resultLocal.offset)
        emit("  std r1, r2, r0")

        val iterLocal = allocLocal(name, nameType)
        genExpr(lo)
        emitAddImm(2, 5, iterLocal.offset)
        emitStore(1, 2, nameType)

        val hiLocal = allocLocal("__quant_hi", nameType)
        genExpr(hi)
        emitAddImm(2, 5, hiLocal.offset)
        emitStore(1, 2, nameType)

        val condLbl = newLabel("quant_cond")
        val endLbl  = newLabel("quant_end")
        val contLbl = newLabel("quant_cont")
        emit(s"$condLbl")
        emitAddImm(1, 5, iterLocal.offset)
        emitLoad(1, 1, nameType)
        emitAddImm(2, 5, hiLocal.offset)
        emitLoad(2, 2, nameType)
        val cmpOp = if nameType.isSigned then "slt" else "sltu"
        if inclusive then
          // Continue while iter <= hi; bail when hi < iter.
          emit(s"  $cmpOp r3, r2, r1")
          emit(s"  bne r3, r0, $endLbl")
        else
          // Continue while iter < hi; bail when !(iter < hi).
          emit(s"  $cmpOp r3, r1, r2")
          emit(s"  beq r3, r0, $endLbl")

        genExpr(pred)
        if kind == "all" then
          emit(s"  bne r1, r0, $contLbl")
          emit("  ldi r1, 0")
          emitAddImm(2, 5, resultLocal.offset)
          emit("  std r1, r2, r0")
          emit(s"  bra $endLbl")
        else
          emit(s"  beq r1, r0, $contLbl")
          emit("  ldi r1, 1")
          emitAddImm(2, 5, resultLocal.offset)
          emit("  std r1, r2, r0")
          emit(s"  bra $endLbl")

        emit(s"$contLbl")
        emitAddImm(1, 5, iterLocal.offset)
        emitLoad(2, 1, nameType)
        emit("  addi r2, r2, 1")
        emitStore(2, 1, nameType)
        emit(s"  bra $condLbl")

        emit(s"$endLbl")
        emitAddImm(1, 5, resultLocal.offset)
        emit("  ldd r1, r1, r0")
        leaveScope()

      case TIfExpr(cond, thenBody, elseBody, typ) =>
        val elseLabel = newLabel("else")
        val endLabel = newLabel("endif")
        val isString = typ == SyslType.StringType
        val isAggregate = typ.isInstanceOf[SyslType.EnumType] || typ.isInstanceOf[SyslType.StructType] || typ.isInstanceOf[SyslType.SliceType] || typ.isInstanceOf[SyslType.FuncType] || typ.isInstanceOf[SyslType.InterfaceType]
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
              case TValuePattern(v) if scrutinee.typ == SyslType.StringType =>
                // String pattern: compare lengths then bytes
                // Evaluate pattern string first (may allocate temps)
                val prePatStr = stackOffset
                genExpr(v)                          // r1 = addr of pattern string struct
                emit("  ldd r2, r1, r0")           // r2 = pattern ptr
                emit("  addi r3, r1, 8")
                emit("  ldd r3, r3, r0")           // r3 = pattern len
                val extraPatStr = prePatStr - stackOffset
                if extraPatStr > 0 then
                  emitAddImm(7, 7, extraPatStr)
                  stackOffset = prePatStr
                emit("  pshd r2")                  // save pattern ptr
                emit("  pshd r3")                  // save pattern len
                stackOffset -= 16
                // Load scrutinee string {ptr, len}
                emitAddImm(1, 5, scrutineeOffset)
                emit("  ldd r1, r1, r0")           // r1 = addr of scrutinee string struct
                emit("  ldd r4, r1, r0")           // r4 = scrutinee ptr
                emit("  addi r1, r1, 8")
                emit("  ldd r1, r1, r0")           // r1 = scrutinee len
                // r1 = scrutinee len, r4 = scrutinee ptr
                // Stack: sp+0 = pattern len, sp+8 = pattern ptr
                // Compare lengths
                emit("  ldd r2, r7, r0")           // r2 = pattern len
                val strPatNext = newLabel("str_pat_next")
                emit(s"  bne r1, r2, $strPatNext") // lengths differ → skip
                // Lengths match — compare bytes (r1 = len as loop counter)
                emitAddImm(2, 7, 8)
                emit("  ldd r2, r2, r0")           // r2 = pattern ptr
                // r4 = scrutinee ptr, r2 = pattern ptr, r1 = len
                val cmpL = newLabel("str_pat_cmp")
                val cmpMis = newLabel("str_pat_mis")
                val strHit = newLabel("str_pat_hit")
                emit(s"$cmpL")
                emit(s"  beq r1, r0, $strHit")    // all bytes matched → hit
                emit("  ldb r3, r4, r0")
                emit("  pshd r1")
                emit("  ldb r1, r2, r0")
                emit(s"  bne r3, r1, $cmpMis")
                emit("  popd r1")
                emit("  addi r4, r4, 1")
                emit("  addi r2, r2, 1")
                emit("  addi r1, r1, -1")
                emit(s"  bra $cmpL")
                emit(s"$cmpMis")
                emit("  popd r1")                  // clean saved counter
                emit(s"$strPatNext")
                // Clean up pattern ptr/len from stack (both paths converge here)
                emitAddImm(7, 7, 16)
                stackOffset += 16
                emit(s"  bra $nextArm")            // no match, skip to next arm
                emit(s"$strHit")
                // Clean up pattern ptr/len, then jump to hit
                emitAddImm(7, 7, 16)
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
              case TDestructurePattern(st, _, _, nested) =>
                if nested.forall(_.isEmpty) then
                  emit(s"  bra $hitLabel")      // destructure always matches
                else
                  // Nested checks: only branch to hit if ALL nested also match.
                  // The struct itself is at scrutineeOffset; nested fields are
                  // at field offsets within it.
                  val patFail = newLabel("pat_fail")
                  for ((subOpt, i) <- nested.zipWithIndex) do subOpt.foreach { sub =>
                    val off = fieldOffset(st, i)
                    emitNestedPatternCheck(sub, st.fields(i)._2, scrutineeOffset, off.toLong, patFail)
                  }
                  emit(s"  bra $hitLabel")
                  emit(s"$patFail")
              case TVariantPattern(et, variantIndex, _, _, nested) =>
                // Load tag from scrutinee enum and compare with variant index.
                emitAddImm(1, 5, scrutineeOffset)
                emit("  ldd r1, r1, r0")     // r1 = enum address
                emit("  ldw r1, r1, r0")     // r1 = tag (i32 at offset 0)
                emitLoadImm(2, variantIndex)
                if nested.forall(_.isEmpty) then
                  emit(s"  beq r1, r2, $hitLabel")
                else
                  // Outer tag must match AND every nested sub-pattern must match
                  // before we can go to hitLabel. Branch to a per-alternative
                  // fail label on any mismatch; fall through to the next
                  // pattern alternative on fail.
                  val patFail = newLabel("pat_fail")
                  emit(s"  bne r1, r2, $patFail")
                  val variantFields = et.variants(variantIndex)._2
                  val dataOff = et.dataOffset.toInt
                  var fieldOff = 0
                  for ((subOpt, i) <- nested.zipWithIndex) do
                    val (_, fieldType) = variantFields(i)
                    val align = stackAlign(fieldType)
                    fieldOff = ((fieldOff + align - 1) / align) * align
                    subOpt.foreach { sub =>
                      emitNestedPatternCheck(sub, fieldType, scrutineeOffset, (dataOff + fieldOff).toLong, patFail)
                    }
                    fieldOff += fieldType.sizeOf.toInt
                  emit(s"  bra $hitLabel")
                  emit(s"$patFail")
          emit(s"  bra $nextArm")
          emit(s"$hitLabel")
          enterScope()
          // Bind destructure/variant patterns BEFORE guard (guard may reference bindings)
          for pat <- arm.patterns do
            pat match
              case TDestructurePattern(st, bindings, fieldTypes, nested) =>
                for (binding, i) <- bindings.zipWithIndex do
                  val fieldType = fieldTypes(i)
                  binding.foreach { name =>
                    val off = fieldOffset(st, i)
                    // Allocate the binding's local first (allocLocal moves sp), then
                    // recompute the scrutinee field address — that address is in
                    // physical memory (fp-relative), so post-allocation it's stable.
                    val local = allocLocal(name, fieldType)
                    emitAddImm(1, 5, scrutineeOffset)
                    emit("  ldd r1, r1, r0")  // r1 = scrutinee address
                    if off != 0 then emitAddImm(1, 1, off)
                    fieldType match
                      case SyslType.StringType | _: SyslType.StructType | _: SyslType.EnumType
                        | _: SyslType.SliceType | _: SyslType.ArrayType
                        | _: SyslType.FuncType | _: SyslType.InterfaceType =>
                        // Aggregate: r1 = field address (src), copy bytes to local.
                        emitAddImm(2, 5, local.offset)
                        emitStore(1, 2, fieldType)
                        // Binding is borrowed: incr the buffer/strings (scope exit
                        // path will decr to balance).
                        fieldType match
                          case SyslType.StringType if needsAllocExtern =>
                            emitAddImm(1, 5, local.offset)
                            emit("  ldd r1, r1, r0")
                            emitRefIncr(1, 8)
                          case st2: SyslType.StructType if structHasStringFields(st2) =>
                            emitStructStringFieldsRC(5, local.offset, st2, incr = true)
                          case et2: SyslType.EnumType if structHasStringFields(et2) =>
                            emitEnumStringFieldsRC(5, local.offset, et2, incr = true)
                          case _ =>
                      case _ =>
                        emitLoad(1, 1, fieldType)
                        emitAddImm(2, 5, local.offset)
                        emitStore(1, 2, fieldType)
                  }
                // Nested patterns: recurse to bind names from sub-patterns.
                // The outer synthetic binding has already copied the field
                // value into a local; we descend into the original scrutinee
                // (with absolute offset accumulating through field positions)
                // to bind any deeper named fields.
                if nested.nonEmpty then
                  for ((subOpt, i) <- nested.zipWithIndex) do subOpt.foreach { sub =>
                    val ft = fieldTypes(i)
                    val off = fieldOffset(st, i)
                    emitNestedPatternBindings(sub, ft, scrutineeOffset, off.toLong)
                  }
              case TVariantPattern(et, variantIndex, bindings, fieldTypes, nested) =>
                val dataOff = et.dataOffset.toInt
                val variantFields = et.variants(variantIndex)._2
                var fieldOff = 0
                for (binding, i) <- bindings.zipWithIndex do
                  val (_, fieldType) = variantFields(i)
                  val align = stackAlign(fieldType)
                  fieldOff = ((fieldOff + align - 1) / align) * align
                  binding.foreach { name =>
                    // Allocate the binding's local first (allocLocal moves sp), then
                    // recompute the scrutinee field address — that address is in
                    // physical memory (fp-relative), so post-allocation it's stable.
                    val local = allocLocal(name, fieldType)
                    emitAddImm(1, 5, scrutineeOffset)
                    emit("  ldd r1, r1, r0")  // r1 = enum address
                    if dataOff + fieldOff != 0 then emitAddImm(1, 1, dataOff + fieldOff)
                    fieldType match
                      case SyslType.StringType | _: SyslType.StructType | _: SyslType.EnumType
                        | _: SyslType.SliceType | _: SyslType.ArrayType
                        | _: SyslType.FuncType | _: SyslType.InterfaceType =>
                        // Aggregate: r1 = field address (src), copy bytes to local.
                        emitAddImm(2, 5, local.offset)
                        emitStore(1, 2, fieldType)
                        // Binding is borrowed: incr the buffer/strings (scope exit
                        // path will decr to balance).
                        fieldType match
                          case SyslType.StringType if needsAllocExtern =>
                            emitAddImm(1, 5, local.offset)
                            emit("  ldd r1, r1, r0")
                            emitRefIncr(1, 8)
                          case st: SyslType.StructType if structHasStringFields(st) =>
                            emitStructStringFieldsRC(5, local.offset, st, incr = true)
                          case et2: SyslType.EnumType if structHasStringFields(et2) =>
                            emitEnumStringFieldsRC(5, local.offset, et2, incr = true)
                          case _ =>
                      case _ =>
                        emitLoad(1, 1, fieldType)
                        emitAddImm(2, 5, local.offset)
                        emitStore(1, 2, fieldType)
                  }
                  // Recurse into nested for THIS field BEFORE incrementing fieldOff.
                  if i < nested.length then nested(i).foreach { sub =>
                    emitNestedPatternBindings(sub, fieldType, scrutineeOffset, (dataOff + fieldOff).toLong)
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
            emit("  ldw r1, r1, r0") // len at offset 8 (i32 in slice struct)
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

      case TSliceExpr(array, low, high, SyslType.StringType) =>
        // Substring s[lo:hi]: allocate new rc'd buffer, memcpy hi-lo bytes.
        // Result is an owned {ptr, len} with rc=1.
        needsAllocExtern = true
        // Eval source string; push ptr(8) and len(8). Reclaim genExpr temps first.
        val pre = stackOffset
        genExpr(array)                       // r1 = addr of {ptr, len}
        emit("  ldd r2, r1, r0")             // r2 = src_ptr
        emit("  addi r3, r1, 8")
        emit("  ldd r3, r3, r0")             // r3 = src_len (i64)
        val extra = pre - stackOffset
        if extra > 0 then
          emitAddImm(7, 7, extra)
          stackOffset = pre
        emit("  pshd r2")                    // [src_ptr]
        emit("  pshd r3")                    // [src_len] [src_ptr]
        stackOffset -= 16
        // Eval lo (default 0)
        low match
          case Some(loExpr) => genExpr(loExpr)
          case None         => emit("  ldi r1, 0")
        emit("  pshd r1")                    // [lo] [src_len] [src_ptr]
        stackOffset -= 8
        // Eval hi (default src_len at sp+8)
        high match
          case Some(hiExpr) => genExpr(hiExpr)
          case None =>
            emitAddImm(1, 7, 8)
            emit("  ldd r1, r1, r0")         // r1 = src_len
        emit("  pshd r1")                    // [hi] [lo] [src_len] [src_ptr]
        stackOffset -= 8
        // Pop hi/lo into r1/r2; src_len stays at sp+0, src_ptr at sp+8
        emit("  popd r1")                    // r1 = hi
        emit("  popd r2")                    // r2 = lo
        stackOffset += 16
        // Bounds check: 0 <= lo <= hi <= src_len
        val errLabel = newLabel("substr_err")
        val okLabel  = newLabel("substr_ok")
        emit("  slt r4, r2, r0")             // lo < 0?
        emit(s"  bne r4, r0, $errLabel")
        emit("  slt r4, r1, r2")             // hi < lo?
        emit(s"  bne r4, r0, $errLabel")
        emit("  ldd r4, r7, r0")             // r4 = src_len
        emit("  slt r4, r4, r1")             // src_len < hi?
        emit(s"  bne r4, r0, $errLabel")
        emit(s"  bra $okLabel")
        emit(s"$errLabel")
        emit("  ldi r1, 1")                  // out-of-bounds
        emit("  trap 1")
        emit(s"$okLabel")
        // Compute new_len = hi - lo. Save lo and new_len on stack.
        emit("  sub r3, r1, r2")             // r3 = new_len
        emit("  pshd r2")                    // [lo] [src_len] [src_ptr]
        emit("  pshd r3")                    // [new_len] [lo] [src_len] [src_ptr]
        stackOffset -= 16
        // malloc(new_len + 8)
        emit("  addi r1, r3, 8")
        emit("  pshd r1")                    // [arg] [new_len] [lo] [src_len] [src_ptr]
        stackOffset -= 8
        emit("  movi r4, malloc")
        emit("  jalr r6, r4")
        emit("  popd r3")                    // pop malloc arg
        stackOffset += 8
        // Null check
        val allocOk = newLabel("substr_alloc_ok")
        emit(s"  bne r1, r0, $allocOk")
        emit("  ldi r1, 2")
        emit("  trap 1")
        emit(s"$allocOk")
        // r1 = base; rc=1 at [base]
        emit("  ldi r2, 1")
        emit("  std r2, r1, r0")
        // Stack: sp+0 = new_len, sp+8 = lo, sp+16 = src_len, sp+24 = src_ptr
        // dest = base + 8
        emit("  addi r1, r1, 8")
        // src = src_ptr + lo
        emitAddImm(2, 7, 24)
        emit("  ldd r2, r2, r0")             // r2 = src_ptr
        emitAddImm(3, 7, 8)
        emit("  ldd r3, r3, r0")             // r3 = lo
        emit("  add r2, r2, r3")             // r2 = src_ptr + lo
        // Copy new_len bytes
        emit("  ldd r3, r7, r0")             // r3 = new_len (counter)
        val copyLoop = newLabel("substr_copy")
        val copyDone = newLabel("substr_copy_done")
        emit(s"$copyLoop")
        emit(s"  beq r3, r0, $copyDone")
        emit("  ldb r4, r2, r0")
        emit("  stb r4, r1, r0")
        emit("  addi r1, r1, 1")
        emit("  addi r2, r2, 1")
        emit("  addi r3, r3, -1")
        emit(s"  bra $copyLoop")
        emit(s"$copyDone")
        // Build result {ptr, len}: data = base+8 (recompute from r1 - new_len),
        // but easier to recompute base from saved state. Use base via re-derivation:
        // We have r1 = base + 8 + new_len. Subtract new_len → base + 8 = data ptr.
        emit("  ldd r3, r7, r0")             // r3 = new_len
        emit("  sub r1, r1, r3")             // r1 = base + 8 = data ptr
        emit("  mov r2, r3")                 // r2 = new_len (also goes to result)
        // Pop saved values: new_len(8) + lo(8) + src_len(8) + src_ptr(8) = 32 bytes
        emitAddImm(7, 7, 32)
        stackOffset += 32
        // Allocate 16-byte result {ptr, len}
        emitAddImm(7, 7, -16)
        stackOffset -= 16
        emit("  std r1, r7, r0")
        emitAddImm(3, 7, 8)
        emit("  std r2, r3, r0")
        emit("  mov r1, r7")

      case TSliceExpr(array, low, high, SyslType.SliceType(elemType)) =>
        val elemSize = stackSize(elemType)
        // Evaluate array and push {ptr, len, cap, backref} onto stack
        genExpr(array)
        array.typ match
          case SyslType.RefType(SyslType.SliceType(_)) =>
            // r1 = data pointer; len at [r1 - 8], cap = len
            // backref = dataPtr - 16 (allocation base where refcount lives)
            emitAddImm(2, 1, -16)
            emit("  pshd r2")           // push backref
            stackOffset -= 8
            emitAddImm(2, 1, -8)
            emit("  ldd r2, r2, r0")    // r2 = length
            emit("  pshd r2")           // push cap (== len)
            emit("  pshd r2")           // push len
            emit("  pshd r1")           // push ptr
            stackOffset -= 24
            // Increment refcount at backref (refcount is at *backref directly)
            emitAddImm(2, 7, 24)        // r2 = address of backref on stack
            emit("  ldd r2, r2, r0")    // r2 = backref value
            emitRefIncr(2, 0)
          case SyslType.SliceType(_) =>
            // r1 = address of 24-byte slice struct {ptr(8), len(4), cap(4), backref(8)}
            // Inherit backref from source
            emit("  addi r2, r1, 16")
            emit("  ldd r2, r2, r0")    // r2 = backref
            emit("  pshd r2")           // push backref
            stackOffset -= 8
            emit("  addi r2, r1, 12")
            emit("  ldw r2, r2, r0")    // r2 = cap (i32)
            emit("  pshd r2")
            emit("  addi r2, r1, 8")
            emit("  ldw r2, r2, r0")    // r2 = len (i32)
            emit("  pshd r2")
            emit("  ldd r2, r1, r0")    // r2 = ptr
            emit("  pshd r2")
            stackOffset -= 24
            // Increment refcount at inherited backref (if non-null)
            emitAddImm(2, 7, 24)        // r2 = address of backref on stack
            emit("  ldd r2, r2, r0")    // r2 = backref value
            emitRefIncr(2, 0)
          case SyslType.ArrayType(_, size) =>
            // r1 = address of array; backref = null (stack array)
            emit("  pshd r0")           // push backref = 0
            stackOffset -= 8
            emitLoadImm(2, size)
            emit("  pshd r2")           // cap
            emit("  pshd r2")           // len
            emit("  pshd r1")           // ptr
            stackOffset -= 24
          case _ => throw new RuntimeException(s"codegen: cannot sub-slice ${array.typ}")
        // Stack (top to bottom): [ptr] [len] [cap] [backref]

        // Evaluate lo (default 0)
        low match
          case Some(loExpr) => genExpr(loExpr) // r1 = lo
          case None => emit("  ldi r1, 0")
        emit("  pshd r1")              // push lo
        stackOffset -= 8
        // Stack: [lo] [ptr] [len] [cap] [backref]

        // Evaluate hi (default len)
        high match
          case Some(hiExpr) => genExpr(hiExpr) // r1 = hi
          case None =>
            // hi = len, at sp+16
            emitAddImm(1, 7, 16)
            emit("  ldd r1, r1, r0")
        emit("  pshd r1")              // push hi
        stackOffset -= 8
        // Stack: [hi] [lo] [ptr] [len] [cap] [backref]

        // Load all values from stack into registers
        emit("  popd r1")              // r1 = hi
        emit("  popd r2")              // r2 = lo
        emit("  popd r3")              // r3 = ptr
        // len, cap, backref still on stack
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

        // Pop len and cap; backref stays on stack
        emit("  popd r4")              // r4 = len (unused now, needed only for bounds)
        stackOffset += 8
        emit("  popd r4")              // r4 = cap
        stackOffset += 8
        // backref still on stack at sp+0

        // Compute result fields:
        // new_len = hi - lo (r1 = hi, r2 = lo)
        emit("  sub r1, r1, r2")       // r1 = new_len
        // new_cap = cap - lo (r4 = cap, r2 = lo)
        emit("  sub r4, r4, r2")       // r4 = new_cap
        // new_ptr = ptr + lo * elemSize (r3 = ptr, r2 = lo)
        if elemSize == 1 then
          emit("  add r3, r3, r2")
        else
          emit("  pshd r1")            // save new_len (emitLoadImm overwrites r1)
          emitLoadImm(1, elemSize)
          emit("  mul r2, r2, r1")     // r2 = lo * elemSize
          emit("  popd r1")            // restore new_len
          emit("  add r3, r3, r2")     // r3 = new_ptr

        // Pop backref into r2
        emit("  popd r2")              // r2 = backref
        stackOffset += 8

        // Allocate 24-byte result on stack: {ptr(8), len(4), cap(4), backref(8)}
        emitAddImm(7, 7, -24)
        stackOffset -= 24
        emit("  std r3, r7, r0")       // result.ptr = new_ptr
        emit("  addi r3, r7, 8")
        emit("  stw r1, r3, r0")       // result.len = new_len (i32)
        emit("  addi r3, r7, 12")
        emit("  stw r4, r3, r0")       // result.cap = new_cap (i32)
        emit("  addi r3, r7, 16")
        emit("  std r2, r3, r0")       // result.backref = backref
        emit("  mov r1, r7")           // r1 = address of result

      case TAppend(sliceExpr, elemExpr, SyslType.SliceType(elemType)) =>
        val elemSize = stackSize(elemType)
        needsAllocExtern = true

        // Aggregate elem types (struct, array, string, slice, enum, func, iface)
        // need extra care: their `genExpr` typically allocates a stack temp and
        // returns r1 = address of that temp. The subsequent `popd r1; popd r2;
        // popd r3; popd r4` would read INTO that temp instead of the slice
        // components above it. Worse, the no-grow path then `pshd`'s into the
        // temp's space. To make the rest of the codegen address-stable, we
        // copy aggregate elem bytes into a dedicated frame scratch slot and
        // reclaim the stack temp before the popd shuffle.
        val isAggregateElem = elemType.underlying match
          case _: SyslType.StructType | _: SyslType.ArrayType | SyslType.StringType
             | _: SyslType.SliceType | _: SyslType.EnumType
             | _: SyslType.FuncType | _: SyslType.InterfaceType => true
          case _ => false
        val elemScratchSize = if isAggregateElem then (elemSize + 7) & ~7 else 0

        // Pre-allocate 24-byte result slot + optional elem scratch.
        emitAddImm(7, 7, -(24 + elemScratchSize))
        stackOffset -= (24 + elemScratchSize)
        val resultOffset = stackOffset
        val elemScratchOffset = resultOffset + 24

        // Evaluate slice → push ptr, len, cap, backref onto stack
        genExpr(sliceExpr)
        emit("  addi r2, r1, 16")
        emit("  ldd r2, r2, r0")       // r2 = backref
        emit("  pshd r2")              // [backref]
        stackOffset -= 8
        emit("  ldd r2, r1, r0")       // ptr
        emit("  addi r3, r1, 8")
        emit("  ldw r3, r3, r0")       // len
        emit("  addi r4, r1, 12")
        emit("  ldw r4, r4, r0")       // cap
        emit("  pshd r4")              // [cap]
        emit("  pshd r3")              // [len] [cap]
        emit("  pshd r2")              // [ptr] [len] [cap] [backref]
        stackOffset -= 24

        // Evaluate elem. For aggregate types this may push a stack temp; we
        // copy the bytes into the frame scratch slot and reclaim the temp
        // so the subsequent popd's see an undisturbed [ptr][len][cap] stack.
        val preElem = stackOffset
        genExpr(elemExpr)
        val elemTempBytes = preElem - stackOffset
        if isAggregateElem then
          emit("  mov r2, r1")                 // r2 = source addr (stack temp or stable)
          emitAddImm(1, 5, elemScratchOffset)  // r1 = frame scratch addr
          emitAggregateCopy(2, 1, elemSize, stackAlign(elemType))
          if elemTempBytes > 0 then
            emitAddImm(7, 7, elemTempBytes)
            stackOffset += elemTempBytes
        emit("  pshd r1")              // [elem] [ptr] [len] [cap] [backref]
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
          emit("  mul r1, r1, r4")     // r1 = len * elemSize
        emit("  add r1, r2, r1")       // r1 = dest addr
        emit("  popd r2")              // r2 = elem
        // For aggregate elem types, emitStore → emitAggregateCopy clobbers r3
        // and r4. Save/restore r3 (len) so the new_len computation below sees
        // the right value. (cap is reloaded fresh from the stack, so r4 is OK.)
        if isAggregateElem then emit("  pshd r3")
        emitStore(2, 1, elemType)      // store elem at dest
        if isAggregateElem then emit("  popd r3")
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
        // Inherit backref from source (still on stack)
        emit("  ldd r2, r7, r0")       // r2 = backref (top of remaining stack)
        emit("  addi r3, r1, 16")
        emit("  std r2, r3, r0")       // result.backref
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
          emit("  mov r1, r4")         // r1 = len
          emitLoadImm(4, elemSize)
          emit("  mul r1, r1, r4")     // r1 = len * elemSize
          emit("  mov r4, r1")         // r4 = bytes to copy
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
          emit("  mul r1, r1, r4")     // r1 = len * elemSize
        emit("  add r1, r2, r1")       // r1 = dest addr
        emitAddImm(4, 7, 32)
        emit("  ldd r4, r4, r0")       // r4 = elem
        emitStore(4, 1, elemType)      // store elem
        // Build result: new_ptr, len+1, new_cap, backref=0
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
        emit("  addi r2, r1, 16")
        emit("  std r0, r2, r0")       // result.backref = null (grow allocates new buffer)

        emit(s"$doneLabel")
        // Pop the source backref from the stack (both paths leave it)
        emitAddImm(7, 7, 8)
        stackOffset += 8
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

      case TAsmExpr(code, _) =>
        for line <- code.split("\\\\n|\\n") do
          emit(s"  ${line.trim}")

      case TSizeof(size, _) =>
        emitLoadImm(1, size.toInt)

      case TStructLit(st @ SyslType.StructType(_, fields, _)) =>
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
          // Borrowed rc-bearing field: incr the buffer (caller still owns its copy)
          fieldType match
            case SyslType.StringType if needsAllocExtern && !isOwnedStringExpr(arg) =>
              emit("  pshd r1")
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitAddImm(1, 3, 8 + off)
              emit("  ldd r1, r1, r0")
              emitRefIncr(1, 8)
              emit("  popd r1")
            case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitStructStringFieldsRC(3, 8 + off, nested, incr = true)
            case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitEnumStringFieldsRC(3, 8 + off, nested, incr = true)
            case _: SyslType.FuncType if needsAllocExtern && !isOwnedClosureExpr(arg) =>
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitClosureDescrIncr(3, 8 + off)
            case _ =>
        // r1 = data pointer (past refcount header)
        emitAddImm(1, 5, ptrOffset)
        emit("  ldd r1, r1, r0")
        emitAddImm(1, 1, 8)
        // Clean up temp
        emitAddImm(7, 7, 8)
        stackOffset += 8

      case TNewEnum(et, variantIndex, args) =>
        // Heap-allocate ref-counted enum: [refcount_i64 | tag_i32 | padding | variant_data...]
        val dataSize = stackSize(et)
        val totalAlloc = dataSize + 8 // 8 bytes for refcount header
        // Call malloc(totalAlloc) — result in r1
        emitLoadImm(1, totalAlloc)
        emit("  pshd r1")
        stackOffset -= 8
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
        // Zero-fill data area (past refcount header)
        emitAddImm(1, 1, 8) // r1 = data start
        for i <- 0 until ((dataSize + 7) & ~7) by 8 do
          emitAddImm(2, 1, i)
          emit("  std r0, r2, r0")
        // Write tag (i32 at data offset 0)
        emitLoadImm(1, variantIndex)
        emitAddImm(2, 5, ptrOffset)
        emit("  ldd r2, r2, r0")    // r2 = malloc result
        emitAddImm(2, 2, 8)         // r2 = data start (past refcount)
        emit("  stw r1, r2, r0")    // store tag
        // Write variant fields at data + dataOffset
        val dataOff = et.dataOffset.toInt
        val variantFields = et.variants(variantIndex)._2
        var fieldOff = 0
        for (arg, i) <- args.zipWithIndex do
          val (_, fieldType) = variantFields(i)
          val align = stackAlign(fieldType)
          fieldOff = ((fieldOff + align - 1) / align) * align
          genExpr(arg) // r1 = field value
          // Reload base pointer from stack
          emitAddImm(3, 5, ptrOffset)
          emit("  ldd r3, r3, r0")          // r3 = malloc result
          emitAddImm(2, 3, 8 + dataOff + fieldOff) // r2 = field address (past header + data offset)
          emitStore(1, 2, fieldType)
          // Borrowed string field: incr the buffer (caller still owns its copy)
          fieldType match
            case SyslType.StringType if needsAllocExtern && !isOwnedStringExpr(arg) =>
              emit("  pshd r1")
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitAddImm(1, 3, 8 + dataOff + fieldOff)
              emit("  ldd r1, r1, r0")
              emitRefIncr(1, 8)
              emit("  popd r1")
            case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              // r3 = malloc result; field address = r3 + 8 + dataOff + fieldOff
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitStructStringFieldsRC(3, 8 + dataOff + fieldOff, nested, incr = true)
            case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitEnumStringFieldsRC(3, 8 + dataOff + fieldOff, nested, incr = true)
            case _: SyslType.FuncType if needsAllocExtern && !isOwnedClosureExpr(arg) =>
              emitAddImm(3, 5, ptrOffset)
              emit("  ldd r3, r3, r0")
              emitClosureDescrIncr(3, 8 + dataOff + fieldOff)
            case _ =>
          fieldOff += fieldType.sizeOf.toInt
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
          // Borrowed string/closure field: incr (caller still owns its copy)
          fieldType match
            case SyslType.StringType if needsAllocExtern && !isOwnedStringExpr(arg) =>
              emit("  pshd r1")
              emitAddImm(1, 5, structBaseOffset + off)
              emit("  ldd r1, r1, r0")
              emitRefIncr(1, 8)
              emit("  popd r1")
            case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              emitStructStringFieldsRC(5, structBaseOffset + off, nested, incr = true)
            case _: SyslType.FuncType if needsAllocExtern && !isOwnedClosureExpr(arg) =>
              emitClosureDescrIncr(5, structBaseOffset + off)
            case _ =>
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
          // Borrowed string/struct/enum/closure field: incr (caller still owns its copy)
          fieldType match
            case SyslType.StringType if needsAllocExtern && !isOwnedStringExpr(arg) =>
              emit("  pshd r1")
              emitAddImm(1, 5, enumBaseOffset + dataOff + fieldOff)
              emit("  ldd r1, r1, r0")
              emitRefIncr(1, 8)
              emit("  popd r1")
            case nested: SyslType.StructType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              emitStructStringFieldsRC(5, enumBaseOffset + dataOff + fieldOff, nested, incr = true)
            case nested: SyslType.EnumType if structHasStringFields(nested) && !isOwnedStructExpr(arg) =>
              emitEnumStringFieldsRC(5, enumBaseOffset + dataOff + fieldOff, nested, incr = true)
            case _: SyslType.FuncType if needsAllocExtern && !isOwnedClosureExpr(arg) =>
              emitClosureDescrIncr(5, enumBaseOffset + dataOff + fieldOff)
            case _ =>
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

  // Emit truncation for narrow integer types after arithmetic (no-op for i64/u64)
  private def emitNarrow(reg: Int, typ: SyslType): Unit = typ match
    case SyslType.UIntType(8)  => emit(s"  zeb r$reg, r$reg")
    case SyslType.UIntType(16) => emit(s"  zes r$reg, r$reg")
    case SyslType.UIntType(32) => emit(s"  zew r$reg, r$reg")
    case SyslType.IntType(8)   => emit(s"  seb r$reg, r$reg")
    case SyslType.IntType(16)  => emit(s"  ses r$reg, r$reg")
    case SyslType.IntType(32)  => emit(s"  sew r$reg, r$reg")
    case _ =>

  // Emit reg = base + offset, handling large offsets that don't fit in addi
  private def emitAddImm(destReg: Int, baseReg: Int, offset: Int): Unit =
    if offset >= -64 && offset <= 63 then
      emit(s"  addi r$destReg, r$baseReg, $offset")
    else
      // Use destReg itself as the offset scratch when destReg != baseReg —
      // `movi destReg, X; add destReg, baseReg, destReg` reads baseReg before
      // writing destReg, so baseReg is preserved and no third register is
      // clobbered. When destReg == baseReg, fall back to a separate temp
      // (r2 if destReg=3 else r3).
      //
      // Earlier bug: tmp was picked as r3 with only destReg avoided, so
      // emitAddImm(4, 3, ≥64) emitted `movi r3, X; add r4, r3, r3` and
      // clobbered the source pointer (sysl/tests/aggregate_copy_offset_64).
      // A first attempt routed the temp away from r3 by picking r2 — that
      // unblocked tabwriter but broke emitStructReturn, which holds r2 and
      // r3 live as destBase/srcBase across the copy loop. Self-tmp avoids
      // both pitfalls.
      val tmp =
        if destReg != baseReg then destReg
        else if destReg == 3 then 2
        else 3
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
        val st = innerObj.typ match
          case s: SyslType.StructType => s
          case SyslType.PtrType(s: SyslType.StructType) => s
          case other => throw new RuntimeException(s"emitStructAddr/TFieldAccess: expected StructType, got $other")
        val off = fieldOffset(st, fieldIndex)
        emitStructAddr(innerObj)  // r1 = parent struct address
        if off != 0 then emitAddImm(1, 1, off)
      case _ => genExpr(obj) // struct value (local/global) — genExpr produces address for struct types

  // Allocate a unique label for a string-literal blob and queue it for rodata
  // emission. Returns (label, byte-length). The blob in rodata is 8 bytes of
  // immortal-refcount header followed by the UTF-8 bytes plus a NUL terminator.
  private def internStringLiteral(value: String): (String, Int) =
    labelCounter += 1
    val label = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_$labelCounter" else s"__str_$labelCounter"
    stringLiterals += ((label, value))
    (label, value.getBytes("ISO-8859-1").length)

  // Data directive for a type: db (1 byte), ds (2), dw (4), dl (8)
  private def emitDataDirective(typ: SyslType): String = typ match
    case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType => "db"
    case SyslType.IntType(16) | SyslType.UIntType(16) => "ds"
    case SyslType.IntType(32) | SyslType.UIntType(32) => "dw"
    case SyslType.FloatType(32) => "dw"   // f32 stored as 4 bytes (IEEE-754 single bit pattern)
    case SyslType.FloatType(64) => "dd"
    case SyslType.IntType(64) | SyslType.UIntType(64) | _: SyslType.PtrType | _: SyslType.RefType => "dl"
    case other => throw new RuntimeException(s"emitDataDirective: unexpected type $other")

  // Emit address of local variable into target register
  private def emitLocalAddr(name: String, reg: Int): Unit =
    val local = locals(name)
    emitAddImm(reg, 5, local.offset)

  // Emit the __str_int helper function: converts i64 in stack arg to a refcounted string
  // ABI: r1 = hidden return slot ptr, [fp+24] = integer value
  // Returns: {ptr, len} written to return slot, r1 = return slot address
  private def emitStrIntHelper(): Unit =
    val mp = if modulePrefix.nonEmpty then s"_$modulePrefix" else ""
    emit(s"# helper: __str_int$mp(value: int) -> string")
    emit(s"global __str_int$mp, func, 1 i64 i64")
    emit(s"__str_int$mp:")
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
    // Save digit_count to [fp-16]
    emitAddImm(4, 5, -16)
    emit("  std r3, r4, r0")
    emit("  ldi r3, 10")
    emit("  mov r2, r1")           // r2 = remaining
    emit("  rem r2, r3")           // r2 = remaining % 10 (digit)
    emit("  div r1, r1, r3")       // r1 = remaining / 10
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

  // Emit the __str_float helper: converts f64 to a refcounted string with
  // fixed 6-digit fractional precision (e.g. 3.14 -> "3.140000", -0.5 -> "-0.500000").
  // ABI: r1 = hidden return slot ptr, [fp+24] = f64 value (bits)
  // Returns: {ptr, len} written to return slot, r1 = return slot address
  private def emitStrFloatHelper(): Unit =
    val mp = if modulePrefix.nonEmpty then s"_$modulePrefix" else ""
    emit(s"# helper: __str_float$mp(value: f64) -> string")
    emit(s"global __str_float$mp, func, 1 i64 i64")
    emit(s"__str_float$mp:")
    // Pre-prologue: save hidden return ptr
    emit("  pshd r1")
    // Prologue
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    // [fp+16] = hidden return ptr, [fp+24] = f64 value (bits)
    //
    // Locals (56 bytes):
    //   [fp-8]  = is_negative (i64: 0 or 1)
    //   [fp-16] = int_part    (i64: |v| truncated to integer)
    //   [fp-24] = scaled_frac (i64: rounded |frac| * 1e6, in [0, 999999])
    //   [fp-32] = int_digit_count (i64)
    //   [fp-56] = digit_buffer[24]  (reversed int digits)
    emitAddImm(7, 7, -56)

    // Load value from [fp+24]
    emitAddImm(2, 5, 24)
    emit("  ldd r1, r2, r0")       // r1 = v (f64 bits)

    // Check sign: v < 0.0
    emit("  ldc r2, 0.0")
    emit("  fslt r3, r1, r2")      // r3 = 1 if v < 0
    emitAddImm(4, 5, -8)
    emit("  std r3, r4, r0")       // save is_negative

    // Abs
    emit("  fabs r1, r1")

    // int_part = fint(|v|) (truncates)
    emit("  fint r2, r1")          // r2 = int_part (i64)

    // frac_f = |v| - cvt(int_part)
    emit("  cvt r3, r2")           // r3 = float bits of int_part
    emit("  fsub r1, r1, r3")      // r1 = frac (f64, in [0, 1))

    // scaled = fint(frac * 1e6 + 0.5)   (round to nearest)
    emit("  ldc r3, 1000000.0")
    emit("  fmul r1, r1, r3")
    emit("  ldc r3, 0.5")
    emit("  fadd r1, r1, r3")
    emit("  fint r1, r1")          // r1 = scaled_frac (i64)

    // Rollover: if scaled_frac >= 1000000, int_part++, scaled_frac = 0
    emit("  movi r3, 1000000")
    emit("  slt r4, r1, r3")       // r4 = 1 if scaled_frac < 1000000
    val noRollLabel = newLabel("strf_noroll")
    emit(s"  bne r4, r0, $noRollLabel")
    emit("  addi r2, r2, 1")
    emit("  ldi r1, 0")
    emit(s"$noRollLabel")

    // Save int_part and scaled_frac
    emitAddImm(4, 5, -16)
    emit("  std r2, r4, r0")       // int_part
    emitAddImm(4, 5, -24)
    emit("  std r1, r4, r0")       // scaled_frac

    // Extract integer digits into buffer at [fp-56]
    emitAddImm(4, 5, -16)
    emit("  ldd r1, r4, r0")       // r1 = int_part
    emit("  ldi r3, 0")            // r3 = digit_count

    // Special case: int_part == 0
    val intLoopLabel = newLabel("strf_int_loop")
    val intDoneLabel = newLabel("strf_int_done")
    emit(s"  bne r1, r0, $intLoopLabel")
    emitAddImm(4, 5, -56)
    emit("  ldi r2, 48")           // '0'
    emit("  stb r2, r4, r0")
    emit("  ldi r3, 1")
    emit(s"  bra $intDoneLabel")

    // Division loop: extract int digits
    emit(s"$intLoopLabel")
    emit(s"  beq r1, r0, $intDoneLabel")
    // Save digit_count
    emitAddImm(4, 5, -32)
    emit("  std r3, r4, r0")
    emit("  ldi r3, 10")
    emit("  mov r2, r1")           // r2 = remaining
    emit("  rem r2, r3")           // r2 = remaining % 10 (digit)
    emit("  div r1, r1, r3")       // r1 = remaining / 10
    emit("  addi r2, r2, 48")      // r2 = ASCII digit
    // Restore digit_count
    emitAddImm(4, 5, -32)
    emit("  ldd r3, r4, r0")
    emitAddImm(4, 5, -56)
    emit("  add r4, r4, r3")
    emit("  stb r2, r4, r0")
    emit("  addi r3, r3, 1")
    emit(s"  bra $intLoopLabel")

    emit(s"$intDoneLabel")
    // Save int_digit_count
    emitAddImm(4, 5, -32)
    emit("  std r3, r4, r0")

    // total_length = is_negative + int_digit_count + 1 (.) + 6 (frac digits)
    emitAddImm(4, 5, -8)
    emit("  ldd r2, r4, r0")       // r2 = is_negative
    emit("  add r1, r3, r2")       // r1 = is_negative + int_digit_count
    emit("  addi r1, r1, 7")       // + 1 (dot) + 6 (frac)
    emit("  pshd r1")              // save total_length

    // Malloc(8 + total_length)
    emit("  addi r1, r1, 8")
    emit("  movi r4, malloc")
    emit("  jalr r6, r4")

    // Null check
    val allocOkLabel = newLabel("strf_alloc_ok")
    emit(s"  bne r1, r0, $allocOkLabel")
    emit("  ldi r1, 2")
    emit("  trap 1")
    emit(s"$allocOkLabel")

    // Set refcount = 1 at [base+0]
    emit("  ldi r2, 1")
    emit("  std r2, r1, r0")
    // data_ptr = base + 8
    emit("  addi r1, r1, 8")

    // Pop total_length (keep on stack briefly for return)
    emit("  popd r3")              // r3 = total_length
    emit("  pshd r1")              // save data_ptr
    emit("  pshd r3")              // save total_length

    // Write '-' if negative
    emitAddImm(4, 5, -8)
    emit("  ldd r2, r4, r0")
    val noSignLabel = newLabel("strf_nosign")
    emit(s"  beq r2, r0, $noSignLabel")
    emit("  ldi r2, 45")           // '-'
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")
    emit(s"$noSignLabel")

    // Copy int digits from buffer in reverse
    emitAddImm(4, 5, -32)
    emit("  ldd r3, r4, r0")       // r3 = int_digit_count
    emitAddImm(4, 5, -56)
    emit("  add r4, r4, r3")       // r4 = &buffer[count] (one past last)
    val copyIntLabel = newLabel("strf_copy_int")
    val copyIntDoneLabel = newLabel("strf_copy_int_done")
    emit(s"$copyIntLabel")
    emit(s"  beq r3, r0, $copyIntDoneLabel")
    emit("  addi r4, r4, -1")
    emit("  ldb r2, r4, r0")
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")
    emit("  addi r3, r3, -1")
    emit(s"  bra $copyIntLabel")
    emit(s"$copyIntDoneLabel")

    // Write '.'
    emit("  ldi r2, 46")           // '.'
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")

    // Write 6 fractional digits from scaled_frac (MSB first — divide by 100000, 10000, ...)
    // Load scaled_frac
    emitAddImm(4, 5, -24)
    emit("  ldd r2, r4, r0")       // r2 = scaled_frac
    // For each divisor 100000, 10000, 1000, 100, 10, 1:
    for divisor <- List(100000, 10000, 1000, 100, 10, 1) do
      emitLoadImm(3, divisor)
      emit("  mov r4, r2")         // r4 = scaled_frac (preserve for rem)
      emit("  rem r4, r3")         // r4 = scaled_frac % divisor (new remainder)
      emit("  div r3, r2, r3")     // r3 = scaled_frac / divisor (digit)
      emit("  mov r2, r4")         // r2 = new remainder
      emit("  addi r3, r3, 48")    // r3 = ASCII digit
      emit("  stb r3, r1, r0")
      emit("  addi r1, r1, 1")

    // Pop total_length and data_ptr
    emit("  popd r3")              // r3 = total_length
    emit("  popd r1")              // r1 = data_ptr

    // Write {ptr, len} to return slot
    emitAddImm(4, 5, 16)
    emit("  ldd r4, r4, r0")       // r4 = return slot address
    emit("  std r1, r4, r0")
    emit("  addi r2, r4, 8")
    emit("  std r3, r2, r0")

    // r1 = return slot address
    emit("  mov r1, r4")

    // Epilogue
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emitAddImm(7, 7, 8)            // skip 1 reg param
    emit("  jalr r0, r6")

  /** Emit the `__str_fmt_i64` helper used to lower formatted integer
   *  interpolations (`f"{n}%d"`, `f"{n}%x"`, etc.) on the TRISC backend.
   *
   *  Mirrors SVM's `__svm_str_fmt_i64` (see `SVMRuntime.scala:604–890`).
   *  Audit item #14 — closes the last gap in the cross-backend f-string
   *  story. Without this the TRISC `genExpr` would throw on any `TFmtStr`.
   *
   *  ABI:
   *    r1 = hidden return slot ptr (16 bytes for the {ptr, len} struct)
   *    Stack args (pushed right-to-left at call site):
   *      [fp+24] = n       (i64; magnitude when base == 10 && n < 0)
   *      [fp+32] = base    (i64: 2, 8, 10, or 16)
   *      [fp+40] = width   (i64; 0 means no padding)
   *      [fp+48] = flags   (i64 bitmask):
   *                  0x1 = zero-pad   (right-aligned with '0' fill)
   *                  0x2 = left-align (with space fill — overrides zero-pad)
   *                  0x4 = show-sign  (always emit '+' for non-negatives, base 10 only)
   *                  0x8 = upper-case (hex digits A–F instead of a–f)
   *
   *  Returns: writes {ptr, len} into the return slot; r1 = return slot.
   */
  private def emitStrFmtI64Helper(): Unit =
    val mp = if modulePrefix.nonEmpty then s"_$modulePrefix" else ""
    emit(s"# helper: __str_fmt_i64$mp(n: i64, base: i64, width: i64, flags: i64) -> string")
    emit(s"global __str_fmt_i64$mp, func, 1 i64 i64 i64 i64 i64")
    emit(s"__str_fmt_i64$mp:")
    // Pre-prologue: save hidden return ptr (passed in r1)
    emit("  pshd r1")
    // Prologue
    emit("  pshd r6")
    emit("  pshd r5")
    emit("  mov r5, r7")
    // Locals (104 bytes):
    //   [fp-8]   is_neg     (i64: 0 or 1)
    //   [fp-16]  digit_count (i64)
    //   [fp-24]  sign_char  (i64: 0, '-' = 45, or '+' = 43)
    //   [fp-32]  pad_count  (i64)
    //   [fp-40]  final_len  (i64; total length including sign + padding)
    //   [fp-104] digit_buffer[64]  (digits stored LSB-first; i64 in binary needs 64 bytes)
    emitAddImm(7, 7, -104)

    // Use unique labels per emitStrFmtI64Helper invocation (only invoked once
    // per program, so plain newLabel is enough; mirrors __str_int).
    val checkSign = newLabel("fmt_check_sign")
    val saveNeg   = newLabel("fmt_save_neg")
    val pos       = newLabel("fmt_pos")
    val zeroCase  = newLabel("fmt_zero")
    val loop      = newLabel("fmt_loop")
    val letter    = newLabel("fmt_letter")
    val lower     = newLabel("fmt_lower")
    val store     = newLabel("fmt_store")
    val done      = newLabel("fmt_done")
    val signPlus  = newLabel("fmt_sign_check_plus")
    val signDone  = newLabel("fmt_sign_done")
    val padOk     = newLabel("fmt_pad_ok")
    val allocOk   = newLabel("fmt_alloc_ok")
    val laMode    = newLabel("fmt_la_mode")
    val zpMode    = newLabel("fmt_zp_mode")
    val finish    = newLabel("fmt_finish")

    // ── Step 1: compute is_neg = (base == 10) && (n < 0).  For non-decimal
    //    bases we treat n as unsigned (no sign char).
    emitAddImm(2, 5, 32); emit("  ldd r2, r2, r0")    // r2 = base
    emit("  ldi r3, 10")
    emit(s"  beq r2, r3, $checkSign")
    emit("  ldi r2, 0")
    emit(s"  bra $saveNeg")
    emit(s"$checkSign")
    emitAddImm(2, 5, 24); emit("  ldd r2, r2, r0")    // r2 = n
    emit("  slt r2, r2, r0")                          // r2 = 1 if n < 0
    emit(s"$saveNeg")
    emitAddImm(3, 5, -8); emit("  std r2, r3, r0")    // is_neg = r2

    // ── Step 2: u = is_neg ? -n : n (in r1).  For non-decimal r1 = n unchanged.
    emitAddImm(1, 5, 24); emit("  ldd r1, r1, r0")    // r1 = n
    emit(s"  beq r2, r0, $pos")
    emit("  neg r1, r1")                              // u = -n
    emit(s"$pos")

    // ── Step 3: extract digits from u (in r1) into digit_buffer.
    //    Digits are stored LSB-first; we'll reverse at write time.
    emit("  ldi r3, 0")                               // r3 = digit_count
    emit(s"  bne r1, r0, $loop")                      // u != 0 → loop
    // Special case: u == 0 → store '0', digit_count = 1
    emitAddImm(4, 5, -104); emit("  ldi r2, 48")
    emit("  stb r2, r4, r0")
    emit("  ldi r3, 1")
    emit(s"  bra $done")

    emit(s"$loop")
    emit(s"  beq r1, r0, $done")
    // Save digit_count to [fp-16] across the divmod (clobbers r3)
    emitAddImm(4, 5, -16); emit("  std r3, r4, r0")
    emitAddImm(4, 5, 32); emit("  ldd r3, r4, r0")    // r3 = base
    emit("  mov r2, r1")                              // r2 = u
    emit("  remu r2, r3")                             // r2 = u % base (digit value 0..base-1)
    emit("  divu r1, r1, r3")                         // r1 = u / base (unsigned: handles non-decimal)
    // Convert digit value → ASCII.
    //   digit < 10 → '0' + digit
    //   else: subtract 10, add 'A' (uppercase) or 'a' (lowercase)
    emit("  ldi r3, 10")
    emit("  slt r3, r2, r3")                          // r3 = 1 if digit < 10
    emit(s"  beq r3, r0, $letter")
    emit("  addi r2, r2, 48")                         // '0' + digit
    emit(s"  bra $store")
    emit(s"$letter")
    emit("  addi r2, r2, -10")                        // digit -= 10
    // Test uppercase flag (bit 0x8) on flags
    emitAddImm(4, 5, 48); emit("  ldd r3, r4, r0")    // r3 = flags
    emit("  ldi r4, 8")
    emit("  and r3, r3, r4")                          // r3 = flags & 0x8
    emit(s"  beq r3, r0, $lower")
    // 'A' + digit: 65 doesn't fit in addi's 7-bit signed range, use ldi + add.
    emit("  ldi r3, 65")
    emit("  add r2, r2, r3")
    emit(s"  bra $store")
    emit(s"$lower")
    // 'a' + digit: 97 doesn't fit in addi's 7-bit signed range, use ldi + add.
    emit("  ldi r3, 97")
    emit("  add r2, r2, r3")
    emit(s"$store")
    // Restore digit_count
    emitAddImm(4, 5, -16); emit("  ldd r3, r4, r0")
    // Store digit at digit_buffer[count]
    emitAddImm(4, 5, -104); emit("  add r4, r4, r3")
    emit("  stb r2, r4, r0")
    emit("  addi r3, r3, 1")
    emit(s"  bra $loop")

    emit(s"$done")
    // Save digit_count
    emitAddImm(4, 5, -16); emit("  std r3, r4, r0")

    // ── Step 4: compute sign_char.
    //   is_neg → '-' (45)
    //   else if (flags & 0x4) && base == 10 → '+' (43)
    //   else 0 (no sign char)
    emit("  ldi r2, 0")                               // r2 = sign_char (default 0)
    emitAddImm(3, 5, -8); emit("  ldd r3, r3, r0")    // r3 = is_neg
    emit(s"  beq r3, r0, $signPlus")
    emit("  ldi r2, 45")                              // '-'
    emit(s"  bra $signDone")
    emit(s"$signPlus")
    emitAddImm(3, 5, 48); emit("  ldd r3, r3, r0")    // r3 = flags
    emit("  ldi r4, 4")
    emit("  and r3, r3, r4")                          // r3 = flags & 0x4 (showSign)
    emit(s"  beq r3, r0, $signDone")
    emitAddImm(3, 5, 32); emit("  ldd r3, r3, r0")    // r3 = base
    emit("  ldi r4, 10")
    emit(s"  bne r3, r4, $signDone")
    emit("  ldi r2, 43")                              // '+'
    emit(s"$signDone")
    emitAddImm(3, 5, -24); emit("  std r2, r3, r0")   // sign_char = r2

    // ── Step 5: pad_count = max(0, width - (digit_count + sign?));
    //          final_len = digit_count + sign? + pad_count.
    emitAddImm(3, 5, -16); emit("  ldd r3, r3, r0")   // r3 = digit_count
    emitAddImm(4, 5, -24); emit("  ldd r4, r4, r0")   // r4 = sign_char
    emit("  slt r4, r0, r4")                          // r4 = 1 if sign_char > 0 (i.e. non-zero)
    emit("  add r3, r3, r4")                          // r3 = total_len_no_pad
    emitAddImm(2, 5, 40); emit("  ldd r2, r2, r0")    // r2 = width
    emit("  sub r2, r2, r3")                          // r2 = width - total_len_no_pad
    emit("  slt r4, r2, r0")                          // r4 = 1 if r2 < 0
    emit(s"  beq r4, r0, $padOk")
    emit("  ldi r2, 0")
    emit(s"$padOk")
    // r2 = pad_count, r3 = total_len_no_pad
    emitAddImm(1, 5, -32); emit("  std r2, r1, r0")   // pad_count
    emit("  add r4, r2, r3")                          // r4 = final_len = total_len + pad
    emitAddImm(1, 5, -40); emit("  std r4, r1, r0")   // final_len

    // ── Step 6: malloc(8 + final_len). Trap on null.
    emit("  addi r1, r4, 8")
    emit("  movi r4, malloc")
    emit("  jalr r6, r4")
    emit(s"  bne r1, r0, $allocOk")
    emit("  ldi r1, 2")
    emit("  trap 1")
    emit(s"$allocOk")
    // Set refcount = 1 at [base+0]; data_ptr = base + 8.
    emit("  ldi r2, 1")
    emit("  std r2, r1, r0")
    emit("  addi r1, r1, 8")                          // r1 = data_ptr (write cursor)
    // Save data_ptr base for return-slot write (we'll re-derive after writes).
    emit("  pshd r1")                                 // save data_ptr_base on stack

    // ── Step 7: dispatch on fill mode and write characters.
    //   left-align (flags & 0x2): [sign?][digits][spaces]
    //   else zero-pad (flags & 0x1): [sign?][zeros][digits]
    //   else (default space-pad):  [spaces][sign?][digits]
    emitAddImm(2, 5, 48); emit("  ldd r2, r2, r0")    // r2 = flags
    emit("  ldi r3, 2")
    emit("  and r3, r2, r3")
    emit(s"  bne r3, r0, $laMode")
    emit("  ldi r3, 1")
    emit("  and r3, r2, r3")
    emit(s"  bne r3, r0, $zpMode")

    // ── Default (space-pad right-align): [spaces*pad][sign?][digits]
    // r1 currently = data_ptr (write cursor).
    // Step A: write pad_count spaces.
    writePadChars(' ')                                // space = 32
    // Step B: write sign char if any.
    writeSignChar()
    // Step C: write digits in MSB-first order.
    writeDigitsReverse()
    emit(s"  bra $finish")

    // ── Zero-pad mode: [sign?][zeros*pad][digits]
    emit(s"$zpMode")
    writeSignChar()
    writePadChars('0')                                // zero = 48
    writeDigitsReverse()
    emit(s"  bra $finish")

    // ── Left-align mode: [sign?][digits][spaces*pad]
    emit(s"$laMode")
    writeSignChar()
    writeDigitsReverse()
    writePadChars(' ')                                // trailing spaces

    // ── Step 8: write {data_ptr_base, final_len} into the return slot.
    emit(s"$finish")
    emit("  popd r1")                                 // r1 = data_ptr_base
    emitAddImm(2, 5, -40); emit("  ldd r2, r2, r0")   // r2 = final_len
    emitAddImm(3, 5, 16); emit("  ldd r3, r3, r0")    // r3 = return slot address
    emit("  std r1, r3, r0")                          // [ret+0] = data_ptr_base
    emit("  addi r3, r3, 8")
    emit("  std r2, r3, r0")                          // [ret+8] = final_len
    // Reload return slot address into r1 for ABI return.
    emitAddImm(1, 5, 16); emit("  ldd r1, r1, r0")

    // Epilogue. Stack-arg cleanup (4 stack args = 32 bytes) is the caller's
    // responsibility per the existing __str_int convention; we only skip the
    // hidden return-ptr register slot here.
    emit("  mov r7, r5")
    emit("  popd r5")
    emit("  popd r6")
    emitAddImm(7, 7, 8)                               // skip 1 reg param (hidden return ptr)
    emit("  jalr r0, r6")

  /** Inline helper used inside `emitStrFmtI64Helper`: write `pad_count`
   *  copies of `fillChar` to [r1], advancing r1 past them.  Reads pad_count
   *  from [fp-32]; clobbers r2, r3, r4. */
  private def writePadChars(fillChar: Char): Unit =
    val loopL = newLabel("fmt_pad_loop")
    val doneL = newLabel("fmt_pad_done")
    emitAddImm(3, 5, -32); emit("  ldd r3, r3, r0")   // r3 = pad_count
    emit(s"$loopL")
    emit(s"  beq r3, r0, $doneL")
    emit(s"  ldi r2, ${fillChar.toInt}")
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")
    emit("  addi r3, r3, -1")
    emit(s"  bra $loopL")
    emit(s"$doneL")

  /** Inline helper used inside `emitStrFmtI64Helper`: if sign_char != 0,
   *  write it to [r1] and advance r1.  Reads sign_char from [fp-24];
   *  clobbers r2, r3. */
  private def writeSignChar(): Unit =
    val skipL = newLabel("fmt_no_sign")
    emitAddImm(3, 5, -24); emit("  ldd r2, r3, r0")   // r2 = sign_char
    emit(s"  beq r2, r0, $skipL")
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")
    emit(s"$skipL")

  /** Inline helper used inside `emitStrFmtI64Helper`: copy digits from the
   *  reverse-order buffer at [fp-104] to [r1] in MSB-first order, advancing
   *  r1 past the digits.  Reads digit_count from [fp-16]; clobbers r2, r3, r4. */
  private def writeDigitsReverse(): Unit =
    val loopL = newLabel("fmt_copy")
    val doneL = newLabel("fmt_copy_done")
    emitAddImm(3, 5, -16); emit("  ldd r3, r3, r0")   // r3 = digit_count
    emitAddImm(4, 5, -104); emit("  add r4, r4, r3")  // r4 = &buffer[count] (one past last)
    emit(s"$loopL")
    emit(s"  beq r3, r0, $doneL")
    emit("  addi r4, r4, -1")
    emit("  ldb r2, r4, r0")
    emit("  stb r2, r1, r0")
    emit("  addi r1, r1, 1")
    emit("  addi r3, r3, -1")
    emit(s"  bra $loopL")
    emit(s"$doneL")

  /** Push one line of TRISC asm into the output array. The line is parsed into a
   *  structured `TriscPeephole.Line` (Instr / Label / Directive / Comment / Blank)
   *  so the peephole optimizer can pattern-match operands without re-parsing. */
  private def emit(line: String): Unit =
    out += TriscPeephole.parseLine(line)
