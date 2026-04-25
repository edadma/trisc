package io.github.edadma.trisc

import scala.collection.mutable

class SyslSVMCodegen:
  private val out = new StringBuilder
  private var labelCounter = 0
  private var modulePrefix = ""
  private val stringLiterals = new mutable.ListBuffer[(String, String)]

  // Local variable tracking — maps name → local index
  private case class LocalInfo(index: Int, typ: SyslType)
  private var locals: mutable.LinkedHashMap[String, LocalInfo] = null
  private var nextLocalIndex: Int = 0
  // Names of scalar locals whose address is taken at some point in the body.
  // These are stored on the memory stack instead of in SVM local slots so
  // that &local and writes-through-pointer observe the same storage.
  private var addressedLocals: mutable.HashSet[String] = null

  // Globals
  private val globals = new mutable.LinkedHashMap[String, SyslType]
  private val globalConstants = new mutable.LinkedHashMap[String, Long]

  // Canonical struct types (name -> field-populated StructType). Placeholders
  // (StructType(_, Nil)) can leak into expression types; this map resolves them.
  private val structTypes = new mutable.HashMap[String, SyslType.StructType]

  // Interface itables encountered during codegen. Key = itable symbol name,
  // value = (iface type, concrete struct name). Emitted in rodata at EOF.
  private val itables = new mutable.LinkedHashMap[String, (SyslType.InterfaceType, String)]

  // Set of function names defined in this module (for method name resolution).
  private val definedFuncNames = new mutable.HashSet[String]
  private def canonicalStruct(st: SyslType.StructType): SyslType.StructType =
    if st.fields.isEmpty then structTypes.getOrElse(st.name, st) else st

  /** Extract the canonical StructType from any expression's type (handles
    * NamedType / RefType / PtrType wrappers and empty placeholder structs). */
  private def structOf(t: SyslType): SyslType.StructType = t.underlying match
    case s: SyslType.StructType => canonicalStruct(s)
    case SyslType.RefType(s) => s.underlying match
      case ss: SyslType.StructType => canonicalStruct(ss)
      case _ => sys.error(s"not a struct type: $t")
    case SyslType.PtrType(s) => s.underlying match
      case ss: SyslType.StructType => canonicalStruct(ss)
      case _ => sys.error(s"not a struct type: $t")
    case _ => sys.error(s"not a struct type: $t")

  // Loop labels for break/continue
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]

  // Deferred statements — per-function stack, emitted LIFO at every return.
  private val deferStack = new mutable.Stack[TStmt]
  private def emitDefers(): Unit = for stmt <- deferStack do genStmt(stmt)

  // Current function
  private var currentFunction: TFunDecl = null
  private var needsSpExtern: Boolean = false
  private var needsStrConcat: Boolean = false
  private var needsStrEq: Boolean = false
  private var needsNewSlice: Boolean = false
  private var needsStrFromI64: Boolean = false
  private var needsStrFromBool: Boolean = false
  private var needsStrFmtI64: Boolean = false

  // Map: function name → parameter types (for arg-coercion at call sites).
  private val funcParamTypes = new mutable.HashMap[String, List[SyslType]]

  // Closures: hoisted bodies generated alongside regular functions. The hoisted
  // function's first param is a hidden env_ptr (stored in local 0).
  private var closureCounter = 0
  private val pendingClosures = new mutable.ListBuffer[(String, TClosure)]
  // While compiling a hoisted closure body: capture name → (env offset, type).
  // TVarRef checks this first.
  private var closureCaptures: Map[String, (Long, SyslType)] = Map.empty
  // Per-function shims: ignore env_ptr and forward to plain function.
  private val emittedShims = new mutable.HashSet[String]
  private val pendingShims = new mutable.ListBuffer[(String, String, List[SyslType], SyslType)]
  // (shimName, targetName, paramTypes, returnType)
  private def shimNameFor(target: String): String = s"__shim__$target"

  private def emit(s: String): Unit = out ++= s + "\n"
  private def newLabel(prefix: String): String =
    labelCounter += 1
    if modulePrefix.nonEmpty then s".${prefix}_${modulePrefix}_$labelCounter"
    else s".${prefix}_$labelCounter"

  // Pre-count locals needed for a function body
  private def countLocals(body: TFunBody): Int =
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
              case TDestructurePattern(_, bindings, _) => count += bindings.count(_.isDefined)
              case TVariantPattern(_, _, bindings, _) => count += bindings.count(_.isDefined)
              case TValuePattern(v) => scanExpr(v)
              case TRangePattern(lo, hi) => scanExpr(lo); scanExpr(hi)
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
      case TInterfaceBox(inner, _) => count += 2; scanExpr(inner)
      case TInterfaceDispatch(v, _, args, _) => count += 1; scanExpr(v); args.foreach(scanExpr)
      case TBinary(l, _, r, _) => scanExpr(l); scanExpr(r)
      case TUnary(_, o, _) => scanExpr(o)
      case TCast(inner, _) => scanExpr(inner)
      case TStringFromSlice(s, _) => count += 2; scanExpr(s)
      case TStringFromPtr(p, l, _) => count += 3; scanExpr(p); scanExpr(l)
      case TCall(_, args, _) => args.foreach(scanExpr)
      case TTempAddr(e, _) => scanExpr(e)
      case TIndirectCall(c, args, _) => scanExpr(c); args.foreach(scanExpr)
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
      case _ =>
    body match
      case TExprBody(e) => scanExpr(e)
      case TBlockBody(stmts) => scanStmts(stmts)
    count

  // Determine smallest push instruction for an integer
  private def emitPushInt(n: Long): Unit =
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

  private def isUnsigned(t: SyslType): Boolean = t.isInstanceOf[SyslType.UIntType]
  private def isFloat(t: SyslType): Boolean = t.isFloat

  /** True if this type needs memory allocation (can't fit in a single 64-bit local slot). */
  private def needsMemAlloc(t: SyslType): Boolean = t match
    case _: SyslType.ArrayType => true
    case _: SyslType.StructType => true
    case _: SyslType.EnumType => true
    case _: SyslType.FuncType => true     // 16-byte {func_ptr, env_ptr} closure descriptor
    case _ => false

  /** Emit code to allocate `size` bytes on the memory stack. Leaves address on data stack. */
  private def emitMemAlloc(size: Long): Unit =
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

  // Materialize a fixed [N]T array as a slice struct {ptr, len, cap, backref}
  // on the memory stack. Leaves the struct address on TOS.
  private def emitArrayToSlice(arg: TExpr, size: Long): Unit =
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

  private def allocLocal(name: String, typ: SyslType): Int =
    val idx = nextLocalIndex
    locals(name) = LocalInfo(idx, typ)
    nextLocalIndex += 1
    idx

  private def constEval(e: TExpr): Option[Long] = e match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(v, _) => Some(if v then 1 else 0)
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

  private def isZeroInit(typ: SyslType, init: TExpr): Boolean =
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
    needsNewSlice = false
    needsStrFromI64 = false
    needsStrFromBool = false
    needsStrFmtI64 = false

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
      case v @ TVarDecl(_, typ, init, _, _, _) =>
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
      case TVarDecl(name, SyslType.StringType, TStringLit(s, _), _, _, _) =>
        labelCounter += 1
        val lbl = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_${labelCounter}__g_$name"
                  else s"__str_${labelCounter}__g_$name"
        stringLiterals += ((lbl, s))
        stringGlobalLabels(name) = lbl
      case TVarDecl(name, SyslType.ArrayType(SyslType.StringType, _), TArrayLit(elements, _), _, _, _) =>
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
        val bytes = value.getBytes("UTF-8")
        emit(s"global $label, data, ${bytes.length + 9}")
      for (iname, (iface, _)) <- itables do
        emit(s"global $iname, data, ${iface.methods.length * 8}")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
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
        case TVarDecl(name, typ, _, _, _, _) =>
          emit(s"global $name, data, ${typ.sizeOf.max(8)}")
        case _ =>
      for decl <- dataGlobals do decl match
        case TVarDecl(name, typ, init, _, _, _) =>
          emit(s"  align 8")
          emit(s"$name:")
          typ match
            case SyslType.StringType =>
              // Static string literal: inline 16-byte {ptr, len}. Label was
              // pre-registered before rodata emission.
              init match
                case TStringLit(s, _) =>
                  val bytes = s.getBytes("UTF-8")
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
                      val bytes = s.getBytes("UTF-8")
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
                  val bytes = s.getBytes("UTF-8")
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
        case TVarDecl(name, typ, _, _, _, _) =>
          emit(s"global $name, data, ${typ.sizeOf.max(8)}")
        case _ =>
      for decl <- bssGlobals do decl match
        case TVarDecl(name, typ, _, _, _, _) =>
          emit(s"  align 8")
          emit(s"$name:")
          val size = typ.sizeOf.max(8)
          emit(s"  rl ${((size + 7) / 8).toInt}")
        case _ =>

    // Emit extern declarations
    val generated = out.toString
    val definedSymbols = program.decls.flatMap {
      case TFunDecl(name, _, _, _, _, _, _, _, _) => Some(name)
      case TVarDecl(name, _, _, _, _, _) => Some(name)
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
    if needsStrFromI64 && !definedSymbols.contains("__svm_str_from_i64") then
      emit("extern __svm_str_from_i64")
    if needsStrFromBool && !definedSymbols.contains("__svm_str_from_bool") then
      emit("extern __svm_str_from_bool")
    if needsStrFmtI64 && !definedSymbols.contains("__svm_str_fmt_i64") then
      emit("extern __svm_str_fmt_i64")
    if needsNewSlice && !definedSymbols.contains("__svm_new_slice") then
      emit("extern __svm_new_slice")

    out.toString

  // ========================================================================
  // genFunction
  // ========================================================================
  private def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    nextLocalIndex = 0
    deferStack.clear()
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
      case TInterfaceBox(i, _) => scanAddrOfE(i)
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

    // Pop args from stack into locals.
    // Caller pushes args left-to-right, so TOS = last arg pushed.
    // We need to pop in reverse param order.
    for i <- (0 until nParams).reverse do
      locals(fun.params(i).name) = LocalInfo(i, fun.params(i).typ)
    // Pop: TOS is last param (index nParams-1), next is nParams-2, etc.
    for i <- (nParams - 1) to 0 by -1 do
      emit(s"  local_set $i")
    nextLocalIndex = nParams

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

    fun.body match
      case TExprBody(expr) =>
        genExpr(expr)
        emitDefers()
        emit("  ret")
      case TBlockBody(stmts) =>
        if stmts.isEmpty then
          emitDefers()
          emit("  ret")
        else if fun.returnType != SyslType.VoidType then
          genStmtsAsExpr(stmts)
          emitDefers()
          emit("  ret")
        else
          genStmts(stmts)
          if !stmts.lastOption.exists(_.isInstanceOf[TReturnStmt]) then
            emitDefers()
            emit("  ret")

  // ========================================================================
  // Closure layout helpers
  // ========================================================================
  /** Compute env layout: list of (name, offset, type) and total size. */
  private def envLayout(captures: List[(String, SyslType)]): (List[(String, Long, SyslType)], Long) =
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
  private def genClosureExpr(c: TClosure): Unit =
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
      // Store each capture into env at its offset.
      for (capName, off, capTyp) <- layout do
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

  /** Emit a hoisted closure body as a regular function. The first param is a
    * hidden env_ptr (local 0); explicit params follow. Captures are accessed
    * via env_ptr+offset using `closureCaptures`. */
  private def genHoistedClosure(name: String, c: TClosure): Unit =
    emit(s"global $name, func")
    val (layout, _) = envLayout(c.captures)
    val captureMap = layout.map { case (n, off, t) => (n, (off, t)) }.toMap

    // Build a synthetic TFunDecl-like context. We'll call genFunction-style
    // logic but with closureCaptures populated.
    val savedCaptures = closureCaptures
    val savedLocals = locals
    val savedNextIdx = nextLocalIndex
    val savedAddressed = addressedLocals
    val savedDeferStack = deferStack.toList
    val savedFunc = currentFunction

    closureCaptures = captureMap
    locals = new mutable.LinkedHashMap
    nextLocalIndex = 0
    addressedLocals = new mutable.HashSet[String]
    deferStack.clear()

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
        else if c.returnType != SyslType.VoidType then
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
    deferStack.clear()
    deferStack.pushAll(savedDeferStack.reverse)
    currentFunction = savedFunc

  /** Fallback for TStr on types we can't render: emit "???" string. */
  private def emitStrPlaceholder(inner: TExpr): Unit =
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
  private def genShim(shim: String, target: String, paramTypes: List[SyslType], retType: SyslType): Unit =
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
  // genStmts / genStmt
  // ========================================================================
  private def genStmts(stmts: List[TStmt]): Unit = stmts.foreach(genStmt)

  /** Generate statements where the last one leaves its value on the stack (for if-expr, match-expr, function bodies).
    * Always pushes exactly 1 value on the stack. If the last expression is void-typed (which can occur when an
    * if-expression's branches have mismatched types — the analyzer types the enclosing expression based on the
    * first branch only), synthesize a push_0 so the stack stays balanced.
    */
  private def genStmtsAsExpr(stmts: List[TStmt]): Unit =
    if stmts.isEmpty then emitPushInt(0)
    else
      genStmts(stmts.init)
      stmts.last match
        case TExprStmt(expr) =>
          genExpr(expr)
          if expr.typ == SyslType.VoidType then emitPushInt(0)
        case TReturnStmt(Some(expr)) => genExpr(expr); emitDefers(); emit("  ret")
        case other => genStmt(other); emitPushInt(0)

  private def genStmt(stmt: TStmt): Unit = stmt match
    case TVarStmt(name, typ, init, _, _) if addressedLocals.contains(name) && !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] =>
      // Scalar local whose address is taken. Allocate an 8-byte cell on the
      // memory stack; the local slot holds the cell's address. Loads and
      // stores go through the pointer so &x and the local refer to the
      // same storage.
      val idx = allocLocal(name, typ)
      emitMemAlloc(8)
      emit("  dup")
      emit(s"  local_set $idx")
      genExpr(init)
      emit("  swap")
      emitStore(typ)

    case TVarStmt(name, typ, init, _, _) =>
      val idx = allocLocal(name, typ)
      if needsMemAlloc(typ) then
        // Allocate memory on the memory stack, store address in local
        val size = typ.sizeOf
        emitMemAlloc(size)
        emit(s"  dup")
        emit(s"  local_set $idx") // local holds the address
        // Zero-initialize the memory
        val aligned = ((size + 7) / 8 * 8).toInt
        for i <- 0 until aligned by 8 do
          emit("  dup")
          if i > 0 then { emitPushInt(i); emit("  add") }
          emit("  push_0")
          emit("  swap")
          emit("  store64")
        emit("  drop")
        // If init is an array literal or struct construct, populate values
        init match
          case TArrayLit(elements, _) =>
            val elemType = typ match { case SyslType.ArrayType(e, _) => e; case _ => SyslType.I64 }
            for (elem, i) <- elements.zipWithIndex do
              emit(s"  local_get $idx")
              emitPushInt(i * elemType.sizeOf)
              emit("  add")
              genExpr(elem)
              emit("  swap")
              emitStore(elemType)
          case TStructConstruct(structType, args) =>
            for (arg, i) <- args.zipWithIndex do
              val off = fieldOffset(structType, i)
              val fieldType = structType.fields(i)._2
              emit(s"  local_get $idx")
              if off != 0 then { emitPushInt(off); emit("  add") }
              genExpr(arg)
              emit("  swap")
              emitStore(fieldType)
          case TEnumConstruct(et, variantIndex, args) =>
            // Tag at offset 0 (i32)
            emit(s"  local_get $idx")
            emitPushInt(variantIndex)
            emit("  swap")
            emit("  store32")
            // Variant fields at dataOffset
            val dataOff = et.dataOffset.toInt
            val variantFields = et.variants(variantIndex)._2
            var fieldOff = 0
            for (arg, i) <- args.zipWithIndex do
              val (_, fieldType) = variantFields(i)
              val align = fieldType.alignOf.toInt.max(1)
              fieldOff = ((fieldOff + align - 1) / align) * align
              emit(s"  local_get $idx")
              val totalOff = dataOff + fieldOff
              if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
              genExpr(arg)
              emit("  swap")
              emitStore(fieldType)
              fieldOff += fieldType.sizeOf.toInt
          case _: TArrayDecl | _: TStructLit => // already zeroed
          case _ =>
            // General case: init returns an address, bulk copy into our allocation
            genExpr(init) // ( src_addr )
            val copySize = ((typ.sizeOf + 7) / 8 * 8).toInt
            for i <- 0 until copySize by 8 do
              emit("  dup")
              if i > 0 then { emitPushInt(i); emit("  add") }
              emit("  load64")
              emit(s"  local_get $idx")
              if i > 0 then { emitPushInt(i); emit("  add") }
              emit("  store64")
            emit("  drop") // drop src_addr
      else
        genExpr(init)
        emit(s"  local_set $idx")

    case TAssignStmt(target, value) =>
      genExpr(value)
      locals.get(target) match
        case Some(LocalInfo(idx, typ)) if addressedLocals.contains(target) && !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] =>
          // Addressed scalar: write through the cell's pointer.
          emit(s"  local_get $idx")
          emitStore(typ)
        case Some(LocalInfo(idx, _)) => emit(s"  local_set $idx")
        case None if globals.contains(target) =>
          emit(s"  push_i64 $target")
          emit("  store64")
        case None =>
          // Implicit local declaration (e.g. `v = expr?` sugar lowered by the
          // analyzer into `TAssignStmt` with a fresh target).
          val idx = allocLocal(target, value.typ)
          emit(s"  local_set $idx")

    case TCompoundAssignStmt(target, op, value) =>
      locals.get(target) match
        case Some(LocalInfo(idx, typ)) if addressedLocals.contains(target) && !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] =>
          // Addressed scalar: read, compute, write through pointer.
          emit(s"  local_get $idx")
          emitLoad(typ)
          genExpr(value)
          emitBinaryOp(op, typ)
          emit(s"  local_get $idx")
          emitStore(typ)
        case Some(LocalInfo(idx, typ)) =>
          emit(s"  local_get $idx")
          genExpr(value)
          emitBinaryOp(op, typ)
          emit(s"  local_set $idx")
        case None =>
          // Global: load, compute, store
          emit(s"  push_i64 $target")
          emit("  dup")
          emit("  load64")
          genExpr(value)
          emitBinaryOp(op, globals.getOrElse(target, SyslType.I64))
          emit("  swap")
          emit("  store64")

    case TDerefAssignStmt(pointer, value) =>
      genExpr(value)
      genExpr(pointer)
      pointer.typ match
        case SyslType.PtrType(pointee) => emitStore(pointee)
        case _ => emit("  store64")

    case TIndexAssignStmt(array, index, value) =>
      val elemType = array.typ match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case _ => SyslType.I64
      genExpr(value)
      genExpr(array)
      array.typ match
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          emit("  load64") // deref slice struct → data ptr
        case _ =>
      genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emitStore(elemType)

    case TFieldAssignStmt(obj, fieldIndex, value) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genExpr(value)
      genStructAddr(obj)
      if off != 0 then
        emitPushInt(off)
        emit("  add")
      emitStore(fieldType)

    case TReturnStmt(Some(expr)) =>
      genExpr(expr)
      emitDefers()
      emit("  ret")

    case TReturnStmt(None) =>
      emitDefers()
      emit("  ret")

    case TWhileStmt(cond, body, _) =>
      val loopLabel = newLabel("while")
      val endLabel = newLabel("while_end")
      breakLabels.push(endLabel)
      continueLabels.push(loopLabel)
      emit(s"$loopLabel:")
      genExpr(cond)
      emit(s"  jumpz $endLabel")
      genStmts(body)
      emit(s"  jump $loopLabel")
      emit(s"$endLabel:")
      breakLabels.pop()
      continueLabels.pop()

    case TForStmt(init, cond, update, body, _) =>
      val loopLabel = newLabel("for")
      val updateLabel = newLabel("for_upd")
      val endLabel = newLabel("for_end")
      genStmt(init)
      breakLabels.push(endLabel)
      continueLabels.push(updateLabel)
      emit(s"$loopLabel:")
      genExpr(cond)
      emit(s"  jumpz $endLabel")
      genStmts(body)
      emit(s"$updateLabel:")
      genStmt(update)
      emit(s"  jump $loopLabel")
      emit(s"$endLabel:")
      breakLabels.pop()
      continueLabels.pop()

    case TDoWhileStmt(cond, body, _) =>
      val loopLabel = newLabel("do")
      val endLabel = newLabel("do_end")
      breakLabels.push(endLabel)
      continueLabels.push(loopLabel)
      emit(s"$loopLabel:")
      genStmts(body)
      genExpr(cond)
      emit(s"  jumpnz $loopLabel")
      emit(s"$endLabel:")
      breakLabels.pop()
      continueLabels.pop()

    case TLoopStmt(body, _) =>
      val loopLabel = newLabel("loop")
      val endLabel = newLabel("loop_end")
      breakLabels.push(endLabel)
      continueLabels.push(loopLabel)
      emit(s"$loopLabel:")
      genStmts(body)
      emit(s"  jump $loopLabel")
      emit(s"$endLabel:")
      breakLabels.pop()
      continueLabels.pop()

    case TBreakStmt(_) =>
      emit(s"  jump ${breakLabels.top}")

    case TContinueStmt(_) =>
      emit(s"  jump ${continueLabels.top}")

    case TExprStmt(TMatchExpr(scrutinee, arms, default, matchTyp)) =>
      genMatch(scrutinee, arms, default, matchTyp, asExpr = matchTyp != SyslType.VoidType)
      if matchTyp != SyslType.VoidType then emit("  drop")

    case TExprStmt(TIfExpr(cond, thenBody, elseBody, ifTyp)) if ifTyp == SyslType.VoidType =>
      // Void-typed if-stmt: generate bodies as plain statements (no synthetic
      // 0 push, which would leak onto the data stack because the outer
      // TExprStmt won't drop void-typed values).
      val elseLabel = newLabel("else")
      val endLabel = newLabel("endif")
      genExpr(cond)
      emit(s"  jumpz $elseLabel")
      genStmts(thenBody)
      emit(s"  jump $endLabel")
      emit(s"$elseLabel:")
      elseBody match
        case Some(stmts) => genStmts(stmts)
        case None =>
      emit(s"$endLabel:")

    case TExprStmt(expr) =>
      genExpr(expr)
      if expr.typ != SyslType.VoidType then emit("  drop")

    case TAsmStmt(code) =>
      emit(s"  $code")

    case TDeferStmt(body) =>
      deferStack.push(body)

    case TMultiStmt(children) =>
      children.foreach(genStmt)

    case TContractCheck(_, expr, _) =>
      genExpr(expr)
      val pass = newLabel("contract_pass")
      emit(s"  jumpnz $pass")
      emit("  halt")
      emit(s"$pass:")

    case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      // Load current value
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup") // keep address
      emitLoad(fieldType)
      genExpr(value)
      emitBinaryOp(op, fieldType)
      emit("  swap") // ( new_val addr )
      emitStore(fieldType)

    case TDestructureStmt(names, types, init) =>
      genExpr(init) // address of struct on stack
      for (name, i) <- names.zipWithIndex do
        if name != "_" then
          val idx = allocLocal(name, types(i))
          val st = structOf(init.typ)
          val off = fieldOffset(st, i)
          emit("  dup") // keep struct addr
          if off != 0 then { emitPushInt(off); emit("  add") }
          emitLoad(types(i))
          emit(s"  local_set $idx")
      emit("  drop") // discard struct address

    case TDestructureAssignStmt(names, types, init) =>
      // Like TDestructureStmt, but the names already refer to existing locals.
      genExpr(init)
      for (name, i) <- names.zipWithIndex do
        if name != "_" then
          val st = structOf(init.typ)
          val off = fieldOffset(st, i)
          val target = locals.getOrElse(name, {
            val idx = allocLocal(name, types(i))
            LocalInfo(idx, types(i))
          })
          emit("  dup")
          if off != 0 then { emitPushInt(off); emit("  add") }
          emitLoad(types(i))
          emit(s"  local_set ${target.index}")
      emit("  drop")

    case _ => // TODO: remaining stmt types

  // ========================================================================
  // genExpr — leaves exactly one value on the data stack
  // ========================================================================
  private def genExpr(expr: TExpr): Unit = expr match
    case TIntLit(n, _) => emitPushInt(n)

    case TFloatLit(d, _) =>
      val bits = java.lang.Double.doubleToLongBits(d)
      if bits == 0L then emit("  push_f0")
      else if d == 1.0 then emit("  push_f1")
      else emit(s"  push_i64 $bits")

    case TBoolLit(true, _) => emit("  push_1")
    case TBoolLit(false, _) => emit("  push_0")

    case TSizeof(size, _) => emitPushInt(size)

    case TVarRef(name, typ) =>
      // Captures (when compiling a hoisted closure body): read from env_ptr
      // (local 0) at the capture's offset.
      closureCaptures.get(name) match
        case Some((off, capTyp)) =>
          emit("  local_get 0")               // env_ptr
          if off > 0 then { emitPushInt(off); emit("  add") }
          // For aggregates, the address into env IS the value. For scalars, load.
          if !needsMemAlloc(capTyp) && capTyp != SyslType.StringType && !capTyp.isInstanceOf[SyslType.SliceType] then
            emitLoad(capTyp)
          return
        case None =>
      locals.get(name) match
        case Some(LocalInfo(idx, localTyp)) if addressedLocals.contains(name) && !needsMemAlloc(localTyp) && localTyp != SyslType.StringType && !localTyp.isInstanceOf[SyslType.SliceType] =>
          // Addressed scalar: load through the cell's pointer.
          emit(s"  local_get $idx")
          emitLoad(localTyp)
        case Some(LocalInfo(idx, _)) => emit(s"  local_get $idx")
        case None =>
          // Global. Scalars load the cell; aggregates (string / slice /
          // struct / enum / array) are address-represented so the symbol's
          // address IS the value.
          emit(s"  push_i64 $name")
          typ.underlying match
            case _: SyslType.StructType | _: SyslType.EnumType
               | SyslType.StringType | _: SyslType.SliceType
               | _: SyslType.ArrayType => ()
            case _ => emit("  load64")

    case TAddrOf(name, _) =>
      locals.get(name) match
        case Some(LocalInfo(idx, typ)) if needsMemAlloc(typ) =>
          // Aggregate local: the local already holds the memory address
          emit(s"  local_get $idx")
        case Some(LocalInfo(idx, typ)) if addressedLocals.contains(name) =>
          // Addressed scalar: the local already holds the cell's pointer.
          emit(s"  local_get $idx")
        case Some(LocalInfo(idx, typ)) =>
          // Scalar local, not pre-flagged as addressed. Spill to a new slot
          // — caveat: subsequent modifications through this pointer will
          // NOT sync back to the local (fallback path for unscanned uses).
          emitMemAlloc(8)
          emit("  dup")
          emit(s"  local_get $idx")
          emit("  swap")
          emit("  store64")
        case None =>
          emit(s"  push_i64 $name")

    case TAddrOfIndex(array, index, typ) =>
      genExpr(array)
      val elemType = array.typ.underlying match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case _ => SyslType.I64
      array.typ.underlying match
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          emit("  load64") // slice struct → data ptr
        case _ =>
      genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")

    case TAddrOfField(obj, fieldIndex, typ) =>
      genStructAddr(obj)
      val st = obj.typ match
        case s: SyslType.StructType => s
        case SyslType.RefType(s: SyslType.StructType) => s
        case SyslType.PtrType(s: SyslType.StructType) => s
        case _ => sys.error(s"field addr on non-struct: ${obj.typ}")
      val off = fieldOffset(st, fieldIndex)
      if off != 0 then
        emitPushInt(off)
        emit("  add")

    case TDeref(ptr, typ) =>
      genExpr(ptr)
      // Aggregates are address-represented; dereferencing a pointer to one
      // is a no-op — the pointer value IS the aggregate "value".
      if !needsMemAlloc(typ) && typ != SyslType.StringType && !typ.isInstanceOf[SyslType.SliceType] then
        emitLoad(typ)

    case TIndex(array, index, typ) =>
      genExpr(array)
      val elemType = array.typ match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case SyslType.StringType => SyslType.UIntType(8)
        case _ => typ
      array.typ match
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) | SyslType.StringType =>
          emit("  load64") // deref struct → data ptr (strings and slices both start with ptr at offset 0)
        case _ =>
      genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emitLoad(typ)

    case TFieldAccess(obj, fieldIndex, typ) =>
      genStructAddr(obj)
      val st = canonicalStruct(obj.typ.underlying match
        case s: SyslType.StructType => s
        case SyslType.RefType(s) => s.underlying match
          case ss: SyslType.StructType => ss
          case _ => sys.error(s"field access on non-struct: ${obj.typ}")
        case SyslType.PtrType(s) => s.underlying match
          case ss: SyslType.StructType => ss
          case _ => sys.error(s"field access on non-struct: ${obj.typ}")
        case _ => sys.error(s"field access on non-struct: ${obj.typ}"))
      val off = fieldOffset(st, fieldIndex)
      if off != 0 then
        emitPushInt(off)
        emit("  add")
      emitLoad(typ)

    case TBinary(left, "&&", right, _) =>
      val falseLabel = newLabel("and_f")
      val endLabel = newLabel("and_end")
      genExpr(left)
      emit(s"  jumpz $falseLabel")
      genExpr(right)
      emit(s"  jumpz $falseLabel")
      emit("  push_1")
      emit(s"  jump $endLabel")
      emit(s"$falseLabel:")
      emit("  push_0")
      emit(s"$endLabel:")

    case TBinary(left, "||", right, _) =>
      val trueLabel = newLabel("or_t")
      val endLabel = newLabel("or_end")
      genExpr(left)
      emit(s"  jumpnz $trueLabel")
      genExpr(right)
      emit(s"  jumpnz $trueLabel")
      emit("  push_0")
      emit(s"  jump $endLabel")
      emit(s"$trueLabel:")
      emit("  push_1")
      emit(s"$endLabel:")

    case TBinary(left, op @ ("+" | "-"), right, typ) if left.typ.isInstanceOf[SyslType.PtrType] =>
      // Pointer arithmetic: scale the integer operand by pointee size
      val pointee = left.typ.asInstanceOf[SyslType.PtrType].pointee
      genExpr(left)
      genExpr(right)
      val elemSize = pointee.sizeOf
      if elemSize != 1 then
        emitPushInt(elemSize)
        emit("  mul")
      emitBinaryOp(op, SyslType.I64)

    case TBinary(left, "+", right, SyslType.StringType) =>
      genExpr(left)
      genExpr(right)
      emit("  call __svm_str_concat")
      needsStrConcat = true

    case TBinary(left, op @ ("==" | "!="), right, _) if left.typ == SyslType.StringType =>
      genExpr(left)
      genExpr(right)
      emit("  call __svm_str_eq")
      if op == "!=" then emit("  eqz")
      needsStrEq = true

    case TBinary(left, op, right, typ) =>
      genExpr(left)
      genExpr(right)
      emitBinaryOp(op, left.typ)

    case TUnary("-", operand, _) =>
      genExpr(operand)
      if isFloat(operand.typ) then emit("  fneg")
      else emit("  neg")
      truncateForNarrow(operand.typ)

    case TUnary("!", operand, _) =>
      genExpr(operand)
      emit("  eqz")

    case TUnary("~", operand, _) =>
      genExpr(operand)
      emit("  not")
      truncateForNarrow(operand.typ)

    case TRangeCheck(inner, range, _, _) =>
      genExpr(inner) // stack: [val]
      val failLbl = newLabel("range_fail")
      val passLbl = newLabel("range_pass")
      val u = inner.typ.underlying.isUnsigned
      val f = inner.typ.underlying.isFloat
      def pushNum(n: Any): Unit = n match
        case v: Long => emitPushInt(v)
        case v: Double =>
          val bits = java.lang.Double.doubleToRawLongBits(v)
          emit(s"  push_i64 $bits")
      def geOp(): String = if f then "fge" else if u then "geu" else "ge"
      def ltOp(): String = if f then "flt" else if u then "ltu" else "lt"
      def leOp(): String = if f then "fle" else if u then "leu" else "le"
      range match
        case IntRange(lo, hi, excl) =>
          emit("  dup")
          pushNum(lo)
          emit(s"  ${geOp()}")
          emit(s"  jumpz $failLbl")
          emit("  dup")
          pushNum(hi)
          emit(s"  ${if excl then ltOp() else leOp()}")
          emit(s"  jumpz $failLbl")
        case FloatRange(lo, hi, excl) =>
          emit("  dup")
          pushNum(lo)
          emit(s"  ${geOp()}")
          emit(s"  jumpz $failLbl")
          emit("  dup")
          pushNum(hi)
          emit(s"  ${if excl then ltOp() else leOp()}")
          emit(s"  jumpz $failLbl")
      emit(s"  jump $passLbl")
      emit(s"$failLbl:")
      emit("  halt")
      emit(s"$passLbl:")

    case TStringFromSlice(slice, _) =>
      // []byte -> string: allocate fresh 16-byte {ptr, len i64}, copy slice ptr/len
      genExpr(slice)
      val srcIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $srcIdx")
      emitMemAlloc(16)
      val dstIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dstIdx")
      emit(s"  local_get $srcIdx"); emit("  load64")
      emit(s"  local_get $dstIdx"); emit("  store64")
      emit(s"  local_get $srcIdx"); emitPushInt(8); emit("  add"); emit("  load32")
      emit(s"  local_get $dstIdx"); emitPushInt(8); emit("  add"); emit("  store64")
      emit(s"  local_get $dstIdx")

    case TStringFromPtr(ptr, len, _) =>
      // string(ptr, len) -> string: allocate 16-byte {ptr, len}, fill both
      genExpr(ptr)
      val ptrIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $ptrIdx")
      genExpr(len)
      val lenIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $lenIdx")
      emitMemAlloc(16)
      val dstIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dstIdx")
      emit(s"  local_get $ptrIdx"); emit(s"  local_get $dstIdx"); emit("  store64")
      emit(s"  local_get $lenIdx"); emit(s"  local_get $dstIdx"); emitPushInt(8); emit("  add"); emit("  store64")
      emit(s"  local_get $dstIdx")

    case TCast(inner, target) =>
      genExpr(inner)
      emitCast(inner.typ, target)

    case TCall(name, args, _) =>
      // Push args left-to-right, materializing a slice struct when the param
      // expects a slice and the caller is handing over a fixed array.
      val paramTypes = funcParamTypes.getOrElse(name, Nil)
      for (arg, idx) <- args.zipWithIndex do
        val paramType = paramTypes.lift(idx)
        (arg.typ.underlying, paramType.map(_.underlying)) match
          case (SyslType.ArrayType(_, size), Some(_: SyslType.SliceType)) =>
            emitArrayToSlice(arg, size)
          case _ => genExpr(arg)
      emit(s"  call $name")

    case TStr(inner) =>
      inner.typ.underlying match
        case SyslType.StringType => genExpr(inner) // identity
        case SyslType.BoolType =>
          genExpr(inner)
          emit("  call __svm_str_from_bool")
          needsStrFromBool = true
        case t if t.isIntegral =>
          genExpr(inner)
          // Widen narrow ints to i64 for the runtime helper. Sign-extend signed
          // types; zero-extend unsigned.
          if t.bitWidth < 64 then
            if t.isSigned then
              emitPushInt(64 - t.bitWidth); emit("  shl")
              emitPushInt(64 - t.bitWidth); emit("  sar")
            else
              t.bitWidth match
                case 8 => emitPushInt(0xff); emit("  and")
                case 16 => emitPushInt(0xffff); emit("  and")
                case 32 => emit("  push_i64 4294967295"); emit("  and")
                case _ => ()
          emit("  call __svm_str_from_i64")
          needsStrFromI64 = true
        case _: SyslType.EnumType =>
          // Simple enum (no data variants): use the runtime int helper on the tag.
          // Data-enum variants would need the analyzer's tag-dispatch helpers,
          // which std/ doesn't currently exercise on SVM. Fall through to ???
          // for non-simple enums.
          val isSimple = inner.typ.underlying match
            case SyslType.EnumType(_, vs) => vs.forall(_._2.isEmpty)
            case _ => false
          if isSimple then
            genExpr(inner)
            // Tag is loaded as i32; load it from the address and convert.
            emit("  load32s")
            emit("  call __svm_str_from_i64")
            needsStrFromI64 = true
          else
            emitStrPlaceholder(inner)
        case _ =>
          emitStrPlaceholder(inner)

    case TTempAddr(inner, _) =>
      inner.typ.underlying match
        case _: SyslType.StructType | _: SyslType.EnumType
           | SyslType.StringType | _: SyslType.SliceType
           | _: SyslType.ArrayType =>
          // Address-represented aggregate: genExpr already returns an address.
          genExpr(inner)
        case _ =>
          // Scalar: spill to 8-byte slot on the memory stack, return slot addr.
          genExpr(inner)
          emitMemAlloc(8)
          emit("  dup")           // (val, slot, slot)
          emit("  rot")           // (slot, slot, val)
          emit("  swap")          // (slot, val, slot)
          emit("  store64")       // stack: (slot)

    case TIfExpr(cond, thenBody, Some(elseBody), typ) =>
      val elseLabel = newLabel("else")
      val endLabel = newLabel("endif")
      genExpr(cond)
      emit(s"  jumpz $elseLabel")
      if typ == SyslType.VoidType then genStmts(thenBody) else genStmtsAsExpr(thenBody)
      emit(s"  jump $endLabel")
      emit(s"$elseLabel:")
      if typ == SyslType.VoidType then genStmts(elseBody) else genStmtsAsExpr(elseBody)
      emit(s"$endLabel:")

    case TIfExpr(cond, thenBody, None, typ) =>
      if typ == SyslType.VoidType then
        val endLabel = newLabel("endif")
        genExpr(cond)
        emit(s"  jumpz $endLabel")
        genStmts(thenBody)
        emit(s"$endLabel:")
      else
        // Non-void if-without-else: skip path needs a synthetic value so the
        // stack is balanced regardless of branch taken. (Analyzer types such
        // expressions non-void based on the then-body's last expression.)
        val elseLabel = newLabel("else")
        val endLabel = newLabel("endif")
        genExpr(cond)
        emit(s"  jumpz $elseLabel")
        genStmtsAsExpr(thenBody)
        emit(s"  jump $endLabel")
        emit(s"$elseLabel:")
        emitPushInt(0)
        emit(s"$endLabel:")

    case TMatchExpr(scrutinee, arms, default, matchTyp) =>
      genMatch(scrutinee, arms, default, matchTyp, asExpr = true)

    case TPreInc(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  inc")
      emit("  dup")
      emit(s"  local_set $idx")

    case TPreDec(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  dec")
      emit("  dup")
      emit(s"  local_set $idx")

    case TPostInc(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  dup")
      emit("  inc")
      emit(s"  local_set $idx")

    case TPostDec(name, _) =>
      val LocalInfo(idx, _) = locals(name): @unchecked
      emit(s"  local_get $idx")
      emit("  dup")
      emit("  dec")
      emit(s"  local_set $idx")

    case TStringLit(value, _) =>
      labelCounter += 1
      val label = if modulePrefix.nonEmpty then s"__str_${modulePrefix}_$labelCounter" else s"__str_$labelCounter"
      stringLiterals += ((label, value))
      val bytes = value.getBytes("UTF-8")
      // String is a 16-byte fat pointer {ptr, len} allocated on memory stack.
      // The label points past the refcount header to the byte data.
      emitMemAlloc(16)
      emit("  dup")
      emit(s"  push_i64 $label") // ptr to byte data
      emit("  swap")
      emit("  store64")          // store ptr at offset 0
      emit("  dup")
      emitPushInt(8)
      emit("  add")
      emitPushInt(bytes.length)
      emit("  swap")
      emit("  store64")          // store len at offset 8

    case TAsmExpr(code, _) =>
      emit(s"  $code")

    case TFmtStr(inner, spec) =>
      // Lower formatted-string interpolations to a runtime helper. For
      // integer verbs we route through __svm_str_fmt_i64 with the appropriate
      // base/width/flag bits. For %s we just pass the string through (with
      // optional padding via __svm_str_fmt_i64 — not supported yet, fall back
      // to the unpadded string).
      val verb = spec.verb
      verb match
        case 'd' | 'x' | 'X' | 'o' | 'b' if inner.typ.isIntegral =>
          genExpr(inner)
          // Widen narrow ints to i64 for the runtime helper.
          val t = inner.typ.underlying
          if t.bitWidth < 64 then
            if t.isSigned then
              emitPushInt(64 - t.bitWidth); emit("  shl")
              emitPushInt(64 - t.bitWidth); emit("  sar")
            else
              t.bitWidth match
                case 8 => emitPushInt(0xff); emit("  and")
                case 16 => emitPushInt(0xffff); emit("  and")
                case 32 => emit("  push_i64 4294967295"); emit("  and")
                case _ => ()
          val base = verb match
            case 'd' => 10; case 'x' | 'X' => 16; case 'o' => 8; case 'b' => 2
            case _ => 10
          emitPushInt(base)
          emitPushInt(spec.width)
          var flags = 0
          if spec.zeroPad then flags |= 0x1
          if spec.leftAlign then flags |= 0x2
          if spec.showSign then flags |= 0x4
          if spec.upperCase || verb == 'X' then flags |= 0x8
          emitPushInt(flags)
          emit("  call __svm_str_fmt_i64")
          needsStrFmtI64 = true
        case 's' if inner.typ.underlying == SyslType.StringType =>
          // %s without width: pass through. With width, padding isn't yet
          // implemented for string verbs; just pass through (matches current
          // behaviour of LLVM with simple specs).
          genExpr(inner)
        case _ =>
          // Any other shape: fall back to plain TStr semantics.
          genExpr(TStr(inner))

    case TQuantifier(kind, name, nameType, lo, hi, inclusive, pred, _) =>
      // Lower to a short-circuiting loop. `result` is the accumulator —
      // starts at 1 for "all" (vacuous truth on empty range) and 0 for
      // "some". On a counterexample (all) or witness (some), set the result
      // and break out of the loop.
      val resultIdx = nextLocalIndex; nextLocalIndex += 1
      val iterIdx = nextLocalIndex; nextLocalIndex += 1
      val endIdx = nextLocalIndex; nextLocalIndex += 1
      val initBit = if kind == "all" then 1 else 0
      emitPushInt(initBit)
      emit(s"  local_set $resultIdx")
      genExpr(lo)
      emit(s"  local_set $iterIdx")
      genExpr(hi)
      if !inclusive then emit("  dec")
      emit(s"  local_set $endIdx")
      // Bind the loop variable so genExpr(pred) finds it as a regular local.
      val savedBinding = locals.get(name)
      locals(name) = LocalInfo(iterIdx, nameType)
      val condLbl = newLabel("quant_cond")
      val incLbl = newLabel("quant_inc")
      val endLbl = newLabel("quant_end")
      emit(s"$condLbl:")
      emit(s"  local_get $iterIdx")
      emit(s"  local_get $endIdx")
      emit(if nameType.isUnsigned then "  leu" else "  le")
      emit(s"  jumpz $endLbl")
      genExpr(pred)
      if kind == "all" then
        // pred true → continue; pred false → set 0 and break
        emit(s"  jumpnz $incLbl")
        emit("  push_0")
        emit(s"  local_set $resultIdx")
        emit(s"  jump $endLbl")
      else
        // pred true → set 1 and break; pred false → continue
        emit(s"  jumpz $incLbl")
        emit("  push_1")
        emit(s"  local_set $resultIdx")
        emit(s"  jump $endLbl")
      emit(s"$incLbl:")
      emit(s"  local_get $iterIdx")
      emit("  inc")
      emit(s"  local_set $iterIdx")
      emit(s"  jump $condLbl")
      emit(s"$endLbl:")
      // Restore prior binding (or remove the synthetic one).
      savedBinding match
        case Some(b) => locals(name) = b
        case None => locals.remove(name)
      emit(s"  local_get $resultIdx")

    case TIntrinsicCall(intrName, args, retTyp) =>
      // Compiler intrinsics — wrapping/saturating arithmetic. SVM int ops
      // wrap naturally for i64; for narrow types `emitBinaryOp` already
      // truncates. Saturating variants need explicit overflow detection.
      intrName match
        case "wrapping_add" | "wrapping_sub" | "wrapping_mul" =>
          genExpr(args(0))
          genExpr(args(1))
          val op = intrName.stripPrefix("wrapping_") match
            case "add" => "+"; case "sub" => "-"; case "mul" => "*"
          emitBinaryOp(op, retTyp)
        case "saturating_add" | "saturating_sub" | "saturating_mul" =>
          val signed = retTyp.isSigned
          val width = retTyp.bitWidth
          // Bounds for the target type
          val (minV, maxV) = if signed then
            (-(1L << (width - 1)), (1L << (width - 1)) - 1)
          else
            (0L, if width == 64 then -1L else (1L << width) - 1)
          // Stash a, b in temp locals
          val aIdx = nextLocalIndex; nextLocalIndex += 1
          val bIdx = nextLocalIndex; nextLocalIndex += 1
          val rIdx = nextLocalIndex; nextLocalIndex += 1
          genExpr(args(0))
          emit(s"  local_set $aIdx")
          genExpr(args(1))
          emit(s"  local_set $bIdx")
          // r = wrapping op (full i64 then truncate at the end)
          emit(s"  local_get $aIdx")
          emit(s"  local_get $bIdx")
          val op = intrName match
            case "saturating_add" => "+"
            case "saturating_sub" => "-"
            case "saturating_mul" => "*"
          emit(op match { case "+" => "  add"; case "-" => "  sub"; case "*" => "  mul" })
          emit(s"  local_set $rIdx")
          val satLbl = newLabel("sat_done")
          if !signed then
            // unsigned overflow detection:
            // add: width<64 → r > MAX; width=64 → r < a (wrap)
            // sub: a < b → underflow → set 0
            // mul: if a != 0 && r/a != b → overflow → set MAX
            intrName match
              case "saturating_add" =>
                if width < 64 then
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gtu")
                else
                  emit(s"  local_get $rIdx"); emit(s"  local_get $aIdx"); emit("  ltu")
                val notOv = newLabel("sat_no_ov")
                emit(s"  jumpz $notOv")
                emitPushInt(maxV)
                emit(s"  local_set $rIdx")
                emit(s"$notOv:")
              case "saturating_sub" =>
                emit(s"  local_get $aIdx"); emit(s"  local_get $bIdx")
                emit("  ltu")
                val notUf = newLabel("sat_no_uf")
                emit(s"  jumpz $notUf")
                emit("  push_0")
                emit(s"  local_set $rIdx")
                emit(s"$notUf:")
              case "saturating_mul" =>
                // For narrow widths the wrapping result already truncated; check
                // against MAX.  For i64, use divu by a to detect overflow.
                if width < 64 then
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gtu")
                  val notOv = newLabel("sat_no_ov")
                  emit(s"  jumpz $notOv")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"$notOv:")
                else
                  // 64-bit unsigned saturating_mul: if a != 0 && r/a != b → overflow.
                  emit(s"  local_get $aIdx"); emit("  push_0"); emit("  neq")
                  val skip = newLabel("sat_skip")
                  emit(s"  jumpz $skip")     // a == 0 → r already 0, no overflow
                  emit(s"  local_get $rIdx"); emit(s"  local_get $aIdx"); emit("  divu")
                  emit(s"  local_get $bIdx"); emit("  neq")
                  val notOv = newLabel("sat_no_ov")
                  emit(s"  jumpz $notOv")
                  emit("  push_m1")          // unsigned MAX = -1
                  emit(s"  local_set $rIdx")
                  emit(s"$notOv:")
                  emit(s"$skip:")
              case _ => ()
          else
            // signed overflow detection
            intrName match
              case "saturating_add" =>
                // overflow if (a >= 0 && b >= 0 && r < 0) → MAX
                // underflow if (a < 0 && b < 0 && r >= 0) → MIN
                val noOv = newLabel("sat_no_ov")
                val checkUf = newLabel("sat_check_uf")
                // a >= 0?
                emit(s"  local_get $aIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpnz $checkUf") // a < 0 → check underflow
                emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpnz $noOv")    // b < 0 → no overflow
                emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpz $noOv")     // r >= 0 → no overflow
                emitPushInt(maxV); emit(s"  local_set $rIdx")
                emit(s"  jump $noOv")
                emit(s"$checkUf:")
                emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpz $noOv")     // b >= 0 → no underflow
                emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpnz $noOv")    // r < 0 → no underflow
                emitPushInt(minV); emit(s"  local_set $rIdx")
                emit(s"$noOv:")
              case "saturating_sub" =>
                // overflow if (a >= 0 && b < 0 && r < 0) → MAX
                // underflow if (a < 0 && b >= 0 && r >= 0) → MIN
                val noOv = newLabel("sat_no_ov")
                val checkUf = newLabel("sat_check_uf")
                emit(s"  local_get $aIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpnz $checkUf")
                emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpz $noOv")     // b >= 0 → no overflow (a-b: a>=0, b>=0 stays in range or underflows below)
                emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpz $noOv")
                emitPushInt(maxV); emit(s"  local_set $rIdx")
                emit(s"  jump $noOv")
                emit(s"$checkUf:")
                emit(s"  local_get $bIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpnz $noOv")
                emit(s"  local_get $rIdx"); emit("  push_0"); emit("  lt")
                emit(s"  jumpnz $noOv")
                emitPushInt(minV); emit(s"  local_set $rIdx")
                emit(s"$noOv:")
              case "saturating_mul" =>
                // For narrow widths: check against [minV, maxV].
                if width < 64 then
                  val skip = newLabel("sat_skip")
                  val ov = newLabel("sat_ov")
                  emit(s"  local_get $rIdx"); emitPushInt(maxV); emit("  gt")
                  emit(s"  jumpnz $ov")
                  emit(s"  local_get $rIdx"); emitPushInt(minV); emit("  lt")
                  emit(s"  jumpz $skip")
                  emit(s"$ov:")
                  // sign of (a XOR b) determines clamp direction
                  emit(s"  local_get $aIdx"); emit(s"  local_get $bIdx"); emit("  xor")
                  emit("  push_0"); emit("  lt")
                  val negSign = newLabel("sat_neg")
                  emit(s"  jumpnz $negSign")
                  emitPushInt(maxV); emit(s"  local_set $rIdx")
                  emit(s"  jump $skip")
                  emit(s"$negSign:")
                  emitPushInt(minV); emit(s"  local_set $rIdx")
                  emit(s"$skip:")
                else
                  // 64-bit signed saturating_mul: omit (rare; std/ doesn't use)
                  ()
              case _ => ()
          emit(s"$satLbl:")
          emit(s"  local_get $rIdx")
        case _ =>
          sys.error(s"unsupported intrinsic '$intrName' on SVM backend")

    case TAddrLit(fpOffset) =>
      // Address relative to the frame pointer — used only by hidden return
      // slot args, which SVM doesn't use. Push the local slot's address as
      // the byte offset; rely on the fact that locals are 8-byte cells.
      // Since SVM doesn't have a frame-relative address mode, surface this as
      // an error if it ever gets exercised — std/ doesn't reach here.
      sys.error(s"TAddrLit(fp+$fpOffset) unsupported on SVM (no frame-relative addressing)")

    case TFuncRef(name, typ) =>
      // FuncType is a 16-byte aggregate {func_ptr, env_ptr}. Construct a
      // descriptor on the memory stack pointing at a per-function shim that
      // ignores env and forwards to `name`. Without the shim, an indirect
      // call would push env_ptr as a hidden first arg that `name` does not
      // accept.
      val (paramTypes, retType) = typ match
        case SyslType.FuncType(p, r, _, _) => (p, r)
        case _ => (Nil, SyslType.VoidType)
      val shim = shimNameFor(name)
      if !emittedShims.contains(shim) then
        emittedShims += shim
        pendingShims += ((shim, name, paramTypes, retType))
      emitMemAlloc(16)
      emit("  dup")
      emit(s"  push_i64 $shim")
      emit("  swap")
      emit("  store64")            // descr[0] = shim_ptr
      emit("  dup")
      emitPushInt(8)
      emit("  add")
      emit("  push_0")
      emit("  swap")
      emit("  store64")            // descr[8] = 0 (no env)

    case c: TClosure =>
      genClosureExpr(c)

    case TIndirectCall(callee, args, _) =>
      // Closure-style indirect call: callee evaluates to a 16-byte descriptor
      // address. We push env_ptr as a hidden first arg, then explicit args,
      // then load the func_ptr and `callr`. Plain function pointers go through
      // their per-function shim (constructed by TFuncRef) which ignores env.
      callee.typ match
        case _: SyslType.FuncType =>
          genExpr(callee)               // descr_addr
          val descrIdx = nextLocalIndex
          nextLocalIndex += 1
          emit(s"  local_set $descrIdx")
          // Push env_ptr (hidden first arg)
          emit(s"  local_get $descrIdx")
          emitPushInt(8)
          emit("  add")
          emit("  load64")
          // Push explicit args
          for a <- args do genExpr(a)
          // Push func_ptr and callr
          emit(s"  local_get $descrIdx")
          emit("  load64")
          emit("  callr")
        case _ =>
          // Legacy/non-FuncType callee: treat as raw 8-byte function pointer.
          for a <- args do genExpr(a)
          genExpr(callee)
          emit("  callr")

    case TLen(inner, _) =>
      inner.typ match
        case SyslType.StringType =>
          genExpr(inner)
          emitPushInt(8)
          emit("  add")
          emit("  load64")
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          genExpr(inner)
          emitPushInt(8)
          emit("  add")
          emit("  load32")
        case SyslType.ArrayType(_, size) =>
          emitPushInt(size)
        case _ =>
          genExpr(inner)

    case TCap(inner, _) =>
      inner.typ match
        case SyslType.SliceType(_) | SyslType.RefType(SyslType.SliceType(_)) =>
          genExpr(inner)
          emitPushInt(12)
          emit("  add")
          emit("  load32")
        case SyslType.ArrayType(_, size) =>
          emitPushInt(size)
        case _ =>
          genExpr(inner)

    case TStructLit(typ) =>
      // Zero-initialized struct on memory stack
      val size = typ.sizeOf
      emitMemAlloc(size)
      // emitMemAlloc already returns fresh (zeroed by convention? no — we must zero)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")

    case TNewArray(elemType, size) =>
      // __svm_new_slice(byteSize, elemCount) — returns pointer to 24-byte slice struct
      genExpr(size)                 // elemCount
      emit("  dup")                 // dup for byteSize computation
      emitPushInt(elemType.sizeOf)
      emit("  mul")                 // byteSize on TOS
      emit("  swap")                // (byteSize, elemCount)
      emit("  call __svm_new_slice")
      needsNewSlice = true

    case TAppend(slice, elem, SyslType.SliceType(elemType)) =>
      val elemSize = elemType.sizeOf
      // Eval slice addr, save to local
      genExpr(slice)
      val sliceIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $sliceIdx")
      // oldPtr = slice.ptr
      emit(s"  local_get $sliceIdx")
      emit("  load64")
      val oldPtrIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $oldPtrIdx")
      // oldLen = slice.len
      emit(s"  local_get $sliceIdx")
      emitPushInt(8)
      emit("  add")
      emit("  load32")
      val oldLenIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $oldLenIdx")
      // Allocate new slice of (oldLen + 1) elements
      emit(s"  local_get $oldLenIdx")
      emit("  inc")
      emit("  dup")
      emitPushInt(elemSize)
      emit("  mul")
      emit("  swap")
      emit("  call __svm_new_slice")
      val newIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $newIdx")
      needsNewSlice = true
      // dst = new.ptr
      emit(s"  local_get $newIdx")
      emit("  load64")
      val dstIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dstIdx")
      // remaining = oldLen * elemSize
      emit(s"  local_get $oldLenIdx")
      emitPushInt(elemSize)
      emit("  mul")
      val remIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $remIdx")
      // byte copy loop
      val loop = newLabel("app_copy")
      val done = newLabel("app_copy_done")
      emit(s"$loop:")
      emit(s"  local_get $remIdx"); emit("  eqz"); emit(s"  jumpnz $done")
      emit(s"  local_get $oldPtrIdx"); emit("  load8")
      emit(s"  local_get $dstIdx"); emit("  store8")
      emit(s"  local_get $oldPtrIdx"); emit("  inc"); emit(s"  local_set $oldPtrIdx")
      emit(s"  local_get $dstIdx"); emit("  inc"); emit(s"  local_set $dstIdx")
      emit(s"  local_get $remIdx"); emit("  dec"); emit(s"  local_set $remIdx")
      emit(s"  jump $loop")
      emit(s"$done:")
      // Store new element at new.ptr + oldLen * elemSize
      genExpr(elem)
      emit(s"  local_get $newIdx")
      emit("  load64")
      emit(s"  local_get $oldLenIdx")
      emitPushInt(elemSize)
      emit("  mul")
      emit("  add")
      emitStore(elemType)
      // Leave new slice addr on TOS
      emit(s"  local_get $newIdx")

    case TInterfaceBox(inner, iface) =>
      // Box a concrete value into a 16-byte {itable_ptr, data_ptr} struct
      // on the memory stack. For struct values the data_ptr is the struct's
      // backing address; for pointer/ref types the pointer IS the data_ptr.
      val structName = inner.typ.underlying match
        case SyslType.StructType(n, _, _) => n
        case SyslType.PtrType(s) => s.underlying match
          case SyslType.StructType(n, _, _) => n
          case other => sys.error(s"TInterfaceBox: unsupported $other")
        case SyslType.RefType(s) => s.underlying match
          case SyslType.StructType(n, _, _) => n
          case other => sys.error(s"TInterfaceBox: unsupported $other")
        case other => sys.error(s"TInterfaceBox: unsupported $other")
      val itableName = s"__itable_${structName}_${iface.name}"
      if !itables.contains(itableName) then
        itables(itableName) = (iface, structName)
      // Evaluate inner — for struct types genExpr leaves the struct address
      // on TOS; for ptr/ref types it leaves the pointer value.
      genExpr(inner)
      val dataIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $dataIdx")
      // Allocate 16-byte iface struct
      emitMemAlloc(16)
      val ifaceIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $ifaceIdx")
      // struct.itable = &itableName
      emit(s"  push_i64 $itableName")
      emit(s"  local_get $ifaceIdx")
      emit("  store64")
      // struct.data = dataPtr
      emit(s"  local_get $dataIdx")
      emit(s"  local_get $ifaceIdx")
      emitPushInt(8)
      emit("  add")
      emit("  store64")
      emit(s"  local_get $ifaceIdx")

    case TInterfaceDispatch(ifaceVal, methodIndex, args, _) =>
      // Load data_ptr (becomes first arg, as implicit self), push user args,
      // then call through itable[methodIndex].
      genExpr(ifaceVal)                      // iface struct addr
      val ifaceIdx = nextLocalIndex; nextLocalIndex += 1
      emit(s"  local_set $ifaceIdx")
      emit(s"  local_get $ifaceIdx")
      emitPushInt(8)
      emit("  add")
      emit("  load64")                        // data_ptr → pushed as first arg
      for a <- args do genExpr(a)
      emit(s"  local_get $ifaceIdx")
      emit("  load64")                        // itable_ptr
      if methodIndex != 0 then
        emitPushInt(methodIndex * 8)
        emit("  add")
      emit("  load64")                        // method fn ptr
      emit("  callr")

    case TNewEnum(et, variantIndex, args) =>
      // Heap-allocated enum variant. SVM has no real heap; allocate on the
      // memory stack and leak per-test (same convention as TNew). The result
      // type is RefType(EnumType) but at the bytecode level the value IS the
      // data pointer — there is no separate rc header on SVM.
      val size = et.sizeOf
      emitMemAlloc(size)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Tag at offset 0 (i32)
      emit("  dup")
      emitPushInt(variantIndex)
      emit("  swap")
      emit("  store32")
      val dataOff = et.dataOffset.toInt
      val variantFields = et.variants(variantIndex)._2
      var fieldOff = 0
      for (arg, i) <- args.zipWithIndex do
        val (_, fieldType) = variantFields(i)
        val align = fieldType.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        emit("  dup")
        val totalOff = dataOff + fieldOff
        if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)
        fieldOff += fieldType.sizeOf.toInt

    case TNew(structType, args) =>
      // Allocate on memory stack (no real heap in SVM); behaves like
      // TStructConstruct but the type is RefType(StructType) rather than
      // the struct itself. Resulting TOS is the struct's base address.
      val size = structType.sizeOf
      emitMemAlloc(size)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      for (arg, i) <- args.zipWithIndex do
        val off = fieldOffset(structType, i)
        val fieldType = structType.fields(i)._2
        emit("  dup")
        if off != 0 then { emitPushInt(off); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)

    case TSliceExpr(array, lowOpt, highOpt, resultTyp) =>
      // Allocate a 24-byte slice struct on memory stack, fill with
      //   ptr = base + lo * elemSize
      //   len = hi - lo
      //   cap = hi - lo
      //   backref = 0
      val elemType = resultTyp match
        case SyslType.SliceType(e) => e
        case SyslType.RefType(SyslType.SliceType(e)) => e
        case _ => SyslType.I64
      // Compute base pointer + source length based on array.typ
      val baseIdx = nextLocalIndex; nextLocalIndex += 1
      val lenIdx = nextLocalIndex; nextLocalIndex += 1
      array.typ match
        case SyslType.ArrayType(_, n) =>
          genExpr(array)
          emit(s"  local_set $baseIdx")
          emitPushInt(n)
          emit(s"  local_set $lenIdx")
        case SyslType.SliceType(_) =>
          // array is address of 24-byte slice struct
          genExpr(array)
          emit("  dup")             // keep addr
          emit("  load64")          // ptr
          emit(s"  local_set $baseIdx")
          emitPushInt(8)
          emit("  add")
          emit("  load32")          // len (i32)
          emit(s"  local_set $lenIdx")
        case SyslType.RefType(SyslType.SliceType(_)) =>
          // refs are pointers to slice structs in our impl; treat as slice
          genExpr(array)
          emit("  dup")
          emit("  load64")
          emit(s"  local_set $baseIdx")
          emitPushInt(8)
          emit("  add")
          emit("  load32")
          emit(s"  local_set $lenIdx")
        case _ =>
          genExpr(array)
          emit(s"  local_set $baseIdx")
          emitPushInt(0)
          emit(s"  local_set $lenIdx")
      // Evaluate lo (default 0)
      val loIdx = nextLocalIndex; nextLocalIndex += 1
      lowOpt match
        case Some(e) => genExpr(e); emit(s"  local_set $loIdx")
        case None    => emitPushInt(0); emit(s"  local_set $loIdx")
      // Evaluate hi (default len)
      val hiIdx = nextLocalIndex; nextLocalIndex += 1
      highOpt match
        case Some(e) => genExpr(e); emit(s"  local_set $hiIdx")
        case None    => emit(s"  local_get $lenIdx"); emit(s"  local_set $hiIdx")
      // Allocate 24-byte slice struct
      emitMemAlloc(24)
      val structIdx = nextLocalIndex; nextLocalIndex += 1
      emit("  dup")
      emit(s"  local_set $structIdx")
      // struct.ptr = base + lo * elemSize
      emit(s"  local_get $baseIdx")
      emit(s"  local_get $loIdx")
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emit("  swap")                // (ptr, struct_addr)
      emit("  store64")
      // struct.len = hi - lo
      emit(s"  local_get $hiIdx")
      emit(s"  local_get $loIdx")
      emit("  sub")
      emit(s"  local_get $structIdx")
      emitPushInt(8)
      emit("  add")
      emit("  store32")
      // struct.cap = hi - lo
      emit(s"  local_get $hiIdx")
      emit(s"  local_get $loIdx")
      emit("  sub")
      emit(s"  local_get $structIdx")
      emitPushInt(12)
      emit("  add")
      emit("  store32")
      // struct.backref = 0
      emit("  push_0")
      emit(s"  local_get $structIdx")
      emitPushInt(16)
      emit("  add")
      emit("  store64")
      // leave struct addr on TOS
      emit(s"  local_get $structIdx")

    case TEnumConstruct(et, variantIndex, args) =>
      // Allocate enum on memory stack, zero-init, populate tag + variant fields
      val size = et.sizeOf
      emitMemAlloc(size)
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Tag at offset 0 (i32)
      emit("  dup")
      emitPushInt(variantIndex)
      emit("  swap")
      emit("  store32")
      // Variant fields at dataOffset
      val dataOff = et.dataOffset.toInt
      val variantFields = et.variants(variantIndex)._2
      var fieldOff = 0
      for (arg, i) <- args.zipWithIndex do
        val (_, fieldType) = variantFields(i)
        val align = fieldType.alignOf.toInt.max(1)
        fieldOff = ((fieldOff + align - 1) / align) * align
        emit("  dup") // keep enum addr
        val totalOff = dataOff + fieldOff
        if totalOff != 0 then { emitPushInt(totalOff); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)
        fieldOff += fieldType.sizeOf.toInt

    case TStructConstruct(structType, args) =>
      // Allocate struct on memory stack, populate fields
      val size = structType.sizeOf
      emitMemAlloc(size)
      // Zero-init first
      val aligned = ((size + 7) / 8 * 8).toInt
      for i <- 0 until aligned by 8 do
        emit("  dup")
        if i > 0 then { emitPushInt(i); emit("  add") }
        emit("  push_0")
        emit("  swap")
        emit("  store64")
      // Store each field
      for (arg, i) <- args.zipWithIndex do
        val off = fieldOffset(structType, i)
        val fieldType = structType.fields(i)._2
        emit("  dup") // keep struct addr
        if off != 0 then { emitPushInt(off); emit("  add") }
        genExpr(arg)
        emit("  swap")
        emitStore(fieldType)

    case TFieldPreInc(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup") // keep address
      emitLoad(fieldType)
      emit("  inc")
      emit("  dup")  // ( addr new_val new_val )
      emit("  rot")  // ( new_val new_val addr )
      emitStore(fieldType)

    case TFieldPreDec(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup")
      emitLoad(fieldType)
      emit("  dec")
      emit("  dup")
      emit("  rot")
      emitStore(fieldType)

    case TFieldPostInc(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup")
      emitLoad(fieldType)
      emit("  dup")  // ( addr old_val old_val )
      emit("  inc")  // ( addr old_val new_val )
      emit("  rot")  // ( old_val new_val addr )
      emitStore(fieldType)

    case TFieldPostDec(obj, fieldIndex, typ) =>
      val st = structOf(obj.typ)
      val off = fieldOffset(st, fieldIndex)
      val fieldType = st.fields(fieldIndex)._2
      genStructAddr(obj)
      if off != 0 then { emitPushInt(off); emit("  add") }
      emit("  dup")
      emitLoad(fieldType)
      emit("  dup")
      emit("  dec")
      emit("  rot")
      emitStore(fieldType)

    case _ =>
      // TODO: remaining expr types
      emitPushInt(0) // placeholder

  // ========================================================================
  // Helpers
  // ========================================================================

  private def emitBinaryOp(op: String, operandType: SyslType): Unit =
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
  private def truncateForNarrow(t: SyslType): Unit = t.underlying match
    case SyslType.UIntType(8)  => emitPushInt(0xff); emit("  and")
    case SyslType.UIntType(16) => emitPushInt(0xffff); emit("  and")
    case SyslType.UIntType(32) => emit("  push_i64 4294967295"); emit("  and")
    case SyslType.IntType(8)   => emitPushInt(56); emit("  shl"); emitPushInt(56); emit("  sar")
    case SyslType.IntType(16)  => emitPushInt(48); emit("  shl"); emitPushInt(48); emit("  sar")
    case SyslType.IntType(32)  => emitPushInt(32); emit("  shl"); emitPushInt(32); emit("  sar")
    case _ =>

  private def emitStore(typ: SyslType): Unit = typ match
    case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType => emit("  store8")
    case SyslType.IntType(16) | SyslType.UIntType(16) => emit("  store16")
    case SyslType.IntType(32) | SyslType.UIntType(32) => emit("  store32")
    case _: SyslType.StructType | _: SyslType.EnumType | SyslType.StringType | _: SyslType.SliceType
       | _: SyslType.FuncType =>
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

  private def emitLoad(typ: SyslType): Unit = typ.underlying match
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

  private def emitCast(from: SyslType, to: SyslType): Unit =
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

  private def fieldOffset(st: SyslType.StructType, fieldIndex: Int): Long =
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
  private def genMatch(scrutinee: TExpr, arms: List[TMatchArm], default: Option[List[TStmt]], matchTyp: SyslType, asExpr: Boolean): Unit =
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
        case TValuePattern(v) =>
          genExpr(v)
          emit(s"  local_get $scrIdx")
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
        case TDestructurePattern(_, _, _) =>
          emit(s"  jump $hitLabel")
        case TVariantPattern(_, variantIndex, _, _) =>
          // Load tag (i32 at offset 0 of enum), compare with variant index
          emit(s"  local_get $scrIdx")
          emit("  load32")
          emitPushInt(variantIndex)
          emit("  eq")
          emit(s"  jumpnz $hitLabel")
      emit(s"  jump $nextArm")
      emit(s"$hitLabel:")
      // Bind destructure/variant pattern fields to locals before guard
      for pat <- arm.patterns do pat match
        case TVariantPattern(et, variantIndex, bindings, _) =>
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
            fieldOff += fieldType.sizeOf.toInt
        case TDestructurePattern(st, bindings, _) =>
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

  private def genStructAddr(obj: TExpr): Unit = obj match
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
