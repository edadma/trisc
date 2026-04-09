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

  // Globals
  private val globals = new mutable.LinkedHashMap[String, SyslType]
  private val globalConstants = new mutable.LinkedHashMap[String, Long]

  // Loop labels for break/continue
  private val breakLabels = new mutable.Stack[String]
  private val continueLabels = new mutable.Stack[String]

  // Current function
  private var currentFunction: TFunDecl = null

  private def emit(s: String): Unit = out ++= s + "\n"
  private def newLabel(prefix: String): String =
    labelCounter += 1
    if modulePrefix.nonEmpty then s".${prefix}_${modulePrefix}_$labelCounter"
    else s".${prefix}_$labelCounter"

  // Pre-count locals needed for a function body
  private def countLocals(body: TFunBody): Int =
    var count = 0
    def scanStmts(stmts: List[TStmt]): Unit = stmts.foreach(scanStmt)
    def scanStmt(s: TStmt): Unit = s match
      case TVarStmt(_, _, _) => count += 1
      case TWhileStmt(_, body) => scanStmts(body)
      case TForStmt(init, _, update, body) => scanStmt(init); scanStmt(update); scanStmts(body)
      case TDoWhileStmt(_, body) => scanStmts(body)
      case TIfExpr(_, thenBody, elseBody, _) => scanStmts(thenBody); elseBody.foreach(scanStmts)
      case TExprStmt(TIfExpr(_, thenBody, elseBody, _)) => scanStmts(thenBody); elseBody.foreach(scanStmts)
      case TDestructureStmt(names, _, _) => count += names.length
      case _ =>
    body match
      case TExprBody(_) =>
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
      case v => emit(s"  push_i64 $v")

  private def isUnsigned(t: SyslType): Boolean = t.isInstanceOf[SyslType.UIntType]
  private def isFloat(t: SyslType): Boolean = t == SyslType.DoubleType

  private def allocLocal(name: String, typ: SyslType): Int =
    val idx = nextLocalIndex
    locals(name) = LocalInfo(idx, typ)
    nextLocalIndex += 1
    idx

  private def constEval(e: TExpr): Option[Long] = e match
    case TIntLit(n, _) => Some(n)
    case TBoolLit(v, _) => Some(if v then 1 else 0)
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

    modulePrefix = program.decls.collectFirst { case TModuleDecl(path) => path.mkString("_") }.getOrElse("")

    // Emit entry + globals from module metadata
    val meta = ModuleMeta.fromProgram(program)
    val hasMain = meta.symbols.exists(s => s.name == "main" && s.typ.isInstanceOf[SymbolMeta.Kind.Func])
    if hasMain then emit("entry main")
    out ++= meta.toAsmGlobals

    // Collect globals
    val dataGlobals = new mutable.ListBuffer[TDecl]
    val bssGlobals = new mutable.ListBuffer[TDecl]

    for decl <- program.decls do decl match
      case v @ TVarDecl(_, typ, init, _) =>
        globals(v.name) = typ
        constEval(init).foreach(n => globalConstants(v.name) = n)
        if isZeroInit(typ, init) then bssGlobals += v
        else dataGlobals += v
      case _ =>

    // Emit code segment — functions
    emit("segment code")
    for decl <- program.decls do decl match
      case f: TFunDecl => genFunction(f)
      case _ =>

    // Emit rodata segment — string literals
    if stringLiterals.nonEmpty then
      emit("segment rodata")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"global $label, data, ${bytes.length + 9}")
      for (label, value) <- stringLiterals do
        val bytes = value.getBytes("UTF-8")
        emit(s"  dl -1") // immortal refcount header
        emit(s"$label:")
        for b <- bytes do emit(s"  db ${b & 0xff}")
        emit("  db 0")

    // Emit data segment
    if dataGlobals.nonEmpty then
      emit("segment data")
      for decl <- dataGlobals do decl match
        case TVarDecl(name, typ, init, _) =>
          emit(s"  align 8")
          emit(s"$name:")
          constEval(init) match
            case Some(n) => emit(s"  dl $n")
            case None => emit(s"  dl 0")
        case _ =>

    // Emit bss segment
    if bssGlobals.nonEmpty then
      emit("segment bss")
      for decl <- bssGlobals do decl match
        case TVarDecl(name, typ, _, _) =>
          emit(s"  align 8")
          emit(s"$name:")
          emit(s"  rl ${typ.sizeOf.max(8) / 8}")
        case _ =>

    // Emit extern declarations
    val generated = out.toString
    val definedSymbols = program.decls.flatMap {
      case TFunDecl(name, _, _, _, _, _, _) => Some(name)
      case TVarDecl(name, _, _, _) => Some(name)
      case _ => None
    }.toSet

    for decl <- program.decls do decl match
      case TExternFuncDecl(name, _, _) if !definedSymbols.contains(name) =>
        emit(s"extern $name")
      case TExternVarDecl(name, _) if !definedSymbols.contains(name) =>
        emit(s"extern $name")
      case _ =>

    out.toString

  // ========================================================================
  // genFunction
  // ========================================================================
  private def genFunction(fun: TFunDecl): Unit =
    currentFunction = fun
    locals = new mutable.LinkedHashMap
    nextLocalIndex = 0

    val nParams = fun.params.length
    val nBodyLocals = countLocals(fun.body)
    val totalLocals = nParams + nBodyLocals

    emit(s"${fun.name}:")
    if totalLocals > 0 then emit(s"  frame $totalLocals")

    // Pop args from stack into locals.
    // Caller pushes args left-to-right, so TOS = last arg pushed.
    // We need to pop in reverse param order.
    for i <- (0 until nParams).reverse do
      locals(fun.params(i).name) = LocalInfo(i, fun.params(i).typ)
    // Pop: TOS is last param (index nParams-1), next is nParams-2, etc.
    for i <- (nParams - 1) to 0 by -1 do
      emit(s"  local_set $i")
    nextLocalIndex = nParams

    fun.body match
      case TExprBody(expr) =>
        genExpr(expr)
        emit("  ret")
      case TBlockBody(stmts) =>
        if stmts.isEmpty then
          emit("  ret")
        else if fun.returnType != SyslType.VoidType then
          genStmtsAsExpr(stmts)
          emit("  ret")
        else
          genStmts(stmts)
          if !stmts.lastOption.exists(_.isInstanceOf[TReturnStmt]) then
            emit("  ret")

  // ========================================================================
  // genStmts / genStmt
  // ========================================================================
  private def genStmts(stmts: List[TStmt]): Unit = stmts.foreach(genStmt)

  /** Generate statements where the last one leaves its value on the stack (for if-expr, match-expr, function bodies). */
  private def genStmtsAsExpr(stmts: List[TStmt]): Unit =
    if stmts.isEmpty then emitPushInt(0)
    else
      genStmts(stmts.init)
      stmts.last match
        case TExprStmt(expr) => genExpr(expr)
        case TReturnStmt(Some(expr)) => genExpr(expr); emit("  ret")
        case other => genStmt(other); emitPushInt(0)

  private def genStmt(stmt: TStmt): Unit = stmt match
    case TVarStmt(name, typ, init) =>
      val idx = allocLocal(name, typ)
      genExpr(init)
      emit(s"  local_set $idx")

    case TAssignStmt(target, value) =>
      genExpr(value)
      locals.get(target) match
        case Some(LocalInfo(idx, _)) => emit(s"  local_set $idx")
        case None =>
          // Global
          emit(s"  push_i64 $target")
          emit("  store64")

    case TCompoundAssignStmt(target, op, value) =>
      locals.get(target) match
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
        case _ => SyslType.I64
      genExpr(value)
      genExpr(array)
      if array.typ.isInstanceOf[SyslType.SliceType] then emit("  load64") // deref slice ptr
      genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emitStore(elemType)

    case TFieldAssignStmt(obj, fieldIndex, value) =>
      val st = obj.typ.asInstanceOf[SyslType.StructType]
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
      emit("  ret")

    case TReturnStmt(None) =>
      emit("  ret")

    case TWhileStmt(cond, body) =>
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

    case TForStmt(init, cond, update, body) =>
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

    case TDoWhileStmt(cond, body) =>
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

    case TBreakStmt =>
      emit(s"  jump ${breakLabels.top}")

    case TContinueStmt =>
      emit(s"  jump ${continueLabels.top}")

    case TExprStmt(expr) =>
      genExpr(expr)
      if expr.typ != SyslType.VoidType then emit("  drop")

    case TAsmStmt(code) =>
      emit(s"  $code")

    case TDeferStmt(_) =>
      // TODO: defer support
      ()

    case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
      val st = obj.typ.asInstanceOf[SyslType.StructType]
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
        val idx = allocLocal(name, types(i))
        val st = init.typ match
          case s: SyslType.StructType => s
          case SyslType.RefType(s: SyslType.StructType) => s
          case _ => sys.error("destructure requires struct type")
        val off = fieldOffset(st, i)
        emit("  dup") // keep struct addr
        if off != 0 then { emitPushInt(off); emit("  add") }
        emitLoad(types(i))
        emit(s"  local_set $idx")
      emit("  drop") // discard struct address

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
      locals.get(name) match
        case Some(LocalInfo(idx, _)) => emit(s"  local_get $idx")
        case None =>
          // Global variable
          emit(s"  push_i64 $name")
          emit("  load64")

    case TAddrOf(name, _) =>
      locals.get(name) match
        case Some(_) =>
          // Can't take address of SVM local — would need to spill to memory
          sys.error(s"cannot take address of local variable '$name' in SVM backend")
        case None =>
          emit(s"  push_i64 $name")

    case TDeref(ptr, typ) =>
      genExpr(ptr)
      emitLoad(typ)

    case TIndex(array, index, typ) =>
      genExpr(array)
      val elemType = array.typ match
        case SyslType.ArrayType(e, _) => e
        case SyslType.PtrType(e) => e
        case SyslType.SliceType(e) => e
        case _ => typ
      if array.typ.isInstanceOf[SyslType.SliceType] then emit("  load64") // deref slice → data ptr
      genExpr(index)
      emitPushInt(elemType.sizeOf)
      emit("  mul")
      emit("  add")
      emitLoad(typ)

    case TFieldAccess(obj, fieldIndex, typ) =>
      genStructAddr(obj)
      val st = obj.typ match
        case s: SyslType.StructType => s
        case SyslType.RefType(s: SyslType.StructType) => s
        case SyslType.PtrType(s: SyslType.StructType) => s
        case _ => sys.error(s"field access on non-struct: ${obj.typ}")
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

    case TBinary(left, op, right, typ) =>
      genExpr(left)
      genExpr(right)
      emitBinaryOp(op, left.typ)

    case TUnary("-", operand, _) =>
      genExpr(operand)
      if isFloat(operand.typ) then emit("  fneg")
      else emit("  neg")

    case TUnary("!", operand, _) =>
      genExpr(operand)
      emit("  eqz")

    case TUnary("~", operand, _) =>
      genExpr(operand)
      emit("  not")

    case TCast(inner, target) =>
      genExpr(inner)
      emitCast(inner.typ, target)

    case TCall(name, args, _) =>
      // Push args left-to-right
      for arg <- args do genExpr(arg)
      emit(s"  call $name")

    case TIfExpr(cond, thenBody, Some(elseBody), typ) =>
      val elseLabel = newLabel("else")
      val endLabel = newLabel("endif")
      genExpr(cond)
      emit(s"  jumpz $elseLabel")
      genStmtsAsExpr(thenBody)
      emit(s"  jump $endLabel")
      emit(s"$elseLabel:")
      genStmtsAsExpr(elseBody)
      emit(s"$endLabel:")

    case TIfExpr(cond, thenBody, None, typ) =>
      val endLabel = newLabel("endif")
      genExpr(cond)
      emit(s"  jumpz $endLabel")
      genStmtsAsExpr(thenBody)
      emit(s"$endLabel:")

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
      val label = newLabel("str")
      stringLiterals += ((label, value))
      val bytes = value.getBytes("UTF-8")
      // Push address of {ptr, len} — but strings are fat pointers.
      // For now, push the raw data pointer and length.
      // The string literal data is: [refcount(8)] [bytes...] [null]
      // The label points past the refcount to the byte data.
      emit(s"  push_i64 $label")
      // TODO: proper fat pointer handling for strings

    case TAsmExpr(code, _) =>
      emit(s"  $code")

    case TFuncRef(name, _) =>
      emit(s"  push_i64 $name")

    case TLen(inner, _) =>
      inner.typ match
        case SyslType.StringType =>
          genExpr(inner)
          emitPushInt(8)
          emit("  add")
          emit("  load64")
        case SyslType.SliceType(_) =>
          genExpr(inner)
          emitPushInt(8)
          emit("  add")
          emit("  load32")
        case SyslType.ArrayType(_, size) =>
          emitPushInt(size)
        case _ =>
          genExpr(inner)

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
      case "+" => emit(if f then "  fadd" else "  add")
      case "-" => emit(if f then "  fsub" else "  sub")
      case "*" => emit(if f then "  fmul" else "  mul")
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
      case "<<" => emit("  shl")
      case ">>" => emit(if u then "  shr" else "  sar")
      case _ => sys.error(s"unsupported binary operator: $op")

  private def emitLoad(typ: SyslType): Unit = typ match
    case SyslType.IntType(8) => emit("  load8s")
    case SyslType.UIntType(8) | SyslType.BoolType => emit("  load8")
    case SyslType.IntType(16) => emit("  load16s")
    case SyslType.UIntType(16) => emit("  load16")
    case SyslType.IntType(32) => emit("  load32s")
    case SyslType.UIntType(32) => emit("  load32")
    case _ => emit("  load64")

  private def emitStore(typ: SyslType): Unit = typ match
    case SyslType.IntType(8) | SyslType.UIntType(8) | SyslType.BoolType => emit("  store8")
    case SyslType.IntType(16) | SyslType.UIntType(16) => emit("  store16")
    case SyslType.IntType(32) | SyslType.UIntType(32) => emit("  store32")
    case _ => emit("  store64")

  private def emitCast(from: SyslType, to: SyslType): Unit =
    import SyslType.*
    val srcFloat = from == DoubleType
    val tgtFloat = to == DoubleType
    if srcFloat && !tgtFloat then emit("  f2i")
    else if !srcFloat && tgtFloat then emit("  i2f")
    else to match
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
    var offset = 0L
    for i <- 0 until fieldIndex do
      val fType = st.fields(i)._2
      val align = fType.alignOf.max(1)
      offset = ((offset + align - 1) / align) * align
      offset += fType.sizeOf
    val targetType = st.fields(fieldIndex)._2
    val align = targetType.alignOf.max(1)
    ((offset + align - 1) / align) * align

  private def genStructAddr(obj: TExpr): Unit =
    obj.typ match
      case _: SyslType.PtrType | _: SyslType.RefType =>
        genExpr(obj) // already a pointer
      case _ =>
        genExpr(obj) // for stack-allocated structs, this would need address
