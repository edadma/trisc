package io.github.edadma.trisc

import scala.collection.mutable

sealed trait Pointer:
  def deref: Cell
  def index(i: Int): Cell
  def add(n: Int): Pointer
  def sub(n: Int): Pointer

case class CellPtr(cell: Cell) extends Pointer:
  def deref: Cell = cell
  def index(i: Int): Cell =
    if i == 0 then cell
    else throw RuntimeException(s"cannot index a cell pointer with offset $i")
  def add(n: Int): Pointer =
    throw RuntimeException("cannot do arithmetic on a cell pointer")
  def sub(n: Int): Pointer =
    throw RuntimeException("cannot do arithmetic on a cell pointer")

case class ArrayPtr(cells: Array[Cell], offset: Int) extends Pointer:
  def deref: Cell = cells(offset)
  def index(i: Int): Cell =
    val idx = offset + i
    if idx < 0 || idx >= cells.length then throw RuntimeException(s"array index out of bounds: $idx")
    cells(idx)
  def add(n: Int): Pointer = ArrayPtr(cells, offset + n)
  def sub(n: Int): Pointer = ArrayPtr(cells, offset - n)

enum Value:
  case IntVal(n: Long)
  case FloatVal(d: Double)
  case PtrVal(ptr: Pointer)
  case ArrVal(cells: Array[Cell], offset: Int)
  case FuncVal(name: String)
  case SliceVal(cells: Array[Cell], offset: Int, length: Int, capacity: Int)
  case RefVal(cells: Array[Cell], refCount: java.util.concurrent.atomic.AtomicInteger, typeName: String = "")
  case RefSliceVal(cells: Array[Cell], length: Int, refCount: java.util.concurrent.atomic.AtomicInteger)
  case EnumVal(tag: Int, fields: Array[Cell])
  case RefEnumVal(tag: Int, fields: Array[Cell], refCount: java.util.concurrent.atomic.AtomicInteger)
  // Sysl strings: immutable UTF-8 byte sequence. JVM GC handles lifetime — no refcount.
  // Iterate bytes when needed (matches native semantics where strings are byte arrays).
  case StringVal(bytes: Array[Byte])
  case ClosureVal(body: TFunBody, params: List[TParam], captured: scala.collection.mutable.LinkedHashMap[String, Cell])
  case InterfaceVal(methodMap: Map[String, String], dataVal: Value, concreteType: SyslType)

class Cell(var value: Value)

class SyslInterpreter(output: String => Unit = s => print(s))
    extends SyslInterpreterStatements
    with SyslInterpreterExpressions:
  import Value.*

  case class ReturnException(value: Value) extends RuntimeException
  case class BreakException(label: Option[String]) extends RuntimeException
  case class ContinueException(label: Option[String]) extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  /** True if a break/continue exception is "for me" — label is None (nearest loop)
   *  or matches this loop's own label. */
  protected def claimsLoop(exLabel: Option[String], myLabel: Option[String]): Boolean =
    exLabel.isEmpty || exLabel == myLabel

  protected type Env = mutable.LinkedHashMap[String, Cell]
  protected val deferStack = new mutable.ArrayBuffer[(TStmt, Env)]

  protected def matchPattern(pat: TMatchPattern, value: Value, env: Env): Boolean =
    pat match
      case TWildcard => true
      case TBindPattern(_, _) => true
      case TValuePattern(expr) =>
        val pv = evalAny(expr, env)
        // String comparison must be structural (byte-for-byte), not pointer-based.
        (pv, value) match
          case (StringVal(lb), StringVal(rb)) => java.util.Arrays.equals(lb, rb)
          case _ => toLong(pv) == toLong(value)
      case TRangePattern(low, high) =>
        val v = toLong(value)
        v >= toLong(evalAny(low, env)) && v <= toLong(evalAny(high, env))
      case TDestructurePattern(_, _, _, nested) =>
        // Struct destructure always matches at the discriminator level (no
        // value check, just binding). Nested sub-patterns must also match.
        if nested.isEmpty then true
        else
          val (cells, off) = value match
            case ArrVal(c, o) => (c, o)
            case RefVal(c, _, _) => (c, 0)
            case _ => return false
          nested.zipWithIndex.forall {
            case (Some(sub), i) => matchPattern(sub, cells(off + i).value, env)
            case (None, _) => true
          }
      case TVariantPattern(_, variantIndex, _, _, nested) =>
        val tagOk = value match
          case EnumVal(tag, _) => tag == variantIndex
          case RefEnumVal(tag, _, _) => tag == variantIndex
          case _ => false
        if !tagOk then false
        else if nested.isEmpty then true
        else
          val fields = value match
            case EnumVal(_, f) => f
            case RefEnumVal(_, f, _) => f
            case _ => return false
          nested.zipWithIndex.forall {
            case (Some(sub), i) => matchPattern(sub, fields(i).value, env)
            case (None, _) => true
          }

  protected def bindPattern(pat: TMatchPattern, value: Value, env: Env): Unit =
    pat match
      case TBindPattern(name, _) =>
        env(name) = new Cell(value)
      case TDestructurePattern(_, bindings, _, nested) =>
        val (cells, off) = value match
          case ArrVal(c, o) => (c, o)
          case RefVal(c, _, _) => (c, 0)
          case _ => return
        for (binding, i) <- bindings.zipWithIndex do
          binding.foreach { name =>
            env(name) = new Cell(cells(off + i).value)
          }
        // Recursively bind nested patterns AFTER binding the synthetic outer
        // names. The synthetic names cover the field values; the nested pattern
        // descends one level deeper to bind its own variant-field names.
        if nested.nonEmpty then
          for ((sub, i) <- nested.zipWithIndex; p <- sub) do
            bindPattern(p, cells(off + i).value, env)
      case TVariantPattern(_, _, bindings, _, nested) =>
        val fields = value match
          case EnumVal(_, f) => f
          case RefEnumVal(_, f, _) => f
          case other => throw RuntimeError(s"cannot bind variant pattern on $other")
        for (binding, i) <- bindings.zipWithIndex do
          binding.foreach { name =>
            env(name) = new Cell(fields(i).value)
          }
        if nested.nonEmpty then
          for ((sub, i) <- nested.zipWithIndex; p <- sub) do
            bindPattern(p, fields(i).value, env)
      case _ => // nothing to bind

  protected def toLong(v: Value): Long = v match
    case IntVal(n)    => n
    case FloatVal(d)  => d.toLong
    case PtrVal(ptr)  => pointerToLong(ptr)
    case ArrVal(cells, off) => pointerToLong(ArrayPtr(cells, off))
    case RefVal(cells, _, _) => pointerToLong(ArrayPtr(cells, 0))
    case RefSliceVal(cells, _, _) => pointerToLong(ArrayPtr(cells, 0))
    case StringVal(bytes) => pointerToLong(ArrayPtr(bytes.map(b => new Cell(IntVal(b & 0xff))), 0))
    case FuncVal(_)         => 1L // non-zero sentinel for casts (address not meaningful in interpreter)
    case ClosureVal(_, _, _) => 1L // non-zero sentinel
    case InterfaceVal(_, _, _) => throw RuntimeError("expected integer, got interface")
    case SliceVal(_, _, _, _) => throw RuntimeError("expected integer, got slice")
    case EnumVal(_, _) => throw RuntimeError("expected integer, got enum value")
    case RefEnumVal(_, _, _) => throw RuntimeError("expected integer, got ref enum value")

  /** Truncate a Long result to the width of a narrow integer type. */
  protected def truncateNarrow(raw: Long, typ: SyslType): Long = typ match
    case SyslType.UIntType(8)  => raw & 0xFFL
    case SyslType.UIntType(16) => raw & 0xFFFFL
    case SyslType.UIntType(32) => raw & 0xFFFFFFFFL
    case SyslType.IntType(8)   => (raw << 56) >> 56
    case SyslType.IntType(16)  => (raw << 48) >> 48
    case SyslType.IntType(32)  => (raw << 32) >> 32
    case _ => raw

  protected def toDouble(v: Value): Double = v match
    case FloatVal(d)  => d
    case IntVal(n)    => n.toDouble
    case _            => throw RuntimeError("expected numeric value")

  protected val globals: Env = new mutable.LinkedHashMap
  protected val functions = new mutable.LinkedHashMap[String, TFunDecl]
  // Map struct name → deinit function name (handles module-mangled deinit names)
  protected val deinitMap = new mutable.HashMap[String, String]

  // Address table for pointer ↔ integer round-tripping
  protected val ptrToAddr = new mutable.HashMap[Pointer, Long]
  protected val addrToPtr = new mutable.HashMap[Long, Pointer]
  protected var nextAddr = 0x10000L // start at a non-zero base

  protected def pointerToLong(ptr: Pointer): Long =
    ptr match
      case ArrayPtr(cells, _) if cells.isEmpty => 0L // null pointer
      case _ =>
        ptrToAddr.getOrElseUpdate(ptr, {
          val addr = ptr match
            case ArrayPtr(cells, offset) =>
              // Use stable identity: base array identity + offset
              val base = ptrToAddr.getOrElse(ArrayPtr(cells, 0), {
                val a = nextAddr
                nextAddr += cells.length.max(1)
                ptrToAddr(ArrayPtr(cells, 0)) = a
                addrToPtr(a) = ArrayPtr(cells, 0)
                a
              })
              base + offset
            case _ =>
              val a = nextAddr
              nextAddr += 1
              a
          addrToPtr(addr) = ptr
          addr
        })

  protected def longToPointer(addr: Long): Pointer =
    addrToPtr.get(addr) match
      case Some(ptr) => ptr
      case None =>
        // Try to find a base allocation containing this address
        addrToPtr.collectFirst {
          case (base, ArrayPtr(cells, 0)) if addr >= base && addr < base + cells.length =>
            ArrayPtr(cells, (addr - base).toInt)
        }.getOrElse(throw RuntimeError(s"invalid pointer address: 0x${addr.toHexString}"))

  // Format a double consistently across platforms: no trailing .0 for whole numbers
  protected def formatDouble(d: Double): String =
    if d.isWhole && !d.isInfinite && !d.isNaN then
      val l = d.toLong
      l.toString
    else d.toString

  // Heap for sbrk: 1MB of cells, bump pointer
  protected val heapCells = Array.fill(1024 * 1024)(new Cell(IntVal(0)))
  protected var heapBreak = 0

  // Virtual MMIO memory for `#address(N)` vars. Real hardware addresses aren't accessible
  // from the JVM interpreter, so we simulate with a map. Reads of an untouched address
  // yield 0 (hardware-like default); writes persist for the duration of the run.
  protected val mmioMemory = new mutable.LongMap[Long]

  protected val builtins: mutable.Map[String, List[Value] => Value] = mutable.Map(
    "putchar" -> (args => { output(toLong(args.head).toChar.toString); args.head }),
    "print" -> (args => { args.foreach { case FloatVal(d) => output(formatDouble(d)); case a => output(toLong(a).toString) }; IntVal(0) }),
    "println" -> (args => { args.foreach { case FloatVal(d) => output(formatDouble(d)); case a => output(toLong(a).toString) }; output("\n"); IntVal(0) }),
    "puts" -> (args => { args.head match { case StringVal(bytes) => output(new String(bytes, "UTF-8")); case _ => throw RuntimeError("puts: expected string") }; IntVal(0) }),
    "puti" -> (args => { output(toLong(args.head).toString); IntVal(0) }),
    "malloc" -> (args => {
      val size = toLong(args.head).toInt
      if size <= 0 then PtrVal(ArrayPtr(Array.empty[Cell], 0))
      else
        val cells = Array.fill(size)(new Cell(IntVal(0)))
        PtrVal(ArrayPtr(cells, 0))
    }),
    "free" -> (_ => IntVal(0)), // no-op, JVM GC handles it
    "calloc" -> (args => {
      val count = toLong(args.head).toInt
      val size = toLong(args(1)).toInt
      val total = count * size
      if total <= 0 then PtrVal(ArrayPtr(Array.empty[Cell], 0))
      else
        val cells = Array.fill(total)(new Cell(IntVal(0)))
        PtrVal(ArrayPtr(cells, 0))
    }),
    "realloc" -> (args => {
      val newSize = toLong(args(1)).toInt
      if newSize <= 0 then { IntVal(0) }
      else
        val newCells = Array.fill(newSize)(new Cell(IntVal(0)))
        args.head match
          case PtrVal(ArrayPtr(oldCells, off)) =>
            val copyLen = math.min(oldCells.length - off, newSize)
            for i <- 0 until copyLen do newCells(i).value = oldCells(off + i).value
          case _ => // null or non-pointer — just return fresh allocation
        PtrVal(ArrayPtr(newCells, 0))
    }),
    "sbrk" -> (args => {
      val increment = toLong(args.head).toInt
      if increment == 0 then PtrVal(ArrayPtr(heapCells, heapBreak))
      else
        val oldBreak = heapBreak
        val newBreak = oldBreak + increment
        if newBreak < 0 || newBreak > heapCells.length then PtrVal(ArrayPtr(Array(new Cell(IntVal(-1))), 0))
        else
          heapBreak = newBreak
          PtrVal(ArrayPtr(heapCells, oldBreak))
    }),
    "abort" -> (_ => throw RuntimeError("abort")),
    "panic" -> (args => {
      val msg = args.headOption match
        case Some(StringVal(bytes)) => new String(bytes, "UTF-8")
        case _ => "panic"
      throw RuntimeError(msg)
    }),
    "assert" -> (args => {
      if toLong(args.head) == 0 then
        val msg = args.lift(1) match
          case Some(StringVal(bytes)) => new String(bytes, "UTF-8")
          case _ => "assertion failed"
        throw RuntimeError(msg)
      IntVal(0)
    }),
    "expect" -> (args => {
      val actual = toLong(args.head)
      val expected = toLong(args(1))
      if actual != expected then
        val label = args.lift(2) match
          case Some(StringVal(bytes)) => new String(bytes, "UTF-8")
          case _ => "expect"
        throw RuntimeError(s"$label: expected $expected, got $actual")
      IntVal(0)
    }),
  )

  def registerBuiltins(extra: Map[String, List[Value] => Value]): Unit =
    builtins ++= extra

  def registerGlobal(name: String, value: Value): Unit =
    globals(name) = new Cell(value)

  def run(program: TProgram): Long =
    for decl <- program.decls do
      decl match
        case _: TModuleDecl => // metadata only
        case _: TImportDecl => // not handled in interpreter
        case _: TExternFuncDecl => // not handled in interpreter
        case _: TExternVarDecl => // not handled in interpreter
        case _: TStructDecl => // type only, no runtime effect
        case _: TEnumDecl => // type only, no runtime effect
        case _: TDataEnumDecl => // type only, no runtime effect
        case _: TTypeAliasDecl => // type only, no runtime effect
        case _: TConstDecl => // const is fully folded at analyzer level
        case _: TInterfaceDecl => // type only, no runtime effect
        case f: TFunDecl =>
          functions(f.name) = f
          if f.name.endsWith("_deinit") then
            val structName = f.name.indexOf("__") match
              case -1 => f.name.dropRight(7)
              case i  => f.name.substring(i + 2).dropRight(7)
            deinitMap(structName) = f.name
        case TVarDecl(name, _, init, _, _, _, _) =>
          globals(name) = new Cell(evalAny(init, new mutable.LinkedHashMap))

    functions.get("main") match
      case Some(main) => toLong(call(main, Nil))
      case None => throw RuntimeError("no main function")

  /** Register all declarations without calling main. Used by the test runner. */
  def load(program: TProgram): Unit =
    for decl <- program.decls do
      decl match
        case _: TModuleDecl => // metadata only
        case _: TImportDecl => // not handled in interpreter
        case _: TExternFuncDecl => // not handled in interpreter
        case _: TExternVarDecl => // not handled in interpreter
        case _: TStructDecl => // type only
        case _: TEnumDecl => // type only
        case _: TDataEnumDecl => // type only
        case _: TTypeAliasDecl => // type only
        case _: TConstDecl => // const is fully folded at analyzer level
        case _: TInterfaceDecl => // type only
        case f: TFunDecl =>
          functions(f.name) = f
          if f.name.endsWith("_deinit") then
            val structName = f.name.indexOf("__") match
              case -1 => f.name.dropRight(7)
              case i  => f.name.substring(i + 2).dropRight(7)
            deinitMap(structName) = f.name
        case TVarDecl(name, _, init, _, _, _, _) =>
          globals(name) = new Cell(evalAny(init, new mutable.LinkedHashMap))

  /** Invoke a zero-arg function by name. Throws RuntimeError on panic. */
  def runNamed(name: String): Long =
    functions.get(name) match
      case Some(fn) => toLong(call(fn, Nil))
      case None => throw RuntimeError(s"no function named '$name'")

  protected def runDefers(savedDefers: mutable.ArrayBuffer[(TStmt, Env)]): Unit =
    for (stmt, env) <- savedDefers.reverseIterator do
      exec(stmt, env)

  protected def releaseRefs(env: Env): Unit =
    for (_, cell) <- env do refDecr(cell.value)

  /** Invoke a closure value with the given arguments. Public so JVM hosts can grab a
   *  ClosureVal out of an interpreted program (via globals or a returned function value)
   *  and call back into it without re-entering the AST eval path. */
  def invokeClosure(c: ClosureVal, args: List[Value]): Value =
    val closureEnv: Env = new mutable.LinkedHashMap
    for (name, cell) <- c.captured do
      closureEnv(name) = new Cell(cell.value)
    for (param, arg) <- c.params.zip(args) do
      closureEnv(param.name) = new Cell(arg)
    c.body match
      case TExprBody(expr) => evalAny(expr, closureEnv)
      case TBlockBody(stmts) =>
        try evalBlock(stmts, closureEnv)
        catch case ReturnException(v) => v

  /** Deep-copy a value when its declared type is a value struct, so that
    * `var b = a` and `f(p: Point)` (pass-by-value) produce truly independent
    * copies — writes through one binding never affect another, per the
    * language reference's "Three Allocation Modes" / "Backend Implementation
    * Latitude" section. Recurses through nested struct fields. Scalars,
    * refs, slices, and strings are returned as-is — those carry their own
    * sharing semantics (refcount, immutable, etc.). */
  protected def deepCopyValue(typ: SyslType, value: Value): Value = typ match
    case SyslType.NamedType(_, base, _, _, _) => deepCopyValue(base, value)
    case st: SyslType.StructType =>
      value match
        case ArrVal(cells, off) =>
          val newCells = st.fields.zipWithIndex.map { case ((_, fieldType), i) =>
            new Cell(deepCopyValue(fieldType, cells(off + i).value))
          }.toArray
          ArrVal(newCells, 0)
        case other => other
    case _ => value

  protected def call(fun: TFunDecl, args: List[Value]): Value =
    val env: Env = new mutable.LinkedHashMap
    val savedSize = deferStack.size

    for (param, arg) <- fun.params.zip(args) do
      // Value-struct params are pass-by-value: the body sees a local copy.
      val bound = deepCopyValue(param.typ, arg)
      refIncr(bound)
      env(param.name) = new Cell(bound)

    try
      val result = fun.body match
        case TExprBody(expr) => evalAny(expr, env)
        case TBlockBody(stmts) =>
          try
            evalBlock(stmts, env)
          catch
            case ReturnException(v) => v
      val defers = deferStack.slice(savedSize, deferStack.size)
      runDefers(defers)
      deferStack.dropRightInPlace(deferStack.size - savedSize)
      // Increment result ref before releasing locals (prevents premature free)
      refIncr(result)
      releaseRefs(env)
      refDecr(result) // balance the extra increment — caller owns it now
      result
    catch
      case e: ReturnException => throw e // should not happen — caught above
      case e: Throwable =>
        val defers = deferStack.slice(savedSize, deferStack.size)
        runDefers(defers)
        deferStack.dropRightInPlace(deferStack.size - savedSize)
        releaseRefs(env)
        throw e

  protected def evalBlock(stmts: List[TStmt], env: Env): Value =
    if stmts.nonEmpty then
      execBlock(stmts.init, env)
      stmts.last match
        case TExprStmt(expr) => evalAny(expr, env)
        case other => exec(other, env); IntVal(0)
    else IntVal(0)

  protected def execBlock(stmts: List[TStmt], env: Env): Unit =
    for stmt <- stmts do exec(stmt, env)

  protected def lookupCell(name: String, env: Env): Cell =
    env.getOrElse(name, globals.getOrElse(name, throw RuntimeError(s"undefined variable: $name")))

  protected val IMMORTAL_RC = -1

  protected def refIncr(v: Value): Unit = v match
    case RefVal(_, rc, _) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    case RefEnumVal(_, _, rc) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    case RefSliceVal(_, _, rc) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    // strings: JVM GC handles lifetime, no refcount
    case _ =>

  protected def refDecr(v: Value): Unit = v match
    case RefVal(cells, rc, typeName) =>
      if rc.get() == IMMORTAL_RC then return
      val count = rc.decrementAndGet()
      if count == 0 then
        rc.set(IMMORTAL_RC) // prevent re-entrant deinit from releaseRefs
        if typeName.nonEmpty then
          val deinitName = deinitMap.getOrElse(typeName, s"${typeName}_deinit")
          functions.get(deinitName).foreach { fun =>
            call(fun, List(v))
          }
    case RefEnumVal(_, _, rc) =>
      if rc.get() != IMMORTAL_RC then
        if rc.decrementAndGet() <= 0 then ()
    case RefSliceVal(_, _, rc) =>
      if rc.get() != IMMORTAL_RC then
        if rc.decrementAndGet() <= 0 then ()
    // strings: JVM GC handles lifetime, no refcount
    case _ =>

  protected def derefCell(v: Value): Cell = v match
    case PtrVal(ptr)        => ptr.deref
    case ArrVal(cells, off) => cells(off)
    case RefVal(cells, _, _) => cells(0)
    case _                  => throw RuntimeError("cannot dereference non-pointer")

  protected def indexCell(v: Value, idx: Int): Cell = v match
    case ArrVal(cells, off) =>
      val i = off + idx
      if i < 0 || i >= cells.length then throw RuntimeError(s"array index out of bounds: $i")
      cells(i)
    case PtrVal(ptr) => ptr.index(idx)
    case RefVal(cells, _, _) =>
      if idx < 0 || idx >= cells.length then throw RuntimeError(s"ref field index out of bounds: $idx")
      cells(idx)
    case RefSliceVal(cells, length, _) =>
      if idx < 0 || idx >= length then throw RuntimeError(s"array index out of bounds: $idx (length $length)")
      cells(idx)
    case SliceVal(cells, off, len, _) =>
      if idx < 0 || idx >= len then throw RuntimeError(s"slice index out of bounds: $idx (length $len)")
      cells(off + idx)
    case _ => throw RuntimeError("cannot index non-array")

  /** Default value for `new [n]T` elements and struct field zero-init in the interpreter. */
  protected def zeroValueForType(typ: SyslType, env: Env): Value = typ match
    case st: SyslType.StructType =>
      val cells = st.fields.map((_, ft) => new Cell(zeroValueForType(ft, env))).toArray
      ArrVal(cells, 0)
    case SyslType.ArrayType(elem, size) =>
      val cells = Array.fill(size)(new Cell(zeroValueForType(elem, env)))
      ArrVal(cells, 0)
    case SyslType.PtrType(_) =>
      PtrVal(ArrayPtr(Array.empty[Cell], 0))
    case SyslType.SliceType(_) =>
      SliceVal(Array.empty[Cell], 0, 0, 0)
    case SyslType.BoolType => IntVal(0)
    case SyslType.StringType => StringVal(Array.empty)
    case _: SyslType.IntType | _: SyslType.UIntType => IntVal(0)
    case _: SyslType.FloatType => FloatVal(0.0)
    case SyslType.UnitType | (_: SyslType.FuncType) | SyslType.InterfaceType(_, _) => IntVal(0)
    case SyslType.EnumType(_, _) => EnumVal(0, Array.empty)
    case SyslType.RefType(inner) =>
      // Uninitialized ref cell — represented as null-ish placeholder
      zeroValueForType(SyslType.PtrType(inner), env)
    case SyslType.NamedType(_, base, _, _, _) => zeroValueForType(base, env)

