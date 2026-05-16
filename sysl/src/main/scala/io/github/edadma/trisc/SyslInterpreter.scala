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

class SyslInterpreter(output: String => Unit = s => print(s)):
  import Value.*

  case class ReturnException(value: Value) extends RuntimeException
  case class BreakException(label: Option[String]) extends RuntimeException
  case class ContinueException(label: Option[String]) extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  /** True if a break/continue exception is "for me" — label is None (nearest loop)
   *  or matches this loop's own label. */
  private def claimsLoop(exLabel: Option[String], myLabel: Option[String]): Boolean =
    exLabel.isEmpty || exLabel == myLabel

  private type Env = mutable.LinkedHashMap[String, Cell]
  private val deferStack = new mutable.ArrayBuffer[(TStmt, Env)]

  private def matchPattern(pat: TMatchPattern, value: Value, env: Env): Boolean =
    pat match
      case TWildcard => true
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

  private def bindPattern(pat: TMatchPattern, value: Value, env: Env): Unit =
    pat match
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

  private def toLong(v: Value): Long = v match
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
  private def truncateNarrow(raw: Long, typ: SyslType): Long = typ match
    case SyslType.UIntType(8)  => raw & 0xFFL
    case SyslType.UIntType(16) => raw & 0xFFFFL
    case SyslType.UIntType(32) => raw & 0xFFFFFFFFL
    case SyslType.IntType(8)   => (raw << 56) >> 56
    case SyslType.IntType(16)  => (raw << 48) >> 48
    case SyslType.IntType(32)  => (raw << 32) >> 32
    case _ => raw

  private def toDouble(v: Value): Double = v match
    case FloatVal(d)  => d
    case IntVal(n)    => n.toDouble
    case _            => throw RuntimeError("expected numeric value")

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, TFunDecl]
  // Map struct name → deinit function name (handles module-mangled deinit names)
  private val deinitMap = new mutable.HashMap[String, String]

  // Address table for pointer ↔ integer round-tripping
  private val ptrToAddr = new mutable.HashMap[Pointer, Long]
  private val addrToPtr = new mutable.HashMap[Long, Pointer]
  private var nextAddr = 0x10000L // start at a non-zero base

  private def pointerToLong(ptr: Pointer): Long =
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

  private def longToPointer(addr: Long): Pointer =
    addrToPtr.get(addr) match
      case Some(ptr) => ptr
      case None =>
        // Try to find a base allocation containing this address
        addrToPtr.collectFirst {
          case (base, ArrayPtr(cells, 0)) if addr >= base && addr < base + cells.length =>
            ArrayPtr(cells, (addr - base).toInt)
        }.getOrElse(throw RuntimeError(s"invalid pointer address: 0x${addr.toHexString}"))

  // Format a double consistently across platforms: no trailing .0 for whole numbers
  private def formatDouble(d: Double): String =
    if d.isWhole && !d.isInfinite && !d.isNaN then
      val l = d.toLong
      l.toString
    else d.toString

  // Heap for sbrk: 1MB of cells, bump pointer
  private val heapCells = Array.fill(1024 * 1024)(new Cell(IntVal(0)))
  private var heapBreak = 0

  // Virtual MMIO memory for `#address(N)` vars. Real hardware addresses aren't accessible
  // from the JVM interpreter, so we simulate with a map. Reads of an untouched address
  // yield 0 (hardware-like default); writes persist for the duration of the run.
  private val mmioMemory = new mutable.LongMap[Long]

  private val builtins: mutable.Map[String, List[Value] => Value] = mutable.Map(
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

  private def runDefers(savedDefers: mutable.ArrayBuffer[(TStmt, Env)]): Unit =
    for (stmt, env) <- savedDefers.reverseIterator do
      exec(stmt, env)

  private def releaseRefs(env: Env): Unit =
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
  private def deepCopyValue(typ: SyslType, value: Value): Value = typ match
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

  private def call(fun: TFunDecl, args: List[Value]): Value =
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

  private def evalBlock(stmts: List[TStmt], env: Env): Value =
    if stmts.nonEmpty then
      execBlock(stmts.init, env)
      stmts.last match
        case TExprStmt(expr) => evalAny(expr, env)
        case other => exec(other, env); IntVal(0)
    else IntVal(0)

  private def execBlock(stmts: List[TStmt], env: Env): Unit =
    for stmt <- stmts do exec(stmt, env)

  private def lookupCell(name: String, env: Env): Cell =
    env.getOrElse(name, globals.getOrElse(name, throw RuntimeError(s"undefined variable: $name")))

  private val IMMORTAL_RC = -1

  private def refIncr(v: Value): Unit = v match
    case RefVal(_, rc, _) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    case RefEnumVal(_, _, rc) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    case RefSliceVal(_, _, rc) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    // strings: JVM GC handles lifetime, no refcount
    case _ =>

  private def refDecr(v: Value): Unit = v match
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

  private def derefCell(v: Value): Cell = v match
    case PtrVal(ptr)        => ptr.deref
    case ArrVal(cells, off) => cells(off)
    case RefVal(cells, _, _) => cells(0)
    case _                  => throw RuntimeError("cannot dereference non-pointer")

  private def indexCell(v: Value, idx: Int): Cell = v match
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
  private def zeroValueForType(typ: SyslType, env: Env): Value = typ match
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

  private def exec(stmt: TStmt, env: Env): Unit =
    stmt match
      case TVarStmt(name, _, init, _, _) =>
        val raw = evalAny(init, env)
        // Value-struct binding: deep-copy so `var b = a` doesn't alias.
        val v = deepCopyValue(init.typ, raw)
        // Increment refcount for copies only — TNew/TNewArray already set refcount=1
        init match
          case _: TNew | _: TNewArray => // owned, no incr
          case _ => refIncr(v)
        env(name) = new Cell(v)

      case TDestructureStmt(names, _, init) =>
        val (cells, off) = evalAny(init, env) match
          case ArrVal(c, o) => (c, o)
          case RefVal(c, _, _) => (c, 0)
          case other => throw RuntimeError(s"cannot destructure $other")
        for (name, i) <- names.zipWithIndex if name != "_" do
          env(name) = new Cell(cells(off + i).value)

      case TDestructureAssignStmt(names, _, init) =>
        // Parallel assignment: evaluate RHS fully, then assign all values
        val (cells, off) = evalAny(init, env) match
          case ArrVal(c, o) => (c, o)
          case RefVal(c, _, _) => (c, 0)
          case other => throw RuntimeError(s"cannot destructure $other")
        val values = names.indices.map(i => cells(off + i).value)
        for (name, v) <- names.zip(values) if name != "_" do
          lookupCell(name, env).value = v

      case TAssignStmt(target, value) =>
        val raw = evalAny(value, env)
        // Value-struct binding: deep-copy so reassignment from another struct
        // var doesn't alias.
        val v = deepCopyValue(value.typ, raw)
        // Increment refcount for copies only
        value match
          case _: TNew | _: TNewArray => // owned, no incr
          case _ => refIncr(v)
        if env.contains(target) then
          refDecr(env(target).value)
          env(target).value = v
        else if globals.contains(target) then
          refDecr(globals(target).value)
          globals(target).value = v
        else env(target) = new Cell(v)

      case TCompoundAssignStmt(target, op, value) =>
        val cell = lookupCell(target, env)
        val rv = evalAny(value, env)
        (cell.value, rv) match
          case (FloatVal(_), _) | (_, FloatVal(_)) =>
            val l = toDouble(cell.value)
            val r = toDouble(rv)
            cell.value = FloatVal(op match
              case "+"  => l + r
              case "-"  => l - r
              case "*"  => l * r
              case "/"  => l / r
              case "%"  => l % r
              case _    => throw RuntimeError(s"unsupported float compound operator: $op")
            )
          case (PtrVal(ptr), _) =>
            val n = toLong(rv).toInt
            cell.value = op match
              case "+" => PtrVal(ptr.add(n))
              case "-" => PtrVal(ptr.sub(n))
              case _ => throw RuntimeError(s"unsupported pointer compound operator: $op")
          case (ArrVal(cells, off), _) =>
            val n = toLong(rv).toInt
            cell.value = op match
              case "+" => ArrVal(cells, off + n)
              case "-" => ArrVal(cells, off - n)
              case _ => throw RuntimeError(s"unsupported pointer compound operator: $op")
          case _ =>
            val l = toLong(cell.value)
            val r = toLong(rv)
            val raw = op match
              case "+"  => l + r
              case "-"  => l - r
              case "*"  => l * r
              case "/"  => if r == 0 then throw RuntimeError("division by zero") else l / r
              case "%"  => if r == 0 then throw RuntimeError("modulo by zero") else l % r
              case "&"  => l & r
              case "|"  => l | r
              case "^"  => l ^ r
              case "<<" => l << r.toInt
              case ">>" => l >> r.toInt
            cell.value = IntVal(truncateNarrow(raw, value.typ))

      case TDerefAssignStmt(TCast(TIntLit(addr, _), SyslType.PtrType(_)), value) =>
        // #address MMIO write: store into the virtual mmio map keyed by the literal address.
        mmioMemory(addr) = toLong(evalAny(value, env))

      case TDerefAssignStmt(pointer, value) =>
        val cell = derefCell(evalAny(pointer, env))
        cell.value = evalAny(value, env)

      case TIndexAssignStmt(array, index, value) =>
        val arr = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        val cell = indexCell(arr, idx)
        cell.value = evalAny(value, env)

      case TFieldAssignStmt(obj, fieldIndex, value) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        cells(off + fieldIndex).value = evalAny(value, env)

      case TFieldCompoundAssignStmt(obj, fieldIndex, op, value) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val l = toLong(cell.value)
        val r = toLong(evalAny(value, env))
        val raw = op match
          case "+"  => l + r
          case "-"  => l - r
          case "*"  => l * r
          case "/"  => l / r
          case "%"  => l % r
          case "&"  => l & r
          case "|"  => l | r
          case "^"  => l ^ r
          case "<<" => l << r.toInt
          case ">>" => l >> r.toInt
        cell.value = IntVal(truncateNarrow(raw, value.typ))

      case TReturnStmt(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(IntVal(0)))

      case TDeferStmt(body) =>
        deferStack += ((body, env))

      case TForStmt(init, cond, update, body, myLabel) =>
        exec(init, env)
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
            exec(update, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) => exec(update, env)
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TWhileStmt(cond, body, myLabel) =>
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) =>
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TDoWhileStmt(cond, body, myLabel) =>
        var running = true
        while running do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) =>
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)
          if running then running = toLong(evalAny(cond, env)) != 0

      case TLoopStmt(body, myLabel) =>
        var running = true
        while running do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case e: BreakException if claimsLoop(e.label, myLabel) => running = false
            case e: ContinueException if claimsLoop(e.label, myLabel) =>
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TBreakStmt(label) => throw BreakException(label)
      case TContinueStmt(label) => throw ContinueException(label)

      case TAsmStmt(_) => // no-op in interpreter

      case TMultiStmt(children) =>
        for s <- children do exec(s, env)

      case TContractCheck(kind, expr, message) =>
        val v = toLong(evalAny(expr, env))
        if v == 0 then
          val suffix = if message == kind then "" else s": $message"
          throw RuntimeError(s"$kind check failed$suffix")

      case TExprStmt(expr) =>
        evalAny(expr, env)

  private def evalAny(expr: TExpr, env: Env): Value =
    expr match
      case TIntLit(n, _) => IntVal(n)
      case TFloatLit(d, _) => FloatVal(d)
      case TBoolLit(b, _) => IntVal(if b then 1L else 0L)
      case TUnitLit(_)    => IntVal(0L)  // 0-byte type — represented as 0 at runtime

      case TStringLit(s, _) => StringVal(s.getBytes("ISO-8859-1"))

      case TArrayDecl(size, typ) =>
        def initElem(t: SyslType): Value = t match
          case SyslType.ArrayType(elem, sz) =>
            val cells = Array.fill(sz)(new Cell(initElem(elem)))
            ArrVal(cells, 0)
          case st: SyslType.StructType => evalAny(TStructLit(st), env)
          case _ => IntVal(0)
        val elemType = typ match
          case SyslType.ArrayType(e, _) => e
          case _ => SyslType.I64
        val cells = Array.fill(size)(new Cell(initElem(elemType)))
        ArrVal(cells, 0)

      case TArrayLit(elements, _) =>
        val cells = elements.map(e => new Cell(evalAny(e, env))).toArray
        ArrVal(cells, 0)

      case TVarRef(name, _) => lookupCell(name, env).value

      case TAddrOf(name, _) => PtrVal(CellPtr(lookupCell(name, env)))

      case TTempAddr(expr, _) =>
        // Evaluate expression, store in a temporary cell, return pointer to it
        val value = evalAny(expr, env)
        val cell = new Cell(value)
        PtrVal(CellPtr(cell))

      case TAddrOfField(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        PtrVal(ArrayPtr(cells, off + fieldIndex))

      case TAddrOfIndex(array, index, _) =>
        val arrVal = evalAny(array, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case ArrVal(cells, off) => PtrVal(ArrayPtr(cells, off + idx))
          case PtrVal(ptr) => PtrVal(ptr.add(idx))
          case _ => PtrVal(CellPtr(indexCell(arrVal, idx)))

      case TPreInc(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case PtrVal(ptr) =>
            val nv = PtrVal(ptr.add(1))
            cell.value = nv
            nv
          case ArrVal(cells, off) =>
            val nv = ArrVal(cells, off + 1)
            cell.value = nv
            nv
          case _ =>
            val v = truncateNarrow(toLong(cell.value) + 1, typ)
            cell.value = IntVal(v)
            IntVal(v)

      case TPreDec(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case PtrVal(ptr) =>
            val nv = PtrVal(ptr.sub(1))
            cell.value = nv
            nv
          case ArrVal(cells, off) =>
            val nv = ArrVal(cells, off - 1)
            cell.value = nv
            nv
          case _ =>
            val v = truncateNarrow(toLong(cell.value) - 1, typ)
            cell.value = IntVal(v)
            IntVal(v)

      case TPostInc(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case old @ PtrVal(ptr) =>
            cell.value = PtrVal(ptr.add(1))
            old
          case old @ ArrVal(cells, off) =>
            cell.value = ArrVal(cells, off + 1)
            old
          case _ =>
            val old = toLong(cell.value)
            cell.value = IntVal(truncateNarrow(old + 1, typ))
            IntVal(old)

      case TPostDec(name, typ) =>
        val cell = lookupCell(name, env)
        cell.value match
          case old @ PtrVal(ptr) =>
            cell.value = PtrVal(ptr.sub(1))
            old
          case old @ ArrVal(cells, off) =>
            cell.value = ArrVal(cells, off - 1)
            old
          case _ =>
            val old = toLong(cell.value)
            cell.value = IntVal(truncateNarrow(old - 1, typ))
            IntVal(old)

      case TDeref(TCast(TIntLit(addr, _), SyslType.PtrType(_)), typ) =>
        // #address MMIO read: pull from the virtual mmio map (0 if never written).
        val raw = mmioMemory.getOrElse(addr, 0L)
        IntVal(truncateNarrow(raw, typ))

      case TDeref(inner, _) =>
        evalAny(inner, env) match
          case RefVal(cells, _, _) => ArrVal(cells, 0)  // deref &Struct → expose struct fields
          case RefEnumVal(tag, fields, _) => EnumVal(tag, fields) // deref &Enum → value enum
          case other => derefCell(other).value

      case TIndex(arr, index, _) =>
        val arrVal = evalAny(arr, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case StringVal(bytes) =>
            if idx < 0 || idx >= bytes.length then throw RuntimeError(s"string index out of bounds: $idx (length ${bytes.length})")
            IntVal(bytes(idx) & 0xff)
          case SliceVal(cells, off, len, _) =>
            if idx < 0 || idx >= len then throw RuntimeError(s"slice index out of bounds: $idx (length $len)")
            cells(off + idx).value
          case _ =>
            indexCell(arrVal, idx).value

      case TSliceExpr(arr, low, high, _) =>
        val arrVal = evalAny(arr, env)
        arrVal match
          case StringVal(bytes) =>
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(bytes.length)
            if lo < 0 || hi < lo || hi > bytes.length then
              throw RuntimeError(s"string slice bounds out of range [$lo:$hi] with length ${bytes.length}")
            StringVal(bytes.slice(lo, hi))
          case SliceVal(cells, off, len, cap) =>
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(len)
            if lo < 0 || hi < lo || hi > len then
              throw RuntimeError(s"slice bounds out of range [$lo:$hi] with length $len")
            SliceVal(cells, off + lo, hi - lo, cap - lo)
          case RefSliceVal(cells, length, _) =>
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(length)
            if lo < 0 || hi < lo || hi > length then
              throw RuntimeError(s"slice bounds out of range [$lo:$hi] with length $length")
            SliceVal(cells, lo, hi - lo, length - lo)
          case ArrVal(cells, off) =>
            val totalLen = cells.length - off
            val lo = low.map(l => toLong(evalAny(l, env)).toInt).getOrElse(0)
            val hi = high.map(h => toLong(evalAny(h, env)).toInt).getOrElse(totalLen)
            if lo < 0 || hi < lo || hi > totalLen then
              throw RuntimeError(s"slice bounds out of range [$lo:$hi] with length $totalLen")
            SliceVal(cells, off + lo, hi - lo, totalLen - lo)
          case _ => throw RuntimeError("cannot sub-slice non-slice value")

      case TAppend(sliceExpr, elemExpr, _) =>
        val sliceVal: SliceVal = evalAny(sliceExpr, env) match
          case s: SliceVal => s
          case RefSliceVal(cells, length, _) => SliceVal(cells, 0, length, length)
          case _ => throw RuntimeError("append requires a slice")
        val newElem = evalAny(elemExpr, env)
        if sliceVal.length < sliceVal.capacity then
          sliceVal.cells(sliceVal.offset + sliceVal.length).value = newElem
          SliceVal(sliceVal.cells, sliceVal.offset, sliceVal.length + 1, sliceVal.capacity)
        else
          val newCap = if sliceVal.capacity == 0 then 1 else sliceVal.capacity * 2
          val newCells = Array.fill(newCap)(new Cell(IntVal(0)))
          for i <- 0 until sliceVal.length do
            newCells(i).value = sliceVal.cells(sliceVal.offset + i).value
          newCells(sliceVal.length).value = newElem
          SliceVal(newCells, 0, sliceVal.length + 1, newCap)

      case TStr(inner) =>
        val v = evalAny(inner, env)
        val s = inner.typ.underlying match
          case SyslType.BoolType =>
            // bool → "true" / "false" — canonical across all seven backends.
            // The numeric "1"/"0" form predates a deliberate choice; aligning
            // here also fixes the prior interpreter divergence flagged in
            // feedback_sysl_str_bool_divergence.md.
            v match
              case IntVal(0) => "false"
              case IntVal(_) => "true"
              case _ => throw RuntimeError(s"str(bool): unexpected non-int value $v")
          case _ =>
            v match
              case IntVal(n) => n.toString
              case FloatVal(d) => formatDouble(d)
              case _ => throw RuntimeError(s"str(): unsupported value $v")
        StringVal(s.getBytes("UTF-8"))

      case TFmtStr(inner, spec) =>
        val v = evalAny(inner, env)
        val raw = v match
          case IntVal(n) =>
            val base = spec.verb match
              case 'x' => 16
              case 'o' => 8
              case 'b' => 2
              case _   => 10
            val s = if base == 10 then
              val r = n.toString
              if spec.showSign && n >= 0 then "+" + r else r
            else
              val unsigned = if n < 0 then
                "-" + java.lang.Long.toUnsignedString(-n, base)
              else
                java.lang.Long.toUnsignedString(n, base)
              if spec.upperCase then unsigned.toUpperCase else unsigned
            s
          case FloatVal(d) => d.toString
          case StringVal(b) => new String(b, "UTF-8")
          case _ => throw RuntimeError(s"fmt: unsupported value $v")
        // Apply width padding
        val padded = if spec.width > 0 && raw.length < spec.width then
          val pad = spec.width - raw.length
          if spec.leftAlign then raw + " " * pad
          else if spec.zeroPad && (spec.verb != 's') then
            if raw.startsWith("-") then "-" + "0" * pad + raw.substring(1)
            else if raw.startsWith("+") then "+" + "0" * pad + raw.substring(1)
            else "0" * pad + raw
          else " " * pad + raw
        else raw
        StringVal(padded.getBytes("UTF-8"))

      case TStringFromPtr(ptrExpr, lenExpr, _) =>
        val ptr = evalAny(ptrExpr, env)
        val len = toLong(evalAny(lenExpr, env)).toInt
        val bytes = new Array[Byte](len)
        ptr match
          case PtrVal(p) =>
            for i <- 0 until len do
              bytes(i) = toLong(p.add(i).deref.value).toByte
          case ArrVal(cells, off) =>
            for i <- 0 until len do
              bytes(i) = toLong(cells(off + i).value).toByte
          case _ => throw RuntimeError(s"string(): expected pointer, got $ptr")
        StringVal(bytes)

      case TStringFromSlice(sliceExpr, _) =>
        val (cells, off, slen) = evalAny(sliceExpr, env) match
          case SliceVal(c, o, l, _) => (c, o, l)
          case RefSliceVal(c, l, _) => (c, 0, l)
          case other => throw RuntimeError(s"string() requires a slice, got $other")
        val bytes = new Array[Byte](slen)
        for i <- 0 until slen do
          bytes(i) = toLong(cells(off + i).value).toByte
        StringVal(bytes)

      case TIfExpr(cond, thenBody, elseBody, _) =>
        if toLong(evalAny(cond, env)) != 0 then
          evalBlock(thenBody, env)
        else
          elseBody match
            case Some(stmts) => evalBlock(stmts, env)
            case None => IntVal(0)

      case TQuantifier(kind, name, _, lo, hi, inclusive, pred, _) =>
        val loVal = toLong(evalAny(lo, env))
        val hiVal = toLong(evalAny(hi, env))
        val end = if inclusive then hiVal else hiVal - 1
        val saved = env.get(name)
        val cell = new Cell(IntVal(loVal))
        env(name) = cell
        var result: Long = if kind == "all" then 1L else 0L
        var i = loVal
        var done = false
        while i <= end && !done do
          cell.value = IntVal(i)
          val pv = toLong(evalAny(pred, env))
          if kind == "all" then
            if pv == 0L then { result = 0L; done = true }
          else
            if pv != 0L then { result = 1L; done = true }
          i += 1
        saved match
          case Some(c) => env(name) = c
          case None => env.remove(name)
        IntVal(result)

      case TMatchExpr(scrutinee, arms, default, _) =>
        val sv = evalAny(scrutinee, env)
        val matched = arms.find { arm =>
          val patternMatches = arm.patterns.exists(p => matchPattern(p, sv, env))
          if patternMatches then
            // Bind destructure patterns before checking guard
            arm.patterns.find(p => matchPattern(p, sv, env)).foreach(p => bindPattern(p, sv, env))
            arm.guard.forall(g => toLong(evalAny(g, env)) != 0)
          else false
        }
        matched match
          case Some(arm) => evalBlock(arm.body, env)
          case None =>
            default match
              case Some(stmts) => evalBlock(stmts, env)
              case None => IntVal(0)

      case TBinary(left, op, right, resultType) =>
        val lv = evalAny(left, env)
        (lv, op) match
          case (PtrVal(ptr), "+") =>
            return PtrVal(ptr.add(toLong(evalAny(right, env)).toInt))
          case (PtrVal(ptr), "-") =>
            return PtrVal(ptr.sub(toLong(evalAny(right, env)).toInt))
          case (ArrVal(cells, off), "+") =>
            return ArrVal(cells, off + toLong(evalAny(right, env)).toInt)
          case (ArrVal(cells, off), "-") =>
            return ArrVal(cells, off - toLong(evalAny(right, env)).toInt)
          case _ =>

        // Short-circuit logical operators
        op match
          case "&&" =>
            val l = toLong(lv)
            return IntVal(if l == 0 then 0L else if toLong(evalAny(right, env)) != 0 then 1L else 0L)
          case "||" =>
            val l = toLong(lv)
            return IntVal(if l != 0 then 1L else if toLong(evalAny(right, env)) != 0 then 1L else 0L)
          case _ =>

        val rv = evalAny(right, env)

        // String path: concatenation and comparison (lexicographic, byte-wise unsigned).
        (lv, rv) match
          case (StringVal(lb), StringVal(rb)) =>
            return (op match
              case "+" =>
                val newBytes = new Array[Byte](lb.length + rb.length)
                System.arraycopy(lb, 0, newBytes, 0, lb.length)
                System.arraycopy(rb, 0, newBytes, lb.length, rb.length)
                StringVal(newBytes)
              case "==" => IntVal(if java.util.Arrays.equals(lb, rb) then 1L else 0L)
              case "!=" => IntVal(if !java.util.Arrays.equals(lb, rb) then 1L else 0L)
              case "<" | "<=" | ">" | ">=" =>
                val n = math.min(lb.length, rb.length)
                var i = 0
                var diff = 0
                while i < n && diff == 0 do
                  diff = (lb(i) & 0xFF) - (rb(i) & 0xFF)
                  i += 1
                if diff == 0 then diff = lb.length - rb.length
                val ok = op match
                  case "<"  => diff < 0
                  case "<=" => diff <= 0
                  case ">"  => diff > 0
                  case ">=" => diff >= 0
                IntVal(if ok then 1L else 0L)
              case _ => throw RuntimeError(s"unsupported string operator: $op")
            )
          case _ =>

        // Float path: if either operand is float, use float arithmetic
        (lv, rv) match
          case (FloatVal(_), _) | (_, FloatVal(_)) =>
            val l = toDouble(lv)
            val r = toDouble(rv)
            return (op match
              case "+"  => FloatVal(l + r)
              case "-"  => FloatVal(l - r)
              case "*"  => FloatVal(l * r)
              case "/"  => FloatVal(l / r)
              case "%"  => FloatVal(l % r)
              case "==" => IntVal(if l == r then 1L else 0L)
              case "!=" => IntVal(if l != r then 1L else 0L)
              case "<"  => IntVal(if l < r then 1L else 0L)
              case ">"  => IntVal(if l > r then 1L else 0L)
              case "<=" => IntVal(if l <= r then 1L else 0L)
              case ">=" => IntVal(if l >= r then 1L else 0L)
              case _    => throw RuntimeError(s"unsupported float operator: $op")
            )
          case _ =>

        // Integer path
        val l = toLong(lv)
        val r = toLong(rv)
        val unsigned = left.typ.isUnsigned
        val raw = op match
          case "+"  => l + r
          case "-"  => l - r
          case "*"  => l * r
          case "/"  =>
            if r == 0 then throw RuntimeError("division by zero")
            else if unsigned then java.lang.Long.divideUnsigned(l, r)
            else l / r
          case "%"  =>
            if r == 0 then throw RuntimeError("modulo by zero")
            else if unsigned then java.lang.Long.remainderUnsigned(l, r)
            else l % r
          case "==" => if l == r then 1L else 0L
          case "!=" => if l != r then 1L else 0L
          case "<"  => if (if unsigned then java.lang.Long.compareUnsigned(l, r) < 0 else l < r) then 1L else 0L
          case ">"  => if (if unsigned then java.lang.Long.compareUnsigned(l, r) > 0 else l > r) then 1L else 0L
          case "<=" => if (if unsigned then java.lang.Long.compareUnsigned(l, r) <= 0 else l <= r) then 1L else 0L
          case ">=" => if (if unsigned then java.lang.Long.compareUnsigned(l, r) >= 0 else l >= r) then 1L else 0L
          case "&"  => l & r
          case "|"  => l | r
          case "^"  => l ^ r
          case "<<" => l << r.toInt
          case ">>" => if unsigned then l >>> r.toInt else l >> r.toInt
          case _    => throw RuntimeError(s"unknown operator: $op")
        IntVal(truncateNarrow(raw, resultType))

      case TUnary(op, operand, resultType) =>
        val v = evalAny(operand, env)
        v match
          case FloatVal(d) =>
            op match
              case "-" => FloatVal(-d)
              case _   => throw RuntimeError(s"unsupported float unary operator: $op")
          case _ =>
            val n = toLong(v)
            val raw = op match
              case "-" => -n
              case "!" => if n == 0 then 1L else 0L
              case "~" => ~n
              case _   => throw RuntimeError(s"unknown unary operator: $op")
            IntVal(truncateNarrow(raw, resultType))

      case TRangeCheck(inner, range, aliasName, _) =>
        val v = evalAny(inner, env)
        import SyslType.*
        range match
          case IntRange(lo, hi, excl) =>
            val n = toLong(v)
            val ok = if excl then n >= lo && n < hi else n >= lo && n <= hi
            if !ok then throw RuntimeError(s"range check failed: $aliasName (value $n out of range ${lo}..${if excl then "<" else ""}${hi})")
          case FloatRange(lo, hi, excl) =>
            val d = toDouble(v)
            val ok = if excl then d >= lo && d < hi else d >= lo && d <= hi
            if !ok then throw RuntimeError(s"range check failed: $aliasName (value $d out of range ${lo}..${if excl then "<" else ""}${hi})")
        v

      case TCast(inner, target) =>
        val v = evalAny(inner, env)
        import SyslType.*
        target.underlying match
          case FloatType(32) => FloatVal(toDouble(v).toFloat.toDouble)  // narrow to f32 precision
          case FloatType(64) => FloatVal(toDouble(v))
          case _: FloatType  => FloatVal(toDouble(v))
          case BoolType => v match
            case FuncVal(_) => IntVal(1L) // function references are always non-null
            case RefVal(_, _, _) | RefEnumVal(_, _, _) | RefSliceVal(_, _, _) | StringVal(_) => IntVal(1L)
            case _ => IntVal(if toLong(v) != 0 then 1L else 0L)
          case IntType(64)  => IntVal(toLong(v))
          case IntType(32)  => IntVal((toLong(v) << 32) >> 32)  // sign-extend from 32 bits
          case IntType(16)  => IntVal((toLong(v) << 48) >> 48)  // sign-extend from 16 bits
          case IntType(8)   => IntVal((toLong(v) << 56) >> 56)  // sign-extend from 8 bits
          case _: IntType   => IntVal(toLong(v))
          case UIntType(64) => IntVal(toLong(v))
          case UIntType(32) => IntVal(toLong(v) & 0xFFFFFFFFL)
          case UIntType(16) => IntVal(toLong(v) & 0xFFFFL)
          case UIntType(8)  => IntVal(toLong(v) & 0xFFL)
          case _: UIntType  => IntVal(toLong(v))
          case _: PtrType =>
            v match
              case PtrVal(_) | ArrVal(_, _) => v  // already a pointer
              case RefVal(cells, _, _) => PtrVal(ArrayPtr(cells, 0))  // ref to pointer
              case FuncVal(name) => IntVal(0) // func to pointer (address not meaningful in interpreter)
              case IntVal(0) => PtrVal(ArrayPtr(Array.empty[Cell], 0))  // null pointer
              case IntVal(n) => PtrVal(longToPointer(n))  // integer to pointer
              case _ => v
          case _ => v

      case TAsmExpr(_, _) => IntVal(0) // no-op in interpreter

      case TSizeof(size, _) => IntVal(size)

      case TLen(inner, _) =>
        evalAny(inner, env) match
          case StringVal(bytes) => IntVal(bytes.length.toLong)
          case SliceVal(_, _, len, _) => IntVal(len.toLong)
          case ArrVal(cells, _) => IntVal(cells.length.toLong)
          case RefSliceVal(_, length, _) => IntVal(length.toLong)
          case _ => throw RuntimeError("len: unsupported type")

      case TCap(inner, _) =>
        evalAny(inner, env) match
          case SliceVal(_, _, _, cap) => IntVal(cap.toLong)
          case ArrVal(cells, _) => IntVal(cells.length.toLong)
          case RefSliceVal(_, length, _) => IntVal(length.toLong)
          case _ => throw RuntimeError("cap: unsupported type")

      case TIntrinsicCall(name, args, typ) =>
        val a = toLong(evalAny(args(0), env))
        val b = toLong(evalAny(args(1), env))
        val width = typ.bitWidth
        val signed = typ.isSigned
        val (minV, maxV) =
          if signed then (-(1L << (width - 1)), (1L << (width - 1)) - 1)
          else (0L, if width == 64 then -1L else (1L << width) - 1)  // unsigned: -1L = max u64
        def mask(v: Long): Long = truncateNarrow(v, typ)
        def saturate(v: java.math.BigInteger): Long =
          val mn = java.math.BigInteger.valueOf(minV)
          val mx =
            if !signed && width == 64 then
              new java.math.BigInteger("FFFFFFFFFFFFFFFF", 16)
            else java.math.BigInteger.valueOf(maxV)
          if v.compareTo(mn) < 0 then minV
          else if v.compareTo(mx) > 0 then if !signed && width == 64 then -1L else maxV
          else v.longValue()
        val ba = java.math.BigInteger.valueOf(a)
        val bb = java.math.BigInteger.valueOf(b)
        name match
          case "wrapping_add" => IntVal(mask(a + b))
          case "wrapping_sub" => IntVal(mask(a - b))
          case "wrapping_mul" => IntVal(mask(a * b))
          case "saturating_add" => IntVal(saturate(ba.add(bb)))
          case "saturating_sub" => IntVal(saturate(ba.subtract(bb)))
          case "saturating_mul" => IntVal(saturate(ba.multiply(bb)))
          case other => throw RuntimeError(s"unknown intrinsic: $other")

      case TFieldPreInc(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val v = toLong(cell.value) + 1
        cell.value = IntVal(v)
        IntVal(v)

      case TFieldPreDec(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val v = toLong(cell.value) - 1
        cell.value = IntVal(v)
        IntVal(v)

      case TFieldPostInc(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val old = toLong(cell.value)
        cell.value = IntVal(old + 1)
        IntVal(old)

      case TFieldPostDec(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        val cell = cells(off + fieldIndex)
        val old = toLong(cell.value)
        cell.value = IntVal(old - 1)
        IntVal(old)

      case TStructLit(SyslType.StructType(_, fields, _)) =>
        val cells = fields.map((_, typ) => new Cell(zeroValueForType(typ, env))).toArray
        ArrVal(cells, 0)

      case TNew(SyslType.StructType(name, fields, _), args) =>
        val cells = fields.zip(args).map { case ((_, _), arg) =>
          new Cell(evalAny(arg, env))
        }.toArray
        RefVal(cells, new java.util.concurrent.atomic.AtomicInteger(1), name)

      case TNewEnum(_, variantIndex, args) =>
        val cells = args.map(arg => new Cell(evalAny(arg, env))).toArray
        RefEnumVal(variantIndex, cells, new java.util.concurrent.atomic.AtomicInteger(1))

      case TNewArray(elemType, sizeExpr) =>
        val n = toLong(evalAny(sizeExpr, env)).toInt
        val cells = Array.fill(n)(new Cell(zeroValueForType(elemType, env)))
        RefSliceVal(cells, n, new java.util.concurrent.atomic.AtomicInteger(1))

      case TStructConstruct(SyslType.StructType(_, fields, _), args) =>
        val cells = fields.zip(args).map { case ((_, typ), arg) =>
          val value = evalAny(arg, env)
          new Cell(value)
        }.toArray
        ArrVal(cells, 0)

      case TEnumConstruct(_, variantIndex, args) =>
        val cells = args.map(arg => new Cell(evalAny(arg, env))).toArray
        EnumVal(variantIndex, cells)

      case TFieldAccess(obj, fieldIndex, _) =>
        val struct = evalAny(obj, env) match
          case arr: ArrVal => arr
          case PtrVal(ptr) => ptr.deref.value.asInstanceOf[ArrVal]  // auto-deref pointer to struct
          case other => throw RuntimeError(s"cannot access field on $other")
        struct.cells(struct.offset + fieldIndex).value

      case TFuncRef(name, _) => FuncVal(name)

      case TClosure(params, _, body, captures, _, _, selfName) =>
        // Capture current values by value (copy). The self-name (if any) is bound to a
        // fresh cell after the ClosureVal is built so the body can recurse via name.
        val capturedEnv = new mutable.LinkedHashMap[String, Cell]
        for (varName, _) <- captures if !selfName.contains(varName) do
          val cell = lookupCell(varName, env)
          capturedEnv(varName) = new Cell(cell.value) // copy value, not share cell
        val closureVal = ClosureVal(body, params, capturedEnv)
        selfName.foreach { n => capturedEnv(n) = new Cell(closureVal) }
        closureVal

      case TInterfaceBox(expr, iface) =>
        val dataVal = evalAny(expr, env)
        // Build method map: interface method name → actual registered function name
        val structName = expr.typ match
          case SyslType.StructType(name, _, _) => name
          case SyslType.PtrType(SyslType.StructType(name, _, _)) => name
          case SyslType.RefType(SyslType.StructType(name, _, _)) => name
          case other => throw RuntimeError(s"cannot box $other into interface")
        val methodMap = iface.methods.map { (mname, _, _, _) =>
          val shortKey = s"${structName}_$mname"
          // Try short name first, then search for mangled variant
          val funcName = functions.get(shortKey) match
            case Some(f) => f.name
            case None =>
              functions.values.find(f => f.name.endsWith(s"__$shortKey"))
                .map(_.name)
                .getOrElse(shortKey) // fallback to short name
          (mname, funcName)
        }.toMap
        InterfaceVal(methodMap, dataVal, expr.typ)

      case TInterfaceDispatch(ifaceVal, methodIndex, args, _) =>
        val InterfaceVal(methodMap, dataVal, concreteType) = evalAny(ifaceVal, env): @unchecked
        val iface = ifaceVal.typ.asInstanceOf[SyslType.InterfaceType]
        val (methodName, _, _, _) = iface.methods(methodIndex)
        val funcName = methodMap(methodName)
        val argValues = args.map(evalAny(_, env))
        // Build self arg — for value types, wrap in a cell so the method can modify via pointer
        val selfArg = concreteType match
          case _: SyslType.StructType =>
            // Wrap data in a single-element array to create a pointer-like cell
            val cells = Array(new Cell(dataVal))
            PtrVal(ArrayPtr(cells, 0))
          case _ => dataVal // already a pointer or ref
        functions.get(funcName) match
          case Some(fun) => call(fun, selfArg :: argValues)
          case None => throw RuntimeError(s"interface dispatch: undefined method '$funcName'")

      case TCall(name, args, _) =>
        val argValues = args.map(evalAny(_, env))
        functions.get(name) match
          case Some(fun) => call(fun, argValues)
          case None =>
            builtins.get(name) match
              case Some(f) => f(argValues)
              case None => throw RuntimeError(s"undefined function: $name")

      case TIndirectCall(callee, args, _) =>
        val calleeVal = evalAny(callee, env)
        val argValues = args.map(evalAny(_, env))
        calleeVal match
          case FuncVal(name) =>
            functions.get(name) match
              case Some(fun) => call(fun, argValues)
              case None =>
                builtins.get(name) match
                  case Some(f) => f(argValues)
                  case None => throw RuntimeError(s"undefined function: $name")
          case c: ClosureVal => invokeClosure(c, argValues)
          case other => throw RuntimeError(s"cannot call ${other}")
