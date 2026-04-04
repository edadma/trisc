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
  case StrVal(s: String)
  case SliceVal(cells: Array[Cell], offset: Int, length: Int, capacity: Int)
  case RefVal(cells: Array[Cell], refCount: java.util.concurrent.atomic.AtomicInteger, typeName: String = "")
  case RefSliceVal(cells: Array[Cell], length: Int, refCount: java.util.concurrent.atomic.AtomicInteger)
  case RefStringVal(bytes: Array[Byte], length: Int, refCount: java.util.concurrent.atomic.AtomicInteger)

class Cell(var value: Value)

class SyslInterpreter(output: String => Unit = s => print(s)):
  import Value.*

  case class ReturnException(value: Value) extends RuntimeException
  case object BreakException extends RuntimeException
  case object ContinueException extends RuntimeException
  case class RuntimeError(msg: String) extends RuntimeException(msg)

  private type Env = mutable.LinkedHashMap[String, Cell]
  private val deferStack = new mutable.ArrayBuffer[(TStmt, Env)]

  private def toLong(v: Value): Long = v match
    case IntVal(n)    => n
    case FloatVal(d)  => d.toLong
    case PtrVal(ptr)  => pointerToLong(ptr)
    case ArrVal(cells, off) => pointerToLong(ArrayPtr(cells, off))
    case RefVal(cells, _, _) => pointerToLong(ArrayPtr(cells, 0))
    case RefSliceVal(cells, _, _) => pointerToLong(ArrayPtr(cells, 0))
    case RefStringVal(bytes, _, _) => pointerToLong(ArrayPtr(bytes.map(b => new Cell(IntVal(b & 0xff))), 0))
    case FuncVal(_)         => throw RuntimeError("expected integer, got function")
    case StrVal(_)          => throw RuntimeError("expected integer, got string")
    case SliceVal(_, _, _, _) => throw RuntimeError("expected integer, got slice")

  private def toDouble(v: Value): Double = v match
    case FloatVal(d)  => d
    case IntVal(n)    => n.toDouble
    case _            => throw RuntimeError("expected numeric value")

  private val globals: Env = new mutable.LinkedHashMap
  private val functions = new mutable.LinkedHashMap[String, TFunDecl]

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

  private val builtins: mutable.Map[String, List[Value] => Value] = mutable.Map(
    "putchar" -> (args => { output(toLong(args.head).toChar.toString); args.head }),
    "print" -> (args => { args.foreach { case FloatVal(d) => output(formatDouble(d)); case a => output(toLong(a).toString) }; IntVal(0) }),
    "println" -> (args => { args.foreach { case FloatVal(d) => output(formatDouble(d)); case a => output(toLong(a).toString) }; output("\n"); IntVal(0) }),
    "puts" -> (args => { args.head match { case RefStringVal(bytes, len, _) => output(new String(bytes, 0, len, "UTF-8")); case _ => throw RuntimeError("puts: expected string") }; IntVal(0) }),
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
        case _: TTypeAliasDecl => // type only, no runtime effect
        case f: TFunDecl => functions(f.name) = f
        case TVarDecl(name, _, init, _) =>
          globals(name) = new Cell(evalAny(init, new mutable.LinkedHashMap))

    functions.get("main") match
      case Some(main) => toLong(call(main, Nil))
      case None => throw RuntimeError("no main function")

  private def runDefers(savedDefers: mutable.ArrayBuffer[(TStmt, Env)]): Unit =
    for (stmt, env) <- savedDefers.reverseIterator do
      exec(stmt, env)

  private def releaseRefs(env: Env): Unit =
    for (_, cell) <- env do refDecr(cell.value)

  private def call(fun: TFunDecl, args: List[Value]): Value =
    val env: Env = new mutable.LinkedHashMap
    val savedSize = deferStack.size

    for (param, arg) <- fun.params.zip(args) do
      refIncr(arg)
      env(param.name) = new Cell(arg)

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
    case RefSliceVal(_, _, rc) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    case RefStringVal(_, _, rc) => if rc.get() != IMMORTAL_RC then rc.incrementAndGet()
    case _ =>

  private def refDecr(v: Value): Unit = v match
    case RefVal(cells, rc, typeName) =>
      if rc.get() == IMMORTAL_RC then return
      val count = rc.decrementAndGet()
      if count == 0 then
        rc.set(IMMORTAL_RC) // prevent re-entrant deinit from releaseRefs
        if typeName.nonEmpty then
          val deinitName = s"${typeName}_deinit"
          functions.get(deinitName).foreach { fun =>
            call(fun, List(v))
          }
    case RefSliceVal(_, _, rc) =>
      if rc.get() != IMMORTAL_RC then
        if rc.decrementAndGet() <= 0 then ()
    case RefStringVal(_, _, rc) =>
      if rc.get() != IMMORTAL_RC then
        if rc.decrementAndGet() <= 0 then ()
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

  private def exec(stmt: TStmt, env: Env): Unit =
    stmt match
      case TVarStmt(name, _, init) =>
        val v = evalAny(init, env)
        // Increment refcount for copies only — TNew/TNewArray already set refcount=1
        init match
          case _: TNew | _: TNewArray => // owned, no incr
          case _ => refIncr(v)
        env(name) = new Cell(v)

      case TDestructureStmt(names, _, init) =>
        val ArrVal(cells, off) = evalAny(init, env): @unchecked
        for (name, i) <- names.zipWithIndex do
          env(name) = new Cell(cells(off + i).value)

      case TDestructureAssignStmt(names, _, init) =>
        // Parallel assignment: evaluate RHS fully, then assign all values
        val ArrVal(cells, off) = evalAny(init, env): @unchecked
        val values = names.indices.map(i => cells(off + i).value)
        for (name, v) <- names.zip(values) do
          lookupCell(name, env).value = v

      case TAssignStmt(target, value) =>
        val v = evalAny(value, env)
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
            cell.value = IntVal(op match
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
            )

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
        cell.value = IntVal(op match
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
        )

      case TReturnStmt(value) =>
        throw ReturnException(value.map(evalAny(_, env)).getOrElse(IntVal(0)))

      case TDeferStmt(body) =>
        deferStack += ((body, env))

      case TForStmt(init, cond, update, body) =>
        exec(init, env)
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
            exec(update, env)
          catch
            case BreakException => running = false
            case ContinueException => exec(update, env)
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TWhileStmt(cond, body) =>
        var running = true
        while running && toLong(evalAny(cond, env)) != 0 do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case BreakException => running = false
            case ContinueException =>
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)

      case TDoWhileStmt(cond, body) =>
        var running = true
        while running do
          val savedKeys = env.keySet.toSet
          try
            execBlock(body, env)
          catch
            case BreakException    => running = false
            case ContinueException =>
          // Release refs for variables created in this iteration
          for key <- env.keySet.toSet -- savedKeys do
            refDecr(env(key).value)
            env.remove(key)
          if running then running = toLong(evalAny(cond, env)) != 0

      case TBreakStmt => throw BreakException
      case TContinueStmt => throw ContinueException

      case TAsmStmt(_) => // no-op in interpreter

      case TExprStmt(expr) =>
        evalAny(expr, env)

  private def evalAny(expr: TExpr, env: Env): Value =
    expr match
      case TIntLit(n, _) => IntVal(n)
      case TFloatLit(d, _) => FloatVal(d)
      case TBoolLit(b, _) => IntVal(if b then 1L else 0L)

      case TStringLit(s, _) =>
        val bytes = s.getBytes("UTF-8")
        RefStringVal(bytes, bytes.length, new java.util.concurrent.atomic.AtomicInteger(IMMORTAL_RC))

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
            val v = toLong(cell.value) + 1
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
            val v = toLong(cell.value) - 1
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
            cell.value = IntVal(old + 1)
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
            cell.value = IntVal(old - 1)
            IntVal(old)

      case TDeref(inner, _) =>
        evalAny(inner, env) match
          case RefVal(cells, _, _) => ArrVal(cells, 0)  // deref &T → expose struct fields
          case other => derefCell(other).value

      case TIndex(arr, index, _) =>
        val arrVal = evalAny(arr, env)
        val idx = toLong(evalAny(index, env)).toInt
        arrVal match
          case RefStringVal(bytes, length, _) =>
            if idx < 0 || idx >= length then throw RuntimeError(s"string index out of bounds: $idx (length $length)")
            IntVal(bytes(idx) & 0xff)
          case SliceVal(cells, off, len, _) =>
            if idx < 0 || idx >= len then throw RuntimeError(s"slice index out of bounds: $idx (length $len)")
            cells(off + idx).value
          case _ =>
            indexCell(arrVal, idx).value

      case TSliceExpr(arr, low, high, _) =>
        val arrVal = evalAny(arr, env)
        arrVal match
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
        RefStringVal(bytes, len, new java.util.concurrent.atomic.AtomicInteger(IMMORTAL_RC))

      case TStringFromSlice(sliceExpr, _) =>
        val (cells, off, slen) = evalAny(sliceExpr, env) match
          case SliceVal(c, o, l, _) => (c, o, l)
          case RefSliceVal(c, l, _) => (c, 0, l)
          case other => throw RuntimeError(s"string() requires a slice, got $other")
        val bytes = new Array[Byte](slen)
        for i <- 0 until slen do
          bytes(i) = toLong(cells(off + i).value).toByte
        RefStringVal(bytes, slen, new java.util.concurrent.atomic.AtomicInteger(IMMORTAL_RC))

      case TIfExpr(cond, thenBody, elseBody, _) =>
        if toLong(evalAny(cond, env)) != 0 then
          evalBlock(thenBody, env)
        else
          elseBody match
            case Some(stmts) => evalBlock(stmts, env)
            case None => IntVal(0)

      case TBinary(left, op, right, _) =>
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

        // String path: concatenation and comparison
        (lv, rv) match
          case (RefStringVal(lb, ll, _), RefStringVal(rb, rl, _)) =>
            return (op match
              case "+" =>
                val newBytes = new Array[Byte](ll + rl)
                System.arraycopy(lb, 0, newBytes, 0, ll)
                System.arraycopy(rb, 0, newBytes, ll, rl)
                RefStringVal(newBytes, newBytes.length, new java.util.concurrent.atomic.AtomicInteger(1))
              case "==" => IntVal(if java.util.Arrays.equals(lb, 0, ll, rb, 0, rl) then 1L else 0L)
              case "!=" => IntVal(if !java.util.Arrays.equals(lb, 0, ll, rb, 0, rl) then 1L else 0L)
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
        IntVal(op match
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
        )

      case TUnary(op, operand, _) =>
        val v = evalAny(operand, env)
        v match
          case FloatVal(d) =>
            op match
              case "-" => FloatVal(-d)
              case _   => throw RuntimeError(s"unsupported float unary operator: $op")
          case _ =>
            val n = toLong(v)
            IntVal(op match
              case "-" => -n
              case "!" => if n == 0 then 1L else 0L
              case "~" => ~n
              case _   => throw RuntimeError(s"unknown unary operator: $op")
            )

      case TCast(inner, target) =>
        val v = evalAny(inner, env)
        import SyslType.*
        target match
          case DoubleType  => FloatVal(toDouble(v))
          case BoolType => IntVal(if toLong(v) != 0 then 1L else 0L)
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
              case IntVal(0) => PtrVal(ArrayPtr(Array.empty[Cell], 0))  // null pointer
              case IntVal(n) => PtrVal(longToPointer(n))  // integer to pointer
              case _ => v
          case _ => v

      case TSizeof(size, _) => IntVal(size)

      case TLen(inner, _) =>
        evalAny(inner, env) match
          case RefStringVal(_, length, _) => IntVal(length.toLong)
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

      case TStructLit(SyslType.StructType(_, fields)) =>
        def initField(typ: SyslType): Value = typ match
          case st: SyslType.StructType => evalAny(TStructLit(st), env)
          case SyslType.ArrayType(elem, size) =>
            val cells = Array.fill(size)(new Cell(initField(elem)))
            ArrVal(cells, 0)
          case _ => IntVal(0)
        val cells = fields.map((_, typ) => new Cell(initField(typ))).toArray
        ArrVal(cells, 0)

      case TNew(SyslType.StructType(name, fields), args) =>
        val cells = fields.zip(args).map { case ((_, _), arg) =>
          new Cell(evalAny(arg, env))
        }.toArray
        RefVal(cells, new java.util.concurrent.atomic.AtomicInteger(1), name)

      case TNewArray(elemType, sizeExpr) =>
        val n = toLong(evalAny(sizeExpr, env)).toInt
        val cells = Array.fill(n)(new Cell(IntVal(0)))
        RefSliceVal(cells, n, new java.util.concurrent.atomic.AtomicInteger(1))

      case TStructConstruct(SyslType.StructType(_, fields), args) =>
        val cells = fields.zip(args).map { case ((_, typ), arg) =>
          val value = evalAny(arg, env)
          new Cell(value)
        }.toArray
        ArrVal(cells, 0)

      case TFieldAccess(obj, fieldIndex, _) =>
        val ArrVal(cells, off) = evalAny(obj, env): @unchecked
        cells(off + fieldIndex).value

      case TFuncRef(name, _) => FuncVal(name)

      case TCall(name, args, _) =>
        val argValues = args.map(evalAny(_, env))
        functions.get(name) match
          case Some(fun) => call(fun, argValues)
          case None =>
            builtins.get(name) match
              case Some(f) => f(argValues)
              case None => throw RuntimeError(s"undefined function: $name")

      case TIndirectCall(callee, args, _) =>
        val FuncVal(name) = evalAny(callee, env): @unchecked
        val argValues = args.map(evalAny(_, env))
        functions.get(name) match
          case Some(fun) => call(fun, argValues)
          case None =>
            builtins.get(name) match
              case Some(f) => f(argValues)
              case None => throw RuntimeError(s"undefined function: $name")
