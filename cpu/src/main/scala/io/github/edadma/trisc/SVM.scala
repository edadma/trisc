package io.github.edadma.trisc

import scala.annotation.tailrec
import scala.collection.mutable
import io.github.edadma.logger._

/** Sysl Virtual Machine — bytecode stack machine interpreter.
  *
  * Shares the same device/interrupt infrastructure as the TRISC CPU via the Processor trait. Bytecode is loaded into the
  * same Addressable memory space, allowing SVM programs to interact with memory-mapped devices identically to TRISC
  * programs.
  */
class SVM(
    mem: Addressable,
    tick: Seq[Processor => Unit] = Nil,
    stackDepth: Int = 4096,
    returnStackDepth: Int = 1024,
    maxLocals: Int = 256,
    maxCallDepth: Int = 1024,
) extends Processor:

  // --- Addressable delegation ---
  val name: String = mem.name
  val base: Long = mem.base
  val size: Long = mem.size

  def readByte(addr: Long): Int = mem.readByte(addr)
  def writeByte(addr: Long, data: Long): Unit = mem.writeByte(addr, data)
  def loadByte(addr: Long, data: Long): Unit = mem.loadByte(addr, data)
  override def readShort(addr: Long): Int = mem.readShort(addr)
  override def readInt(addr: Long): Int = mem.readInt(addr)
  override def readLong(addr: Long): Long = mem.readLong(addr)
  override def writeShort(addr: Long, data: Long): Unit = mem.writeShort(addr, data)
  override def writeInt(addr: Long, data: Long): Unit = mem.writeInt(addr, data)
  override def writeLong(addr: Long, data: Long): Unit = mem.writeLong(addr, data)

  // --- Processor state ---
  var state: State = State.Halt
  var cycles: Long = 0
  var limit: Int = -1

  // --- VM state ---
  var ip: Long = 0 // instruction pointer (byte address in memory)
  private var interruptPending: Boolean = false

  // Data stack — TOS kept separate for performance
  private val stack = new Array[Long](stackDepth)
  private var sp: Int = -1 // stack pointer (-1 = empty)
  private var tos: Long = 0 // top of stack cache

  // Return stack
  private val rstack = new Array[Long](returnStackDepth)
  private var rsp: Int = -1

  // Call frames — each frame saves the return IP and locals base
  private case class CallFrame(returnIP: Long, localsBase: Int, localsCount: Int)
  private val frames = new Array[CallFrame](maxCallDepth)
  private var fp: Int = -1 // frame pointer

  // Local variables — flat array, frames index into it
  private val locals = new Array[Long](maxLocals * maxCallDepth)
  private var localsTop: Int = 0 // next free local slot

  /** Read the top-of-stack value (e.g., return value after halt). */
  def result: Long = tos

  // Interrupt vector table: slot 0 = initial IP, slot 1 = interrupt handler
  private val VECTOR_INIT_IP = 0L
  private val VECTOR_INTERRUPT = 8L

  // Logging
  val log: Logger =
    val l = new Logger(new ConsoleHandler, new DefaultLogFormatter(includeTimestamp = false))
    l.setLogLevel(LogLevel.OFF)
    l

  // --- Stack helpers ---
  private inline def push(v: Long): Unit =
    if sp >= 0 then stack(sp) = tos
    sp += 1
    tos = v

  private inline def pop(): Long =
    val v = tos
    if sp > 0 then tos = stack(sp - 1)
    sp -= 1
    v

  private inline def peek: Long = tos

  // depth: number of values on the stack (sp+1 when TOS is cached)
  private inline def depth: Int = sp + 1

  // --- Byte fetching ---
  private inline def fetchU8(): Int =
    val b = mem.readByte(ip) & 0xff
    ip += 1
    b

  private inline def fetchI8(): Int =
    val b = mem.readByte(ip).toByte.toInt
    ip += 1
    b

  private inline def fetchI16(): Int =
    val hi = mem.readByte(ip) & 0xff
    val lo = mem.readByte(ip + 1) & 0xff
    ip += 2
    ((hi << 8) | lo).toShort.toInt

  private inline def fetchI32(): Int =
    val b0 = (mem.readByte(ip) & 0xff).toLong
    val b1 = (mem.readByte(ip + 1) & 0xff).toLong
    val b2 = (mem.readByte(ip + 2) & 0xff).toLong
    val b3 = (mem.readByte(ip + 3) & 0xff).toLong
    ip += 4
    ((b0 << 24) | (b1 << 16) | (b2 << 8) | b3).toInt

  private inline def fetchI64(): Long =
    val hi = fetchI32().toLong & 0xffffffffL
    val lo = fetchI32().toLong & 0xffffffffL
    (hi << 32) | lo

  // --- Interrupt handling (mirrors CPU pattern) ---
  def interrupt(): Unit =
    if state == State.Run || state == State.Wfi then
      interruptPending = true
      if state == State.Wfi then state = State.Interrupt

  private def handleInterrupt(): Unit =
    interruptPending = false
    val handler = mem.readLong(VECTOR_INTERRUPT)
    if handler != 0 then
      // Push return address onto return stack
      rsp += 1
      rstack(rsp) = ip
      ip = handler
      state = State.Run

  // --- Reset ---
  def reset(): Unit =
    sp = -1
    rsp = -1
    fp = -1
    localsTop = 0
    tos = 0
    cycles = 0
    interruptPending = false
    // Load initial IP from vector table slot 0
    ip = mem.readLong(VECTOR_INIT_IP)
    state = State.Run

  // --- Execute one instruction ---
  def execute(): Unit =
    if state == State.Interrupt then
      handleInterrupt()
      if state != State.Run then return

    val opcode = fetchU8()
    cycles += 1

    log.trace(f"ip=${ip - 1}%04x op=$opcode%02x sp=$sp tos=$tos%x", category = "SVM")

    opcode match
      // === 0x00-0x0F: Stack Operations ===
      case 0x00 => // NOP

      case 0x01 => // DROP
        pop()

      case 0x02 => // DUP
        push(tos)

      case 0x03 => // SWAP
        val a = pop()
        val b = pop()
        push(a)
        push(b)

      case 0x04 => // OVER
        val b = tos
        val a = stack(sp - 1)
        push(a)

      case 0x05 => // ROT ( a b c -- b c a )
        val c = pop()
        val b = pop()
        val a = pop()
        push(b)
        push(c)
        push(a)

      case 0x06 => // NROT ( a b c -- c a b )
        val c = pop()
        val b = pop()
        val a = pop()
        push(c)
        push(a)
        push(b)

      case 0x07 => // NIP ( a b -- b )
        val b = pop()
        pop()
        push(b)

      case 0x08 => // TUCK ( a b -- b a b )
        val b = pop()
        val a = pop()
        push(b)
        push(a)
        push(b)

      case 0x09 => // DROP2
        pop()
        pop()

      case 0x0A => // DUP2 ( a b -- a b a b )
        val b = tos
        val a = stack(sp - 1)
        push(a)
        push(b)

      case 0x0B => // SWAP2 ( a b c d -- c d a b )
        val d = pop()
        val c = pop()
        val b = pop()
        val a = pop()
        push(c)
        push(d)
        push(a)
        push(b)

      case 0x0C => // OVER2 ( a b c d -- a b c d a b )
        val a = stack(sp - 3)
        val b = stack(sp - 2)
        push(a)
        push(b)

      case 0x0D => // DEPTH
        push(depth)

      // === 0x10-0x1F: Literals ===
      case 0x10 => push(0L) // PUSH_0
      case 0x11 => push(1L) // PUSH_1
      case 0x12 => push(2L) // PUSH_2
      case 0x13 => push(-1L) // PUSH_M1

      case 0x14 => push(fetchI8().toLong) // PUSH_i8
      case 0x15 => push(fetchU8().toLong) // PUSH_u8
      case 0x16 => push(fetchI16().toLong) // PUSH_i16
      case 0x17 => push(fetchI32().toLong) // PUSH_i32
      case 0x18 => push(fetchI64()) // PUSH_i64

      // === 0x20-0x2F: Integer Arithmetic ===
      case 0x20 => // ADD
        val b = pop(); tos += b

      case 0x21 => // SUB
        val b = pop()
        val a = pop()
        push(a - b)

      case 0x22 => // MUL
        val b = pop(); tos *= b

      case 0x23 => // DIV
        val b = pop()
        val a = pop()
        if b == 0 then state = State.IllegalDivide
        else push(a / b)

      case 0x24 => // MOD
        val b = pop()
        val a = pop()
        if b == 0 then state = State.IllegalDivide
        else push(a % b)

      case 0x25 => // DIVMOD ( a b -- rem quot )
        val b = pop()
        val a = pop()
        if b == 0 then state = State.IllegalDivide
        else
          push(a % b)
          push(a / b)

      case 0x26 => // DIVU
        val b = pop()
        val a = pop()
        if b == 0 then state = State.IllegalDivide
        else push(java.lang.Long.divideUnsigned(a, b))

      case 0x27 => // MODU
        val b = pop()
        val a = pop()
        if b == 0 then state = State.IllegalDivide
        else push(java.lang.Long.remainderUnsigned(a, b))

      case 0x28 => tos = -tos // NEG
      case 0x29 => tos = math.abs(tos) // ABS
      case 0x2A => tos += 1 // INC
      case 0x2B => tos -= 1 // DEC

      // === 0x30-0x3F: Bitwise Operations ===
      case 0x30 => val b = pop(); tos &= b // AND
      case 0x31 => val b = pop(); tos |= b // OR
      case 0x32 => val b = pop(); tos ^= b // XOR
      case 0x33 => tos = ~tos // NOT

      case 0x34 => // SHL
        val s = pop().toInt
        val v = pop()
        push(v << s)

      case 0x35 => // SHR (logical)
        val s = pop().toInt
        val v = pop()
        push(v >>> s)

      case 0x36 => // SAR (arithmetic)
        val s = pop().toInt
        val v = pop()
        push(v >> s)

      case 0x37 => tos = java.lang.Long.numberOfLeadingZeros(tos) // CLZ
      case 0x38 => tos = java.lang.Long.numberOfTrailingZeros(tos) // CTZ
      case 0x39 => tos = java.lang.Long.bitCount(tos) // POPCNT

      case 0x3A => // ROTL
        val s = pop().toInt & 63
        val v = pop()
        push((v << s) | (v >>> (64 - s)))

      case 0x3B => // ROTR
        val s = pop().toInt & 63
        val v = pop()
        push((v >>> s) | (v << (64 - s)))

      case 0x3C => tos = java.lang.Long.reverseBytes(tos) // BSWAP

      // === 0x40-0x4F: Integer Comparison ===
      case 0x40 => val b = pop(); val a = pop(); push(if a == b then 1 else 0) // EQ
      case 0x41 => val b = pop(); val a = pop(); push(if a != b then 1 else 0) // NEQ
      case 0x42 => val b = pop(); val a = pop(); push(if a < b then 1 else 0) // LT
      case 0x43 => val b = pop(); val a = pop(); push(if a > b then 1 else 0) // GT
      case 0x44 => val b = pop(); val a = pop(); push(if a <= b then 1 else 0) // LE
      case 0x45 => val b = pop(); val a = pop(); push(if a >= b then 1 else 0) // GE

      case 0x46 => // LTU
        val b = pop(); val a = pop()
        push(if java.lang.Long.compareUnsigned(a, b) < 0 then 1 else 0)
      case 0x47 => // GTU
        val b = pop(); val a = pop()
        push(if java.lang.Long.compareUnsigned(a, b) > 0 then 1 else 0)
      case 0x48 => // LEU
        val b = pop(); val a = pop()
        push(if java.lang.Long.compareUnsigned(a, b) <= 0 then 1 else 0)
      case 0x49 => // GEU
        val b = pop(); val a = pop()
        push(if java.lang.Long.compareUnsigned(a, b) >= 0 then 1 else 0)

      case 0x4A => tos = if tos == 0 then 1 else 0 // EQZ
      case 0x4B => tos = if tos != 0 then 1 else 0 // NEZ
      case 0x4C => tos = if tos < 0 then 1 else 0 // LTZ
      case 0x4D => tos = if tos > 0 then 1 else 0 // GTZ
      case 0x4E => tos = if tos <= 0 then 1 else 0 // LEZ
      case 0x4F => tos = if tos >= 0 then 1 else 0 // GEZ

      // === 0x50-0x5F: Memory ===
      case 0x50 => tos = mem.readByte(tos) & 0xffL // LOAD8
      case 0x51 => tos = mem.readByte(tos).toByte.toLong // LOAD8S
      case 0x52 => tos = mem.readShort(tos) & 0xffffL // LOAD16
      case 0x53 => tos = mem.readShort(tos).toShort.toLong // LOAD16S
      case 0x54 => tos = mem.readInt(tos) & 0xffffffffL // LOAD32
      case 0x55 => tos = mem.readInt(tos).toLong // LOAD32S
      case 0x56 => tos = mem.readLong(tos) // LOAD64

      case 0x58 => // STORE8
        val addr = pop()
        val v = pop()
        mem.writeByte(addr, v)

      case 0x59 => // STORE16
        val addr = pop()
        val v = pop()
        mem.writeShort(addr, v)

      case 0x5A => // STORE32
        val addr = pop()
        val v = pop()
        mem.writeInt(addr, v)

      case 0x5B => // STORE64
        val addr = pop()
        val v = pop()
        mem.writeLong(addr, v)

      // === 0x60-0x6F: Control Flow ===
      case 0x60 => // JUMP
        val offset = fetchI16()
        ip += offset

      case 0x61 => // JUMPZ
        val offset = fetchI16()
        if pop() == 0 then ip += offset

      case 0x62 => // JUMPNZ
        val offset = fetchI16()
        if pop() != 0 then ip += offset

      case 0x63 => // CALL
        val offset = fetchI32()
        rsp += 1
        rstack(rsp) = ip
        ip += offset

      case 0x64 => // RET
        // Restore frame
        if fp >= 0 then
          val frame = frames(fp)
          localsTop = frame.localsBase
          fp -= 1
        ip = rstack(rsp)
        rsp -= 1

      case 0x65 => // TAIL
        val offset = fetchI32()
        // Reuse current frame — just jump
        ip += offset

      case 0x66 => // CALLR
        val addr = pop()
        rsp += 1
        rstack(rsp) = ip
        ip = addr

      case 0x67 => // TAILR
        val addr = pop()
        ip = addr

      case 0x68 => // JUMP_WIDE
        val offset = fetchI32()
        ip += offset

      case 0x69 => // CALL_ABS
        val addr = fetchI64()
        rsp += 1
        rstack(rsp) = ip
        ip = addr

      case 0x6A => // TRAP
        val trapNum = fetchU8()
        handleTrap(trapNum)

      case 0x6B => // HALT
        state = State.Halt

      // === 0x70-0x7F: Local Variables ===
      case 0x70 => // FRAME
        val n = fetchU8()
        fp += 1
        frames(fp) = CallFrame(ip, localsTop, n)
        // Zero-init locals
        var i = 0
        while i < n do
          locals(localsTop + i) = 0
          i += 1
        localsTop += n

      case 0x71 => // LOCAL_GET
        val idx = fetchU8()
        val base = if fp >= 0 then frames(fp).localsBase else 0
        push(locals(base + idx))

      case 0x72 => // LOCAL_SET
        val idx = fetchU8()
        val base = if fp >= 0 then frames(fp).localsBase else 0
        locals(base + idx) = pop()

      case 0x73 => // LOCAL_TEE
        val idx = fetchU8()
        val base = if fp >= 0 then frames(fp).localsBase else 0
        locals(base + idx) = tos

      // === 0x80-0x8F: Return Stack ===
      case 0x80 => // R_PUSH
        rsp += 1
        rstack(rsp) = pop()

      case 0x81 => // R_POP
        push(rstack(rsp))
        rsp -= 1

      case 0x82 => // R_PEEK
        push(rstack(rsp))

      // === 0x90-0xAF: Floating Point ===
      case 0x90 => // FADD
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(a + b))

      case 0x91 => // FSUB
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(a - b))

      case 0x92 => // FMUL
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(a * b))

      case 0x93 => // FDIV
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(a / b))

      case 0x94 => // FMOD
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(a % b))

      case 0x95 => // FNEG
        tos = java.lang.Double.doubleToLongBits(-java.lang.Double.longBitsToDouble(tos))

      case 0x96 => // FABS
        tos = java.lang.Double.doubleToLongBits(math.abs(java.lang.Double.longBitsToDouble(tos)))

      case 0x97 => // FSQRT
        tos = java.lang.Double.doubleToLongBits(math.sqrt(java.lang.Double.longBitsToDouble(tos)))

      case 0x98 => // FFLOOR
        tos = java.lang.Double.doubleToLongBits(math.floor(java.lang.Double.longBitsToDouble(tos)))

      case 0x99 => // FCEIL
        tos = java.lang.Double.doubleToLongBits(math.ceil(java.lang.Double.longBitsToDouble(tos)))

      case 0x9A => // FROUND
        tos = java.lang.Double.doubleToLongBits(math.rint(java.lang.Double.longBitsToDouble(tos)))

      case 0x9B => // FTRUNC
        val f = java.lang.Double.longBitsToDouble(tos)
        tos = java.lang.Double.doubleToLongBits(if f >= 0 then math.floor(f) else math.ceil(f))

      case 0x9C => // FMIN
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(math.min(a, b)))

      case 0x9D => // FMAX
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(java.lang.Double.doubleToLongBits(math.max(a, b)))

      case 0x9E => // FEQ
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(if a == b then 1 else 0)

      case 0x9F => // FNEQ
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(if a != b then 1 else 0)

      case 0xA0 => // FLT
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(if a < b then 1 else 0)

      case 0xA1 => // FGT
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(if a > b then 1 else 0)

      case 0xA2 => // FLE
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(if a <= b then 1 else 0)

      case 0xA3 => // FGE
        val b = java.lang.Double.longBitsToDouble(pop())
        val a = java.lang.Double.longBitsToDouble(pop())
        push(if a >= b then 1 else 0)

      case 0xA4 => // F2I
        tos = java.lang.Double.longBitsToDouble(tos).toLong

      case 0xA5 => // I2F
        tos = java.lang.Double.doubleToLongBits(tos.toDouble)

      case 0xA6 => // F2U
        val f = java.lang.Double.longBitsToDouble(tos)
        tos = if f < 0 then 0 else if f >= 1.8446744073709552e19 then -1L else f.toLong

      case 0xA7 => // U2F
        val u = tos
        tos = java.lang.Double.doubleToLongBits(
          if u >= 0 then u.toDouble
          else (u >>> 1).toDouble * 2.0 + (u & 1).toDouble,
        )

      case 0xA8 => push(0L) // PUSH_F0 (0.0 = all zeros)
      case 0xA9 => push(java.lang.Double.doubleToLongBits(1.0)) // PUSH_F1

      // === 0xB0-0xCF: Superinstructions ===
      case 0xB0 => tos += tos // DUP_ADD (multiply by 2)
      case 0xB1 => tos *= tos // DUP_MUL (square)

      case 0xB2 => // OVER_ADD
        val b = tos
        val a = stack(sp - 1)
        tos = a + b

      case 0xB3 => // OVER_SUB
        val b = tos
        val a = stack(sp - 1)
        tos = a - b

      case 0xB4 => tos += fetchI8().toLong // ADD_IMM8
      case 0xB5 => tos -= fetchI8().toLong // SUB_IMM8
      case 0xB6 => tos *= fetchI8().toLong // MUL_IMM8

      case 0xB7 => // EQZ_JUMPZ (branch if nonzero — EQZ produces 0 for nonzero, JUMPZ takes it)
        val offset = fetchI16()
        if pop() != 0 then ip += offset

      case 0xB8 => // EQZ_JUMPNZ (branch if zero)
        val offset = fetchI16()
        if pop() == 0 then ip += offset

      case 0xB9 => // INC_JUMPNZ
        val offset = fetchI16()
        tos += 1
        if tos != 0 then ip += offset

      case 0xBA => // DEC_JUMPNZ
        val offset = fetchI16()
        tos -= 1
        if tos != 0 then ip += offset

      case 0xBB => // LOCAL_GET_ADD
        val idx = fetchU8()
        val lbase = if fp >= 0 then frames(fp).localsBase else 0
        tos += locals(lbase + idx)

      case 0xBC => // LOCAL_GET_SUB
        val idx = fetchU8()
        val lbase = if fp >= 0 then frames(fp).localsBase else 0
        tos -= locals(lbase + idx)

      case 0xBD => // LOCAL_GET_EQZ
        val idx = fetchU8()
        val lbase = if fp >= 0 then frames(fp).localsBase else 0
        push(if locals(lbase + idx) == 0 then 1 else 0)

      case 0xBE => // LOCAL_GET_JUMPZ
        val idx = fetchU8()
        val offset = fetchI16()
        val lbase = if fp >= 0 then frames(fp).localsBase else 0
        if locals(lbase + idx) == 0 then ip += offset

      case 0xBF => // LOCAL_GET_JUMPNZ
        val idx = fetchU8()
        val offset = fetchI16()
        val lbase = if fp >= 0 then frames(fp).localsBase else 0
        if locals(lbase + idx) != 0 then ip += offset

      case 0xC0 => // DUP_LOAD64
        push(tos)
        tos = mem.readLong(tos)

      case 0xC1 => // DROP_JUMP
        pop()
        val offset = fetchI16()
        ip += offset

      case 0xC2 => // PUSH_i8_ADD
        tos += fetchI8().toLong

      case 0xC3 => // PUSH_i8_LOAD64
        tos = mem.readLong(tos + fetchI8().toLong)

      // === 0xFF: Debug ===
      case 0xFF => // BREAKPOINT
        state = State.Halt

      // === Unknown opcode ===
      case _ =>
        state = State.UnimplementedOpcode

  // --- Trap handler (TRAP <u8>) ---
  // Override or extend for custom syscalls. Default: halt on unknown trap.
  protected def handleTrap(num: Int): Unit =
    num match
      case 0 => state = State.Wfi // WFI — wait for interrupt
      case _ => state = State.Halt // unknown trap halts

  // --- Run loop (mirrors CPU.run) ---
  @tailrec
  final def run(): Unit =
    if state == State.Run || state == State.Interrupt then
      execute()

    if limit > 0 then limit -= 1

    tick.foreach(_(this))

    // Check for pending interrupt after tick (device may have raised one)
    if interruptPending && state == State.Run then
      state = State.Interrupt

    if state == State.Wfi && limit < 0 then Thread.sleep(1)

    if (state == State.Run || state == State.Wfi || state == State.Interrupt) && limit != 0 then
      run()

  def resume(): Unit =
    state = State.Run
    run()
