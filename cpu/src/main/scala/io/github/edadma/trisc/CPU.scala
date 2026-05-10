package io.github.edadma.trisc

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import io.github.edadma.logger._

enum Status(val bit: Int):
  case Ind extends Status(1)
  case Mode extends Status(2)
  case C extends Status(4)
  // bit 3 (8) reserved — formerly Irq, now handled by external interrupt controller
  case T extends Status(16)
  case V extends Status(32)

enum State:
  case Reset, Interrupt, InstructionAccess, DataAccess, MisalignedAccess,
    UnimplementedOpcode, PrivilegeViolation, IllegalDivide,
    Trap0, Trap1, Trap2, Trap3, Trap4, Trap5, Trap6, Trap7,
    Trace, Overflow, BoundsCheck,
    Halt, Run, Wfi, DoubleFault

class CPU(mem: Addressable, var tick: Seq[Processor => Unit] = Nil, mpu: Option[MPU] = None, mpuBase: Long = 0, val mmu: Option[MMU] = None, val coreId: Int = 0, reservationMonitor: Option[ReservationMonitor] = None) extends Processor:
  val name: String = mem.name
  val base: Long = mem.base
  val size: Long = mem.size

  /** Faulting virtual address, saved on page fault for the exception handler. */
  var faultAddr: Long = 0
  /** Cause of the last MMU fault. */
  var faultCause: FaultCause = FaultCause.None
  /** Suppress diagnostic stderr output (for tests). */
  var quiet: Boolean = false

  // PC ring buffer for crash diagnostics
  private val _pcRing = new Array[Long](20)
  private var _pcPos = 0

  private val mpuEnd: Long = mpuBase + mpu.map(_.registerSize).getOrElse(0)

  /** Returns true if access is denied by the MPU. */
  private def mpuDenied(addr: Long, access: Access): Boolean =
    mpu match
      case Some(m) => !m.check(addr, access, test(Status.Mode))
      case None    => false

  private def checkMPU(addr: Long, access: Access): Boolean =
    if mpuDenied(addr, access) then
      state = State.DataAccess
      true
    else false

  /** Translate virtual address through MMU. Returns physical address, or -1 on fault. */
  private def xlate(vaddr: Long, access: Access): Long =
    mmu match
      case None => vaddr
      case Some(m) =>
        m.translate(vaddr, access, test(Status.Mode)) match
          case Right(paddr) => paddr
          case Left(cause) =>
            faultAddr = vaddr
            faultCause = cause
            state = State.DataAccess
            -1L

  private def isMpuAddr(addr: Long): Boolean =
    mpu.isDefined && addr >= mpuBase && addr < mpuEnd

  def readByte(addr: Long): Int =
    if isMpuAddr(addr) then
      if !test(Status.Mode) then { state = State.PrivilegeViolation; return 0 }
      mpu.get.readRegister((addr - mpuBase).toInt)
    else
      val paddr = xlate(addr, Access.Read)
      if paddr == -1L then 0
      else if checkMPU(paddr, Access.Read) then 0
      else mem.readByte(paddr)

  def writeByte(addr: Long, data: Long): Unit =
    if isMpuAddr(addr) then
      if !test(Status.Mode) then { state = State.PrivilegeViolation; return }
      mpu.get.writeRegister((addr - mpuBase).toInt, data.toInt)
    else
      val paddr = xlate(addr, Access.Write)
      if paddr != -1L && !checkMPU(paddr, Access.Write) then
        mem.writeByte(paddr, data)
        notifyWrite(paddr)

  def loadByte(addr: Long, data: Long): Unit = mem.loadByte(addr, data)

  /** Decode a 16-bit instruction word. Subclasses (e.g. Trisc16CPU) can install
    * a different decoder to gate the ISA subset. */
  protected def decode(inst: Int): Instruction = Decode(inst)

  /** Offset added to PC by AUIPC. TRISC scales the 8-bit immediate by 256;
    * Trisc16CPU overrides to scale by 2. */
  def auipcOffset(imm: Int): Long = imm.toLong << 8

  /** Memory read used by the LD instruction. Trisc16CPU overrides to a 16-bit read. */
  def ldRead(addr: Long): Long = readInt(addr).toLong

  /** Memory write used by the ST instruction. Trisc16CPU overrides to a 16-bit write. */
  def stWrite(addr: Long, v: Long): Unit = writeInt(addr, v)

  /** Notify reservation monitor of a write (for SMP LL/SC invalidation). */
  private inline def notifyWrite(addr: Long): Unit =
    reservationMonitor match
      case Some(mon) => mon.invalidateOthers(coreId, addr)
      case None => // single-core, no-op

  private def checkAlign(addr: Long, align: Int): Boolean =
    if (addr & (align - 1)) != 0 then
      faultAddr = addr
      state = State.MisalignedAccess
      true
    else false

  override def readShort(addr: Long): Int =
    if checkAlign(addr, 2) then 0
    else
      val paddr = xlate(addr, Access.Read)
      if paddr == -1L then 0
      else if checkMPU(paddr, Access.Read) then 0
      else mem.readShort(paddr)

  override def readInt(addr: Long): Int =
    if checkAlign(addr, 4) then 0
    else
      val paddr = xlate(addr, Access.Read)
      if paddr == -1L then 0
      else if checkMPU(paddr, Access.Read) then 0
      else mem.readInt(paddr)

  override def readLong(addr: Long): Long =
    if checkAlign(addr, 8) then 0
    else
      val paddr = xlate(addr, Access.Read)
      if paddr == -1L then 0
      else if checkMPU(paddr, Access.Read) then 0
      else mem.readLong(paddr)

  override def writeShort(addr: Long, data: Long): Unit =
    if !checkAlign(addr, 2) then
      val paddr = xlate(addr, Access.Write)
      if paddr != -1L && !checkMPU(paddr, Access.Write) then
        mem.writeShort(paddr, data)
        notifyWrite(paddr)

  override def writeInt(addr: Long, data: Long): Unit =
    if !checkAlign(addr, 4) then
      val paddr = xlate(addr, Access.Write)
      if paddr != -1L && !checkMPU(paddr, Access.Write) then
        mem.writeInt(paddr, data)
        notifyWrite(paddr)

  override def writeLong(addr: Long, data: Long): Unit =
    if !checkAlign(addr, 8) then
      val paddr = xlate(addr, Access.Write)
      if paddr != -1L && !checkMPU(paddr, Access.Write) then
        mem.writeLong(paddr, data)
        notifyWrite(paddr)

  /** Mask applied to every register write. Default is all-ones (no masking).
    * Subclasses with narrower registers (e.g. Trisc16CPU) override this. */
  protected def writeMask: Long = -1L

  protected def newReg: Reg = new Reg

  val r = immutable.ArraySeq(
    new Reg0,
    newReg,
    newReg,
    newReg,
    newReg,
    newReg,
    newReg,
    newReg,
  )
  var pc: Long = 0
  var psr: Int = 0
  var usp: Long = 0
  var state: State = State.Halt
  // Local reservation state (single-core fallback). When reservationMonitor is present,
  // these are not used — the monitor tracks all cores' reservations.
  private var _reservationAddr: Long = 0
  private var _reservationValid: Boolean = false

  /** Set a reservation (called by LL instruction). */
  def setReservation(addr: Long): Unit =
    reservationMonitor match
      case Some(mon) => mon.setReservation(coreId, addr)
      case None =>
        _reservationAddr = addr
        _reservationValid = true

  /** Check and clear a reservation (called by SC instruction).
    * Returns true if the reservation was valid and matches the address. */
  def checkAndClearReservation(addr: Long): Boolean =
    reservationMonitor match
      case Some(mon) => mon.checkAndClear(coreId, addr)
      case None =>
        val valid = _reservationValid && _reservationAddr == addr
        _reservationValid = false
        valid

  /** Clear the reservation (called on exception entry). */
  def clearReservation(): Unit =
    reservationMonitor match
      case Some(mon) => mon.clearReservation(coreId)
      case None => _reservationValid = false

  /** Execute an atomic CAS (called by CAS instruction). */
  def atomicCAS(addr: Long, expected: Long, newValue: Long): Long =
    reservationMonitor match
      case Some(mon) => mon.atomicCAS(mem, addr, expected, newValue)
      case None =>
        val old = readLong(addr)
        if old == expected then writeLong(addr, newValue)
        old
  var cycles: Long = 0
  protected var inException: Boolean = false

  var limit: Int = -1
  var trace: Boolean = false

  // Logging — OFF by default, enable with cpu.log.setLogLevel(LogLevel.DEBUG)
  val log: Logger = {
    val l = new Logger(new ConsoleHandler, new DefaultLogFormatter(includeTimestamp = false))
    l.setLogLevel(LogLevel.OFF)
    l
  }

  // Breakpoints
  private val breakpoints = mutable.Set.empty[Long]
  private var breakpointHit: Boolean = false

  def addBreakpoint(addr: Long): Unit = breakpoints += addr
  def removeBreakpoint(addr: Long): Unit = breakpoints -= addr
  def clearBreakpoints(): Unit = breakpoints.clear()
  def listBreakpoints(): Set[Long] = breakpoints.toSet

  def test(status: Status): Boolean = (psr & status.bit) != 0

  def set(status: Status, set: Boolean): Unit = if set then psr |= status.bit else psr &= ~status.bit

  def reset(): Unit =
    for i <- 1 until 8 do r(i).write(0)

    usp = 0
    cycles = 0
    inException = false
    state = State.Reset
    set(Status.Ind, true)
    set(Status.Mode, true)
    set(Status.C, false)
    set(Status.T, false)
    set(Status.V, false)

  def interrupt(): Unit =
    if !test(Status.Ind) && (state == State.Run || state == State.Wfi) then
      state = State.Interrupt

  protected def enterException(): Unit =
    if inException then
      log.error(f"DoubleFault at pc=$pc%04x, original exception=$state", category = "CPU")
      if !quiet then
        System.err.println(f"[TRISC] DoubleFault at pc=$pc%04x original=$state faultAddr=$faultAddr%08x faultCause=$faultCause")
        System.err.println(f"  r1=${r(1).read}%x r2=${r(2).read}%x r3=${r(3).read}%x r4=${r(4).read}%x r5=${r(5).read}%x r6=${r(6).read}%x r7=${r(7).read}%x usp=$usp%x psr=$psr%x")
      logRegisters()
      state = State.DoubleFault
      return

    inException = true
    log.debug(f"Exception: $state at pc=$pc%04x ssp=${r(7).read}%04x", category = "CPU")

    try
      if state == State.Reset then
        // Reset is special (like 68000): load SSP from vector 0, PC from vector 1
        r(7).write(mem.readLong(0))
        pc = mem.readLong(8)
        state = State.Run
        set(Status.Mode, true)
        set(Status.Ind, true)
        clearReservation()
      else
        // Swap r7 <-> usp if coming from user mode
        if !test(Status.Mode) then
          val tmp = r(7).read
          r(7).write(usp)
          usp = tmp

        // Push PSR then PC onto supervisor stack (8 bytes each)
        // Must use CPU's writeLong (MMU-aware), not raw mem.writeLong
        val preR7 = r(7).read
        r(7).write(preR7 - 8)
        writeLong(preR7 - 8, psr)
        r(7).write(preR7 - 16)
        writeLong(preR7 - 16, pc)

        // Load PC from vector table (offset by 1 since reset occupies slots 0 and 1)
        val vector = (state.ordinal + 1) * 8
        pc = mem.readLong(vector)
        log.debug(f"  → vector[$vector%02x] = $pc%04x, new ssp=${r(7).read}%04x", category = "CPU")
        state = State.Run
        set(Status.Mode, true)
        set(Status.Ind, true)
        set(Status.T, false)
        clearReservation()
    catch
      case e: RuntimeException =>
        if !quiet then
          System.err.println(f"[TRISC] DoubleFault during exception entry at pc=$pc%04x state=$state faultAddr=$faultAddr%08x faultCause=$faultCause: ${e.getMessage}")
          val regs = (1 to 7).map(i => f"r$i=${r(i).read}%x").mkString(" ")
          System.err.println(f"  $regs usp=$usp%x psr=$psr%x")
        log.error(f"DoubleFault during exception entry at pc=$pc%04x", category = "CPU")
        state = State.DoubleFault
    finally
      inException = false

  private def logRegisters(): Unit =
    val regs = (1 to 7).map(i => f"r$i=${r(i).read}%x").mkString(" ")
    log.trace(f"  $regs usp=$usp%x psr=$psr%x", category = "CPU")

  def execute(): Unit =
    if state.ordinal < State.Halt.ordinal then
      enterException()
      if state == State.DoubleFault then return
    // Record PC in ring buffer
    _pcRing(_pcPos % _pcRing.length) = pc
    _pcPos += 1

    // Breakpoint check
    if breakpoints.nonEmpty && breakpoints.contains(pc) then
      log.info(f"Breakpoint hit at pc=$pc%04x", category = "CPU")
      logRegisters()
      breakpointHit = true
      state = State.Halt
      return

    // Translate PC through MMU
    val fetchAddr = mmu match
      case Some(m) =>
        m.translate(pc, Access.Execute, test(Status.Mode)) match
          case Right(paddr) => paddr
          case Left(cause) =>
            faultAddr = pc
            faultCause = cause
            if !quiet then
              System.err.println(f"[TRISC] InstructionAccess fault at pc=$pc%08x cause=$cause psr=$psr%x")
              System.err.println(f"  r1=${r(1).read}%x r2=${r(2).read}%x r3=${r(3).read}%x r4=${r(4).read}%x r5=${r(5).read}%x r6=${r(6).read}%x r7=${r(7).read}%x usp=$usp%x")
              val buf = _pcRing; val pos = _pcPos; val len = _pcRing.length
              System.err.println("  last PCs:")
              for i <- 0 until len do
                val idx = (pos - len + i + len * 2) % len
                System.err.println(f"    ${buf(idx)}%08x")
            state = State.InstructionAccess
            return
      case None => pc

    if mpuDenied(fetchAddr, Access.Execute) then
      state = State.InstructionAccess
      return

    val inst =
      try mem.readShortUnsigned(fetchAddr)
      catch
        case _: RuntimeException =>
          log.warn(f"InstructionAccess fault at pc=$pc%04x", category = "CPU")
          if !quiet then
            System.err.println(f"[TRISC] InstructionAccess fault at pc=$pc%08x (bad fetch)")
            System.err.println(f"  r1=${r(1).read}%x r2=${r(2).read}%x r3=${r(3).read}%x r4=${r(4).read}%x r5=${r(5).read}%x r6=${r(6).read}%x r7=${r(7).read}%x")
            System.err.println("  last PCs:")
            for i <- 0 until _pcRing.length do
              val idx = (_pcPos - _pcRing.length + i + _pcRing.length * 2) % _pcRing.length
              System.err.println(f"    ${_pcRing(idx)}%08x")
          state = State.InstructionAccess
          return

    val decoded = decode(inst)

    if trace then println(f"$pc%04x: $inst%04x  ${decoded.disassemble(this)}")

    log.trace(f"$pc%04x: ${decoded.disassemble(this)}", category = "CPU")

    pc += 2
    cycles += 1

    // Capture T state before instruction — trace fires based on T at start of instruction (like 68k)
    val traceEnabled = test(Status.T)

    val faultingPc = pc - 2
    var memoryFaultDetail: Option[String] = None
    try decoded(this)
    catch
      case ex: RuntimeException =>
        if state == State.Run then
          state = State.DataAccess
          memoryFaultDetail = Option(ex.getMessage).filter(_.nonEmpty).orElse(Some(ex.getClass.getSimpleName))

    // Always print to stderr: CPU log defaults to LogLevel.OFF, so log.warn would not show.
    if state == State.DataAccess && !quiet then
      val extra = memoryFaultDetail.map(m => s" memory: $m").getOrElse("")
      val instLine =
        try
          val w = readShortUnsigned(faultingPc)
          f" inst=${decode(w).disassemble(this)}"
        catch case _: Exception => ""
      val ptbrStr = mmu.map(m => f" ptbr=${m.ptbr}%08x").getOrElse("")
      val regsStr = (1 to 7).map(i => f"r$i=${r(i).read}%x").mkString(" ")
      System.err.println(
        f"[TRISC] DataAccess fault at pc=$faultingPc%04x faultAddr=${faultAddr}%08x cause=$faultCause$extra$instLine$ptbrStr",
      )
      System.err.println(f"  $regsStr usp=$usp%x psr=$psr%x")
      System.err.println("  last PCs:")
      for i <- 0 until _pcRing.length do
        val idx = (_pcPos - _pcRing.length + i + _pcRing.length * 2) % _pcRing.length
        System.err.println(f"    ${_pcRing(idx)}%08x")
      System.err.flush()
      log.warn(
        f"DataAccess at pc=$faultingPc%04x faultAddr=${faultAddr}%08x cause=$faultCause",
        category = "CPU",
      )

    if state == State.MisalignedAccess && !quiet then
      val instLine =
        try
          val w = readShortUnsigned(faultingPc)
          f" inst=${decode(w).disassemble(this)}"
        catch case _: Exception => ""
      val ptbrStr = mmu.map(m => f" ptbr=${m.ptbr}%08x").getOrElse("")
      val regsStr = (1 to 7).map(i => f"r$i=${r(i).read}%x").mkString(" ")
      System.err.println(
        f"[TRISC] MisalignedAccess at pc=$faultingPc%04x faultAddr=${faultAddr}%08x$instLine$ptbrStr",
      )
      System.err.println(f"  $regsStr usp=$usp%x psr=$psr%x")
      System.err.println("  last PCs:")
      for i <- 0 until _pcRing.length do
        val idx = (_pcPos - _pcRing.length + i + _pcRing.length * 2) % _pcRing.length
        System.err.println(f"    ${_pcRing(idx)}%08x")
      System.err.flush()

    val regs = (1 to 7).map(i => f"r$i=${r(i).read}%x").mkString(" ")
    log.trace(f"  $regs", category = "CPU")

    // Trace exception: fires after instruction completes if T was set BEFORE it executed
    if state == State.Run && traceEnabled then state = State.Trace

    if trace then
      for i <- 1 to 7 do print(f"  r$i:${r(i).read}%04x")
      println

  @tailrec
  final def run(): Unit =
    if state != State.Halt && state != State.Wfi && state != State.DoubleFault then
      execute()

    if limit > 0 then limit -= 1

    tick.foreach(_(this))

    // Keep ticking during WFI so background threads wake promptly
    while state == State.Wfi && limit < 0 do tick.foreach(_(this))

    if state != State.Halt && state != State.DoubleFault && limit != 0 then
      run()

  def resume(): Unit =
    state = State.Run
    run()

  class Reg:
    private var r: Long = 0

    def read: Long = r

    def readf: Double = java.lang.Double.longBitsToDouble(r)

    def write(v: Long): Unit = r = v & writeMask

    def write(v: Double): Unit = r = java.lang.Double.doubleToLongBits(v) & writeMask

  class Reg0 extends Reg:
    override def read: Long = 0

    override def readf: Double = 0

    override def write(v: Long): Unit = {}

    override def write(v: Double): Unit = {}

/** Instruction decoder. The decode table maps every 16-bit encoding to an
  * `Instruction` (or `IllegalInstruction` for unassigned slots).
  *
  * Subclasses (e.g. `Trisc16Decode`) reuse the shared populate chunks via
  * inheritance and override `buildInstructionTable` to omit TRISC-only
  * encodings or substitute TRISC16-specific implementations. The class is
  * instantiated once via `object Decode extends Decode` (the singleton TRISC
  * decoder) and again as a singleton inside `Trisc16Decode`.
  */
class Decode:
  protected val instructions: Array[Instruction] = Array.fill[Instruction](0x10000)(IllegalInstruction)

  buildInstructionTable()

  def apply(inst: Int): Instruction = instructions(inst)

  protected def populate(pattern: String, inst: Map[Char, Int] => Instruction): Unit =
    for ((idx, m) <- generate(pattern))
      instructions(idx) = inst(m)

  /** Sign-extend a 7-bit immediate to 32 bits (used by RRI branch and addi
    * decoders). */
  protected def ext(imm7: Int): Int = if (imm7 & 0x40) != 0 then imm7 | 0xffffff80 else imm7

  /** Enumerate all (idx, operand-map) pairs matching a bit-pattern string.
    * Patterns are space-separated bits ("000", "abc", "iiiiiiii") with optional
    * range constraints ("r:1-7"). See the original generator below for details. */
  protected def generate(pattern: String): List[(Int, Map[Char, Int])] =
    case class Variable(v: Char, lower: Int, upper: Int, bits: List[Int])

    val Range = "([a-zA-Z]):([0-9]+)-([0-9]+)".r
    val p = pattern.replace(" ", "").split(";")

    require(p.nonEmpty, "empty pattern")

    val bits = p(0)

    require(bits.length > 0, "pattern should comprise at least one bit")
    require(
      bits.forall(c => c == '0' || c == '1' || c.isLetter || c == '-'),
      "pattern should comprise only 0's, 1's, letters or -'s",
    )

    val ranges = Map(p.drop(1).map { case Range(v, l, u) => v(0) -> (l.toInt, u.toInt) }*)

    require(
      ranges.forall { case (_, (l, u)) => 0 <= l && l <= u },
      "first value of range must be less than or equal to second and be non-negative",
    )

    val (constant, variables) = {
      def scan(acc: Int, pos: Int, chars: List[Char], vars: Map[Char, List[Int]]): (Int, Map[Char, List[Int]]) =
        chars match {
          case Nil                       => (acc, vars)
          case '0' :: t                  => scan(acc, pos << 1, t, vars)
          case '1' :: t                  => scan(acc | pos, pos << 1, t, vars)
          case v :: t if vars contains v => scan(acc, pos << 1, t, vars + (v -> (vars(v) :+ pos)))
          case v :: t                    => scan(acc, pos << 1, t, vars + (v -> List(pos)))
        }

      scan(0, 1, bits.reverse.toList, Map())
    }

    val enumeration = new ListBuffer[(Int, Map[Char, Int])]

    def enumerate(acc: Int, vars: List[Variable], vals: Map[Char, Int]): Unit =
      vars match {
        case Nil => enumeration += ((acc, vals))
        case v :: t =>
          for (i <- v.lower to v.upper)
            enumerate(acc | int2bits(0, i, v.bits), t, vals + (v.v -> i))
      }

    def int2bits(res: Int, n: Int, bits: List[Int]): Int =
      bits match {
        case Nil                   => res
        case b :: t if (n & 1) > 0 => int2bits(res | b, n >> 1, t)
        case b :: t                => int2bits(res, n >> 1, t)
      }

    enumerate(
      constant,
      variables.toList map { case (v, b) =>
        if (ranges contains v) {
          require(ranges(v)._2 < (1 << b.length), "second value of range must be less than 2^#bits")
          Variable(v, ranges(v)._1, ranges(v)._2, b)
        } else
          Variable(v, 0, (1 << b.length) - 1, b)
      },
      Map(),
    )
    enumeration.toList

  /** Build the full TRISC decode table. Override in a subclass to install a
    * subset (and any subclass-specific overrides). */
  protected def buildInstructionTable(): Unit =
    populateRI()
    populateAuipc()
    populateBranchesAndAddi()
    populateRRRBlock0Shared()
    populateRRRBlock0Wide()
    populateRRRBlock0Atomic()
    populateRRRBlock1Shared()
    populateRRRBlock1Float()
    populateRRBlock00Shared()
    populateRRBlock00WideExt()
    populateRRBlock00Float()
    populateRRBlock00Atomic()
    populateRRBlock01Shared()
    populateRRBlock01FloatConv()
    populateRRBlock01Mmu()
    populateRRLoadStore()
    populateRPushPopShared()
    populateRPushPopWide()
    populateRPSRShared()
    populateRRte()
    populateRFenceTrapv()
    populateRWfi()
    populateRStackSwap()
    populateRPshrPopr()
    populateRInterruptCtrl()
    populateRTimer()
    populateTraps()

  // ----- Shared chunks (TRISC16 inherits these unchanged) -----------------

  /** RI: ldi, sli, sti — 8-bit immediate ops with identical TRISC/TRISC16 semantics. */
  protected def populateRI(): Unit =
    populate("111 rrr 00 iiiiiiii; r:1-7", a => new LDI(a('r'), a('i')))
    populate("111 rrr 10 iiiiiiii; r:1-7", a => new SLI(a('r'), a('i')))
    populate("111 rrr 11 iiiiiiii; r:1-7", a => new STI(a('r'), a('i')))

  /** RI auipc — same encoding on both, but the result depends on `cpu.auipcOffset`
    * (a CPU seam — TRISC scales by 256, TRISC16 by 2). The decoder is shared. */
  protected def populateAuipc(): Unit =
    populate("111 rrr 01 iiiiiiii; r:1-7", a => new AUIPC(a('r'), a('i')))

  /** RRI: beq, blu, bls, addi — same on TRISC16. */
  protected def populateBranchesAndAddi(): Unit =
    populate("010 aaa bbb iiiiiii", a => new BEQ(a('a'), a('b'), ext(a('i'))))
    populate("011 aaa bbb iiiiiii", a => new BLU(a('a'), a('b'), ext(a('i'))))
    populate("100 aaa bbb iiiiiii", a => new BLS(a('a'), a('b'), ext(a('i'))))
    populate("101 aaa bbb iiiiiii", a => new ADDI(a('a'), a('b'), ext(a('i'))))

  /** RRR Block 0 — load/store/arith shared with TRISC16:
    * ldb, stb, lds, sts, add, sub, mul, div, and, or, xor. */
  protected def populateRRRBlock0Shared(): Unit =
    populate("000 ddd aaa bbb 0000", a => new LDB(a('d'), a('a'), a('b')))
    populate("000 aaa bbb ccc 0001", a => new STB(a('a'), a('b'), a('c')))
    populate("000 ddd aaa bbb 0010", a => new LDS(a('d'), a('a'), a('b')))
    populate("000 aaa bbb ccc 0011", a => new STS(a('a'), a('b'), a('c')))
    populate("000 ddd aaa bbb 1000", a => new ADD(a('d'), a('a'), a('b')))
    populate("000 ddd aaa bbb 1001", a => new SUB(a('d'), a('a'), a('b')))
    populate("000 ddd aaa bbb 1010", a => new MUL(a('d'), a('a'), a('b')))
    populate("000 ddd aaa bbb 1011", a => new DIV(a('d'), a('a'), a('b')))
    populate("000 ddd aaa bbb 1101", a => new AND(a('d'), a('a'), a('b')))
    populate("000 ddd aaa bbb 1110", a => new OR(a('d'), a('a'), a('b')))
    populate("000 ddd aaa bbb 1111", a => new XOR(a('d'), a('a'), a('b')))

  /** RRR Block 1 shifts/compares shared with TRISC16. */
  protected def populateRRRBlock1Shared(): Unit =
    populate("001 ddd aaa bbb 0000", a => new ASR(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0001", a => new LSR(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0010", a => new LSL(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0011", a => new SLT(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0100", a => new SLTU(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0101", a => new ADC(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0110", a => new SBC(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1000", a => new DIVU(a('d'), a('a'), a('b')))

  /** RR Block 00 unary/binary register ops shared with TRISC16. */
  protected def populateRRBlock00Shared(): Unit =
    populate("110 aaa bbb 00 00000; b:1-7", a => new JALR(a('a'), a('b')))
    populate("110 000 000 00 00000", _ => HALT)
    populate("110 aaa bbb 00 00001", a => new ZEB(a('a'), a('b')))
    populate("110 aaa bbb 00 00100", a => new SEB(a('a'), a('b')))
    populate("110 aaa bbb 00 00111", a => new NEG(a('a'), a('b')))
    populate("110 aaa bbb 00 01000", a => new NOT(a('a'), a('b')))
    populate("110 aaa bbb 00 10001", a => new CLZ(a('a'), a('b')))
    populate("110 aaa bbb 00 10010", a => new CTZ(a('a'), a('b')))
    populate("110 aaa bbb 00 10011", a => new CHK(a('a'), a('b')))
    populate("110 aaa bbb 00 10100", a => new BTST(a('a'), a('b')))
    populate("110 aaa bbb 00 10101", a => new BSET(a('a'), a('b')))
    populate("110 aaa bbb 00 10110", a => new BCLR(a('a'), a('b')))
    populate("110 aaa bbb 00 10111", a => new ROL(a('a'), a('b')))
    populate("110 aaa bbb 00 11000", a => new ROR(a('a'), a('b')))
    populate("110 aaa bbb 00 11001", a => new CNT(a('a'), a('b')))
    populate("110 aaa bbb 00 11010", a => new REV(a('a'), a('b')))
    populate("110 aaa bbb 00 11011", a => new SEXT(a('a'), a('b')))
    populate("110 aaa bbb 00 11100", a => new MOV(a('a'), a('b')))
    populate("110 aaa bbb 00 11101", a => new MIN(a('a'), a('b')))
    populate("110 aaa bbb 00 11110", a => new MAX(a('a'), a('b')))
    populate("110 aaa bbb 00 11111", a => new EXG(a('a'), a('b')))

  /** RR Block 01 multiply-high + remainder — shared with TRISC16
    * (16-bit destructive ra = high16(...) etc. via writeMask). */
  protected def populateRRBlock01Shared(): Unit =
    populate("110 aaa bbb 01 00000", a => new MULH(a('a'), a('b')))
    populate("110 aaa bbb 01 01001", a => new MULHU(a('a'), a('b')))
    populate("110 aaa bbb 01 01010", a => new MULHSU(a('a'), a('b')))
    populate("110 aaa bbb 01 01011", a => new REM(a('a'), a('b')))
    populate("110 aaa bbb 01 01100", a => new REMU(a('a'), a('b')))

  /** RR ld/st with 5-bit immediate — same encoding on both; size and scaling
    * via the `ldRead`/`stWrite`/`auipcOffset` CPU seams. */
  protected def populateRRLoadStore(): Unit =
    populate("110 aaa bbb 10 iiiii", a => new LD(a('a'), a('b'), a('i')))
    populate("110 aaa bbb 11 iiiii", a => new ST(a('a'), a('b'), a('i')))

  /** R format byte/short push/pop — shared. */
  protected def populateRPushPopShared(): Unit =
    populate("111 000 rrr 0000000", o => new PSHB(o('r')))
    populate("111 000 rrr 0000001", o => new POPB(o('r')))
    populate("111 000 rrr 0000010", o => new PSHS(o('r')))
    populate("111 000 rrr 0000011", o => new POPS(o('r')))

  /** R format spsr/gpsr — shared. spsr gates on Status.Mode internally,
    * which TRISC16 keeps set, so the same class works on both. */
  protected def populateRPSRShared(): Unit =
    populate("111 000 rrr 0001000", o => new SPSR(o('r')))
    populate("111 000 rrr 0001001", o => new GPSR(o('r')))

  /** R format fence/trapv — shared. */
  protected def populateRFenceTrapv(): Unit =
    populate("111 000 000 0001011", _ => FENCE)
    populate("111 000 000 0001111", _ => TRAPV)

  /** Software traps — same encoding on both. */
  protected def populateTraps(): Unit =
    populate("111 000 rrr 0011 iii", o => new TRAP(o('i')))

  // ----- TRISC-only chunks (Trisc16Decode does not call these) -------------

  /** RRR Block 0 wide load/store: ldw/stw (32-bit), ldd/std (64-bit). */
  protected def populateRRRBlock0Wide(): Unit =
    populate("000 ddd aaa bbb 0100", a => new LDW(a('d'), a('a'), a('b')))
    populate("000 aaa bbb ccc 0101", a => new STW(a('a'), a('b'), a('c')))
    populate("000 ddd aaa bbb 0110", a => new LDD(a('d'), a('a'), a('b')))
    populate("000 aaa bbb ccc 0111", a => new STD(a('a'), a('b'), a('c')))

  /** RRR Block 0 atomic compare-and-swap. */
  protected def populateRRRBlock0Atomic(): Unit =
    populate("000 ddd aaa bbb 1100", a => new CAS(a('d'), a('a'), a('b')))

  /** RRR Block 1 floating-point arithmetic. */
  protected def populateRRRBlock1Float(): Unit =
    populate("001 ddd aaa bbb 1010", a => new FSLT(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1011", a => new FADD(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1100", a => new FSUB(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1101", a => new FMUL(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1110", a => new FDIV(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1111", a => new FSEQ(a('d'), a('a'), a('b')))

  /** RR Block 00 wider zero/sign extends (32/16-bit zes/zew/ses/sew). */
  protected def populateRRBlock00WideExt(): Unit =
    populate("110 aaa bbb 00 00010", a => new ZES(a('a'), a('b')))
    populate("110 aaa bbb 00 00011", a => new ZEW(a('a'), a('b')))
    populate("110 aaa bbb 00 00101", a => new SES(a('a'), a('b')))
    populate("110 aaa bbb 00 00110", a => new SEW(a('a'), a('b')))

  /** RR Block 00 floating-point unary ops. */
  protected def populateRRBlock00Float(): Unit =
    populate("110 aaa bbb 00 01001", a => new CVT(a('a'), a('b')))
    populate("110 aaa bbb 00 01010", a => new FNEG(a('a'), a('b')))
    populate("110 aaa bbb 00 01100", a => new FINT(a('a'), a('b')))
    populate("110 aaa bbb 00 01101", a => new FSQRT(a('a'), a('b')))
    populate("110 aaa bbb 00 01110", a => new FABS(a('a'), a('b')))

  /** RR Block 00 LL/SC atomic primitives. */
  protected def populateRRBlock00Atomic(): Unit =
    populate("110 aaa bbb 00 01111", a => new LL(a('a'), a('b')))
    populate("110 aaa bbb 00 10000", a => new SC(a('a'), a('b')))

  /** RR Block 01 single↔double conversion. */
  protected def populateRRBlock01FloatConv(): Unit =
    populate("110 aaa bbb 01 10010", a => new F32TOF64(a('a'), a('b')))
    populate("110 aaa bbb 01 10011", a => new F64TOF32(a('a'), a('b')))

  /** RR Block 01 MMU instructions. */
  protected def populateRRBlock01Mmu(): Unit =
    populate("110 aaa bbb 01 00001", a => new TLBI(a('a'), a('b')))
    populate("110 aaa bbb 01 00010", a => new TLBIA(a('a'), a('b')))
    populate("110 aaa bbb 01 00011", a => new SPTBR(a('a'), a('b')))
    populate("110 aaa bbb 01 00100", a => new GPTBR(a('a'), a('b')))
    populate("110 aaa bbb 01 00101", a => new GFAULT(a('a'), a('b')))
    populate("110 aaa bbb 01 00110", a => new SASID(a('a'), a('b')))
    populate("110 aaa bbb 01 00111", a => new GASID(a('a'), a('b')))
    populate("110 aaa bbb 01 01000", a => new GFCAUSE(a('a'), a('b')))

  /** R format word/double push/pop — TRISC-only (TRISC16 has no 32/64-bit type). */
  protected def populateRPushPopWide(): Unit =
    populate("111 000 rrr 0000100", o => new PSHW(o('r')))
    populate("111 000 rrr 0000101", o => new POPW(o('r')))
    populate("111 000 rrr 0000110", o => new PSHD(o('r')))
    populate("111 000 rrr 0000111", o => new POPD(o('r')))

  /** RTE — TRISC's stack-popping version. TRISC16 overrides with `Trisc16RTE`. */
  protected def populateRRte(): Unit =
    populate("111 000 000 0001010", _ => RTE)

  /** wfi — supervisor, TRISC-only. TRISC16 reuses this slot for `gepc`. */
  protected def populateRWfi(): Unit =
    populate("111 000 000 0001100", _ => WFI)

  /** gusp/susp — TRISC-only USP access. TRISC16 reuses `0001101` for `gcause`. */
  protected def populateRStackSwap(): Unit =
    populate("111 000 rrr 0001101", o => new GUSP(o('r')))
    populate("111 000 rrr 0001110", o => new SUSP(o('r')))

  /** R format pshr/popr — TRISC's 8-byte-per-register variant. TRISC16
    * overrides with 2-byte versions. */
  protected def populateRPshrPopr(): Unit =
    populate("111 000 rrr 0010000; r:1-6", o => new PSHR(o('r')))
    populate("111 000 rrr 0010001; r:1-6", o => new POPR(o('r')))

  /** cli/sti/swsp — supervisor interrupt + stack-pointer-swap, TRISC-only. */
  protected def populateRInterruptCtrl(): Unit =
    populate("111 000 000 0010010", _ => CLI)
    populate("111 000 000 0010011", _ => STI)
    populate("111 000 000 0010100", _ => SWSP)

  /** tsr — read cycle counter, TRISC-only. */
  protected def populateRTimer(): Unit =
    populate("111 000 rrr 0010101", o => new TSR(o('r')))

/** Singleton TRISC decoder. The full instruction table is built by the parent
  * `class Decode` constructor. */
object Decode extends Decode
