package io.github.edadma.trisc

/** TRISC16: a 16-bit subset of TRISC.
  *
  * Same instruction encoding and (mostly) semantics as TRISC, but with 16-bit
  * registers, a 16-bit address space, ×2 scaling for AUIPC/LD/ST, a 4-byte
  * boot vector (SP@0, PC@2), and a simplified exception model (EPC/ECAUSE,
  * fixed handler at 0x4).
  *
  * The PSR layout matches TRISC bit-for-bit (see specs/exceptions.md and
  * cpu/TRISC16.md). Bits Ind (0) and T (4) are reserved-as-zero on TRISC16
  * because TRISC16 has no interrupts and no trace exception. Mode (1), C (2),
  * and V (5) carry their TRISC meanings; TRISC16 keeps Mode set throughout
  * normal operation, since TRISC16 has no real user/supervisor split — every
  * instruction that gates on `Status.Mode` (HALT, SPSR, RTE) just works.
  *
  * The "currently inside an exception handler" state used to detect nested
  * faults lives outside PSR, in the public `inHandler` boolean. Set on
  * exception entry, cleared by `rte`.
  *
  * See cpu/TRISC16.md for the full ISA spec.
  */
class Trisc16CPU(
    mem: Addressable,
    tick: Seq[Processor => Unit] = Nil,
) extends CPU(mem, tick):

  /** TRISC16 internal exception registers. */
  var epc: Long = 0
  var ecause: Int = 0

  /** True while a handler is executing (between exception entry and `rte`).
    * A second exception fired in this window is delivered as `DoubleFault`. */
  var inHandler: Boolean = false

  private val Trisc16HandlerVector: Long = 0x4L

  override protected def writeMask: Long = 0xffffL

  override protected def decode(inst: Int): Instruction = Trisc16Decode(inst)

  override def auipcOffset(imm: Int): Long = imm.toLong << 1

  override def ldRead(addr: Long): Long = readShort(addr).toLong & 0xffffL

  override def stWrite(addr: Long, v: Long): Unit = writeShort(addr, v)

  /** Map a TRISC State to a TRISC16 cause code, per the spec table. */
  private def causeCode(s: State): Int = s match
    case State.UnimplementedOpcode  => 1
    case State.PrivilegeViolation   => 1
    case State.MisalignedAccess     => 2
    case State.IllegalDivide        => 3
    case State.Overflow             => 4
    case State.BoundsCheck          => 4
    case State.Trap0                => 8
    case State.Trap1                => 9
    case State.Trap2                => 10
    case State.Trap3                => 11
    case State.Trap4                => 12
    case State.Trap5                => 13
    case State.Trap6                => 14
    case State.Trap7                => 15
    case _                          => 1

  override protected def enterException(): Unit =
    if state == State.Reset then
      // Boot vector: SP@0x0, PC@0x2 (16-bit each).
      r(7).write(mem.readShortUnsigned(0))
      pc = mem.readShortUnsigned(2).toLong
      // CPU.reset() set Status.Mode and Status.Ind. Clear Ind (TRISC16 has no
      // interrupts, the bit is reserved-as-zero); leave Mode set so HALT/SPSR/RTE
      // — which gate on Status.Mode internally — keep working.
      set(Status.Ind, false)
      inHandler = false
      state = State.Run
      clearReservation()
      return

    if inHandler then
      // Already in handler — TRISC16 has no nested-exception model.
      state = State.DoubleFault
      return

    // PC advanced by 2 in execute() before all software-set states except
    // InstructionAccess (which fires during fetch, before the increment).
    epc = if state == State.InstructionAccess then pc else pc - 2
    ecause = causeCode(state)
    inHandler = true
    // Mirror TRISC's exception entry: re-enter supervisor mode so the handler
    // can run privileged ops (HALT/RTE/SPSR) regardless of what user code
    // had set Mode to. TRISC pops the saved PSR on RTE; TRISC16 doesn't
    // restore Mode (no shadow PSR), so post-RTE code runs with whatever
    // Mode the handler last left.
    set(Status.Mode, true)
    pc = Trisc16HandlerVector
    state = State.Run
    clearReservation()

/** TRISC16 decoder: inherits TRISC's shared populate chunks, omits TRISC-only
  * encodings (FP, MMU, atomics, 32/64-bit ld/st, supervisor stack/timer ops),
  * and substitutes TRISC16-specific implementations for `rte`, `pshr`, `popr`,
  * plus its own `gepc`/`gcause` (which sit in TRISC's `wfi`/`gusp` slots —
  * the one place TRISC16's encoding deviates from TRISC). */
object Trisc16Decode extends Decode:

  override protected def buildInstructionTable(): Unit =
    populateRI()
    populateAuipc()
    populateBranchesAndAddi()
    populateRRRBlock0Shared()
    populateRRRBlock1Shared()
    populateRRBlock00Shared()
    populateRRBlock01Shared()
    populateRRLoadStore()
    populateRPushPopShared()
    populateRPSRShared()
    populateRFenceTrapv()
    populateTraps()
    populateTrisc16Rte()
    populateTrisc16PshrPopr()
    populateTrisc16EpcCause()

  /** TRISC16's RTE: PC ← EPC, inHandler ← false. No PSR pop (no shadow PSR). */
  protected def populateTrisc16Rte(): Unit =
    populate("111 000 000 0001010", _ => Trisc16RTE)

  /** TRISC16's pshr/popr push/pop 2 bytes per register (TRISC pushes 8). */
  protected def populateTrisc16PshrPopr(): Unit =
    populate("111 000 rrr 0010000; r:1-6", o => new Trisc16PSHR(o('r')))
    populate("111 000 rrr 0010001; r:1-6", o => new Trisc16POPR(o('r')))

  /** TRISC16-specific: gepc and gcause read the dedicated EPC/ECAUSE
    * registers. These slots host wfi/gusp on TRISC. */
  protected def populateTrisc16EpcCause(): Unit =
    populate("111 000 rrr 0001100", o => new Trisc16GEPC(o('r')))
    populate("111 000 rrr 0001101", o => new Trisc16GCAUSE(o('r')))

/** TRISC16 RTE: PC ← EPC, clear `inHandler`. */
object Trisc16RTE extends SimpleInstruction:
  val mnemonic = "rte"

  def apply(cpu: CPU): Unit = cpu match
    case t: Trisc16CPU =>
      // Mirrors TRISC's privilege check on RTE: only legal in supervisor mode.
      // TRISC16 keeps Status.Mode set, so this normally succeeds; user code
      // that has cleared Mode via `spsr` will trap.
      if !t.test(Status.Mode) then t.state = State.PrivilegeViolation
      else
        t.pc = t.epc
        t.inHandler = false
    case _ => cpu.state = State.UnimplementedOpcode

class Trisc16GEPC(r: Int) extends RInstruction(r):
  val mnemonic = "gepc"

  def apply(cpu: CPU): Unit = cpu match
    case t: Trisc16CPU => t.r(r).write(t.epc)
    case _             => cpu.state = State.UnimplementedOpcode

class Trisc16GCAUSE(r: Int) extends RInstruction(r):
  val mnemonic = "gcause"

  def apply(cpu: CPU): Unit = cpu match
    case t: Trisc16CPU => t.r(r).write(t.ecause.toLong)
    case _             => cpu.state = State.UnimplementedOpcode

/** TRISC16 multi-register push: 2 bytes per register (vs TRISC's 8). */
class Trisc16PSHR(r: Int) extends Instruction:
  val mnemonic = "pshr"

  def disassemble(cpu: CPU): String = s"$mnemonic r$r"

  def apply(cpu: CPU): Unit =
    for i <- 1 to r do
      cpu.r(7).write(cpu.r(7).read - 2)
      cpu.writeShort(cpu.r(7).read, cpu.r(i).read)

class Trisc16POPR(r: Int) extends Instruction:
  val mnemonic = "popr"

  def disassemble(cpu: CPU): String = s"$mnemonic r$r"

  def apply(cpu: CPU): Unit =
    for i <- r to 1 by -1 do
      cpu.r(i).write(cpu.readShort(cpu.r(7).read))
      cpu.r(7).write(cpu.r(7).read + 2)
