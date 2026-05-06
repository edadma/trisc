package io.github.edadma.trisc

/** TRISC16: a 16-bit subset of TRISC.
  *
  * Same instruction encoding and (mostly) semantics as TRISC, but with 16-bit
  * registers, a 16-bit address space, ×2 scaling for AUIPC/LD/ST, a 4-byte
  * boot vector (SP@0, PC@2), and a simplified exception model (EPC/ECAUSE/PSR.E,
  * fixed handler at 0x4).
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

  /** Bit 0 of psr is PSR.E (in-exception), the only defined bit on TRISC16. */
  private val PsrE: Int = 1
  private val Trisc16HandlerVector: Long = 0x4L

  override protected def writeMask: Long = 0xffffL

  override protected def decode(inst: Int): Instruction = Trisc16Decode(inst)

  override def auipcOffset(imm: Int): Long = imm.toLong << 1

  override def ldRead(addr: Long): Long = readShort(addr).toLong & 0xffffL

  override def stWrite(addr: Long, v: Long): Unit = writeShort(addr, v)

  /** Map a TRISC State to a TRISC16 cause code, per the spec table. */
  private def causeCode(s: State): Int = s match
    case State.UnimplementedOpcode  => 1
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
      // Keep Status.Mode set (CPU.reset() set it) so shared TRISC instructions
      // that gate on it (HALT, RTE, etc.) continue to work in our flat model.
      // Clear only PSR.E.
      psr = psr & ~PsrE
      ecause = 0
      epc = 0
      state = State.Run
      clearReservation()
      return

    if (psr & PsrE) != 0 then
      // Already in handler — TRISC16 has no nested-exception model.
      state = State.DoubleFault
      return

    // PC advanced by 2 in execute() before all software-set states except
    // InstructionAccess (which fires during fetch, before the increment).
    epc = if state == State.InstructionAccess then pc else pc - 2
    ecause = causeCode(state)
    psr = psr | PsrE
    pc = Trisc16HandlerVector
    state = State.Run
    clearReservation()

object Trisc16Decode:
  private val instructions = Array.fill[Instruction](0x10000)(IllegalInstruction)

  buildInstructionTable()

  def apply(inst: Int): Instruction = instructions(inst)

  private def populate(pattern: String, inst: Map[Char, Int] => Instruction): Unit =
    for ((idx, m) <- Decode.generate(pattern))
      instructions(idx) = inst(m)

  private def buildInstructionTable(): Unit =
    val ext = Decode.ext

    // RI — `111 rrr oo iiiiiiii`
    populate("111 rrr 00 iiiiiiii; r:1-7", ops => new LDI(ops('r'), ops('i')))
    populate("111 rrr 01 iiiiiiii; r:1-7", ops => new AUIPC(ops('r'), ops('i')))
    populate("111 rrr 10 iiiiiiii; r:1-7", ops => new SLI(ops('r'), ops('i')))
    populate("111 rrr 11 iiiiiiii; r:1-7", ops => new STI(ops('r'), ops('i')))

    // RRI — branches and addi
    populate("010 aaa bbb iiiiiii", a => new BEQ(a('a'), a('b'), ext(a('i'))))
    populate("011 aaa bbb iiiiiii", a => new BLU(a('a'), a('b'), ext(a('i'))))
    populate("100 aaa bbb iiiiiii", a => new BLS(a('a'), a('b'), ext(a('i'))))
    populate("101 aaa bbb iiiiiii", a => new ADDI(a('a'), a('b'), ext(a('i'))))

    // RRR Block 0 — load/store/arithmetic/logic (no 32/64-bit ld/st, no cas)
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

    // RRR Block 1 — shifts, comparisons, carry (no float, no cas, no fp)
    populate("001 ddd aaa bbb 0000", a => new ASR(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0001", a => new LSR(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0010", a => new LSL(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0011", a => new SLT(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0100", a => new SLTU(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0101", a => new ADC(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 0110", a => new SBC(a('d'), a('a'), a('b')))
    populate("001 ddd aaa bbb 1000", a => new DIVU(a('d'), a('a'), a('b')))

    // RR Block 00 — unary/binary register ops (no fp/atomic/mmu)
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

    // RR Block 01 — multi-limb high + remainder (destructive)
    populate("110 aaa bbb 01 00000", a => new MULH(a('a'), a('b')))
    populate("110 aaa bbb 01 01001", a => new MULHU(a('a'), a('b')))
    populate("110 aaa bbb 01 01010", a => new MULHSU(a('a'), a('b')))
    populate("110 aaa bbb 01 01011", a => new REM(a('a'), a('b')))
    populate("110 aaa bbb 01 01100", a => new REMU(a('a'), a('b')))

    // RR load/store — 16-bit (overridden ldRead/stWrite handle the size)
    populate("110 aaa bbb 10 iiiii", a => new LD(a('a'), a('b'), a('i')))
    populate("110 aaa bbb 11 iiiii", a => new ST(a('a'), a('b'), a('i')))

    // R format — push/pop, exception control, traps
    populate("111 000 rrr 0000000", o => new PSHB(o('r')))
    populate("111 000 rrr 0000001", o => new POPB(o('r')))
    populate("111 000 rrr 0000010", o => new PSHS(o('r')))
    populate("111 000 rrr 0000011", o => new POPS(o('r')))
    populate("111 000 rrr 0001001", o => new GPSR(o('r')))
    populate("111 000 000 0001010", _ => Trisc16RTE)
    populate("111 000 000 0001011", _ => FENCE)
    populate("111 000 rrr 0001100", o => new Trisc16GEPC(o('r')))
    populate("111 000 rrr 0001101", o => new Trisc16GCAUSE(o('r')))
    populate("111 000 000 0001111", _ => TRAPV)
    populate("111 000 rrr 0010000; r:1-6", o => new Trisc16PSHR(o('r')))
    populate("111 000 rrr 0010001; r:1-6", o => new Trisc16POPR(o('r')))

    // Software traps — same encoding as TRISC
    populate("111 000 rrr 0011 iii", o => new TRAP(o('i')))

/** TRISC16 RTE: PC ← EPC, PSR.E ← 0. */
object Trisc16RTE extends SimpleInstruction:
  val mnemonic = "rte"

  def apply(cpu: CPU): Unit = cpu match
    case t: Trisc16CPU =>
      t.pc = t.epc
      t.psr = t.psr & ~1
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
