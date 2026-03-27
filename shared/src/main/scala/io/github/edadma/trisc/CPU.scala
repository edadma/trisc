package io.github.edadma.trisc

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

enum Status(val bit: Int):
  case Ind extends Status(1)
  case Mode extends Status(2)
  case C extends Status(4)
  case Irq extends Status(8)
  case T extends Status(16)
  case V extends Status(32)

enum State:
  case Reset, Interrupt, InstructionAccess, DataAccess, MisalignedAccess,
    UnimplementedOpcode, PrivilegeViolation, IllegalDivide,
    Trap0, Trap1, Trap2, Trap3, Trap4, Trap5, Trap6, Trap7,
    Trace, Overflow, BoundsCheck,
    Halt, Run, Wfi, DoubleFault

class CPU(mem: Addressable, interrupts: Seq[CPU => Unit]) extends Addressable:
  val name: String = mem.name
  val base: Long = mem.base
  val size: Long = mem.size

  def readByte(addr: Long): Int = mem.readByte(addr)

  def writeByte(addr: Long, data: Long): Unit = mem.writeByte(addr, data)

  def loadByte(addr: Long, data: Long): Unit = mem.loadByte(addr, data)

  private def checkAlign(addr: Long, align: Int): Boolean =
    if (addr & (align - 1)) != 0 then
      state = State.MisalignedAccess
      true
    else false

  override def readShort(addr: Long): Int =
    if checkAlign(addr, 2) then 0 else mem.readShort(addr)

  override def readInt(addr: Long): Int =
    if checkAlign(addr, 4) then 0 else mem.readInt(addr)

  override def readLong(addr: Long): Long =
    if checkAlign(addr, 8) then 0 else mem.readLong(addr)

  override def writeShort(addr: Long, data: Long): Unit =
    if !checkAlign(addr, 2) then mem.writeShort(addr, data)

  override def writeInt(addr: Long, data: Long): Unit =
    if !checkAlign(addr, 4) then mem.writeInt(addr, data)

  override def writeLong(addr: Long, data: Long): Unit =
    if !checkAlign(addr, 8) then mem.writeLong(addr, data)

  val r = immutable.ArraySeq(
    new Reg0,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
    new Reg,
  )
  var pc: Long = 0
  var psr: Int = 0
  var usp: Long = 0
  var state: State = State.Halt
  var reservationAddr: Long = 0
  var reservationValid: Boolean = false
  private var inException: Boolean = false

  var limit: Int = -1
  var clump: Int = 1000
  var trace: Boolean = false

  def test(status: Status): Boolean = (psr & status.bit) != 0

  def set(status: Status, set: Boolean): Unit = if set then psr |= status.bit else psr &= ~status.bit

  def reset(): Unit =
    for i <- 1 until 8 do r(i).write(0)

    usp = 0
    inException = false
    state = State.Reset
    set(Status.Ind, true)
    set(Status.Mode, true)
    set(Status.C, false)
    set(Status.T, false)
    set(Status.V, false)

  def interrupt(): Unit =
    set(Status.Irq, true)

    if !test(Status.Ind) then state = State.Interrupt

  private def enterException(): Unit =
    if inException then
      state = State.DoubleFault
      return

    inException = true

    try
      if state == State.Reset then
        // Reset is special (like 68000): load SSP from vector 0, PC from vector 1
        r(7).write(mem.readLong(0))
        pc = mem.readLong(8)
        state = State.Run
        set(Status.Mode, true)
        set(Status.Ind, true)
        reservationValid = false
      else
        // Swap r7 <-> usp if coming from user mode
        if !test(Status.Mode) then
          val tmp = r(7).read
          r(7).write(usp)
          usp = tmp

        // Push PSR then PC onto supervisor stack (8 bytes each)
        r(7).write(r(7).read - 8)
        mem.writeLong(r(7).read, psr)
        r(7).write(r(7).read - 8)
        mem.writeLong(r(7).read, pc)

        // Load PC from vector table (offset by 1 since reset occupies slots 0 and 1)
        pc = mem.readLong((state.ordinal + 1) * 8)
        state = State.Run
        set(Status.Mode, true)
        set(Status.Ind, true)
        set(Status.T, false)
        reservationValid = false
    catch
      case _: RuntimeException =>
        state = State.DoubleFault
    finally
      inException = false

  def execute(): Unit =
    if state.ordinal < State.Halt.ordinal then
      enterException()
      if state == State.DoubleFault then return

    val inst =
      try readShortUnsigned(pc)
      catch
        case _: RuntimeException =>
          state = State.InstructionAccess
          return

    val decoded = Decode(inst)

    if trace then println(f"$pc%04x: $inst%04x  ${decoded.disassemble(this)}")

    pc += 2

    // Capture T state before instruction — trace fires based on T at start of instruction (like 68k)
    val traceEnabled = test(Status.T)

    try decoded(this)
    catch
      case _: RuntimeException =>
        if state == State.Run then state = State.DataAccess

    // Trace exception: fires after instruction completes if T was set BEFORE it executed
    if state == State.Run && traceEnabled then state = State.Trace

    if trace then
      for i <- 1 to 7 do print(f"  r$i:${r(i).read}%04x")
      println

  @tailrec
  final def run(): Unit =
    var count = 0

    while state != State.Halt && state != State.Wfi && state != State.DoubleFault && count < clump do
      execute()
      count += 1

    if limit > 0 then limit -= 1

    if state != State.Halt && state != State.DoubleFault && limit != 0 then
      interrupts foreach (_(this))
      run()

  def resume(): Unit =
    state = State.Run
    run()

  class Reg:
    private var r: Long = 0

    def read: Long = r

    def readf: Double = java.lang.Double.longBitsToDouble(r)

    def write(v: Long): Unit = r = v

    def write(v: Double): Unit = r = java.lang.Double.doubleToLongBits(v)

  class Reg0 extends Reg:
    override def read: Long = 0

    override def readf: Double = 0

    override def write(v: Long): Unit = {}

    override def write(v: Double): Unit = {}

object Decode:
  private val instructions = Array.fill[Instruction](0x10000)(IllegalInstruction)

  buildInstructionTable()

  def apply(inst: Int): Instruction = instructions(inst)

  private def populate(pattern: String, inst: Map[Char, Int] => Instruction) =
    for ((idx, m) <- generate(pattern))
      instructions(idx) = inst(m)

  private def populate(insts: List[(String, Map[Char, Int] => Instruction)]): Unit =
    for ((p, c) <- insts)
      populate(p, c)

  def buildInstructionTable(): Unit =
    populate(
      List[(String, Map[Char, Int] => Instruction)](
        "111 rrr 00 iiiiiiii; r:1-7" -> ((operands: Map[Char, Int]) => new LDI(operands('r'), operands('i'))),
        "111 rrr 10 iiiiiiii; r:1-7" -> ((operands: Map[Char, Int]) => new SLI(operands('r'), operands('i'))),
        "111 rrr 11 iiiiiiii; r:1-7" -> ((operands: Map[Char, Int]) => new STI(operands('r'), operands('i'))),
        "110 000 000 01 iiiii" -> ((operands: Map[Char, Int]) => new TRAP(operands('i'))),
        "110 aaa bbb 00 00000; b:1-7" -> ((args: Map[Char, Int]) => new JALR(args('a'), args('b'))),
        "110 000 000 00 00000" -> (_ => HALT),
        "110 aaa bbb 00 00001" -> ((args: Map[Char, Int]) => new ZEB(args('a'), args('b'))),
        "110 aaa bbb 00 00010" -> ((args: Map[Char, Int]) => new ZES(args('a'), args('b'))),
        "110 aaa bbb 00 00011" -> ((args: Map[Char, Int]) => new ZEW(args('a'), args('b'))),
        "110 aaa bbb 00 00100" -> ((args: Map[Char, Int]) => new SEB(args('a'), args('b'))),
        "110 aaa bbb 00 00101" -> ((args: Map[Char, Int]) => new SES(args('a'), args('b'))),
        "110 aaa bbb 00 00110" -> ((args: Map[Char, Int]) => new SEW(args('a'), args('b'))),
        "110 aaa bbb 00 00111" -> ((args: Map[Char, Int]) => new NEG(args('a'), args('b'))),
        "110 aaa bbb 00 01000" -> ((args: Map[Char, Int]) => new NOT(args('a'), args('b'))),
        "110 aaa bbb 00 01010" -> ((args: Map[Char, Int]) => new FNEG(args('a'), args('b'))),
        "110 aaa bbb 00 01011" -> ((args: Map[Char, Int]) => new FINV(args('a'), args('b'))),
        "110 aaa bbb 00 01001" -> ((args: Map[Char, Int]) => new CVT(args('a'), args('b'))),
        "110 aaa bbb 00 01100" -> ((args: Map[Char, Int]) => new FINT(args('a'), args('b'))),
        "110 aaa bbb 00 01101" -> ((args: Map[Char, Int]) => new FSQRT(args('a'), args('b'))),
        "110 aaa bbb 00 01110" -> ((args: Map[Char, Int]) => new FABS(args('a'), args('b'))),
        "110 aaa bbb 00 01111" -> ((args: Map[Char, Int]) => new LL(args('a'), args('b'))),
        "110 aaa bbb 00 10000" -> ((args: Map[Char, Int]) => new SC(args('a'), args('b'))),
        "110 aaa bbb 00 10001" -> ((args: Map[Char, Int]) => new CLZ(args('a'), args('b'))),
        "110 aaa bbb 00 10010" -> ((args: Map[Char, Int]) => new CTZ(args('a'), args('b'))),
        "110 aaa bbb 00 10011" -> ((args: Map[Char, Int]) => new CHK(args('a'), args('b'))),
        "110 aaa bbb 10 iiiii" -> ((args: Map[Char, Int]) => new LD(args('a'), args('b'), args('i'))),
        "110 aaa bbb 11 iiiii" -> ((args: Map[Char, Int]) => new ST(args('a'), args('b'), args('i'))),
        "111 000 rrr 0000000" -> ((operands: Map[Char, Int]) => new PSHB(operands('r'))),
        "111 000 rrr 0000001" -> ((operands: Map[Char, Int]) => new POPB(operands('r'))),
        "111 000 rrr 0000010" -> ((operands: Map[Char, Int]) => new PSHS(operands('r'))),
        "111 000 rrr 0000011" -> ((operands: Map[Char, Int]) => new POPS(operands('r'))),
        "111 000 rrr 0000100" -> ((operands: Map[Char, Int]) => new PSHW(operands('r'))),
        "111 000 rrr 0000101" -> ((operands: Map[Char, Int]) => new POPW(operands('r'))),
        "111 000 rrr 0000110" -> ((operands: Map[Char, Int]) => new PSHD(operands('r'))),
        "111 000 rrr 0000111" -> ((operands: Map[Char, Int]) => new POPD(operands('r'))),
        "111 000 rrr 0001000" -> ((operands: Map[Char, Int]) => new SPSR(operands('r'))),
        "111 000 rrr 0001001" -> ((operands: Map[Char, Int]) => new GPSR(operands('r'))),
        "111 000 000 0001010" -> (_ => RTE),
        "111 000 000 0001011" -> (_ => FENCE),
        "111 000 000 0001100" -> (_ => WFI),
        "111 000 000 0001111" -> (_ => TRAPV),
        "111 000 rrr 0001101" -> ((operands: Map[Char, Int]) => new GUSP(operands('r'))),
        "111 000 rrr 0001110" -> ((operands: Map[Char, Int]) => new SUSP(operands('r'))),
        "101 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new ADDI(args('a'), args('b'), ext(args('i')))),
        "100 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new BLS(args('a'), args('b'), ext(args('i')))),
        "011 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new BLU(args('a'), args('b'), ext(args('i')))),
        "010 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new BEQ(args('a'), args('b'), ext(args('i')))),
        // 001 block: shifts, comparisons, carry, unsigned, float
        "001 ddd aaa bbb 0000" -> ((args: Map[Char, Int]) => new ASR(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0001" -> ((args: Map[Char, Int]) => new LSR(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0010" -> ((args: Map[Char, Int]) => new LSL(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0011" -> ((args: Map[Char, Int]) => new SLT(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0100" -> ((args: Map[Char, Int]) => new SLTU(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0101" -> ((args: Map[Char, Int]) => new ADC(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0110" -> ((args: Map[Char, Int]) => new SBC(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 0111" -> ((args: Map[Char, Int]) => new MULU(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1000" -> ((args: Map[Char, Int]) => new DIVU(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1001" -> ((args: Map[Char, Int]) => new REMU(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1010" -> ((args: Map[Char, Int]) => new FSLT(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1011" -> ((args: Map[Char, Int]) => new FADD(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1100" -> ((args: Map[Char, Int]) => new FSUB(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1101" -> ((args: Map[Char, Int]) => new FMUL(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1110" -> ((args: Map[Char, Int]) => new FDIV(args('d'), args('a'), args('b'))),
        "001 ddd aaa bbb 1111" -> ((args: Map[Char, Int]) => new FPOW(args('d'), args('a'), args('b'))),
        "111 rrr 01 iiiiiiii; r:1-7" -> ((operands: Map[Char, Int]) => new AUIPC(operands('r'), operands('i'))),
        "000 ddd aaa bbb 0000" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0001" -> ((args: Map[Char, Int]) => new STB(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 0010" -> ((args: Map[Char, Int]) => new LDS(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0011" -> ((args: Map[Char, Int]) => new STS(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 0100" -> ((args: Map[Char, Int]) => new LDW(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0101" -> ((args: Map[Char, Int]) => new STW(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 0110" -> ((args: Map[Char, Int]) => new LDD(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0111" -> ((args: Map[Char, Int]) => new STD(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 1000" -> ((args: Map[Char, Int]) => new ADD(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1001" -> ((args: Map[Char, Int]) => new SUB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1010" -> ((args: Map[Char, Int]) => new MUL(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1011" -> ((args: Map[Char, Int]) => new DIV(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1100" -> ((args: Map[Char, Int]) => new REM(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1101" -> ((args: Map[Char, Int]) => new AND(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1110" -> ((args: Map[Char, Int]) => new OR(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1111" -> ((args: Map[Char, Int]) => new XOR(args('d'), args('a'), args('b'))),
      ),
    )

  def ext(imm7: Int): Int = if (imm7 & 0x40) != 0 then imm7 | 0xffffff80 else imm7

  def generate(pattern: String) =
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
