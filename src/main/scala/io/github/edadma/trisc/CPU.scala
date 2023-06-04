package io.github.edadma.trisc

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

enum Status(val bit: Int):
  case Ind extends Status(1)
  case Mode extends Status(2)
  case C extends Status(4)
  case Irq extends Status(8)

enum State:
  case Reset, Interrupt, DivisionByZero,
    Trap0, Trap1, Trap2, Trap3, Trap4, Trap5, Trap6, Trap7,
    Halt, Run

class CPU(mem: Addressable, interrupts: Seq[CPU => Unit]) extends Addressable:
  val name: String = mem.name
  val base: Long = mem.base
  val size: Long = mem.size

  def readByte(addr: Long): Int = mem.readByte(addr)

  def writeByte(addr: Long, data: Long): Unit = mem.writeByte(addr, data)

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
  var sr = new Array[Long](8)
  var pc: Long = 0
  var spc: Long = 0
  var psr: Int = 0
  var spsr: Int = 0
  var state: State = State.Halt

  var limit: Int = -1
  var clump: Int = 1000
  var trace: Boolean = false

  def test(status: Status): Boolean = (psr & status.bit) != 0

  def set(status: Status, set: Boolean): Unit = if set then psr |= status.bit else psr &= ~status.bit

  def reset(): Unit =
    for i <- 1 until 8 do r(i) write 0

    state = State.Reset
    set(Status.Ind, true)
    set(Status.Mode, true)
    set(Status.C, false)

  def interrupt(): Unit =
    set(Status.Irq, true)

    if !test(Status.Ind) then state = State.Interrupt

  def execute(): Unit =
    if state.ordinal < State.Halt.ordinal then
      for i <- 1 to 7 do sr(i) = r(i).read
      spc = pc
      spsr = psr
      pc = readInt(state.ordinal * 4)
      state = State.Run
      set(Status.Mode, true)

    val inst = readShortUnsigned(pc)
    val decoded = Decode(inst)

    if trace then println(f"$pc%04x: $inst%04x  ${decoded.disassemble(this)}")

    pc += 2
    decoded(this)

    if trace then
      for i <- 1 to 7 do print(f"  r$i:${r(i).read}%04x")
      println

  @tailrec
  final def run(): Unit =
    var count = 0

    while state != State.Halt && count < clump do
      execute()
      count += 1

    if limit > 0 then limit -= 1

    if state != State.Halt && limit != 0 then
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
        "110 aaa bbb 10 iiiii" -> ((args: Map[Char, Int]) => new LD(args('a'), args('b'), args('i'))),
        "110 aaa bbb 11 iiiii" -> ((args: Map[Char, Int]) => new ST(args('a'), args('b'), args('i'))),
        "111 000 rrr 0001000" -> ((operands: Map[Char, Int]) => new SPSR(operands('r'))),
        "111 000 rrr 0001001" -> ((operands: Map[Char, Int]) => new GPSR(operands('r'))),
        "111 000 000 0001010" -> (_ => RTE),
        "101 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new ADDI(args('a'), args('b'), ext(args('i')))),
        "100 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new BLS(args('a'), args('b'), ext(args('i')))),
        "010 aaa bbb iiiiiii" -> ((args: Map[Char, Int]) => new BEQ(args('a'), args('b'), ext(args('i')))),
        "000 ddd aaa bbb 0000" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0001" -> ((args: Map[Char, Int]) => new STB(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 0010" -> ((args: Map[Char, Int]) => new LDS(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0011" -> ((args: Map[Char, Int]) => new STS(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 0100" -> ((args: Map[Char, Int]) => new LDW(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0101" -> ((args: Map[Char, Int]) => new STW(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 0110" -> ((args: Map[Char, Int]) => new LDD(args('d'), args('a'), args('b'))),
        "000 aaa bbb ccc 0111" -> ((args: Map[Char, Int]) => new STD(args('a'), args('b'), args('c'))),
        "000 ddd aaa bbb 1000" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1001" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1010" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1011" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1100" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1101" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1110" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
        "000 ddd aaa bbb 1111" -> ((args: Map[Char, Int]) => new LDB(args('d'), args('a'), args('b'))),
      ),
    )

  def ext(imm7: Int): Int = if (imm7 & 0x40) != 0 then imm7 | 0xffffff80 else imm7

  def generate(pattern: String) =
    case class Variable(v: Char, lower: Int, upper: Int, bits: List[Int])

    val Range = "([a-zA-Z]):([0-9]+)-([0-9]+)".r
    val p = pattern replace (" ", "") split ";"

    require(p.nonEmpty, "empty pattern")

    val bits = p(0)

    require(bits.length > 0, "pattern should comprise at least one bit")
    require(
      bits.forall(c => c == '0' || c == '1' || c.isLetter || c == '-'),
      "pattern should comprise only 0's, 1's, letters or -'s",
    )

    val ranges = Map(p drop 1 map { case Range(v, l, u) => v(0) -> (l.toInt, u.toInt) }: _*)

    require(
      ranges forall { case (_, (l, u)) => 0 <= l && l <= u },
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
