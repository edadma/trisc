package io.github.edadma.trisc

import scala.collection.immutable
import scala.collection.mutable.ListBuffer

class CPU(mem: Addressable):
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
  var pc = 0
  var status: Int = 0
  var running: Boolean = false
  var vector: Int = -1
  var inst: Int = 0
  var trace: Boolean = false

  def reset(): Unit =
    for i <- 1 until 8 do r(i) write 0

    running = true
    vector = 0

  def execute(): Unit =
    if vector >= 0 then
      pc = mem.readInt(vector * 4)
      vector = -1

    inst = mem.readShort(pc)

    if trace then println((pc.toHexString, inst.toHexString))

    pc += 2
    Decode(inst)(this)

  def run(): Unit =
    while running do execute()

  def resume(): Unit =
    running = true
    run()

  class Reg:
    private var r: Int = 0

    def read: Int = r

    def write(v: Int): Unit = r = v

  class Reg0 extends Reg:
    override def read: Int = 0

    override def write(v: Int): Unit = {}

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
        "111 rrr 00 iiiiiiii" -> ((operands: Map[Char, Int]) => new LDI(operands('r'), operands('i'))), // ldi
        "111 rrr 01 iiiiiiii" -> ((operands: Map[Char, Int]) => new LDI(operands('r'), operands('i').toByte)), // ldis
        "111 rrr 11 iiiiiiii" -> ((operands: Map[Char, Int]) => new SLI(operands('r'), operands('i'))), // sli
        "110 000 000 11 00000" -> ((operands: Map[Char, Int]) => BRK),
        "101 aaa bbb iiiiiii" -> ((operands: Map[Char, Int]) =>
          new ADDI(operands('a'), operands('b'), operands('i'))
        ), // addi
      ),
    )

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
