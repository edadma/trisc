package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslCodegenGapTests extends AnyFreeSpec with Matchers {

  // Reuse the minimal boot stub + run helper from TOSTests pattern
  val minimalBoot: String =
    """STDOUT = 0xFF8
      |
      |segment vectors
      |
      |  dl 0xFF0
      |  dl boot
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |  dl default_isr
      |
      |segment code
      |
      |extern main
      |
      |global boot, func
      |entry boot
      |
      |boot
      |  movi r4, main
      |  jalr r6, r4
      |  halt
      |
      |global putchar, func
      |
      |putchar
      |  movi r2, STDOUT
      |  stb r1, r2, r0
      |  jalr r0, r6
      |
      |global default_isr, func
      |
      |default_isr
      |  halt
      |""".stripMargin

  def compileSysl(source: String): TOF =
    val driver = new SyslDriver
    val result = driver.compile(Map("main" -> source))
    val unit = result.units.head
    val codegen = new SyslTriscCodegen()
    val asm = codegen.generate(unit.typed)
    assemble(asm, relocatable = true)

  def runWithBoot(syslSource: String): (CPU, String) =
    val bootTof = assemble(minimalBoot, relocatable = true)
    val progTof = compileSysl(syslSource)
    val linked = Linker.link(Seq(bootTof, progTof))

    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    cpu.run()
    (cpu, output.toString)

  // ===== sizeof =====

  "sizeof int" in {
    val (cpu, _) = runWithBoot(
      """main() -> int = sizeof(int)
        |""".stripMargin)
    cpu.r(1).read shouldBe 4
  }

  "sizeof i64" in {
    val (cpu, _) = runWithBoot(
      """main() -> int = sizeof(i64)
        |""".stripMargin)
    cpu.r(1).read shouldBe 8
  }

  "sizeof byte" in {
    val (cpu, _) = runWithBoot(
      """main() -> int = sizeof(byte)
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  "sizeof pointer" in {
    val (cpu, _) = runWithBoot(
      """main() -> int = sizeof(*int)
        |""".stripMargin)
    cpu.r(1).read shouldBe 8
  }

  // ===== struct literals (zero-init) =====

  "struct literal fields are zero" in {
    val (cpu, _) = runWithBoot(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x + p.y
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  "struct literal then assign" in {
    val (cpu, _) = runWithBoot(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin)
    cpu.r(1).read shouldBe 30
  }

  // ===== field pre/post increment/decrement =====

  "field post-increment" in {
    val (cpu, _) = runWithBoot(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 5
        |    val old = c.count++
        |    old * 100 + c.count
        |""".stripMargin)
    cpu.r(1).read shouldBe 506
  }

  "field pre-increment" in {
    val (cpu, _) = runWithBoot(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 5
        |    val v = ++c.count
        |    v * 100 + c.count
        |""".stripMargin)
    cpu.r(1).read shouldBe 606
  }

  "field post-decrement" in {
    val (cpu, _) = runWithBoot(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 5
        |    val old = c.count--
        |    old * 100 + c.count
        |""".stripMargin)
    cpu.r(1).read shouldBe 504
  }

  "field pre-decrement" in {
    val (cpu, _) = runWithBoot(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 5
        |    val v = --c.count
        |    v * 100 + c.count
        |""".stripMargin)
    cpu.r(1).read shouldBe 404
  }

  // ===== bitwise compound assignment =====

  "and-assign" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var x = 0xFF
        |    x &= 0x0F
        |    x
        |""".stripMargin)
    cpu.r(1).read shouldBe 0x0F
  }

  "or-assign" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var x = 0xF0
        |    x |= 0x0F
        |    x
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xFF
  }

  "xor-assign" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var x = 0xFF
        |    x ^= 0x0F
        |    x
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xF0
  }

  "shift-left-assign" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var x = 1
        |    x <<= 4
        |    x
        |""".stripMargin)
    cpu.r(1).read shouldBe 16
  }

  "shift-right-assign" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var x = 256
        |    x >>= 4
        |    x
        |""".stripMargin)
    cpu.r(1).read shouldBe 16
  }

  // ===== field bitwise compound assignment =====

  "field and-assign" in {
    val (cpu, _) = runWithBoot(
      """struct Bits
        |    flags: int
        |
        |main() -> int
        |    b: Bits
        |    b.flags = 0xFF
        |    b.flags &= 0x0F
        |    b.flags
        |""".stripMargin)
    cpu.r(1).read shouldBe 0x0F
  }

  "field or-assign" in {
    val (cpu, _) = runWithBoot(
      """struct Bits
        |    flags: int
        |
        |main() -> int
        |    b: Bits
        |    b.flags = 0xF0
        |    b.flags |= 0x0F
        |    b.flags
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xFF
  }

  "field shift-left-assign" in {
    val (cpu, _) = runWithBoot(
      """struct Bits
        |    flags: int
        |
        |main() -> int
        |    b: Bits
        |    b.flags = 1
        |    b.flags <<= 8
        |    b.flags
        |""".stripMargin)
    cpu.r(1).read shouldBe 256
  }

  "array decay: pass array where *i8 expected" in {
    val (_, output) = runWithBoot(
      """extern putchar(ch: int)
        |
        |emit(s: *i8)
        |    var i = 0
        |    while s[i] != 0
        |        putchar(int(s[i]))
        |        i += 1
        |
        |var buf: [4]i8
        |
        |main() -> int
        |    buf[0] = 'H'
        |    buf[1] = 'i'
        |    buf[2] = 0
        |    emit(buf)
        |    0
        |""".stripMargin)
    output shouldBe "Hi"
  }
}
