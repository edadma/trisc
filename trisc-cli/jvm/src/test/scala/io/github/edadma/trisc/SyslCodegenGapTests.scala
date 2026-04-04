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

  "for loop with counter" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var sum = 0
        |    for var i = 0; i < 5; i++
        |        sum += i
        |    sum
        |""".stripMargin)
    cpu.r(1).read shouldBe 10
  }

  "continue in while loop" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 10
        |        i += 1
        |        if i % 2 == 0
        |            continue
        |        sum += i
        |    sum
        |""".stripMargin)
    cpu.r(1).read shouldBe 25  // 1+3+5+7+9
  }

  "break in while loop" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 100
        |        if i == 5
        |            break
        |        sum += i
        |        i += 1
        |    sum
        |""".stripMargin)
    cpu.r(1).read shouldBe 10  // 0+1+2+3+4
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

  "string comparison ==" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    val s = "hello"
        |    if s == "hello"
        |        42
        |    else
        |        0
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }

  "string len()" in {
    val (cpu, _) = runWithBoot(
      """main() -> int
        |    val s = "hello"
        |    len(s)
        |""".stripMargin)
    cpu.r(1).read shouldBe 5
  }

  "string as function parameter" in {
    val (_, output) = runWithBoot(
      """extern putchar(ch: int)
        |
        |emit_str(s: string)
        |    var i = 0
        |    while i < len(s)
        |        putchar(s[i])
        |        i += 1
        |
        |main() -> int
        |    emit_str("OK")
        |    0
        |""".stripMargin)
    output shouldBe "OK"
  }

  "user-defined function shadows builtin" in {
    val (_, output) = runWithBoot(
      """extern putchar(ch: int)
        |
        |print(s: *i8)
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
        |    print(buf)
        |    0
        |""".stripMargin)
    output shouldBe "Hi"
  }
}
