package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslUnsignedCodegenTests extends AnyFreeSpec with Matchers {

  def compile(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  def compileAndRun(source: String): Long =
    val asm = compile(source)
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof, tof, Runtime.ioTof))
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read

  // ===== Basic unsigned variable load/store =====

  "u8 variable round-trip" in {
    compileAndRun(
      """main() -> int
        |    var x: u8 = 200
        |    int(x)
        |""".stripMargin) shouldBe 200
  }

  "u16 variable round-trip" in {
    compileAndRun(
      """main() -> int
        |    var x: u16 = 50000
        |    int(x)
        |""".stripMargin) shouldBe 50000
  }

  "u32 variable round-trip" in {
    compileAndRun(
      """main() -> int
        |    var x: u32 = 100000
        |    int(x)
        |""".stripMargin) shouldBe 100000
  }

  // ===== Unsigned loads zero-extend (not sign-extend) =====

  "u8 load zero-extends high bit" in {
    // 200 = 0xC8, high bit set — must NOT sign-extend to negative
    compileAndRun(
      """main() -> int
        |    var x: u8 = 200
        |    i64(x)
        |""".stripMargin) shouldBe 200
  }

  "u16 load zero-extends high bit" in {
    // 50000 = 0xC350, high bit set — must NOT sign-extend
    compileAndRun(
      """main() -> int
        |    var x: u16 = 50000
        |    i64(x)
        |""".stripMargin) shouldBe 50000
  }

  "u32 load zero-extends high bit" in {
    // Store 0xFFFFFFFF via cast, then load back — should be positive
    compileAndRun(
      """main() -> int
        |    var x: u32 = u32(i32(-1))
        |    i64(x)
        |""".stripMargin) shouldBe 0xFFFFFFFFL
  }

  "i8 load sign-extends high bit (contrast)" in {
    // 200 as i8 = -56 (sign-extended)
    compileAndRun(
      """main() -> int
        |    var x: i8 = i8(200)
        |    i64(x)
        |""".stripMargin) shouldBe -56
  }

  // ===== Unsigned arithmetic =====

  "u32 addition" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 100
        |    var b: u32 = 200
        |    int(a + b)
        |""".stripMargin) shouldBe 300
  }

  "u32 subtraction" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 200
        |    var b: u32 = 50
        |    int(a - b)
        |""".stripMargin) shouldBe 150
  }

  "u32 multiplication" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 7
        |    var b: u32 = 6
        |    int(a * b)
        |""".stripMargin) shouldBe 42
  }

  "u32 division" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 100
        |    var b: u32 = 10
        |    int(a / b)
        |""".stripMargin) shouldBe 10
  }

  "u32 remainder" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 17
        |    var b: u32 = 5
        |    int(a % b)
        |""".stripMargin) shouldBe 2
  }

  // ===== Unsigned right shift is logical =====

  "u32 right shift is logical" in {
    // Build 0x80000000 via shift: 1 << 31, then right shift should be logical
    compileAndRun(
      """main() -> int
        |    var one: u32 = 1
        |    var a: u32 = one << 31
        |    var b: u32 = 1
        |    i64(a >> b)
        |""".stripMargin) shouldBe 0x40000000L
  }

  "i32 right shift is arithmetic (contrast)" in {
    // Build -2147483648 via shift, then arithmetic right shift sign-extends
    compileAndRun(
      """main() -> int
        |    var one: i32 = 1
        |    var a: i32 = one << 31
        |    var b: i32 = 1
        |    i64(a >> b)
        |""".stripMargin) shouldBe -1073741824L
  }

  // ===== Unsigned comparisons =====

  "u32 less than" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 5
        |    var b: u32 = 10
        |    if a < b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 greater than" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 10
        |    var b: u32 = 5
        |    if a > b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 comparison treats high bit as value" in {
    // 0xFFFFFFFF as u32 should be > 0 (as signed i32, -1 < 0)
    compileAndRun(
      """main() -> int
        |    var a: u32 = u32(i32(-1))
        |    var b: u32 = 0
        |    if a > b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 less than or equal" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 5
        |    var b: u32 = 5
        |    if a <= b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 greater than or equal" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 10
        |    var b: u32 = 5
        |    if a >= b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Cast codegen =====

  "u8 cast zero-extends" in {
    compileAndRun("main() -> int = int(u8(0xFF))\n") shouldBe 255
  }

  "u16 cast zero-extends" in {
    compileAndRun("main() -> int = int(u16(0xFFFF))\n") shouldBe 65535
  }

  "u32 cast zero-extends" in {
    compileAndRun(
      """main() -> int
        |    var x: i32 = -1
        |    i64(u32(x))
        |""".stripMargin) shouldBe 0xFFFFFFFFL
  }

  "i8 cast sign-extends" in {
    compileAndRun("main() -> int = i32(i8(0xFF))\n") shouldBe -1
  }

  "i16 cast sign-extends" in {
    compileAndRun("main() -> int = i32(i16(0xFFFF))\n") shouldBe -1
  }

  // ===== Unsigned in arrays =====

  "u8 array stores and loads correctly" in {
    compileAndRun(
      """main() -> int
        |    var arr: [3]u8
        |    arr[0] = 200
        |    arr[1] = 128
        |    arr[2] = 255
        |    int(arr[0]) + int(arr[1]) + int(arr[2])
        |""".stripMargin) shouldBe 583
  }

  // ===== Unsigned in loops =====

  "u32 loop counter" in {
    compileAndRun(
      """main() -> int
        |    var sum: u32 = 0
        |    var i: u32 = 1
        |    while i <= 10
        |        sum = sum + i
        |        i = i + 1
        |    int(sum)
        |""".stripMargin) shouldBe 55
  }

  // ===== Unsigned function parameter =====

  "unsigned function parameter and return" in {
    compileAndRun(
      """add_u32(a: u32, b: u32) -> u32 = a + b
        |main() -> int
        |    int(add_u32(100, 200))
        |""".stripMargin) shouldBe 300
  }

  // ===== Bitwise ops codegen =====

  "u32 bitwise and" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 0xFF
        |    var b: u32 = 0x0F
        |    int(a & b)
        |""".stripMargin) shouldBe 0x0F
  }

  "u32 left shift" in {
    compileAndRun(
      """main() -> int
        |    var a: u32 = 1
        |    var b: u32 = 16
        |    int(a << b)
        |""".stripMargin) shouldBe 65536
  }
}
