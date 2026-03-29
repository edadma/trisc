package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslEnumCodegenTests extends AnyFreeSpec with Matchers {

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
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read

  "enum member value" in {
    compileAndRun(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color.Blue
        |""".stripMargin) shouldBe 2
  }

  "enum explicit value" in {
    compileAndRun(
      """enum Status
        |    Ok = 200
        |    NotFound = 404
        |
        |main() -> int = Status.NotFound
        |""".stripMargin) shouldBe 404
  }

  "enum in conditional" in {
    compileAndRun(
      """enum Dir
        |    Up
        |    Down
        |    Left
        |    Right
        |
        |main() -> int
        |    d = Dir.Right
        |    if d == 3 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "type alias in codegen" in {
    compileAndRun(
      """type Num = i32
        |
        |sum(a: Num, b: Num) -> Num = a + b
        |
        |main() -> int = sum(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "literal suffix in codegen" in {
    compileAndRun(
      """main() -> int
        |    var x: u32 = 100u32
        |    int(x + 50u32)
        |""".stripMargin) shouldBe 150
  }

  "hex literal suffix in codegen" in {
    compileAndRun(
      """main() -> int
        |    var x: u8 = 0xC8u8
        |    int(x)
        |""".stripMargin) shouldBe 200
  }
}
