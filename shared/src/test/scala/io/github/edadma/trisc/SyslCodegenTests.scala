package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslTriscCodegenTests extends AnyFreeSpec with Matchers {

  def compile(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  def compileAndRun(source: String, memSize: Int = 0x1000): Long =
    val asm = compile(source)
    // println(asm) // uncomment to debug
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(tof))
    val mem = new Memory("Memory", new RAM(0, memSize))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    // Set up stack pointer
    cpu.r(7).write(memSize - 8)
    cpu.run()
    cpu.r(1).read

  // ===== Constants =====

  "return constant 0" in {
    compileAndRun("main() -> int = 0\n") shouldBe 0
  }

  "return constant 42" in {
    compileAndRun("main() -> int = 42\n") shouldBe 42
  }

  "return constant 255" in {
    compileAndRun("main() -> int = 255\n") shouldBe 255
  }

  "return large constant" in {
    compileAndRun("main() -> int = 1000\n") shouldBe 1000
  }

  // ===== Arithmetic =====

  "addition" in {
    compileAndRun("main() -> int = 3 + 4\n") shouldBe 7
  }

  "subtraction" in {
    compileAndRun("main() -> int = 10 - 3\n") shouldBe 7
  }

  "multiplication" in {
    compileAndRun("main() -> int = 6 * 7\n") shouldBe 42
  }

  "division" in {
    compileAndRun("main() -> int = 42 / 6\n") shouldBe 7
  }

  "modulo" in {
    compileAndRun("main() -> int = 17 % 5\n") shouldBe 2
  }

  "operator precedence" in {
    compileAndRun("main() -> int = 2 + 3 * 4\n") shouldBe 14
  }

  "parentheses" in {
    compileAndRun("main() -> int = (2 + 3) * 4\n") shouldBe 20
  }

  "unary minus" in {
    compileAndRun("main() -> int = -42\n") shouldBe -42
  }

  "complex expression" in {
    compileAndRun("main() -> int = (10 + 20) * 2 - 5\n") shouldBe 55
  }

  // ===== Variables =====

  "local variable" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    compileAndRun(
      """main() -> int
        |    x = 1
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    compileAndRun(
      """main() -> int
        |    a = 10
        |    b = 20
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  "variable in expression" in {
    compileAndRun(
      """main() -> int
        |    x = 21
        |    x * 2
        |""".stripMargin) shouldBe 42
  }

  // ===== Comparison =====

  "equal true" in {
    compileAndRun("main() -> int = 5 == 5\n") shouldBe 1
  }

  "equal false" in {
    compileAndRun("main() -> int = 5 == 3\n") shouldBe 0
  }

  "not equal true" in {
    compileAndRun("main() -> int = 5 != 3\n") shouldBe 1
  }

  "less than true" in {
    compileAndRun("main() -> int = 3 < 5\n") shouldBe 1
  }

  "less than false" in {
    compileAndRun("main() -> int = 5 < 3\n") shouldBe 0
  }

  "greater than true" in {
    compileAndRun("main() -> int = 5 > 3\n") shouldBe 1
  }

  // ===== If expression =====

  "if true branch" in {
    compileAndRun(
      """main() -> int
        |    if true
        |        return 42
        |    0
        |""".stripMargin) shouldBe 42
  }

  "if false branch" in {
    compileAndRun(
      """main() -> int
        |    if false
        |        return 42
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Function calls =====

  "function call" in {
    compileAndRun(
      """double(x: int) -> int = x * 2
        |
        |main() -> int = double(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== While loop =====

  "while loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 5
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 10
  }
}
