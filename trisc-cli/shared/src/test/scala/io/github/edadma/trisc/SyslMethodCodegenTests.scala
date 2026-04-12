package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslMethodCodegenTests extends AnyFreeSpec with Matchers {

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
    val cpu = new CPU(mem) { limit = 100000; quiet = true }
    cpu.reset()
    cpu.run()
    cpu.r(1).read

  "method getter" in {
    compileAndRun(
      """struct Box
        |    value: int
        |
        |Box.get() -> int = self.value
        |
        |main() -> int
        |    b: Box
        |    b.value = 42
        |    b.get()
        |""".stripMargin) shouldBe 42
  }

  "method mutation" in {
    compileAndRun(
      """struct Counter
        |    value: int
        |
        |Counter.inc()
        |    self.value = self.value + 1
        |
        |main() -> int
        |    c: Counter
        |    c.value = 0
        |    c.inc()
        |    c.inc()
        |    c.inc()
        |    c.value
        |""".stripMargin) shouldBe 3
  }

  "method with param" in {
    compileAndRun(
      """struct Acc
        |    total: int
        |
        |Acc.accumulate(n: int)
        |    self.total = self.total + n
        |
        |main() -> int
        |    a: Acc
        |    a.total = 0
        |    a.accumulate(10)
        |    a.accumulate(20)
        |    a.accumulate(12)
        |    a.total
        |""".stripMargin) shouldBe 42
  }

  "method on pointer" in {
    compileAndRun(
      """struct Box
        |    value: int
        |
        |Box.get() -> int = self.value
        |
        |main() -> int
        |    b: Box
        |    b.value = 42
        |    p = &b
        |    p.get()
        |""".stripMargin) shouldBe 42
  }

  "multiple methods" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |Pair.first() -> int = self.a
        |Pair.second() -> int = self.b
        |Pair.sum() -> int = self.a + self.b
        |
        |main() -> int
        |    p: Pair
        |    p.a = 20
        |    p.b = 22
        |    p.sum()
        |""".stripMargin) shouldBe 42
  }
}
