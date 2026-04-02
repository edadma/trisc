package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTupleTests extends SyslTestHelpers {

  "basic tuple literal" in {
    eval(
      """main() -> int
        |    val t = (10, 20)
        |    t.0 + t.1
        |""".stripMargin) shouldBe 30
  }

  "three-element tuple" in {
    eval(
      """main() -> int
        |    val t = (1, 2, 3)
        |    t.0 + t.1 + t.2
        |""".stripMargin) shouldBe 6
  }

  "tuple destructuring" in {
    eval(
      """main() -> int
        |    val (a, b) = (10, 32)
        |    a + b
        |""".stripMargin) shouldBe 42
  }

  "tuple from function return" in {
    eval(
      """divmod(a: int, b: int) -> int
        |    a / b
        |
        |main() -> int
        |    val t = (10, 3)
        |    t.0 / t.1
        |""".stripMargin) shouldBe 3
  }

  "tuple with mixed types" in {
    eval(
      """main() -> int
        |    val t = (42, true)
        |    if t.1 then t.0 else 0
        |""".stripMargin) shouldBe 42
  }

  "destructuring three elements" in {
    eval(
      """main() -> int
        |    val (x, y, z) = (10, 20, 12)
        |    x + y + z
        |""".stripMargin) shouldBe 42
  }

  "wrong destructure count rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    val (a, b, c) = (1, 2)
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
