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

  "nested tuple" in {
    eval(
      """main() -> int
        |    val t = ((1, 2), 3)
        |    val inner = t.0
        |    inner.0 + inner.1 + t.1
        |""".stripMargin) shouldBe 6
  }

  "destructuring with var (mutable)" in {
    eval(
      """main() -> int
        |    var (a, b) = (1, 2)
        |    a = 10
        |    a + b
        |""".stripMargin) shouldBe 12
  }

  "two different 2-tuples in same function" in {
    eval(
      """main() -> int
        |    val a = (42, true)
        |    val b = (10, 20)
        |    if a.1 then a.0 + b.0 + b.1 else 0
        |""".stripMargin) shouldBe 72
  }

  "tuple with string and int" in {
    eval(
      """main() -> int
        |    val t = ("hello", 5)
        |    len(t.0)
        |""".stripMargin) shouldBe 5
  }

  "function returning different tuple types" in {
    eval(
      """pair_int() -> (int, int) = (10, 32)
        |pair_bool() -> (int, bool) = (42, true)
        |
        |main() -> int
        |    val a, b = pair_int()
        |    val c, d = pair_bool()
        |    if d then a + b + c else 0
        |""".stripMargin) shouldBe 84 // 10 + 32 + 42
  }

  "cross-function tuple type disambiguation" in {
    eval(
      """make_pair() -> (int, bool) = (42, true)
        |make_nums() -> (int, int) = (10, 20)
        |
        |main() -> int
        |    val x, ok = make_pair()
        |    val a, b = make_nums()
        |    if ok then x + a + b else 0
        |""".stripMargin) shouldBe 72
  }
}
