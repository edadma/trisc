package io.github.edadma.trisc

class SyslCodegenArithTests extends SyslCodegenHelpers {

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

  "complex expression" in {
    compileAndRun("main() -> int = (10 + 20) * 2 - 5\n") shouldBe 55
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

  "less than or equal true" in {
    compileAndRun("main() -> int = 3 <= 5\n") shouldBe 1
  }

  "less than or equal equal" in {
    compileAndRun("main() -> int = 5 <= 5\n") shouldBe 1
  }

  "less than or equal false" in {
    compileAndRun("main() -> int = 7 <= 5\n") shouldBe 0
  }

  "greater than or equal true" in {
    compileAndRun("main() -> int = 5 >= 3\n") shouldBe 1
  }

  "greater than or equal equal" in {
    compileAndRun("main() -> int = 5 >= 5\n") shouldBe 1
  }

  "greater than or equal false" in {
    compileAndRun("main() -> int = 3 >= 5\n") shouldBe 0
  }

  // ===== Logical operators =====

  "and true true" in {
    compileAndRun("main() -> int = true && true\n") shouldBe 1
  }

  "and true false" in {
    compileAndRun("main() -> int = true && false\n") shouldBe 0
  }

  "and false short-circuits" in {
    compileAndRun("main() -> int = false && true\n") shouldBe 0
  }

  "or false false" in {
    compileAndRun("main() -> int = false || false\n") shouldBe 0
  }

  "or true short-circuits" in {
    compileAndRun("main() -> int = true || false\n") shouldBe 1
  }

  "or false true" in {
    compileAndRun("main() -> int = false || true\n") shouldBe 1
  }

  // ===== Compound assignment =====

  "plus equals" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    x += 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  "minus equals" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    x -= 3
        |    x
        |""".stripMargin) shouldBe 7
  }

  "times equals" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x *= 4
        |    x
        |""".stripMargin) shouldBe 20
  }

  // ===== Pre/post increment/decrement =====

  "pre-increment" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    ++x
        |""".stripMargin) shouldBe 6
  }

  "post-increment returns old value" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x++
        |""".stripMargin) shouldBe 5
  }

  "post-increment modifies variable" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x++
        |    x
        |""".stripMargin) shouldBe 6
  }

  "pre-decrement" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    --x
        |""".stripMargin) shouldBe 4
  }

  "pre-increment i32 local" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 41
        |    ++x
        |""".stripMargin) shouldBe 42
  }

  "pre-decrement i32 local" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 43
        |    --x
        |""".stripMargin) shouldBe 42
  }

  "post-increment i32 returns old value" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 42
        |    x++
        |""".stripMargin) shouldBe 42
  }

  "post-increment i32 modifies variable" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 41
        |    x++
        |    x
        |""".stripMargin) shouldBe 42
  }

  "calling convention: left shift by variable" in {
    compileAndRun(
      """main() -> int
        |    val x = 2
        |    1 << x
        |""".stripMargin) shouldBe 4
  }

  "calling convention: bitwise OR with shift" in {
    compileAndRun(
      """var mask = 0
        |
        |set_bit(n: int)
        |    mask = mask | (1 << n)
        |
        |main() -> int
        |    set_bit(0)
        |    set_bit(2)
        |    mask
        |""".stripMargin) shouldBe 5
  }
}
