package io.github.edadma.trisc

/** `assume <bool> [, "msg"]` — Ada/SPARK pragma Assume equivalent. At runtime it traps if
  * false (like assert); statically it tells a future prover to take the predicate as an
  * axiom rather than a proof obligation. Stripped under `--no-contracts`. */
class SyslAssumeTests extends SyslTestHelpers {

  "assume passes when true" in {
    eval(
      """main() -> int
        |    var x = 5
        |    assume x > 0
        |    x
        |""".stripMargin) shouldBe 5
  }

  "assume traps when false" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x = 5
          |    assume x > 100
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("assume")
  }

  "assume with custom message reports the message" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x = 5
          |    assume x > 100, "x must be large"
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("x must be large")
  }

  "assume rejects non-bool expression" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    assume 42
          |    0
          |""".stripMargin)
    }
    thrown.getMessage should include("assume expression must be bool")
  }

  "assume works inside a loop body" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    for i = 1; i <= 5; i++
        |        assume i > 0
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "assume after a loop carries the post-condition" in {
    eval(
      """main() -> int
        |    var i = 0
        |    while i < 10
        |        i = i + 1
        |    assume i == 10
        |    i
        |""".stripMargin) shouldBe 10
  }
}
