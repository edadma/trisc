package io.github.edadma.trisc

class SyslExpectTests extends SyslTestHelpers {

  "expect passes when values equal" in {
    eval(
      """main() -> int
        |    expect(42, 42, "should match")
        |    0
        |""".stripMargin) shouldBe 0
  }

  "expect panics with message when values differ" in {
    val e = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    expect(37, 42, "value check")
          |    0
          |""".stripMargin)
    }
    e.getMessage should include("value check")
    e.getMessage should include("expected 42")
    e.getMessage should include("got 37")
  }

  "expect works with zero" in {
    eval(
      """main() -> int
        |    expect(0, 0, "zero")
        |    0
        |""".stripMargin) shouldBe 0
  }

  "expect works with negative values" in {
    eval(
      """main() -> int
        |    expect(-1, -1, "neg")
        |    0
        |""".stripMargin) shouldBe 0
  }

  "expect shows negative values on failure" in {
    val e = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    expect(-1, 1, "sign")
          |    0
          |""".stripMargin)
    }
    e.getMessage should include("expected 1")
    e.getMessage should include("got -1")
  }

  "expect works with expressions" in {
    eval(
      """main() -> int
        |    val a = 10
        |    val b = 20
        |    expect(a + b, 30, "sum")
        |    0
        |""".stripMargin) shouldBe 0
  }
}
