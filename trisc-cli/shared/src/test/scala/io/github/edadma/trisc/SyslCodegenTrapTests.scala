package io.github.edadma.trisc

class SyslCodegenTrapTests extends SyslCodegenHelpers {

  // When trap fires, the boot vector routes to _fault which halts. r1 still
  // holds the error code set before `trap 1` (since _fault is just `halt`).

  "panic emits trap with r1 = 4" in {
    compileAndRun(
      """main() -> int
        |    panic("boom")
        |    0
        |""".stripMargin) shouldBe 4
  }

  "assert passes through when condition is true" in {
    compileAndRun(
      """main() -> int
        |    assert(1 == 1, "nope")
        |    42
        |""".stripMargin) shouldBe 42
  }

  "assert traps with r1 = 4 when condition is false" in {
    compileAndRun(
      """main() -> int
        |    assert(1 == 2, "math failed")
        |    0
        |""".stripMargin) shouldBe 4
  }

  "assert on variable condition true passes" in {
    compileAndRun(
      """main() -> int
        |    var x = 10
        |    assert(x > 0, "must be positive")
        |    x
        |""".stripMargin) shouldBe 10
  }

  "assert on variable condition false traps" in {
    compileAndRun(
      """main() -> int
        |    var x = 0 - 5
        |    assert(x > 0, "must be positive")
        |    99
        |""".stripMargin) shouldBe 4
  }

  "abort still uses error code 3" in {
    compileAndRun(
      """main() -> int
        |    abort()
        |    0
        |""".stripMargin) shouldBe 3
  }

}
