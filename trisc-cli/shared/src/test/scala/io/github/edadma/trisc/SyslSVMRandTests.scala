package io.github.edadma.trisc

class SyslSVMRandTests extends SyslSVMCodegenHelpers {

  "xorshift determinism" in {
    compileAndRun(
      """var state: i64 = 1i64
        |
        |seed(s: i64)
        |    state = s
        |
        |next() -> i64
        |    state = state ^ (state << 13i64)
        |    state = state ^ (state >> 7i64)
        |    state = state ^ (state << 17i64)
        |    state
        |
        |main() -> i64
        |    seed(42i64)
        |    var a = next()
        |    seed(42i64)
        |    var b = next()
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "two nexts same seed produce equal values" in {
    compileAndRun(
      """var state: i64 = 1i64
        |
        |seed(s: i64)
        |    state = s
        |
        |next() -> i64
        |    state = state ^ (state << 13i64)
        |    state = state ^ (state >> 7i64)
        |    state = state ^ (state << 17i64)
        |    state
        |
        |main() -> i64
        |    seed(42i64)
        |    var a = next()
        |    seed(42i64)
        |    var b = next()
        |    a - b
        |""".stripMargin) shouldBe 0
  }

  "store-then-return preserves the value" in {
    val first = compileAndRun(
      """var state: i64 = 1i64
        |
        |seed(s: i64)
        |    state = s
        |
        |next() -> i64
        |    state = state ^ (state << 13i64)
        |    state = state ^ (state >> 7i64)
        |    state = state ^ (state << 17i64)
        |    state
        |
        |main() -> i64
        |    seed(42i64)
        |    next()
        |""".stripMargin)
    val stored = compileAndRun(
      """var state: i64 = 1i64
        |
        |seed(s: i64)
        |    state = s
        |
        |next() -> i64
        |    state = state ^ (state << 13i64)
        |    state = state ^ (state >> 7i64)
        |    state = state ^ (state << 17i64)
        |    state
        |
        |main() -> i64
        |    seed(42i64)
        |    var a = next()
        |    a
        |""".stripMargin)
    stored shouldBe first
  }
}
