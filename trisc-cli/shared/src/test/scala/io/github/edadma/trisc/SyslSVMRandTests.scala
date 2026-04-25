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

  "single next value" in {
    val result = compileAndRun(
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
    println(s"First next = $result (0x${result.toHexString})")
  }

  "two nexts same seed" in {
    val result = compileAndRun(
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
        |    next()
        |""".stripMargin)
    println(s"Second next = $result (0x${result.toHexString})")
  }

  "a minus b" in {
    val result = compileAndRun(
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
        |""".stripMargin)
    println(s"a-b = $result")
  }

  "first stored then fetched" in {
    val result = compileAndRun(
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
    println(s"stored first = $result (0x${result.toHexString})")
  }
}
