package io.github.edadma.trisc

class SyslLoopVariantTests extends SyslTestHelpers {

  // ===== Variant succeeds when expr strictly decreases =====

  "variant accepts strictly decreasing counter" in {
    eval("""
      |main() -> int
      |    var n = 5
      |    var count = 0
      |    while n > 0
      |        variant n
      |        count = count + 1
      |        n = n - 1
      |    count
      |""".stripMargin) shouldBe 5
  }

  "variant accepts decreasing by step larger than 1" in {
    eval("""
      |main() -> int
      |    var n = 100
      |    var count = 0
      |    while n > 0
      |        variant n
      |        count = count + 1
      |        n = n - 10
      |    count
      |""".stripMargin) shouldBe 10
  }

  "variant with for-in loop (counter decreasing in body)" in {
    eval("""
      |main() -> int
      |    var remaining = 10
      |    for i in 0..<10
      |        variant remaining
      |        remaining = remaining - 1
      |    remaining
      |""".stripMargin) shouldBe 0
  }

  // ===== Variant traps on non-decreasing =====

  "variant traps when expr stays the same" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    var n = 5
        |    var iters = 0
        |    while iters < 3
        |        variant n
        |        iters = iters + 1
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("loop variant failed")
  }

  "variant traps when expr increases" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    var n = 5
        |    var iters = 0
        |    while iters < 3
        |        variant n
        |        n = n + 1
        |        iters = iters + 1
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("loop variant failed")
  }

  "variant traps when expr becomes negative" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    var n = 2
        |    while n > -5
        |        variant n
        |        n = n - 1
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("loop variant failed")
  }

  // ===== Variant allows single iteration =====

  "variant does not trap on first iteration" in {
    eval("""
      |main() -> int
      |    var n = 5
      |    while n == 5
      |        variant n
      |        n = 0
      |    n
      |""".stripMargin) shouldBe 0
  }

  // ===== Variant in nested loops =====

  "independent variants in nested loops each track their own state" in {
    eval("""
      |main() -> int
      |    var sum = 0
      |    var outer_ctr = 3
      |    while outer_ctr > 0
      |        variant outer_ctr
      |        var inner_ctr = 4
      |        while inner_ctr > 0
      |            variant inner_ctr
      |            sum = sum + 1
      |            inner_ctr = inner_ctr - 1
      |        outer_ctr = outer_ctr - 1
      |    sum
      |""".stripMargin) shouldBe 12
  }

  // ===== Variant outside a loop =====

  "variant in mid-body (not a function contract, not a loop) fails" in {
    // `variant <expr>` is now legal as a function-level contract (top of body) or as a
    // loop-header clause. Anywhere else — e.g. interleaved between regular statements —
    // it's still rejected by the catch-all in the analyzer.
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    var x = 5
        |    variant 5
        |    return x
        |""".stripMargin)
    }
    thrown.getMessage should include("must appear at the top level of a loop body")
  }

  // ===== Variant in do/while =====

  "variant in do/while" in {
    eval("""
      |main() -> int
      |    var n = 3
      |    var iters = 0
      |    do
      |        variant n
      |        iters = iters + 1
      |        n = n - 1
      |    while n > 0
      |    iters
      |""".stripMargin) shouldBe 3
  }
}
