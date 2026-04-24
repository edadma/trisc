package io.github.edadma.trisc

/** Ada/SPARK-style loop invariants. The `invariant <bool>[, "msg"]` statement must appear in
  * the leading "header" of a loop body (variants may interleave) and is hoisted by the
  * analyzer to the loop's cut point — checked at the top of every iteration, regardless of
  * how it was laid out in source. */
class SyslLoopInvariantTests extends SyslTestHelpers {

  // ===== Holds across iterations on every loop type =====

  "while: invariant holds across iterations" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        invariant i >= 0
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for: invariant holds across iterations" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    for i = 0; i < 5; i++
        |        invariant i >= 0
        |        invariant i <= 5
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "do/while: invariant holds across iterations" in {
    eval(
      """main() -> int
        |    var i = 0
        |    do
        |        invariant i >= 0
        |        i = i + 1
        |    while i < 4
        |    i
        |""".stripMargin) shouldBe 4
  }

  "loop: invariant holds across iterations" in {
    eval(
      """main() -> int
        |    var i = 0
        |    loop
        |        invariant i >= 0
        |        if i == 3 then break
        |        i = i + 1
        |    i
        |""".stripMargin) shouldBe 3
  }

  // ===== Multiple invariants =====

  "multiple invariants in header all checked" in {
    eval(
      """main() -> int
        |    var i = 0
        |    var j = 10
        |    while i < j
        |        invariant i >= 0
        |        invariant j > 0
        |        invariant i + j == 10
        |        i = i + 1
        |        j = j - 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  // ===== Trap behavior =====

  "while: invariant traps when broken on later iteration" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var i = 0
          |    while i < 5
          |        invariant i >= 0
          |        i = 0 - 1
          |    i
          |""".stripMargin)
    }
    thrown.getMessage should include("loop invariant")
  }

  "for: invariant traps with custom message" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x = 0
          |    for i = 0; i < 5; i++
          |        invariant x < 10, "x must stay under 10"
          |        x = x + 5
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("x must stay under 10")
  }

  "do/while: invariant traps on first iteration if false" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var i = 0 - 1
          |    do
          |        invariant i >= 0
          |        i = i + 1
          |    while i < 1
          |    i
          |""".stripMargin)
    }
    thrown.getMessage should include("loop invariant")
  }

  // ===== Cut-point hoist semantics =====

  "while: invariant is checked even when written between leading variants" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var n = 5
          |    var x = 0
          |    while n > 0
          |        variant n
          |        invariant x < 3
          |        x = x + 1
          |        n = n - 1
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("loop invariant")
  }

  "while: never enters → invariant never trapped (false-cond skip)" in {
    eval(
      """main() -> int
        |    var i = 10
        |    while i < 0
        |        invariant false
        |        i = i + 1
        |    i
        |""".stripMargin) shouldBe 10
  }

  // ===== Placement enforcement =====

  "invariant after a non-invariant statement fails to analyze" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var i = 0
          |    while i < 5
          |        i = i + 1
          |        invariant i >= 0
          |    i
          |""".stripMargin)
    }
    thrown.getMessage should include("invariant statement must appear at the top of a loop body")
  }

  "invariant nested inside an if fails to analyze" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var i = 0
          |    while i < 5
          |        if i > 2
          |            invariant i >= 0
          |        i = i + 1
          |    i
          |""".stripMargin)
    }
    thrown.getMessage should include("invariant statement must appear at the top of a loop body")
  }

  "invariant at function-body top level fails to analyze" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x = 1
          |    invariant x > 0
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("invariant statement must appear at the top of a loop body")
  }

  // ===== Nested loops have independent invariants =====

  "invariants in nested loops each see their own scope" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    var outer = 3
        |    while outer > 0
        |        invariant outer >= 0
        |        var inner = 4
        |        while inner > 0
        |            invariant inner >= 0
        |            sum = sum + 1
        |            inner = inner - 1
        |        outer = outer - 1
        |    sum
        |""".stripMargin) shouldBe 12
  }

  // ===== Mixed with variant in header =====

  "invariant and variant both in leading header" in {
    eval(
      """main() -> int
        |    var n = 5
        |    var count = 0
        |    while n > 0
        |        invariant count >= 0
        |        variant n
        |        count = count + 1
        |        n = n - 1
        |    count
        |""".stripMargin) shouldBe 5
  }
}
