package io.github.edadma.trisc

class SyslLabeledLoopTests extends SyslTestHelpers {

  // ===== Basic labeled break/continue (single loop, still behaves like unlabeled) =====

  "labeled break on single loop" in {
    eval("""
      |main() -> int
      |    var sum = 0
      |    outer: for i in 0..<10
      |        if i == 5 then break outer
      |        sum = sum + i
      |    sum
      |""".stripMargin) shouldBe 10
  }

  "labeled continue on single loop" in {
    eval("""
      |main() -> int
      |    var sum = 0
      |    outer: for i in 0..<10
      |        if i % 2 == 0 then continue outer
      |        sum = sum + i
      |    sum
      |""".stripMargin) shouldBe 25
  }

  // ===== Break to outer loop from inner =====

  "break outer skips all remaining iterations of inner and outer" in {
    eval("""
      |main() -> int
      |    var count = 0
      |    outer: for i in 0..<5
      |        for j in 0..<5
      |            if i == 2 && j == 2 then break outer
      |            count = count + 1
      |    count
      |""".stripMargin) shouldBe 12
  }

  "break inner (without label) still works alongside outer-aware label" in {
    eval("""
      |main() -> int
      |    var count = 0
      |    outer: for i in 0..<5
      |        for j in 0..<5
      |            if j == 3 then break
      |            count = count + 1
      |    count
      |""".stripMargin) shouldBe 15
  }

  "unlabeled break targets innermost loop even when outer is labeled" in {
    eval("""
      |main() -> int
      |    var sum = 0
      |    outer: for i in 0..<3
      |        for j in 0..<10
      |            if j == 2 then break
      |            sum = sum + j
      |    sum
      |""".stripMargin) shouldBe 3
  }

  // ===== Continue to outer loop =====

  "continue outer skips rest of inner and starts next outer iteration" in {
    eval("""
      |main() -> int
      |    var count = 0
      |    outer: for i in 0..<4
      |        for j in 0..<4
      |            if j == 2 then continue outer
      |            count = count + 1
      |    count
      |""".stripMargin) shouldBe 8
  }

  // ===== Search-and-exit pattern =====

  "find first match across 2D grid using break outer" in {
    eval("""
      |main() -> int
      |    var result = -1
      |    outer: for i in 0..<5
      |        for j in 0..<5
      |            if i * 10 + j == 23 then
      |                result = 100 + i * 10 + j
      |                break outer
      |    result
      |""".stripMargin) shouldBe 123
  }

  // ===== Three-deep nesting =====

  "break to middle of three-deep nest" in {
    eval("""
      |main() -> int
      |    var count = 0
      |    for i in 0..<2
      |        mid: for j in 0..<3
      |            for k in 0..<4
      |                if k == 2 && j == 1 then break mid
      |                count = count + 1
      |    count
      |""".stripMargin) shouldBe 12
  }

  "break to outermost of three-deep nest" in {
    eval("""
      |main() -> int
      |    var count = 0
      |    top: for i in 0..<5
      |        for j in 0..<5
      |            for k in 0..<5
      |                if i == 1 && j == 2 && k == 3 then break top
      |                count = count + 1
      |    count
      |""".stripMargin) shouldBe 38
  }

  // ===== Labeled while =====

  "break outer labeled while" in {
    eval("""
      |main() -> int
      |    var i = 0
      |    var count = 0
      |    outer: while i < 10
      |        var j = 0
      |        while j < 10
      |            if i * 10 + j == 25 then break outer
      |            count = count + 1
      |            j = j + 1
      |        i = i + 1
      |    count
      |""".stripMargin) shouldBe 25
  }

  "continue outer labeled while skips inner remainder" in {
    eval("""
      |main() -> int
      |    var i = 0
      |    var count = 0
      |    outer: while i < 3
      |        var j = 0
      |        i = i + 1
      |        while j < 5
      |            if j == 2 then continue outer
      |            count = count + 1
      |            j = j + 1
      |    count
      |""".stripMargin) shouldBe 6
  }

  // ===== Labeled do/while =====

  "break outer labeled do/while" in {
    eval("""
      |main() -> int
      |    var count = 0
      |    var i = 0
      |    outer: do
      |        for j in 0..<10
      |            if i == 1 && j == 3 then break outer
      |            count = count + 1
      |        i = i + 1
      |    while i < 5
      |    count
      |""".stripMargin) shouldBe 13
  }

  // ===== Error cases =====

  "unknown label in break fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    for i in 0..<5
        |        break outer
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("no enclosing loop")
    thrown.getMessage should include("outer")
  }

  "unknown label in continue fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    for i in 0..<5
        |        continue outer
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("no enclosing loop")
  }

  "break outside any loop fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    break
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("break outside of loop")
  }

  "duplicate nested loop label fails" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int
        |    outer: for i in 0..<3
        |        outer: for j in 0..<3
        |            break outer
        |    0
        |""".stripMargin)
    }
    thrown.getMessage should include("duplicate loop label")
  }

  "same label reused in sequential (non-nested) loops is fine" in {
    eval("""
      |main() -> int
      |    var a = 0
      |    var b = 0
      |    outer: for i in 0..<5
      |        if i == 3 then break outer
      |        a = a + 1
      |    outer: for j in 0..<5
      |        if j == 4 then break outer
      |        b = b + 1
      |    a + b
      |""".stripMargin) shouldBe 7
  }

  // ===== Label doesn't collide with a local variable name =====

  "loop label does not shadow a same-named local" in {
    eval("""
      |main() -> int
      |    var outer = 100
      |    outer: for i in 0..<3
      |        if i == 2 then break outer
      |        outer = outer + i
      |    outer
      |""".stripMargin) shouldBe 101
  }
}
