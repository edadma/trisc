package io.github.edadma.trisc

class SyslCodegenScopeTests extends SyslCodegenHelpers {

  // === Loop body scoping ===

  "loop var not visible after loop" in {
    a[Exception] should be thrownBy {
      compile(
        """main() -> int
          |    var i = 0
          |    while i < 3
          |        val x = i * 10
          |        i += 1
          |    x
          |""".stripMargin)
    }
  }

  "loop var shadows outer var" in {
    compileAndRun(
      """main() -> int
        |    var x = 99
        |    var i = 0
        |    while i < 3
        |        var x = i
        |        i += 1
        |    x
        |""".stripMargin) shouldBe 99
  }

  "loop val shadows outer val" in {
    compileAndRun(
      """main() -> int
        |    val x = 42
        |    var i = 0
        |    while i < 1
        |        val x = 7
        |        i += 1
        |    x
        |""".stripMargin) shouldBe 42
  }

  "outer var survives loop with shadowing" in {
    compileAndRun(
      """main() -> int
        |    var total = 0
        |    var i = 0
        |    while i < 3
        |        var total = i * 10
        |        i += 1
        |    total
        |""".stripMargin) shouldBe 0
  }

  "outer var modified in loop (no shadow)" in {
    compileAndRun(
      """main() -> int
        |    var total = 0
        |    var i = 0
        |    while i < 3
        |        total += i
        |        i += 1
        |    total
        |""".stripMargin) shouldBe 3
  }

  // === If/else scoping ===

  "if-body var not visible after if" in {
    a[Exception] should be thrownBy {
      compile(
        """main() -> int
          |    if 1 > 0
          |        val y = 5
          |    y
          |""".stripMargin)
    }
  }

  "if-body var shadows outer" in {
    compileAndRun(
      """main() -> int
        |    var x = 100
        |    if 1 > 0
        |        var x = 5
        |    x
        |""".stripMargin) shouldBe 100
  }

  "else-body var not visible after else" in {
    a[Exception] should be thrownBy {
      compile(
        """main() -> int
          |    if 1 < 0
          |        val a = 1
          |    else
          |        val b = 2
          |    b
          |""".stripMargin)
    }
  }

  // === Nested scoping ===

  "nested loop vars independent" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 2
        |        var j = 0
        |        while j < 3
        |            val v = i * 10 + j
        |            sum += v
        |            j += 1
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 36 // 0+1+2+10+11+12
  }

  "inner scope shadow restored after block" in {
    compileAndRun(
      """main() -> int
        |    var x = 1
        |    var i = 0
        |    while i < 1
        |        var x = 50
        |        x += 1
        |        i += 1
        |    // x should still be 1
        |    x
        |""".stripMargin) shouldBe 1
  }

  // === Function-level scoping ===

  "function params not leaked to caller scope" in {
    a[Exception] should be thrownBy {
      compile(
        """foo(n: int) -> int
          |    n + 1
          |
          |main() -> int
          |    foo(5)
          |    n
          |""".stripMargin)
    }
  }
}
