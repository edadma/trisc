package io.github.edadma.trisc

/** `loop_entry(expr)` — Ada/SPARK-style loop-entry snapshot, valid only inside a loop
  * invariant. Captures the value of `expr` once, at the moment control first reaches the
  * loop (before the first iteration). Nested loops each get their own snapshot scope. */
class SyslLoopEntryTests extends SyslTestHelpers {

  "loop_entry captures the initial value in a for-loop" in {
    eval(
      """main() -> int
        |    var x = 0
        |    for i = 0; i < 5; i++
        |        invariant i >= loop_entry(i)
        |        x = x + i
        |    x
        |""".stripMargin) shouldBe 10
  }

  "loop_entry captures a surrounding-scope value for a while-loop" in {
    eval(
      """main() -> int
        |    var base = 7
        |    var i = 0
        |    while i < 3
        |        invariant base == loop_entry(base)
        |        i = i + 1
        |    base
        |""".stripMargin) shouldBe 7
  }

  "loop_entry traps when the captured relation is violated" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x = 5
          |    var i = 0
          |    while i < 3
          |        invariant x == loop_entry(x)
          |        x = x + 1
          |        i = i + 1
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("loop invariant")
  }

  "loop_entry works in a do/while loop" in {
    eval(
      """main() -> int
        |    var start = 100
        |    var k = 0
        |    do
        |        invariant start == loop_entry(start)
        |        k = k + 1
        |    while k < 3
        |    start
        |""".stripMargin) shouldBe 100
  }

  "loop_entry works in an unconditional loop" in {
    eval(
      """main() -> int
        |    var sentinel = 42
        |    var i = 0
        |    loop
        |        invariant sentinel == loop_entry(sentinel)
        |        i = i + 1
        |        if i >= 3 then break
        |    sentinel
        |""".stripMargin) shouldBe 42
  }

  "loop_entry snapshot is evaluated once, not per iteration" in {
    eval(
      """main() -> int
        |    var base = 10
        |    var i = 0
        |    while i < 5
        |        invariant loop_entry(base) == 10
        |        base = base + 1
        |        i = i + 1
        |    base
        |""".stripMargin) shouldBe 15
  }

  "loop_entry can be used in a monotonic growth invariant" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    for i = 1; i <= 4; i++
        |        invariant sum >= loop_entry(sum)
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "loop_entry in nested loops uses the innermost loop's entry" in {
    eval(
      """main() -> int
        |    var total = 0
        |    for i = 0; i < 3; i++
        |        invariant i >= loop_entry(i)
        |        for j = 0; j < 3; j++
        |            invariant j >= loop_entry(j)
        |            total = total + 1
        |    total
        |""".stripMargin) shouldBe 9
  }

  "nested loop_entry snapshots are independent across loops" in {
    eval(
      """main() -> int
        |    var outer_counter = 0
        |    for i = 0; i < 2; i++
        |        invariant i >= loop_entry(i)
        |        var inner_start = outer_counter
        |        for j = 0; j < 3; j++
        |            invariant inner_start == loop_entry(inner_start)
        |            outer_counter = outer_counter + 1
        |    outer_counter
        |""".stripMargin) shouldBe 6
  }

  "loop_entry outside any loop invariant is a compile-time error" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var x = loop_entry(5)
          |    x
          |""".stripMargin)
    }
    thrown.getMessage should include("loop_entry() is only valid inside a loop invariant")
  }

  "loop_entry outside an invariant but inside a loop body is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var i = 0
          |    while i < 3
          |        var snap = loop_entry(i)
          |        i = i + 1
          |    0
          |""".stripMargin)
    }
    thrown.getMessage should include("loop_entry() is only valid inside a loop invariant")
  }

  "loop_entry in an ensure clause is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """f() -> int
          |    ensure loop_entry(result) == 0
          |    0
          |main() -> int
          |    f()
          |""".stripMargin)
    }
    thrown.getMessage should include("loop_entry() is only valid inside a loop invariant")
  }

  "loop_entry takes exactly one argument" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var i = 0
          |    while i < 1
          |        invariant loop_entry(i, i) == 0
          |        i = i + 1
          |    0
          |""".stripMargin)
    }
    thrown.getMessage should include("loop_entry() takes exactly 1 argument")
  }

  "loop_entry coexists with old() across a function body" in {
    eval(
      """f(start: int) -> int
        |    ensure result == old(start) + 3
        |    var x = start
        |    var i = 0
        |    while i < 3
        |        invariant x == loop_entry(x) + i
        |        x = x + 1
        |        i = i + 1
        |    x
        |main() -> int
        |    f(10)
        |""".stripMargin) shouldBe 13
  }

  "loop_entry of a for-loop induction variable sees the initial value" in {
    eval(
      """main() -> int
        |    var sum = 0
        |    for i = 5; i < 10; i++
        |        invariant loop_entry(i) == 5
        |        sum = sum + i
        |    sum
        |""".stripMargin) shouldBe 35
  }

  "loop_entry of a mutated for-loop captured state" in {
    eval(
      """main() -> int
        |    var x = 0
        |    for i = 0; i < 5; i++
        |        invariant x >= loop_entry(x)
        |        x = x + i
        |    x
        |""".stripMargin) shouldBe 10
  }

  "loop_entry multiple captures in the same invariant" in {
    eval(
      """main() -> int
        |    var a = 3
        |    var b = 7
        |    var i = 0
        |    while i < 5
        |        invariant loop_entry(a) + loop_entry(b) == 10
        |        a = a + 1
        |        b = b - 1
        |        i = i + 1
        |    a + b
        |""".stripMargin) shouldBe 10
  }
}
