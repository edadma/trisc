package io.github.edadma.trisc

class SyslCodegenDeferTests extends SyslCodegenHelpers {

  "defer runs cleanup at end of function" in {
    compileAndRun(
      """var result = 0
        |
        |cleanup()
        |    result = 99
        |
        |work() -> int
        |    defer cleanup()
        |    result = 1
        |    result
        |
        |main() -> int
        |    work()
        |    result
        |""".stripMargin) shouldBe 99
  }

  "defer runs after return value computed" in {
    compileAndRun(
      """var counter = 0
        |
        |inc()
        |    counter += 1
        |
        |get_and_inc() -> int
        |    defer inc()
        |    counter
        |
        |main() -> int
        |    val a = get_and_inc()
        |    val b = counter
        |    a * 10 + b
        |""".stripMargin) shouldBe 1  // a=0, b=1 (defer ran after return value captured)
  }

  "multiple defers in LIFO order" in {
    compileAndRun(
      """var order = 0
        |
        |first()
        |    order = order * 10 + 1
        |
        |second()
        |    order = order * 10 + 2
        |
        |third()
        |    order = order * 10 + 3
        |
        |work()
        |    defer first()
        |    defer second()
        |    defer third()
        |
        |main() -> int
        |    work()
        |    order
        |""".stripMargin) shouldBe 321  // third, second, first
  }

  "defer runs on early return" in {
    compileAndRun(
      """var cleaned = 0
        |
        |release()
        |    cleaned = 1
        |
        |work(flag: int) -> int
        |    defer release()
        |    if flag == 0
        |        return -1
        |    42
        |
        |main() -> int
        |    work(0)
        |    cleaned
        |""".stripMargin) shouldBe 1
  }

  "defer preserves return value across cleanup" in {
    compileAndRun(
      """var side_effect = 0
        |
        |noop()
        |    side_effect += 1
        |
        |compute() -> int
        |    defer noop()
        |    return 42
        |
        |main() -> int
        |    val r = compute()
        |    r + side_effect
        |""".stripMargin) shouldBe 43  // 42 + 1
  }

  "defer with implicit return (no return stmt)" in {
    compileAndRun(
      """var x = 0
        |
        |bump()
        |    x += 10
        |
        |do_work()
        |    defer bump()
        |    x = 5
        |
        |main() -> int
        |    do_work()
        |    x
        |""".stripMargin) shouldBe 15
  }

  "defer in expression-body function" in {
    compileAndRun(
      """var count = 0
        |
        |tick()
        |    count += 1
        |
        |get_val() -> int
        |    defer tick()
        |    100
        |
        |main() -> int
        |    get_val() + count
        |""".stripMargin) shouldBe 101  // 100 + 1
  }

  "nested function calls with independent defers" in {
    compileAndRun(
      """var log = 0
        |
        |log_a()
        |    log = log * 10 + 1
        |
        |log_b()
        |    log = log * 10 + 2
        |
        |inner() -> int
        |    defer log_b()
        |    return 10
        |
        |outer() -> int
        |    defer log_a()
        |    val r = inner()
        |    return r + 20
        |
        |main() -> int
        |    outer()
        |    log
        |""".stripMargin) shouldBe 21  // inner defer: log=2, outer defer: log=2*10+1=21
  }
}
