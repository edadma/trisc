package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslDeferTests extends SyslTestHelpers {

  "defer runs at end of function" in {
    output(
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
        |""".stripMargin) shouldBe ""
    eval(
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
    eval(
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
        |""".stripMargin) shouldBe 1  // a=0 (before inc), b=1 (after inc), 0*10+1=1
  }

  "multiple defers run in LIFO order" in {
    output(
      """main() -> int
        |    defer puts("first")
        |    defer puts("second")
        |    defer puts("third")
        |    0
        |""".stripMargin) shouldBe "thirdsecondfirst"
  }

  "defer runs on early return" in {
    eval(
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
        |    val r = work(0)
        |    cleaned * 100 + r + 1
        |""".stripMargin) shouldBe 100  // cleaned=1, r=-1, 1*100+(-1)+1=100
  }

  "defer with no return statement" in {
    eval(
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
}
