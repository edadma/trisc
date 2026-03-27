package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslStructOpsTests extends SyslTestHelpers {

  // ===== sizeof =====

  "sizeof(int)" in {
    eval("main() -> int = sizeof(int)\n") shouldBe 8
  }

  "sizeof(byte)" in {
    eval("main() -> int = sizeof(byte)\n") shouldBe 1
  }

  "sizeof(bool)" in {
    eval("main() -> int = sizeof(bool)\n") shouldBe 8
  }

  "sizeof(char)" in {
    eval("main() -> int = sizeof(char)\n") shouldBe 8
  }

  "sizeof pointer" in {
    eval("main() -> int = sizeof(*int)\n") shouldBe 8
  }

  "sizeof array" in {
    eval("main() -> int = sizeof([5]int)\n") shouldBe 40
  }

  "sizeof struct" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int = sizeof(Point)
        |""".stripMargin) shouldBe 16
  }

  "sizeof struct three fields" in {
    eval(
      """struct Vec3
        |    x: int
        |    y: int
        |    z: int
        |
        |main() -> int = sizeof(Vec3)
        |""".stripMargin) shouldBe 24
  }

  "sizeof in expression" in {
    eval("main() -> int = sizeof(int) + sizeof(byte)\n") shouldBe 9
  }

  // ===== sizeof expression =====

  "sizeof variable" in {
    eval(
      """main() -> int
        |    x = 42
        |    sizeof(x)
        |""".stripMargin) shouldBe 8
  }

  "sizeof array variable" in {
    eval(
      """main() -> int
        |    arr: [5]int
        |    sizeof(arr)
        |""".stripMargin) shouldBe 40
  }

  "sizeof struct variable" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    sizeof(p)
        |""".stripMargin) shouldBe 16
  }

  "sizeof expression result" in {
    eval(
      """main() -> int
        |    x = 42
        |    sizeof(x + 1)
        |""".stripMargin) shouldBe 8
  }

  "sizeof bool variable" in {
    eval(
      """main() -> int
        |    b = true
        |    sizeof(b)
        |""".stripMargin) shouldBe 8
  }

  // ===== Field compound assignment =====

  "field plus equals" in {
    eval(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 10
        |    c.count += 5
        |    c.count
        |""".stripMargin) shouldBe 15
  }

  "field minus equals" in {
    eval(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 10
        |    c.count -= 3
        |    c.count
        |""".stripMargin) shouldBe 7
  }

  "field times equals" in {
    eval(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 5
        |    c.count *= 4
        |    c.count
        |""".stripMargin) shouldBe 20
  }

  "field compound assign in loop" in {
    eval(
      """struct Accum
        |    total: int
        |    count: int
        |
        |main() -> int
        |    a: Accum
        |    for i = 1; i <= 5; i++
        |        a.total += i
        |        a.count += 1
        |    a.total
        |""".stripMargin) shouldBe 15
  }

  // ===== Field pre/post increment =====

  "field pre-increment" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    c.n = 5
        |    ++c.n
        |""".stripMargin) shouldBe 6
  }

  "field pre-increment modifies field" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    c.n = 5
        |    ++c.n
        |    c.n
        |""".stripMargin) shouldBe 6
  }

  "field post-increment returns old value" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    c.n = 5
        |    c.n++
        |""".stripMargin) shouldBe 5
  }

  "field post-increment modifies field" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    c.n = 5
        |    c.n++
        |    c.n
        |""".stripMargin) shouldBe 6
  }

  "field pre-decrement" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    c.n = 5
        |    --c.n
        |""".stripMargin) shouldBe 4
  }

  "field post-decrement" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    c.n = 5
        |    c.n--
        |""".stripMargin) shouldBe 5
  }

  "field increment in loop" in {
    eval(
      """struct Counter
        |    n: int
        |
        |main() -> int
        |    c: Counter
        |    for i = 0; i < 10; i++
        |        c.n++
        |    c.n
        |""".stripMargin) shouldBe 10
  }

  // ===== Combined: struct with sizeof =====

  "sizeof used with struct allocation" in {
    eval(
      """struct Pair
        |    a: int
        |    b: int
        |
        |main() -> int
        |    val size = sizeof(Pair)
        |    size / sizeof(int)
        |""".stripMargin) shouldBe 2
  }
}
