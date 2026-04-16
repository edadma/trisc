package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslMethodTests extends SyslTestHelpers {

  // ===== Basic methods =====

  "method with no params" in {
    eval(
      """struct Counter
        |    value: int
        |
        |Counter.get() -> int = self.value
        |
        |main() -> int
        |    c: Counter
        |    c.value = 42
        |    c.get()
        |""".stripMargin) shouldBe 42
  }

  "method that modifies self" in {
    eval(
      """struct Counter
        |    value: int
        |
        |Counter.inc()
        |    self.value = self.value + 1
        |
        |main() -> int
        |    c: Counter
        |    c.value = 0
        |    c.inc()
        |    c.inc()
        |    c.inc()
        |    c.value
        |""".stripMargin) shouldBe 3
  }

  // ===== Methods with parameters =====

  "method with parameter" in {
    eval(
      """struct Counter
        |    value: int
        |
        |Counter.add(n: int)
        |    self.value = self.value + n
        |
        |main() -> int
        |    c: Counter
        |    c.value = 10
        |    c.add(32)
        |    c.value
        |""".stripMargin) shouldBe 42
  }

  "method with return value and params" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |Point.sum_with(other: *Point) -> int
        |    self.x + self.y + other.x + other.y
        |
        |main() -> int
        |    a: Point
        |    a.x = 10
        |    a.y = 20
        |    b: Point
        |    b.x = 5
        |    b.y = 7
        |    a.sum_with(&b)
        |""".stripMargin) shouldBe 42
  }

  // ===== Method on pointer to struct =====

  "method called on pointer" in {
    eval(
      """struct Counter
        |    value: int
        |
        |Counter.get() -> int = self.value
        |
        |main() -> int
        |    c: Counter
        |    c.value = 42
        |    p = &c
        |    p.get()
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple methods on same struct =====

  "multiple methods" in {
    eval(
      """struct Pair
        |    a: int
        |    b: int
        |
        |Pair.first() -> int = self.a
        |Pair.second() -> int = self.b
        |Pair.sum() -> int = self.a + self.b
        |
        |main() -> int
        |    p: Pair
        |    p.a = 20
        |    p.b = 22
        |    p.sum()
        |""".stripMargin) shouldBe 42
  }

  // ===== Method used as expression =====

  "method result in arithmetic" in {
    eval(
      """struct Box
        |    value: int
        |
        |Box.get() -> int = self.value
        |
        |main() -> int
        |    a: Box
        |    a.value = 20
        |    b: Box
        |    b.value = 22
        |    a.get() + b.get()
        |""".stripMargin) shouldBe 42
  }

  // ===== Self is implicit pointer =====

  "self write persists after method returns" in {
    eval(
      """struct Acc
        |    total: int
        |
        |Acc.reset()
        |    self.total = 0
        |
        |Acc.accumulate(n: int)
        |    self.total = self.total + n
        |
        |main() -> int
        |    a: Acc
        |    a.reset()
        |    a.accumulate(10)
        |    a.accumulate(20)
        |    a.accumulate(12)
        |    a.total
        |""".stripMargin) shouldBe 42
  }

  // ===== Auto-deref *T → T for function arguments =====

  "auto-deref pointer arg to value param" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |main() -> int
        |    p = Point(20, 22)
        |    val ptr: *Point = &p
        |    sum(ptr)
        |""".stripMargin) shouldBe 42
  }

  "auto-deref self in method calling standalone function" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: Point) -> int = p.x + p.y
        |
        |Point.total() -> int = sum(self)
        |
        |main() -> int
        |    p = Point(20, 22)
        |    p.total()
        |""".stripMargin) shouldBe 42
  }

  "auto-deref with multiple struct args" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |add(a: Point, b: Point) -> int = a.x + b.y
        |
        |main() -> int
        |    a = Point(20, 0)
        |    b = Point(0, 22)
        |    add(&a, &b)
        |""".stripMargin) shouldBe 42
  }

  // ===== Error cases =====

  "calling nonexistent method is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Foo
        |    x: int
        |
        |main() -> int
        |    f: Foo
        |    f.nonexistent()
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
