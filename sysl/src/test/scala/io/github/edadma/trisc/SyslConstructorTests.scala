package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslConstructorTests extends SyslTestHelpers {

  "basic struct constructor" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = Point(10, 20)
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "constructor with single field" in {
    eval(
      """struct Wrapper
        |    value: int
        |
        |main() -> int
        |    val w = Wrapper(42)
        |    w.value
        |""".stripMargin) shouldBe 42
  }

  "constructor with three fields" in {
    eval(
      """struct Vec3
        |    x: int
        |    y: int
        |    z: int
        |
        |main() -> int
        |    val v = Vec3(1, 2, 3)
        |    v.x + v.y + v.z
        |""".stripMargin) shouldBe 6
  }

  "constructor with expressions" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val a = 10
        |    val p = Point(a * 2, a + 5)
        |    p.x + p.y
        |""".stripMargin) shouldBe 35
  }

  "constructor passed to function" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: *Point) -> int = p.x + p.y
        |
        |main() -> int
        |    val p = Point(15, 27)
        |    sum(&p)
        |""".stripMargin) shouldBe 42
  }

  "wrong number of args rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = Point(1)
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "wrong arg type rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val p = Point(true, 2)
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "constructor in expression position" in {
    eval(
      """struct Pair
        |    a: int
        |    b: int
        |
        |get_sum(p: *Pair) -> int = p.a + p.b
        |
        |main() -> int
        |    var p = Pair(20, 22)
        |    get_sum(&p)
        |""".stripMargin) shouldBe 42
  }

  "constructor with nested struct" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |struct Line
        |    start: Point
        |    end_: Point
        |
        |main() -> int
        |    val l = Line(Point(1, 2), Point(3, 4))
        |    l.start.x + l.end_.y
        |""".stripMargin) shouldBe 5
  }

  "constructor assigned to var then field modified" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var p = Point(1, 2)
        |    p.x = 10
        |    p.x + p.y
        |""".stripMargin) shouldBe 12
  }
}
