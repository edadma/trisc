package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslStructReturnTests extends SyslTestHelpers {

  "return struct from function" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |make_point(x: int, y: int) -> Point
        |    Point(x, y)
        |
        |main() -> int
        |    val p = make_point(10, 32)
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  "return tuple from function" in {
    eval(
      """make_pair(a: int, b: int) -> (int, int)
        |    (a, b)
        |
        |main() -> int
        |    val t = make_pair(10, 32)
        |    t.0 + t.1
        |""".stripMargin) shouldBe 42
  }

  "destructure tuple from function" in {
    eval(
      """divmod(a: int, b: int) -> (int, int)
        |    (a / b, a % b)
        |
        |main() -> int
        |    val (q, r) = divmod(17, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 32
  }

  "return struct via explicit return" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |make(flag: int) -> Point
        |    if flag == 0
        |        return Point(1, 2)
        |    Point(3, 4)
        |
        |main() -> int
        |    val a = make(0)
        |    val b = make(1)
        |    a.x + a.y + b.x + b.y
        |""".stripMargin) shouldBe 10
  }

  "chain: struct return passed to another function" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |make(x: int, y: int) -> Point
        |    Point(x, y)
        |
        |sum(p: *Point) -> int = p.x + p.y
        |
        |main() -> int
        |    val p = make(20, 22)
        |    sum(&p)
        |""".stripMargin) shouldBe 42
  }

  "nested struct return" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |struct Line
        |    start: Point
        |    end_: Point
        |
        |make_line() -> Line
        |    Line(Point(1, 2), Point(3, 4))
        |
        |main() -> int
        |    val l = make_line()
        |    l.start.x + l.start.y + l.end_.x + l.end_.y
        |""".stripMargin) shouldBe 10
  }

  "tuple return type syntax" in {
    eval(
      """swap(a: int, b: int) -> (int, int)
        |    (b, a)
        |
        |main() -> int
        |    val (x, y) = swap(1, 2)
        |    x * 10 + y
        |""".stripMargin) shouldBe 21
  }

  // ===== Go-style paren-free destructuring =====

  "Go-style destructure: a, b = f()" in {
    eval(
      """divmod(a: int, b: int) -> (int, int)
        |    (a / b, a % b)
        |
        |main() -> int
        |    q, r = divmod(17, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 32
  }

  "Go-style destructure with val" in {
    eval(
      """swap(a: int, b: int) -> (int, int) = (b, a)
        |
        |main() -> int
        |    val x, y = swap(10, 20)
        |    x * 100 + y
        |""".stripMargin) shouldBe 2010
  }

  "Go-style destructure with var" in {
    eval(
      """pair() -> (int, int) = (1, 2)
        |
        |main() -> int
        |    var a, b = pair()
        |    a += 10
        |    b += 20
        |    a * 100 + b
        |""".stripMargin) shouldBe 1122
  }
}
