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

  // ===== Paren-free tuple construction =====

  "return a, b (no parens)" in {
    eval(
      """divmod(a: int, b: int) -> (int, int)
        |    return a / b, a % b
        |
        |main() -> int
        |    q, r = divmod(17, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 32
  }

  "expression function returns a, b" in {
    eval(
      """swap(a: int, b: int) -> (int, int) = b, a
        |
        |main() -> int
        |    x, y = swap(10, 20)
        |    x * 100 + y
        |""".stripMargin) shouldBe 2010
  }

  "assign tuple literal without parens" in {
    eval(
      """main() -> int
        |    x = 10, 20
        |    0
        |""".stripMargin) shouldBe 0  // just check it parses — x is a tuple
  }

  // ===== Parallel assignment (swap) =====

  "a, b = b, a swaps values" in {
    eval(
      """main() -> int
        |    a = 10
        |    b = 20
        |    a, b = b, a
        |    a * 100 + b
        |""".stripMargin) shouldBe 2010
  }

  "parallel assignment from function" in {
    eval(
      """divmod(a: int, b: int) -> (int, int) = a / b, a % b
        |
        |main() -> int
        |    q = 0
        |    r = 0
        |    q, r = divmod(17, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 32
  }

  // ===== Struct destructuring =====

  "destructure named struct" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(10, 20)
        |    x, y = p
        |    x * 100 + y
        |""".stripMargin) shouldBe 1020
  }

  "destructure struct from function return" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |origin() -> Point = Point(0, 0)
        |offset() -> Point = Point(3, 4)
        |
        |main() -> int
        |    x, y = offset()
        |    x * 10 + y
        |""".stripMargin) shouldBe 34
  }

  "destructure struct with three fields" in {
    eval(
      """struct RGB
        |    r: int
        |    g: int
        |    b: int
        |
        |main() -> int
        |    c = RGB(255, 128, 0)
        |    r, g, b = c
        |    r * 10000 + g * 100 + b
        |""".stripMargin) shouldBe 2562800
  }

  "destructure ref struct" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = new Point(30, 40)
        |    x, y = p
        |    x * 10 + y
        |""".stripMargin) shouldBe 340
  }

  "destructure struct wrong count is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p = Point(10, 20)
        |    a, b, c = p
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "mixed declared/undeclared is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    a = 10
        |    a, b = 20, 30
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
