package io.github.edadma.trisc

class SyslCodegenStructReturnTests extends SyslCodegenHelpers {

  "return struct from function" in {
    compileAndRun(
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

  "return struct via explicit return" in {
    compileAndRun(
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

  "return struct with expression body" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |origin() -> Point = Point(0, 0)
        |
        |main() -> int
        |    val p = origin()
        |    p.x + p.y
        |""".stripMargin) shouldBe 0
  }

  "struct return passed to another function" in {
    compileAndRun(
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

  "multiple struct returns" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |make(x: int, y: int) -> Point
        |    Point(x, y)
        |
        |main() -> int
        |    val a = make(1, 2)
        |    val b = make(3, 4)
        |    val c = make(5, 6)
        |    a.x + b.y + c.x
        |""".stripMargin) shouldBe 10
  }

  "return tuple from function" in {
    compileAndRun(
      """make_pair(a: int, b: int) -> (int, int)
        |    (a, b)
        |
        |main() -> int
        |    val t = make_pair(10, 32)
        |    t.0 + t.1
        |""".stripMargin) shouldBe 42
  }

  "destructure tuple from function" in {
    compileAndRun(
      """divmod(a: int, b: int) -> (int, int)
        |    (a / b, a % b)
        |
        |main() -> int
        |    val (q, r) = divmod(17, 5)
        |    q * 10 + r
        |""".stripMargin) shouldBe 32
  }

  "swap via tuple return" in {
    compileAndRun(
      """swap(a: int, b: int) -> (int, int)
        |    (b, a)
        |
        |main() -> int
        |    val (x, y) = swap(1, 2)
        |    x * 10 + y
        |""".stripMargin) shouldBe 21
  }

  "struct return with mixed-width fields" in {
    compileAndRun(
      """struct Mixed
        |    a: i8
        |    b: i32
        |    c: i64
        |
        |make_mixed() -> Mixed
        |    Mixed(1, 2, 3)
        |
        |main() -> int
        |    val m = make_mixed()
        |    m.a + m.b + int(m.c)
        |""".stripMargin) shouldBe 6
  }

  "nested struct return" in {
    compileAndRun(
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

  "struct return from function with 3+ params" in {
    compileAndRun(
      """struct Vec3
        |    x: int
        |    y: int
        |    z: int
        |
        |make_vec(x: int, y: int, z: int) -> Vec3
        |    Vec3(x, y, z)
        |
        |main() -> int
        |    val v = make_vec(10, 20, 12)
        |    v.x + v.y + v.z
        |""".stripMargin) shouldBe 42
  }

  "struct return with defer" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |var side = 0
        |
        |bump()
        |    side = 1
        |
        |make() -> Point
        |    defer bump()
        |    Point(20, 22)
        |
        |main() -> int
        |    val p = make()
        |    p.x + p.y + side
        |""".stripMargin) shouldBe 43
  }
}
