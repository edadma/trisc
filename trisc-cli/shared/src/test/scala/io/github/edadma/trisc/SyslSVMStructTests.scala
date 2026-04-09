package io.github.edadma.trisc

class SyslSVMStructTests extends SyslSVMCodegenHelpers {

  "struct field access" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |main() -> i64
        |    var p: Point
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "struct construct" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |main() -> i64
        |    var p = Point(3, 7)
        |    p.x + p.y
        |""".stripMargin) shouldBe 10
  }

  "struct field assign" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |main() -> i64
        |    var p = Point(1, 2)
        |    p.x = 100
        |    p.x + p.y
        |""".stripMargin) shouldBe 102
  }

  "struct compound assign" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |main() -> i64
        |    var p = Point(10, 20)
        |    p.x += 5
        |    p.y -= 3
        |    p.x + p.y
        |""".stripMargin) shouldBe 32
  }

  "struct passed by pointer" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |sum(p: *Point) -> i64 = p.x + p.y
        |
        |main() -> i64
        |    var pt = Point(11, 22)
        |    sum(&pt)
        |""".stripMargin) shouldBe 33
  }

  "struct write through pointer" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |set_x(p: *Point, v: i64)
        |    p.x = v
        |
        |main() -> i64
        |    var pt = Point(0, 5)
        |    set_x(&pt, 42)
        |    pt.x + pt.y
        |""".stripMargin) shouldBe 47
  }

  "struct field pre-increment" in {
    compileAndRun(
      """struct Counter
        |    count: i64
        |
        |main() -> i64
        |    var c = Counter(9)
        |    ++c.count
        |""".stripMargin) shouldBe 10
  }

  "struct field post-increment" in {
    compileAndRun(
      """struct Counter
        |    count: i64
        |
        |main() -> i64
        |    var c = Counter(9)
        |    c.count++
        |""".stripMargin) shouldBe 9
  }

  "struct zero init" in {
    compileAndRun(
      """struct Pair
        |    a: i64
        |    b: i64
        |
        |main() -> i64
        |    var p: Pair
        |    p.a + p.b
        |""".stripMargin) shouldBe 0
  }

  "nested struct" in {
    compileAndRun(
      """struct Inner
        |    v: i64
        |
        |struct Outer
        |    a: Inner
        |    b: i64
        |
        |main() -> i64
        |    var o: Outer
        |    o.a.v = 42
        |    o.b = 8
        |    o.a.v + o.b
        |""".stripMargin) shouldBe 50
  }

  "struct return from function" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |make_point(x: i64, y: i64) -> Point = Point(x, y)
        |
        |main() -> i64
        |    var p = make_point(13, 29)
        |    p.x + p.y
        |""".stripMargin) shouldBe 42
  }

  "destructure struct" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |main() -> i64
        |    var p = Point(15, 27)
        |    var (a, b) = p
        |    a + b
        |""".stripMargin) shouldBe 42
  }
}
