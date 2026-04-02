package io.github.edadma.trisc

class SyslCodegenConstructorTests extends SyslCodegenHelpers {

  "basic struct constructor" in {
    compileAndRun(
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
    compileAndRun(
      """struct Wrapper
        |    value: int
        |
        |main() -> int
        |    val w = Wrapper(42)
        |    w.value
        |""".stripMargin) shouldBe 42
  }

  "constructor with three fields" in {
    compileAndRun(
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
    compileAndRun(
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

  "constructor passed to function via pointer" in {
    compileAndRun(
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

  "constructor with mixed-width fields" in {
    compileAndRun(
      """struct Mixed
        |    a: i8
        |    b: i32
        |    c: i64
        |
        |main() -> int
        |    val m = Mixed(1, 2, 3)
        |    m.a + m.b + int(m.c)
        |""".stripMargin) shouldBe 6
  }

  "constructor in global init" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |do_sum(p: *Pair) -> int = p.a + p.b
        |
        |main() -> int
        |    val p = Pair(20, 22)
        |    do_sum(&p)
        |""".stripMargin) shouldBe 42
  }

  "multiple constructors" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val a = Point(1, 2)
        |    val b = Point(3, 4)
        |    a.x + a.y + b.x + b.y
        |""".stripMargin) shouldBe 10
  }

  "constructor with function call arg" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |dbl(x: int) -> int = x * 2
        |
        |main() -> int
        |    val p = Pair(dbl(5), dbl(7))
        |    p.a + p.b
        |""".stripMargin) shouldBe 24
  }
}
