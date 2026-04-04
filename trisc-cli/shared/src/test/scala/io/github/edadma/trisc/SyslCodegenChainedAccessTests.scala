package io.github.edadma.trisc

class SyslCodegenChainedAccessTests extends SyslCodegenHelpers {

  // ===== Single-level field access =====

  "single field read" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x = 10
        |    p.x
        |""".stripMargin) shouldBe 10
  }

  "single field write" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.y = 42
        |    p.y
        |""".stripMargin) shouldBe 42
  }

  // ===== Chained field access (a.b.c) =====

  "nested struct field read" in {
    compileAndRun(
      """struct Inner
        |    value: int
        |
        |struct Outer
        |    inner: Inner
        |
        |main() -> int
        |    o: Outer
        |    o.inner.value = 42
        |    o.inner.value
        |""".stripMargin) shouldBe 42
  }

  "triple nested struct field read" in {
    compileAndRun(
      """struct A
        |    num: int
        |
        |struct B
        |    a: A
        |
        |struct C
        |    b: B
        |
        |main() -> int
        |    c: C
        |    c.b.a.num = 99
        |    c.b.a.num
        |""".stripMargin) shouldBe 99
  }

  // ===== Array indexing after field access (a.field[i]) =====

  "field then index read" in {
    compileAndRun(
      """struct Container
        |    items: [3]int
        |
        |main() -> int
        |    c: Container
        |    c.items[0] = 10
        |    c.items[1] = 20
        |    c.items[2] = 30
        |    c.items[1]
        |""".stripMargin) shouldBe 20
  }

  // ===== Index then field access (a[i].field) =====

  "index then field read" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    points: [2]Point
        |    points[0].x = 10
        |    points[0].y = 20
        |    points[1].x = 30
        |    points[1].y = 40
        |    points[1].x
        |""".stripMargin) shouldBe 30
  }

  "index then field sum" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    pts: [2]Point
        |    pts[0].x = 1
        |    pts[0].y = 2
        |    pts[1].x = 3
        |    pts[1].y = 4
        |    pts[0].x + pts[0].y + pts[1].x + pts[1].y
        |""".stripMargin) shouldBe 10
  }

  // ===== Field access via pointer =====

  "pointer to struct field read" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x = 42
        |    ptr = &p
        |    ptr.x
        |""".stripMargin) shouldBe 42
  }

  "field access on function result via pointer" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |get_b(p: *Pair) -> int = p.b
        |
        |main() -> int
        |    pair: Pair
        |    pair.a = 10
        |    pair.b = 42
        |    get_b(&pair)
        |""".stripMargin) shouldBe 42
  }
}
