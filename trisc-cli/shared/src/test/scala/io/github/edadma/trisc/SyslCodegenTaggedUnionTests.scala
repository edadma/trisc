package io.github.edadma.trisc

class SyslCodegenTaggedUnionTests extends SyslCodegenHelpers {

  "construct and match single-field variant" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    s = Circle(5)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w + h
        |""".stripMargin) shouldBe 5
  }

  "match second variant" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    s = Rect(3, 4)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |""".stripMargin) shouldBe 12
  }

  "no-arg variant" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |    Empty
        |
        |main() -> int
        |    s = Empty
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |        Empty -> 99
        |""".stripMargin) shouldBe 99
  }

  "match with else" in {
    compileAndRun(
      """enum Color
        |    Red(intensity: int)
        |    Green(intensity: int)
        |    Blue(intensity: int)
        |
        |main() -> int
        |    c = Blue(200)
        |    c match
        |        Red(i) -> i
        |        else -> 0
        |""".stripMargin) shouldBe 0
  }

  "match with wildcard fields" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    s = Circle(7)
        |    s match
        |        Circle(_) -> 1
        |        Rect(_, _) -> 2
        |""".stripMargin) shouldBe 1
  }

  "enum as function argument" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |area(s: Shape) -> int
        |    s match
        |        Circle(r) -> r * r * 3
        |        Rect(w, h) -> w * h
        |
        |main() -> int = area(Circle(10))
        |""".stripMargin) shouldBe 300
  }

  "enum as return value" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |make_shape(kind: int) -> Shape
        |    if kind == 0 then Circle(5)
        |    else Rect(3, 4)
        |
        |main() -> int
        |    s = make_shape(1)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w + h
        |""".stripMargin) shouldBe 7
  }

  "match as expression" in {
    compileAndRun(
      """enum Op
        |    Add(a: int, b: int)
        |    Mul(a: int, b: int)
        |
        |main() -> int
        |    op = Add(10, 20)
        |    result = op match
        |        Add(a, b) -> a + b
        |        Mul(a, b) -> a * b
        |    result
        |""".stripMargin) shouldBe 30
  }

  "variant match with guard" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    s = Circle(3)
        |    s match
        |        Circle(r) if r > 5 -> 1
        |        Circle(r) -> 2
        |        Rect(w, h) -> 3
        |""".stripMargin) shouldBe 2
  }

  "multiple enum types" in {
    compileAndRun(
      """enum Color
        |    Red(r: int)
        |    Green(g: int)
        |
        |enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int
        |    c = Red(255)
        |    s = Rect(3, 4)
        |    cr = c match
        |        Red(r) -> r
        |        Green(g) -> g
        |    sr = s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |    cr + sr
        |""".stripMargin) shouldBe 267
  }
}
