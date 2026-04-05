package io.github.edadma.trisc

class SyslTaggedUnionTests extends SyslTestHelpers {

  // ===== Basic construction and matching =====

  "construct and match single-field variant" in {
    eval(
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
    eval(
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
    eval(
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

  "no-arg variant via qualified name" in {
    eval(
      """enum Shape
        |    Circle(radius: int)
        |    Empty
        |
        |main() -> int
        |    s = Shape.Empty
        |    s match
        |        Circle(r) -> r
        |        Empty -> 42
        |""".stripMargin) shouldBe 42
  }

  "match with else" in {
    eval(
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

  "match with wildcard pattern" in {
    eval(
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

  // ===== Enum as function argument/return =====

  "enum as function argument" in {
    eval(
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
    eval(
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

  // ===== Match as expression =====

  "match as expression" in {
    eval(
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

  // ===== sizeof =====

  "sizeof data enum" in {
    eval(
      """enum Shape
        |    Circle(radius: int)
        |    Rect(w: int, h: int)
        |
        |main() -> int = sizeof(Shape)
        |""".stripMargin) shouldBe 12  // 4 (tag) + max(4, 8) = 4 + 8 = 12
  }

  "sizeof data enum with pointer field" in {
    eval(
      """enum Val
        |    Num(n: int)
        |    Ptr(p: *int)
        |
        |main() -> int = sizeof(Val)
        |""".stripMargin) shouldBe 16  // 4 (tag) + 4 (padding to 8) + 8 (pointer) = 16
  }

  // ===== Multiple enum types =====

  "multiple enum types" in {
    eval(
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

  // ===== Match with guard on variant =====

  "variant match with guard" in {
    eval(
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
}
