package io.github.edadma.trisc

class SyslStrEnumTests extends SyslTestHelpers {

  "str() on variant with no fields" in {
    output("""
      |enum Shape
      |    Circle(r: int)
      |    Rect(w: int, h: int)
      |    Empty
      |
      |main() -> int
      |    var s: Shape = Empty
      |    puts(str(s))
      |    0
      |""".stripMargin) shouldBe "Empty"
  }

  "str() on variant with fields" in {
    output("""
      |enum Shape
      |    Circle(r: int)
      |    Rect(w: int, h: int)
      |    Empty
      |
      |main() -> int
      |    var s: Shape = Circle(5)
      |    puts(str(s))
      |    0
      |""".stripMargin) shouldBe "Circle"
  }

  "str() on second variant with fields" in {
    output("""
      |enum Shape
      |    Circle(r: int)
      |    Rect(w: int, h: int)
      |    Empty
      |
      |main() -> int
      |    var s: Shape = Rect(3, 4)
      |    puts(str(s))
      |    0
      |""".stripMargin) shouldBe "Rect"
  }

  "str() usable in string concatenation" in {
    output("""
      |enum Color
      |    Red
      |    Green
      |    Blue
      |    Indigo
      |
      |enum Shape
      |    Circle(c: Color, r: int)
      |    Empty
      |
      |main() -> int
      |    var s: Shape = Circle(Indigo, 5)
      |    puts("shape is " + str(s))
      |    0
      |""".stripMargin) shouldBe "shape is Circle"
  }
}
