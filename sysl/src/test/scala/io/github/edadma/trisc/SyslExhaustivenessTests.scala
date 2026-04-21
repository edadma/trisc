package io.github.edadma.trisc

class SyslExhaustivenessTests extends SyslTestHelpers {

  "complete variant coverage passes" in {
    eval("""
      |enum Shape
      |    Circle(r: int)
      |    Square(s: int)
      |    Rect(w: int, h: int)
      |
      |area(s: Shape) -> int =
      |    s match
      |        Circle(r) -> r * r * 3
      |        Square(s) -> s * s
      |        Rect(w, h) -> w * h
      |
      |main() -> int = area(Square(5))
      |""".stripMargin) shouldBe 25
  }

  "missing variant is a compile error" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Shape
        |    Circle(r: int)
        |    Square(s: int)
        |    Rect(w: int, h: int)
        |
        |area(s: Shape) -> int =
        |    s match
        |        Circle(r) -> r * r * 3
        |        Square(s) -> s * s
        |
        |main() -> int = area(Square(5))
        |""".stripMargin)
    }
    thrown.getMessage should (include("non-exhaustive") and include("Rect"))
  }

  "wildcard pattern satisfies exhaustiveness" in {
    eval("""
      |enum Shape
      |    Circle(r: int)
      |    Square(s: int)
      |    Rect(w: int, h: int)
      |
      |area(s: Shape) -> int =
      |    s match
      |        Circle(r) -> r * r * 3
      |        _ -> 0
      |
      |main() -> int = area(Rect(2, 3))
      |""".stripMargin) shouldBe 0
  }

  "guarded arm does not count toward exhaustiveness" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |enum Shape
        |    Circle(r: int)
        |    Square(s: int)
        |
        |area(s: Shape) -> int =
        |    s match
        |        Circle(r) if r > 0 -> r * r * 3
        |        Square(s) -> s * s
        |
        |main() -> int = area(Circle(5))
        |""".stripMargin)
    }
    thrown.getMessage should (include("non-exhaustive") and include("Circle"))
  }
}
