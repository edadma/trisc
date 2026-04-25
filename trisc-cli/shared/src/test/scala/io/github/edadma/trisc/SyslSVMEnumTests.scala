package io.github.edadma.trisc

class SyslSVMEnumTests extends SyslSVMCodegenHelpers {

  "construct and match single-field variant" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: i64)
        |    Rect(w: i64, h: i64)
        |
        |main() -> i64
        |    var s = Circle(5)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w + h
        |""".stripMargin) shouldBe 5
  }

  "match second variant" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: i64)
        |    Rect(w: i64, h: i64)
        |
        |main() -> i64
        |    var s = Rect(3, 4)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |""".stripMargin) shouldBe 12
  }

  "no-arg variant" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: i64)
        |    Rect(w: i64, h: i64)
        |    Empty
        |
        |main() -> i64
        |    var s = Empty
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w * h
        |        Empty -> 99
        |""".stripMargin) shouldBe 99
  }

  "match with else" in {
    compileAndRun(
      """enum Color
        |    Red(intensity: i64)
        |    Green(intensity: i64)
        |    Blue(intensity: i64)
        |
        |main() -> i64
        |    var c = Blue(200)
        |    c match
        |        Red(i) -> i
        |        else -> 0
        |""".stripMargin) shouldBe 0
  }

  "match with wildcard fields" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: i64)
        |    Rect(w: i64, h: i64)
        |
        |main() -> i64
        |    var s = Circle(7)
        |    s match
        |        Circle(_) -> 1
        |        Rect(_, _) -> 2
        |""".stripMargin) shouldBe 1
  }

  "enum as function argument" in {
    compileAndRun(
      """enum Shape
        |    Circle(radius: i64)
        |    Rect(w: i64, h: i64)
        |
        |area(s: Shape) -> i64
        |    s match
        |        Circle(r) -> r * r * 3
        |        Rect(w, h) -> w * h
        |
        |main() -> i64 = area(Circle(10))
        |""".stripMargin) shouldBe 300
  }

  "match as expression with enum" in {
    compileAndRun(
      """enum Op
        |    Add(a: i64, b: i64)
        |    Mul(a: i64, b: i64)
        |
        |main() -> i64
        |    var op = Add(10, 20)
        |    var result: i64 = op match
        |        Add(a, b) -> a + b
        |        Mul(a, b) -> a * b
        |    result
        |""".stripMargin) shouldBe 30
  }

  "two field variant access" in {
    compileAndRun(
      """enum Shape
        |    Circle(r: i64)
        |    Rect(w: i64, h: i64)
        |
        |main() -> i64
        |    var s = Rect(5, 7)
        |    s match
        |        Circle(r) -> r
        |        Rect(w, h) -> w + h * 2
        |""".stripMargin) shouldBe 19
  }

}
