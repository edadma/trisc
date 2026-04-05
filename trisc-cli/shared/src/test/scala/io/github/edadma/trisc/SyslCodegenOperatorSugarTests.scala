package io.github.edadma.trisc

class SyslCodegenOperatorSugarTests extends SyslCodegenHelpers {

  "+ operator via Add trait on struct" in {
    compileAndRun(
      """struct Vec2
        |    x: int
        |    y: int
        |
        |trait Add[T]
        |    add(a: T, b: T) -> T
        |
        |impl Add[Vec2]
        |    add(a: Vec2, b: Vec2) -> Vec2 = Vec2(a.x + b.x, a.y + b.y)
        |
        |main() -> int
        |    a = Vec2(3, 4)
        |    b = Vec2(10, 20)
        |    c = a + b
        |    c.x * 100 + c.y
        |""".stripMargin) shouldBe 1324
  }

  "< operator via Ord trait" in {
    compileAndRun(
      """struct Money
        |    cents: int
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |
        |impl Ord[Money]
        |    cmp(a: Money, b: Money) -> int = a.cents - b.cents
        |
        |main() -> int
        |    a = Money(100)
        |    b = Money(200)
        |    if a < b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "operator sugar inside generic function with user type" in {
    compileAndRun(
      """struct Money
        |    cents: int
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    gt(a: T, b: T) -> bool = cmp(a, b) > 0
        |
        |impl Ord[Money]
        |    cmp(a: Money, b: Money) -> int = a.cents - b.cents
        |
        |maxOf[T](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    a = Money(100)
        |    b = Money(300)
        |    m = maxOf(a, b)
        |    m.cents
        |""".stripMargin) shouldBe 300
  }
}
