package io.github.edadma.trisc

class SyslOperatorSugarTests extends SyslTestHelpers {

  // ===== Ord operators on user struct =====

  "< operator via Ord trait" in {
    eval(
      """struct Dollar
        |    cents: int
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |    le(a: T, b: T) -> bool = cmp(a, b) <= 0
        |    gt(a: T, b: T) -> bool = cmp(a, b) > 0
        |    ge(a: T, b: T) -> bool = cmp(a, b) >= 0
        |
        |impl Ord[Dollar]
        |    cmp(a: Dollar, b: Dollar) -> int = a.cents - b.cents
        |
        |main() -> int
        |    a = Dollar(100)
        |    b = Dollar(200)
        |    if a < b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "> and == chained via traits" in {
    eval(
      """struct Dollar
        |    cents: int
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |    gt(a: T, b: T) -> bool = cmp(a, b) > 0
        |
        |trait Eq[T]
        |    eq(a: T, b: T) -> bool
        |    ne(a: T, b: T) -> bool = !eq(a, b)
        |
        |impl Ord[Dollar]
        |    cmp(a: Dollar, b: Dollar) -> int = a.cents - b.cents
        |
        |impl Eq[Dollar]
        |    eq(a: Dollar, b: Dollar) -> bool = a.cents == b.cents
        |
        |main() -> int
        |    a = Dollar(500)
        |    b = Dollar(300)
        |    c = Dollar(500)
        |    var r = 0
        |    if a > b then r += 1
        |    if a == c then r += 2
        |    if a != b then r += 4
        |    r
        |""".stripMargin) shouldBe 7
  }

  // ===== Add / arithmetic =====

  "+ via Add trait" in {
    eval(
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
        |    a = Vec2(1, 2)
        |    b = Vec2(10, 20)
        |    c = a + b
        |    c.x * 100 + c.y
        |""".stripMargin) shouldBe 1122
  }

  "arithmetic chain via traits" in {
    eval(
      """struct Money
        |    cents: int
        |
        |trait Add[T]
        |    add(a: T, b: T) -> T
        |
        |trait Sub[T]
        |    sub(a: T, b: T) -> T
        |
        |impl Add[Money]
        |    add(a: Money, b: Money) -> Money = Money(a.cents + b.cents)
        |
        |impl Sub[Money]
        |    sub(a: Money, b: Money) -> Money = Money(a.cents - b.cents)
        |
        |main() -> int
        |    a = Money(1000)
        |    b = Money(300)
        |    c = Money(100)
        |    r = a + b - c
        |    r.cents
        |""".stripMargin) shouldBe 1200
  }

  // ===== Error: missing impl =====

  "operator on user type without impl is error" in {
    an[Exception] should be thrownBy eval(
      """struct Foo
        |    v: int
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |
        |main() -> int
        |    a = Foo(1)
        |    b = Foo(2)
        |    if a < b then 1 else 0
        |""".stripMargin)
  }

  "operator on user type without trait defined is error" in {
    an[Exception] should be thrownBy eval(
      """struct Foo
        |    v: int
        |
        |main() -> int
        |    a = Foo(1)
        |    b = Foo(2)
        |    if a < b then 1 else 0
        |""".stripMargin)
  }

  // ===== Built-in operators still work =====

  "built-in < still works on int" in {
    eval(
      """main() -> int = if 3 < 5 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "built-in + still works on int" in {
    eval(
      """main() -> int = 3 + 4
        |""".stripMargin) shouldBe 7
  }

  // ===== Operator sugar inside generic function =====

  "operator sugar inside generic function — built-in T" in {
    eval(
      """max[T](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int = max(3, 7)
        |""".stripMargin) shouldBe 7
  }

  "operator sugar inside generic function — user T" in {
    eval(
      """struct Dollar
        |    cents: int
        |
        |trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |    gt(a: T, b: T) -> bool = cmp(a, b) > 0
        |
        |impl Ord[Dollar]
        |    cmp(a: Dollar, b: Dollar) -> int = a.cents - b.cents
        |
        |maxOf[T](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    a = Dollar(100)
        |    b = Dollar(300)
        |    m = maxOf(a, b)
        |    m.cents
        |""".stripMargin) shouldBe 300
  }
}
