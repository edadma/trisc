package io.github.edadma.trisc

class SyslTraitBoundsTests extends SyslTestHelpers {

  private val ordTrait =
    """trait Ord[T]
      |    cmp(a: T, b: T) -> int
      |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
      |    gt(a: T, b: T) -> bool = cmp(a, b) > 0
      |""".stripMargin

  private val eqTrait =
    """trait Eq[T]
      |    eq(a: T, b: T) -> bool
      |""".stripMargin

  // ===== Bound satisfied =====

  "generic function with bound, satisfied by user type" in {
    eval(
      ordTrait +
      """struct Dollar
        |    cents: int
        |
        |impl Ord[Dollar]
        |    cmp(a: Dollar, b: Dollar) -> int = a.cents - b.cents
        |
        |maxOf[T: Ord](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    a = Dollar(100)
        |    b = Dollar(300)
        |    m = maxOf(a, b)
        |    m.cents
        |""".stripMargin) shouldBe 300
  }

  "generic function with bound, satisfied by multiple types" in {
    eval(
      ordTrait +
      """struct A
        |    v: int
        |
        |struct B
        |    v: int
        |
        |impl Ord[A]
        |    cmp(a: A, b: A) -> int = a.v - b.v
        |
        |impl Ord[B]
        |    cmp(a: B, b: B) -> int = a.v - b.v
        |
        |maxOf[T: Ord](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    x = A(10)
        |    y = A(20)
        |    mx = maxOf(x, y)
        |    p = B(100)
        |    q = B(50)
        |    mq = maxOf(p, q)
        |    mx.v + mq.v
        |""".stripMargin) shouldBe 120
  }

  "bound on generic function with two type params" in {
    eval(
      ordTrait +
      """struct Thing
        |    k: int
        |
        |impl Ord[Thing]
        |    cmp(a: Thing, b: Thing) -> int = a.k - b.k
        |
        |pickBigger[T: Ord, U](a: T, b: T, tag: U) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    x = Thing(5)
        |    y = Thing(15)
        |    r = pickBigger(x, y, 0)
        |    r.k
        |""".stripMargin) shouldBe 15
  }

  // ===== Bound unsatisfied =====

  "missing impl at call site violates bound" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """struct Widget
        |    id: int
        |
        |maxOf[T: Ord](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int
        |    a = Widget(1)
        |    b = Widget(2)
        |    m = maxOf(a, b)
        |    m.id
        |""".stripMargin)
  }

  "unknown trait in bound is error" in {
    an[Exception] should be thrownBy eval(
      """maxOf[T: Nonexistent](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int = maxOf(1, 2)
        |""".stripMargin)
  }

  // ===== Multiple bounds =====

  "multiple bounds with + syntax" in {
    eval(
      ordTrait + eqTrait +
      """struct Val
        |    n: int
        |
        |impl Ord[Val]
        |    cmp(a: Val, b: Val) -> int = a.n - b.n
        |
        |impl Eq[Val]
        |    eq(a: Val, b: Val) -> bool = a.n == b.n
        |
        |bothCheck[T: Ord + Eq](a: T, b: T) -> int
        |    if a == b then 0
        |    elif a < b then -1
        |    else 1
        |
        |main() -> int
        |    x = Val(10)
        |    y = Val(20)
        |    bothCheck(x, y)
        |""".stripMargin) shouldBe -1
  }

  "multiple bounds: one satisfied, other not is error" in {
    an[Exception] should be thrownBy eval(
      ordTrait + eqTrait +
      """struct Val
        |    n: int
        |
        |impl Ord[Val]
        |    cmp(a: Val, b: Val) -> int = a.n - b.n
        |
        |// no impl Eq[Val]
        |
        |bothCheck[T: Ord + Eq](a: T, b: T) -> bool
        |    a == b
        |
        |main() -> int
        |    x = Val(1)
        |    y = Val(2)
        |    if bothCheck(x, y) then 1 else 0
        |""".stripMargin)
  }

  // ===== Built-in types with bounds =====
  // (Built-in types don't have trait impls; bound requires user to provide one.
  //  So built-in ints don't satisfy a user-defined Ord bound unless impl Ord[int] exists.)

  "built-in int with no Ord impl fails Ord bound" in {
    an[Exception] should be thrownBy eval(
      ordTrait +
      """maxOf[T: Ord](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int = maxOf(3, 7)
        |""".stripMargin)
  }

  "built-in int with impl Ord[int] satisfies bound" in {
    eval(
      ordTrait +
      """impl Ord[int]
        |    cmp(a: int, b: int) -> int = a - b
        |
        |maxOf[T: Ord](a: T, b: T) -> T
        |    if a > b then a else b
        |
        |main() -> int = maxOf(3, 7)
        |""".stripMargin) shouldBe 7
  }
}
