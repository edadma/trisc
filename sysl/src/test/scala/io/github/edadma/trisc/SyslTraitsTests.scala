package io.github.edadma.trisc

class SyslTraitsTests extends SyslTestHelpers {

  // ===== Basic trait with single required method =====

  "trait with required method, call on i32" in {
    eval(
      """trait Showable[T]
        |    showInt(x: T) -> int
        |
        |impl Showable[int]
        |    showInt(x: int) -> int = x * 10
        |
        |main() -> int = Showable.showInt(5)
        |""".stripMargin) shouldBe 50
  }

  "trait with multiple methods" in {
    eval(
      """trait Binop[T]
        |    add(a: T, b: T) -> T
        |    sub(a: T, b: T) -> T
        |
        |impl Binop[int]
        |    add(a: int, b: int) -> int = a + b
        |    sub(a: int, b: int) -> int = a - b
        |
        |main() -> int = Binop.add(3, 4) * Binop.sub(10, 7)
        |""".stripMargin) shouldBe 21
  }

  "trait with multiple impls for different types" in {
    eval(
      """trait Double[T]
        |    twice(x: T) -> T
        |
        |impl Double[int]
        |    twice(x: int) -> int = x + x
        |
        |impl Double[i64]
        |    twice(x: i64) -> i64 = x * 2i64
        |
        |main() -> int
        |    a = Double.twice(5)
        |    b: i64 = Double.twice(7i64)
        |    a + i32(b)
        |""".stripMargin) shouldBe 24
  }

  // ===== Error cases =====

  "impl missing required method is error" in {
    an[Exception] should be thrownBy eval(
      """trait Two[T]
        |    one(x: T) -> T
        |    two(x: T) -> T
        |
        |impl Two[int]
        |    one(x: int) -> int = x + 1
        |
        |main() -> int = Two.one(5)
        |""".stripMargin)
  }

  "impl for unknown trait is error" in {
    an[Exception] should be thrownBy eval(
      """impl Nope[int]
        |    foo(x: int) -> int = x
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "duplicate impl is error" in {
    an[Exception] should be thrownBy eval(
      """trait X[T]
        |    f(x: T) -> T
        |
        |impl X[int]
        |    f(x: int) -> int = x + 1
        |
        |impl X[int]
        |    f(x: int) -> int = x + 2
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "trait method signature mismatch is error" in {
    an[Exception] should be thrownBy eval(
      """trait X[T]
        |    f(x: T) -> T
        |
        |impl X[int]
        |    f(x: int) -> bool = true
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "impl method not declared in trait is error" in {
    an[Exception] should be thrownBy eval(
      """trait X[T]
        |    f(x: T) -> T
        |
        |impl X[int]
        |    f(x: int) -> int = x
        |    g(x: int) -> int = x
        |
        |main() -> int = 0
        |""".stripMargin)
  }

  "no impl for type is error at call site" in {
    an[Exception] should be thrownBy eval(
      """trait X[T]
        |    f(x: T) -> T
        |
        |impl X[int]
        |    f(x: int) -> int = x + 1
        |
        |main() -> int
        |    var x: i64 = 5i64
        |    X.f(x)
        |""".stripMargin)
  }

  // ===== Traits with more than one param =====

  "trait method with two T-typed params" in {
    eval(
      """trait Max[T]
        |    max(a: T, b: T) -> T
        |
        |impl Max[int]
        |    max(a: int, b: int) -> int = if a > b then a else b
        |
        |main() -> int = Max.max(3, 7)
        |""".stripMargin) shouldBe 7
  }

  "trait method with mixed T and concrete" in {
    eval(
      """trait Scale[T]
        |    scale(x: T, n: int) -> T
        |
        |impl Scale[int]
        |    scale(x: int, n: int) -> int = x * n
        |
        |main() -> int = Scale.scale(5, 6)
        |""".stripMargin) shouldBe 30
  }

  // ===== Default methods =====

  "default method used when not overridden" in {
    eval(
      """trait Greeter[T]
        |    baseVal(x: T) -> T
        |    doubled(x: T) -> T = baseVal(x) + baseVal(x)
        |
        |impl Greeter[int]
        |    baseVal(x: int) -> int = x * 3
        |
        |main() -> int = Greeter.doubled(5)
        |""".stripMargin) shouldBe 30
  }

  "default method chain — Ord-style" in {
    eval(
      """trait Ord[T]
        |    cmp(a: T, b: T) -> int
        |    lt(a: T, b: T) -> bool = cmp(a, b) < 0
        |    le(a: T, b: T) -> bool = cmp(a, b) <= 0
        |    gt(a: T, b: T) -> bool = cmp(a, b) > 0
        |    ge(a: T, b: T) -> bool = cmp(a, b) >= 0
        |
        |impl Ord[int]
        |    cmp(a: int, b: int) -> int = a - b
        |
        |main() -> int
        |    var c = 0
        |    if Ord.lt(3, 5) then c += 1
        |    if Ord.le(5, 5) then c += 1
        |    if Ord.gt(5, 3) then c += 1
        |    if Ord.ge(5, 5) then c += 1
        |    if !Ord.lt(5, 3) then c += 1
        |    c
        |""".stripMargin) shouldBe 5
  }

  "overriding a default method works" in {
    eval(
      """trait Cached[T]
        |    compute(x: T) -> T
        |    result(x: T) -> T = compute(x)
        |
        |impl Cached[int]
        |    compute(x: int) -> int = x * 2
        |    result(x: int) -> int = 999
        |
        |main() -> int = Cached.result(5)
        |""".stripMargin) shouldBe 999
  }

  "default method works for different types" in {
    eval(
      """trait Counter[T]
        |    incr(x: T) -> T
        |    twice(x: T) -> T = incr(incr(x))
        |
        |impl Counter[int]
        |    incr(x: int) -> int = x + 1
        |
        |impl Counter[i64]
        |    incr(x: i64) -> i64 = x + 10i64
        |
        |main() -> int
        |    a = Counter.twice(3)
        |    b: i64 = Counter.twice(100i64)
        |    a + i32(b)
        |""".stripMargin) shouldBe (3 + 1 + 1 + 120)
  }

  "default method calling another default" in {
    eval(
      """trait Math[T]
        |    base(x: T) -> T
        |    plus1(x: T) -> T = base(x) + base(x) - base(x) + base(x) - base(x) + 1
        |    plus2(x: T) -> T = plus1(x) + 1
        |
        |impl Math[int]
        |    base(x: int) -> int = x
        |
        |main() -> int = Math.plus2(10)
        |""".stripMargin) shouldBe 12
  }
}
