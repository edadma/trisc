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

  // ===== Custom binary operators via #operator on trait methods =====

  "binary ~ via #operator on trait method" in {
    eval(
      """struct Bits
        |    v: int
        |
        |trait Fuse[T]
        |    #operator("~")
        |    fuse(a: T, b: T) -> T
        |
        |impl Fuse[Bits]
        |    fuse(a: Bits, b: Bits) -> Bits = Bits(a.v | b.v)
        |
        |main() -> int
        |    a = Bits(5)
        |    b = Bits(10)
        |    c = a ~ b
        |    c.v
        |""".stripMargin) shouldBe 15
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

  // ===== Multi-char user operators (greedy lexer + first-char ladder) =====
  // Cover the new pipeline: lexer munches `<>`, `>>>`, `|>` as single
  // tokens; parser slots them at the precedence level matching their
  // first char; analyzer dispatches to the trait method bound via
  // `#operator(...)`.

  "binary <> via #operator on trait method" in {
    eval(
      """struct Set
        |    bits: int
        |
        |trait Union[T]
        |    #operator("<>")
        |    union(a: T, b: T) -> T
        |
        |impl Union[Set]
        |    union(a: Set, b: Set) -> Set = Set(a.bits | b.bits)
        |
        |main() -> int
        |    a = Set(3)
        |    b = Set(12)
        |    c = a <> b
        |    c.bits
        |""".stripMargin) shouldBe 15
  }

  "binary >>> via #operator on trait method" in {
    eval(
      """struct Bits
        |    v: int
        |
        |trait Shr[T]
        |    #operator(">>>")
        |    shr(a: T, b: T) -> T
        |
        |impl Shr[Bits]
        |    shr(a: Bits, b: Bits) -> Bits = Bits(a.v >> b.v)
        |
        |main() -> int
        |    a = Bits(64)
        |    b = Bits(2)
        |    c = a >>> b
        |    c.v
        |""".stripMargin) shouldBe 16
  }

  "binary |> pipe operator" in {
    eval(
      """struct Box
        |    n: int
        |
        |trait Pipe[T]
        |    #operator("|>")
        |    pipe(a: T, b: T) -> T
        |
        |impl Pipe[Box]
        |    pipe(a: Box, b: Box) -> Box = Box(a.n + b.n * 10)
        |
        |main() -> int
        |    a = Box(3)
        |    b = Box(4)
        |    r = a |> b
        |    r.n
        |""".stripMargin) shouldBe 43
  }

  // User op `*>` (starts with `*`) slots at multiplicative precedence.
  // Verifies (a) the muncher doesn't split `*>` into `*` `>` post-fix,
  // and (b) the parser routes it through the multiplicative ladder so
  // the trait's `times` method is dispatched.
  "user op *> at multiplicative precedence" in {
    eval(
      """struct N
        |    v: int
        |
        |trait Times[T]
        |    #operator("*>")
        |    times(a: T, b: T) -> T
        |
        |impl Times[N]
        |    times(a: N, b: N) -> N = N(a.v * b.v)
        |
        |main() -> int
        |    a = N(2)
        |    b = N(3)
        |    c = N(10)
        |    r = a *> b *> c
        |    r.v
        |""".stripMargin) shouldBe 60
  }

  // Diagnostic: unbound user operator gives the user a hint about
  // `#operator(...)` rather than a generic "unknown operator".
  "unbound user operator yields actionable error" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var a = 1
          |    var b = 2
          |    a <~> b
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("<~>"), s"error should mention the operator, got: $msg")
    assert(msg.contains("#operator"), s"error should suggest #operator, got: $msg")
  }

  // Diagnostic: user-op IS bound, but operands are built-in scalars (not
  // a struct/enum that impls the trait) — name the trait so the user
  // knows what to wrap their operands in.
  "user op on wrong-type operands names the trait" in {
    val ex = intercept[Exception] {
      eval(
        """trait Pipe[T]
          |    #operator("|>")
          |    pipe(a: T, b: T) -> T
          |
          |main() -> int
          |    var a = 1
          |    var b = 2
          |    a |> b
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("|>"), s"error should mention the operator, got: $msg")
    assert(msg.contains("Pipe"), s"error should name the bound trait, got: $msg")
  }

  // ===== Either-operand dispatch: at least one operand may be user-defined =====
  //
  // The dispatch rule is "at least one operand is a user-defined named type",
  // not LHS-only. This unblocks parser-combinator-style sugar like
  // `"foo" ~ ident` and scalar-times-vector math like `2 * v`.

  "primitive on LHS + user on RHS dispatches via #operator" in {
    eval(
      """struct Box
        |    n: int
        |
        |trait Pipe[A, B, R]
        |    #operator("~")
        |    pipe(a: A, b: B) -> R
        |
        |impl Pipe[int, Box, int]
        |    pipe(a: int, b: Box) -> int = a + b.n
        |
        |main() -> int = 100 ~ Box(42)
        |""".stripMargin) shouldBe 142
  }

  "user on LHS + primitive on RHS dispatches via #operator" in {
    eval(
      """struct Box
        |    n: int
        |
        |trait Pipe[A, B, R]
        |    #operator("~")
        |    pipe(a: A, b: B) -> R
        |
        |impl Pipe[Box, int, int]
        |    pipe(a: Box, b: int) -> int = a.n + b
        |
        |main() -> int = Box(42) ~ 100
        |""".stripMargin) shouldBe 142
  }

  "scalar * Vec3 dispatches via Mul with mixed-operand impl" in {
    eval(
      """struct Vec3
        |    x: int
        |    y: int
        |    z: int
        |
        |trait Mul[A, B, R]
        |    mul(a: A, b: B) -> R
        |
        |impl Mul[int, Vec3, Vec3]
        |    mul(s: int, v: Vec3) -> Vec3 = Vec3(s * v.x, s * v.y, s * v.z)
        |
        |main() -> int
        |    var v = 3 * Vec3(1, 2, 4)
        |    v.x + v.y + v.z
        |""".stripMargin) shouldBe 21
  }

  "both primitives — no user dispatch, falls back to built-in int + int" in {
    eval(
      """main() -> int = 3 + 4
        |""".stripMargin) shouldBe 7
  }

  "both primitives + custom op (no impl, no built-in) — clean error mentioning trait" in {
    val ex = intercept[Exception] {
      eval(
        """trait Pipe[A, B, R]
          |    #operator("~")
          |    pipe(a: A, b: B) -> R
          |
          |main() -> int = "foo" ~ "bar"
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("~"), s"error should mention the operator, got: $msg")
    assert(msg.contains("Pipe"), s"error should name the bound trait, got: $msg")
  }

  "user on one side + primitive on other, no matching impl — 'no impl' error" in {
    val ex = intercept[Exception] {
      eval(
        """struct Box
          |    n: int
          |
          |trait Pipe[A, B, R]
          |    #operator("~")
          |    pipe(a: A, b: B) -> R
          |
          |impl Pipe[Box, Box, int]
          |    pipe(a: Box, b: Box) -> int = a.n + b.n
          |
          |main() -> int = 1 ~ Box(2)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("~"), s"error should mention the operator, got: $msg")
    assert(msg.contains("no impl"), s"error should say 'no impl', got: $msg")
  }

  "homogeneous Vec2 + Vec2 still dispatches (regression)" in {
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
        |    var v = Vec2(1, 2) + Vec2(10, 20)
        |    v.x + v.y
        |""".stripMargin) shouldBe 33
  }
}
