package io.github.edadma.trisc

class SyslNominalOpDispatchTests extends SyslTestHelpers {

  // ===== Nominal-alias function type as both operands (parsyl repro) =====
  //
  // The bug this fixes: operator dispatch read each operand's `.underlying`
  // before deciding whether the operand is "user-defined". For a nominal alias
  // `type Parser[A] = new (Input) -> int`, that strip turned `Parser[i32]` into
  // `(Input) -> int` and the dispatcher concluded "neither operand is a struct/
  // enum that impls it" — even when an `impl Concat[Parser[X], Parser[Y], ...]`
  // existed and would have matched perfectly.

  "nominal-alias of fn type, both sides, dispatches via #operator" in {
    eval(
      """struct Input
        |    pos: int
        |
        |type Parser[A] = new (Input) -> int
        |
        |trait Concat[A, B, R]
        |    #operator("~")
        |    concat(a: A, b: B) -> R
        |
        |impl[X, Y] Concat[Parser[X], Parser[Y], int]
        |    concat(a: Parser[X], b: Parser[Y]) -> int = 99
        |
        |success() -> Parser[int] = Parser[int]((inp: Input) -> 1)
        |
        |main() -> int
        |    val p = success()
        |    val q = success()
        |    p ~ q
        |""".stripMargin) shouldBe 99
  }

  // ===== Nominal-alias on one side, primitive on the other =====
  //
  // Mixes the "either operand" relaxation (2c428699d) with the nominal-alias
  // fix: even if only one operand is the nominal type, dispatch must still see
  // its outer type — not the underlying.

  "nominal-alias on lhs, primitive on rhs, dispatches" in {
    eval(
      """struct Input
        |    pos: int
        |
        |type Parser[A] = new (Input) -> int
        |
        |trait WithStr[A, R]
        |    #operator("~")
        |    cat(a: A, b: string) -> R
        |
        |impl[X] WithStr[Parser[X], int]
        |    cat(a: Parser[X], b: string) -> int = 7
        |
        |success() -> Parser[int] = Parser[int]((inp: Input) -> 1)
        |
        |main() -> int
        |    val p = success()
        |    p ~ "hello"
        |""".stripMargin) shouldBe 7
  }

  "primitive on lhs, nominal-alias on rhs, dispatches" in {
    eval(
      """struct Input
        |    pos: int
        |
        |type Parser[A] = new (Input) -> int
        |
        |trait FromStr[A, R]
        |    #operator("~")
        |    cat(a: string, b: A) -> R
        |
        |impl[X] FromStr[Parser[X], int]
        |    cat(a: string, b: Parser[X]) -> int = 8
        |
        |success() -> Parser[int] = Parser[int]((inp: Input) -> 1)
        |
        |main() -> int
        |    val p = success()
        |    "x" ~ p
        |""".stripMargin) shouldBe 8
  }

  // ===== Nominal-alias of a non-function type =====
  //
  // The fix isn't function-type-specific. Any nominal alias should preserve
  // its outer name through operator dispatch.

  "nominal alias of struct type dispatches" in {
    eval(
      """struct Cell
        |    v: int
        |
        |type Box[T] = new Cell
        |
        |trait Plus[T, R]
        |    #operator("|+|")
        |    plus(a: T, b: T) -> R
        |
        |impl[X] Plus[Box[X], int]
        |    plus(a: Box[X], b: Box[X]) -> int = 42
        |
        |main() -> int
        |    val a = Box[int](Cell(1))
        |    val b = Box[int](Cell(2))
        |    a |+| b
        |""".stripMargin) shouldBe 42
  }

  // ===== Regression: operators on plain structs and enums still work =====

  "regression: Vec2 + Vec2 with impl Add[Vec2] still dispatches" in {
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
        |    val a = Vec2(1, 2)
        |    val b = Vec2(10, 20)
        |    val c = a + b
        |    c.x * 100 + c.y
        |""".stripMargin) shouldBe 1122
  }

  // ===== Regression: nominal-alias of a numeric type — without impl, plain arithmetic =====
  //
  // Important behaviour: my fix added an "early dispatch" attempt that runs
  // before the nominal-unwrap logic. For `Meters + Meters` where Meters wraps
  // an int and there is NO `impl Add[Meters]`, the existing code unwraps the
  // nominal type, runs int arithmetic, and re-wraps the result. This must keep
  // working — early-dispatch is "lenient" when both operands could fall back to
  // a built-in arithmetic interpretation.

  "regression: nominal alias of int with no impl falls through to arithmetic" in {
    eval(
      """type Meters = new int
        |
        |main() -> int
        |    val a: Meters = Meters(3)
        |    val b: Meters = Meters(4)
        |    val c: Meters = a + b
        |    int(c)
        |""".stripMargin) shouldBe 7
  }

  "regression: nominal alias of int with impl Add fires dispatch" in {
    eval(
      """type Meters = new int
        |
        |trait Add[T]
        |    add(a: T, b: T) -> T
        |
        |impl Add[Meters]
        |    add(a: Meters, b: Meters) -> Meters = Meters(int(a) + int(b) + 1000)
        |
        |main() -> int
        |    val a: Meters = Meters(3)
        |    val b: Meters = Meters(4)
        |    val c: Meters = a + b
        |    int(c)
        |""".stripMargin) shouldBe 1007
  }

  // ===== Negative — transparent alias (no `new`) does not impl-dispatch =====
  //
  // A plain `type Plain = T` alias is transparent: the underlying type is what
  // operator dispatch sees. So a transparent alias of a function type still
  // can't carry an operator impl.

  "transparent alias of fn type does not dispatch user operator" in {
    val ex = intercept[Exception] {
      eval(
        """type Plain = (int) -> int
          |
          |trait Fuse[T]
          |    #operator("~")
          |    fuse(a: T, b: T) -> T
          |
          |id(x: int) -> int = x
          |
          |main() -> int
          |    var f1: Plain = id
          |    var f2: Plain = id
          |    val r = f1 ~ f2
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.toLowerCase.contains("fuse") || msg.toLowerCase.contains("impl") || msg.toLowerCase.contains("operator"),
      s"transparent fn-type alias should fail to dispatch ~, got: $msg")
  }

  // ===== Diagnostic-quality regression =====

  "diagnostic names trait and operand types when no impl matches" in {
    val ex = intercept[Exception] {
      eval(
        """struct Foo
          |    v: int
          |
          |trait Bar[T]
          |    #operator("|+|")
          |    bar(a: T, b: T) -> T
          |
          |main() -> int
          |    val a = Foo(1)
          |    val b = Foo(2)
          |    val r = a |+| b
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("Bar"), s"diagnostic should name the trait, got: $msg")
    assert(msg.contains("Foo"), s"diagnostic should name the operand types, got: $msg")
    assert(msg.contains("|+|"), s"diagnostic should name the operator, got: $msg")
  }
}
