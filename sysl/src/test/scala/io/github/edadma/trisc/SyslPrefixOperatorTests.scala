package io.github.edadma.trisc

class SyslPrefixOperatorTests extends SyslTestHelpers {

  // ===== Prefix operator overloading via `#operator` =====
  //
  // A trait method annotated with `#operator("sym")` and taking a *single*
  // parameter binds `sym` as a prefix operator. The parser admits any
  // operator-char keyword that's not a built-in prefix sigil (`-`, `!`, `~`,
  // `*`, `&`, `++`, `--`) and not in the binary-op reserved set; the analyzer
  // dispatches `UnaryAST(op, e)` through the registered trait when one is
  // bound, otherwise reports an error that points the user at `#operator`.

  "prefix `<>` on user struct dispatches via #operator" in {
    eval(
      """struct N
        |    v: int
        |
        |trait Boost[T]
        |    #operator("<>")
        |    boost(a: T) -> T
        |
        |impl Boost[N]
        |    boost(a: N) -> N = N(a.v * 2 + 1)
        |
        |main() -> int
        |    var x = N(7)
        |    var r = <>x
        |    r.v
        |""".stripMargin) shouldBe 15
  }

  "prefix dispatch survives second registration on a different trait/struct" in {
    // Two prefix ops, two traits, two structs — exercises the customUnary map
    // beyond a single registration and confirms each routes to the right impl.
    eval(
      """struct A
        |    v: int
        |
        |struct B
        |    v: int
        |
        |trait Boost[T]
        |    #operator("<>")
        |    boost(a: T) -> T
        |
        |trait Flip[T]
        |    #operator("~~")
        |    flip(a: T) -> T
        |
        |impl Boost[A]
        |    boost(a: A) -> A = A(a.v + 1)
        |
        |impl Flip[B]
        |    flip(a: B) -> B = B(0 - a.v)
        |
        |main() -> int
        |    var a = <>A(10)
        |    var b = ~~B(7)
        |    a.v * 100 + b.v
        |""".stripMargin) shouldBe 1093
  }

  "prefix `<>` chains right-associatively (via whitespace-separated tokens)" in {
    // Maximal-munch lexing means `<><><>x` would be one Keyword token; chained
    // application requires either whitespace between the prefix ops OR explicit
    // parens. Whitespace separation is the natural shape:
    eval(
      """struct N
        |    v: int
        |
        |trait Inc[T]
        |    #operator("<>")
        |    inc(a: T) -> T
        |
        |impl Inc[N]
        |    inc(a: N) -> N = N(a.v + 1)
        |
        |main() -> int
        |    var x = N(0)
        |    var r = <> <> <> x
        |    r.v
        |""".stripMargin) shouldBe 3
  }

  "prefix `<>` composes with infix dispatch on the operand" in {
    // The operand to the prefix op is itself a `+` over user types; dispatch
    // through `Add` first, then through `Neg2` on the resulting struct.
    eval(
      """struct N
        |    v: int
        |
        |trait Add[T]
        |    add(a: T, b: T) -> T
        |
        |trait Neg2[T]
        |    #operator("<>")
        |    neg2(a: T) -> T
        |
        |impl Add[N]
        |    add(a: N, b: N) -> N = N(a.v + b.v)
        |
        |impl Neg2[N]
        |    neg2(a: N) -> N = N(0 - a.v)
        |
        |main() -> int
        |    var r = <>(N(3) + N(4))
        |    r.v
        |""".stripMargin) shouldBe -7
  }

  // ===== Negative — built-in prefix ops are reserved =====

  "trait method tagged `#operator(\"-\")` on a single param is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """struct N
          |    v: int
          |
          |trait Neg[T]
          |    #operator("-")
          |    neg(a: T) -> T
          |
          |impl Neg[N]
          |    neg(a: N) -> N = N(0 - a.v)
          |
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("reserved") || msg.contains("built-in") || msg.contains("'-'"),
      s"`-` should be reserved, got: ${ex.getMessage}")
  }

  "trait method with `#operator(...)` and zero params is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """trait Bad[T]
          |    #operator("<~>")
          |    nothing() -> int
          |
          |main() -> int = 0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("one") || msg.contains("two") || msg.contains("param"),
      s"zero-arg #operator should be rejected by arity check, got: ${ex.getMessage}")
  }

  // ===== Diagnostics =====

  "unbound prefix operator yields actionable error mentioning #operator" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var x = 1
          |    <>x
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("<>"), s"error should mention the operator, got: $msg")
    assert(msg.contains("#operator") || msg.toLowerCase.contains("prefix"),
      s"error should suggest #operator (or mention 'prefix'), got: $msg")
  }

  "registered prefix op on wrong-type operand surfaces 'no impl'" in {
    val ex = intercept[Exception] {
      eval(
        """struct Box
          |    n: int
          |
          |trait Boost[T]
          |    #operator("<>")
          |    boost(a: T) -> T
          |
          |impl Boost[Box]
          |    boost(a: Box) -> Box = Box(a.n + 1)
          |
          |main() -> int
          |    var x = 7
          |    <>x
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.contains("<>"), s"error should mention the operator, got: $msg")
    assert(msg.toLowerCase.contains("no impl") || msg.contains("Boost"),
      s"error should say 'no impl' (or name the trait), got: $msg")
  }

  // ===== Regression — built-in prefix ops still work =====

  "regression: built-in `-` still negates int" in {
    eval(
      """main() -> int
        |    var x = 7
        |    -x
        |""".stripMargin) shouldBe -7
  }

  "regression: built-in `~` still bitwise-nots int" in {
    eval(
      """main() -> int
        |    var x = 0
        |    ~x
        |""".stripMargin) shouldBe -1
  }

  "regression: built-in `!` still negates bool" in {
    eval(
      """main() -> int = if !false then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Regression — binary `#operator` still works after arity-aware registration =====

  "regression: binary `#operator(\"<>\")` still dispatches alongside prefix `<>`" in {
    // Same trait can't define both prefix and infix `<>` (arity routes by
    // method param count; they go into different maps), but two *different*
    // traits can register prefix and infix separately for the same symbol.
    // Here we keep it simple: only the binary form, ensuring it still
    // works after the arity-routed registrar change.
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
        |    var a = Set(3)
        |    var b = Set(12)
        |    var c = a <> b
        |    c.bits
        |""".stripMargin) shouldBe 15
  }
}
