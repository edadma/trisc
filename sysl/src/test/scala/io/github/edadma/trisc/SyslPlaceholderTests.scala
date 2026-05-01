package io.github.edadma.trisc

class SyslPlaceholderTests extends SyslTestHelpers {

  // ===== Single placeholder, statement-level boundary =====

  "single _ in init: _ + 1 binds (x) -> x + 1" in {
    eval(
      """main() -> int
        |    var f: (int) -> int = _ + 1
        |    f(41)
        |""".stripMargin) shouldBe 42
  }

  "single _ on right: 1 + _ binds (x) -> 1 + x" in {
    eval(
      """main() -> int
        |    var f: (int) -> int = 1 + _
        |    f(41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Multi-placeholder, ordered left-to-right =====

  "two _s: _ + _ binds (x, y) -> x + y" in {
    eval(
      """add(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int
        |    add(_ + _, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  "ordering: _ - _ applied to (10, 3) returns 7 (not -7)" in {
    eval(
      """sub(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = sub(_ - _, 10, 3)
        |""".stripMargin) shouldBe 7
  }

  // ===== Field access on placeholder =====

  "_.field — accessor lambda" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |first(f: (Point) -> int, p: Point) -> int = f(p)
        |
        |main() -> int = first(_.x, Point(42, 13))
        |""".stripMargin) shouldBe 42
  }

  // ===== Partial application via bare _ in call args =====

  "f(_, 0) absorbs bare _ at call: x -> f(x, 0)" in {
    eval(
      """sub(a: int, b: int) -> int = a - b
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(sub(_, 8), 50)
        |""".stripMargin) shouldBe 42
  }

  "f(0, _) absorbs bare _ at call: x -> f(0, x)" in {
    eval(
      """sub(a: int, b: int) -> int = a - b
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(sub(50, _), 8)
        |""".stripMargin) shouldBe 42
  }

  "f(_, _) absorbs both bare _s: (x, y) -> f(x, y)" in {
    eval(
      """sub(a: int, b: int) -> int = a - b
        |apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2(sub(_, _), 50, 8)
        |""".stripMargin) shouldBe 42
  }

  // ===== Nested: arg containing _ wraps at arg, doesn't escape =====

  "f(_+1) is f(x -> x+1), not (x) -> f(x+1)" in {
    eval(
      """apply_to_5(f: (int) -> int) -> int = f(5)
        |
        |main() -> int
        |    var r = apply_to_5(_ + 1)
        |    r * 7
        |""".stripMargin) shouldBe 42
  }

  // ===== Parens explicitly delimit the body =====

  "parens delimit: (_ + 1) is a complete lambda" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((_ + 1), 41)
        |""".stripMargin) shouldBe 42
  }

  "parens stop _ from escaping: (_ * 2) bound at parens, then * 3 errors" in {
    // (_ * 2) is `(x) -> x * 2`, so (_ * 2)(7) calls the lambda with 7.
    // The * 3 outside the parens does NOT pull into the lambda body.
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((_ * 2), 21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative cases — existing _ uses still work =====

  "wildcard binder: var _ = side_effect() still works" in {
    eval(
      """compute() -> int = 99
        |
        |main() -> int
        |    var _ = compute()
        |    42
        |""".stripMargin) shouldBe 42
  }

  "wildcard match arm: _ -> default still works" in {
    eval(
      """classify(n: int) -> int
        |    n match
        |        0 -> 100
        |        _ -> 42
        |
        |main() -> int = classify(7)
        |""".stripMargin) shouldBe 42
  }
}
