package io.github.edadma.trisc

class SyslByNameParamTests extends SyslTestHelpers {

  // ===== Call-by-name parameter syntax (`=> T`) =====
  //
  // The natural shape for parser-combinator grammars and other deferred-
  // evaluation use cases:
  //
  //   or_op(a: Parser[A], b: => Parser[A]) -> Parser[A] = …
  //
  // The `=> T` marks `b` as call-by-name. Internally the parameter's
  // storage type is `() -> T` (a zero-arg thunk); the analyzer:
  //   (1) auto-wraps the matching arg at the call site as `() -> arg`,
  //   (2) auto-evaluates each body reference to `b` as `b()`.
  //
  // Re-evaluation matches Scala's call-by-name (no memoization) — every
  // textual reference re-enters the thunk.

  "by-name param defers evaluation; each ref re-enters" in {
    eval(
      """var hits = 0
        |
        |bump() -> int
        |    hits = hits + 1
        |    7
        |
        |use_lazy(b: => int) -> int = b + b
        |
        |main() -> int
        |    val r = use_lazy(bump())
        |    hits * 100 + r
        |""".stripMargin) shouldBe 214 // hits=2, r=14
  }

  "by-name param not evaluated when never referenced" in {
    eval(
      """fail_if_called() -> int
        |    panic("should not run")
        |    0
        |
        |ignore(b: => int) -> int = 0
        |main() -> int = ignore(fail_if_called())
        |""".stripMargin) shouldBe 0
  }

  "by-name unblocks self-recursive shape — body chooses to evaluate or not" in {
    // The body decides whether to enter the thunk, so a recursive call site
    // wrapped in a by-name slot will only fire if the body actually references
    // it. Here `pick_one` ignores the recursive thunk for x==0 and returns
    // the base case; for x>0 it evaluates the thunk, recursing into a deeper
    // call. Without by-name the recursive call would always evaluate at the
    // *call site*, infinite-looping at construction time.
    eval(
      """pick_one(x: int, rec: => int) -> int = if x == 0 then 0 else rec
        |go(n: int) -> int = pick_one(n, go(n - 1))
        |main() -> int = go(5)
        |""".stripMargin) shouldBe 0
  }

  "passing a by-name to another by-name forwards correctly" in {
    eval(
      """outer(b: => int) -> int = inner(b)
        |inner(c: => int) -> int = c + 1
        |main() -> int = outer(41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative — by-name only valid in param position =====

  "regression: `=> T` outside param position is rejected" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var f: => int = 0
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("by-name") || msg.contains("=>") || msg.contains("parameter"),
      s"`=> T` outside param position should be rejected, got: ${ex.getMessage}")
  }

  // ===== Regression — normal `T` params unchanged =====

  "regression: ordinary param semantics unchanged (eager eval)" in {
    eval(
      """var hits = 0
        |
        |bump() -> int
        |    hits = hits + 1
        |    7
        |
        |take_eager(x: int) -> int = x + x
        |
        |main() -> int
        |    val r = take_eager(bump())
        |    hits * 100 + r
        |""".stripMargin) shouldBe 114 // hits=1, r=14
  }
}
