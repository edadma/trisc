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

  // ===== Negative — `=> T` parser-restricted to param position =====

  "regression: `=> T` outside param position is rejected by the parser" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var f: => int = 0
          |    0
          |""".stripMargin)
    }
    // The parser doesn't accept `=> T` as a `typeRef` (only as `paramTypeRef`),
    // so this surfaces as a parse failure, not an analyzer error.
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("expected") || msg.contains("parser") || msg.contains("by-name") || msg.contains("type") || msg.contains("=>"),
      s"`=> T` outside param position should be rejected, got: ${ex.getMessage}")
  }

  // ===== Operator dispatch with by-name slots in trait method =====
  //
  // The motivating use case: parser-combinator grammars want
  //
  //   trait Or[T]
  //       #operator("|")
  //       or_op(a: T, b: => T) -> T
  //
  // so that `parser1 | parser2` defers `parser2`'s construction. Without
  // by-name on the operator, `expr | (paren ~> expr <~ ")"` infinitely
  // recurses at construction time.
  //
  // Mechanism: the BinaryAST analyzer reads the operator's trait method
  // params, checks each slot for `ByNameTypeAST`, and wraps the matching
  // operand AST in `ClosureAST(Nil, ExprBodyAST(...))` BEFORE analysis.
  // Impl method bodies bind by-name params with `isByName=true` so body
  // references auto-call. Both concrete and generic impls are supported
  // (instantiateImpl + analyzeImplMethods both record byName flags).

  "operator dispatch with by-name RHS defers evaluation (concrete impl)" in {
    eval(
      """var hits = 0
        |
        |bump() -> int
        |    hits = hits + 1
        |    99
        |
        |type Maybe[A] = new int
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: => T) -> T
        |
        |impl Or[Maybe[int]]
        |    or_op(a: Maybe[int], b: => Maybe[int]) -> Maybe[int] = a
        |
        |main() -> int
        |    val x: Maybe[int] = Maybe[int](7)
        |    val r: Maybe[int] = x | Maybe[int](bump())
        |    hits
        |""".stripMargin) shouldBe 0 // RHS never evaluated since LHS won
  }

  "operator dispatch with by-name RHS — generic impl, parser-combinator shape" in {
    eval(
      """var hits = 0
        |
        |type Parser[A] = new int
        |
        |bump() -> Parser[int]
        |    hits = hits + 1
        |    Parser[int](42)
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: => T) -> T
        |
        |impl[A] Or[Parser[A]]
        |    or_op(a: Parser[A], b: => Parser[A]) -> Parser[A] = a
        |
        |p_int() -> Parser[int] = Parser[int](7)
        |
        |main() -> int
        |    val r: Parser[int] = p_int() | bump()
        |    hits
        |""".stripMargin) shouldBe 0 // bump() never fires, by-name defers it
  }

  "operator dispatch with by-name — body that DOES evaluate sees the value" in {
    // Mirror case: when the impl body references the by-name param, it
    // auto-calls and the side effect fires exactly once per textual
    // reference (no memoization, matching the direct-CallAST semantics).
    eval(
      """var hits = 0
        |
        |bump() -> int
        |    hits = hits + 1
        |    7
        |
        |type Box = new int
        |
        |trait Combine[T]
        |    #operator("|")
        |    combine(a: T, b: => T) -> T
        |
        |impl Combine[Box]
        |    combine(a: Box, b: => Box) -> Box = b // forces eval
        |
        |main() -> int
        |    val r: Box = Box(0) | Box(bump())
        |    hits
        |""".stripMargin) shouldBe 1
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
