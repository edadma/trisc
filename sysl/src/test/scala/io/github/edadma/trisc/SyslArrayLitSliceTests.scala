package io.github.edadma.trisc

class SyslArrayLitSliceTests extends SyslTestHelpers {

  // ===== Array literal under a `[]T` annotation produces a real slice =====
  //
  // The long-standing footgun: `var xs: []int = [1, 2, 3]` silently produced
  // `[3]int` (a fixed-size array), even though the annotation explicitly said
  // `[]T`. The mismatch was invisible at compile time and surfaced at runtime
  // as `panic: append requires a slice` once the user reached for `append`.
  //
  // The standard workaround was `(new [0]T)[:0]` — allocate a zero-cap fixed
  // array, then slice it. Unidiomatic and viral: every accumulator / builder
  // pattern carried it.
  //
  // Fix: in the analyzer, when an `ArrayLitAST` is analyzed under a
  // `currentExpected` of `SliceType[T]`, wrap the resulting `TArrayLit` in a
  // `TSliceExpr(_, None, None, SliceType[T])`. That's the `arr[:]` operation
  // the user used to write by hand. Empty `[]` works the same way: produces a
  // zero-cap slice descriptor. Fixed-array context (`[N]T`) and no-annotation
  // context are unchanged.

  // ===== Concrete `[]T` annotations =====

  "var xs: []int = [] is a real slice (append works)" in {
    eval(
      """main() -> int
        |    var xs: []int = []
        |    xs = append(xs, 7)
        |    xs = append(xs, 42)
        |    len(xs)
        |""".stripMargin) shouldBe 2
  }

  "var xs: []int = [1, 2, 3] is a real slice" in {
    eval(
      """main() -> int
        |    var xs: []int = [1, 2, 3]
        |    xs = append(xs, 4)
        |    len(xs)
        |""".stripMargin) shouldBe 4
  }

  "var ys: []string = [s] is a real slice" in {
    eval(
      """main() -> int
        |    val s = "hi"
        |    var ys: []string = [s]
        |    ys = append(ys, "bye")
        |    len(ys)
        |""".stripMargin) shouldBe 2
  }

  "[]T slice elements are read back correctly" in {
    eval(
      """main() -> int
        |    var xs: []int = [10, 20, 30]
        |    xs[0] + xs[1] + xs[2]
        |""".stripMargin) shouldBe 60
  }

  // ===== Generic `[]A` annotations =====

  "generic [A] = [first] inside `[]A` body produces a slice" in {
    eval(
      """collect[A](x: A) -> []A =
        |    var xs: []A = [x]
        |    xs = append(xs, x)
        |    xs
        |
        |main() -> int = len(collect[int](7))
        |""".stripMargin) shouldBe 2
  }

  "generic empty []A annotation produces a slice" in {
    eval(
      """make_empty[A]() -> []A =
        |    var xs: []A = []
        |    xs
        |
        |main() -> int = len(make_empty[int]())
        |""".stripMargin) shouldBe 0
  }

  // ===== Return-type position drives the same coercion =====

  "function returning []T from a literal returns a slice" in {
    eval(
      """make() -> []int = [1, 2, 3]
        |
        |main() -> int
        |    var xs = make()
        |    xs = append(xs, 4)
        |    len(xs)
        |""".stripMargin) shouldBe 4
  }

  "function returning []T from [] returns an empty slice" in {
    eval(
      """make() -> []int = []
        |
        |main() -> int
        |    var xs = make()
        |    xs = append(xs, 99)
        |    len(xs)
        |""".stripMargin) shouldBe 1
  }

  // ===== Argument position drives the same coercion =====

  "passing [a, b, c] into a `[]T` parameter coerces to slice" in {
    eval(
      """sum(xs: []int) -> int
        |    var total = 0
        |    for x in xs do total = total + x
        |    total
        |
        |main() -> int = sum([1, 2, 3, 4])
        |""".stripMargin) shouldBe 10
  }

  // ===== Annotation drives inference through assignment too =====

  "assign to existing []T var: `xs = []` produces a slice" in {
    // Existing-target assignment forwards the var's declared type as the RHS
    // expected type. Without this the bare `[]` would fail with "cannot infer
    // element type for empty array literal" — there's no declared type at the
    // RHS site.
    eval(
      """main() -> int
        |    var xs: []int = [1, 2, 3]
        |    xs = []
        |    xs = append(xs, 99)
        |    len(xs)
        |""".stripMargin) shouldBe 1
  }

  "assign to existing []T var: `xs = [a, b]` re-coerces to slice" in {
    eval(
      """main() -> int
        |    var xs: []int = []
        |    xs = [10, 20, 30]
        |    xs = append(xs, 40)
        |    len(xs)
        |""".stripMargin) shouldBe 4
  }

  "field assign: `self.buf = []` produces a slice when field is []T" in {
    // FieldAssignStmtAST forwards the field's declared type as the RHS expected.
    // Without this, the field-assign equivalent of `self.buf = (new [0]byte)[:0]`
    // couldn't shrink to the natural `self.buf = []`.
    eval(
      """struct Builder
        |    items: []int
        |
        |reset(self: *Builder) -> int
        |    self.items = []
        |    len(self.items)
        |
        |main() -> int
        |    var b = Builder([1, 2, 3])
        |    reset(&b)
        |""".stripMargin) shouldBe 0
  }

  "field assign: `self.buf = [a, b]` coerces to slice" in {
    eval(
      """struct Builder
        |    items: []int
        |
        |seed(self: *Builder) -> int
        |    self.items = [7, 8, 9, 10]
        |    self.items = append(self.items, 11)
        |    len(self.items)
        |
        |main() -> int
        |    var b = Builder([])
        |    seed(&b)
        |""".stripMargin) shouldBe 5
  }

  // ===== Expected-type propagates into generic-call args =====
  //
  // A generic-function call with explicit type args — `success[[]int]([])` —
  // analyzes its arguments AFTER the explicit type-arg substitution, so each
  // arg's expected type is `subst(formalParam.typ)`. Without this hookup, the
  // empty `[]` (or any context-dependent literal) at the call site failed
  // with "cannot infer element type" — same lever as the var-decl path, just
  // sourced from a different upstream expected-type. Closes the last hold-out
  // the array-lit-to-slice fix didn't already cover.

  "[] as arg to generic call with explicit type-arg coerces to slice" in {
    eval(
      """f[B](v: B) -> int = len(v)
        |
        |main() -> int = f[[]int]([])
        |""".stripMargin) shouldBe 0
  }

  "[a, b, c] as arg to generic call with explicit type-arg coerces to slice" in {
    eval(
      """f[B](v: B) -> int = len(v)
        |
        |main() -> int = f[[]int]([1, 2, 3])
        |""".stripMargin) shouldBe 3
  }

  "[] under nested generic context propagates expected type" in {
    eval(
      """f[B](v: B) -> B = v
        |g[A]() -> []A = f[[]A]([])
        |
        |main() -> int = len(g[int]())
        |""".stripMargin) shouldBe 0
  }

  "parsyl-style success[[]A]([]) one-liner compiles and runs" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |success[B](v: B) -> Parser[B] = Parser[B]((_x: int) -> v)
        |
        |empty_list[A]() -> Parser[[]A] = success[[]A]([])
        |
        |main() -> int
        |    val p = empty_list[int]()
        |    len(p(0))
        |""".stripMargin) shouldBe 0
  }

  "regression: bare [] with no expected type still errors" in {
    val ex = intercept[Exception] {
      eval("""main() -> int = len([])""")
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("infer") || msg.contains("element type"),
      s"bare [] without context should still error, got: ${ex.getMessage}")
  }

  // ===== Regressions: fixed-size array context unchanged =====

  "regression: var arr: [3]int = [1, 2, 3] is still a fixed-size array" in {
    eval(
      """main() -> int
        |    var arr: [3]int = [10, 20, 30]
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  "regression: var arr = [1, 2, 3] (no annotation) stays a fixed-size array" in {
    eval(
      """main() -> int
        |    var arr = [1, 2, 3]
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 6
  }

  "regression: empty [] without expected type is still rejected" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var xs = []
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("element type") || msg.contains("infer"),
      s"unannotated [] should require expected type, got: $msg")
  }

  // ===== Bound array → slice param at the call site =====
  //
  // When a `val a = [1, 2, 3]` (typed `[3]int` because no expected type was a
  // SliceType) is then passed to a `f(xs: []int)` parameter, the analyzer used
  // to leave the arg as ArrayType and downstream backends (TRISC in particular)
  // pushed only the array's address as a single 8-byte scalar — the callee then
  // read slice fields from arbitrary memory. Fix wraps such args with the same
  // `TSliceExpr(arr, None, None, …)` the explicit `arr[:]` would produce, so
  // the callee receives a real {ptr, len, backref} descriptor.

  "bound [N]T → []T at call site coerces to slice (read len)" in {
    eval(
      """take(xs: []int) -> int = len(xs)
        |main() -> int
        |    val a = [10, 20, 30]
        |    take(a)
        |""".stripMargin) shouldBe 3
  }

  "bound [N]byte → []byte at call site coerces to slice (read elements)" in {
    eval(
      """take(xs: []byte) -> int
        |    var sum = 0
        |    for i in 0..<len(xs)
        |        sum = sum + int(xs[i])
        |    sum
        |
        |main() -> int
        |    val a = [byte(1), byte(2), byte(3)]
        |    take(a)
        |""".stripMargin) shouldBe 6
  }

  "two bound arrays → two []T args (mirrors std/testing assert_slice_eq)" in {
    eval(
      """eq(got: []byte, want: []byte) -> int
        |    if len(got) != len(want) then return -1
        |    for i in 0..<len(got)
        |        if got[i] != want[i] then return i
        |    len(got)
        |
        |main() -> int
        |    val a = [byte(1), byte(2), byte(3)]
        |    val b = [byte(1), byte(2), byte(3)]
        |    eq(a, b)
        |""".stripMargin) shouldBe 3
  }

  "regression: empty [] cannot satisfy [N]T with N > 0" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var arr: [3]int = []
          |    0
          |""".stripMargin)
    }
    val msg = ex.getMessage.toLowerCase
    assert(msg.contains("fixed-array") || msg.contains("element"),
      s"empty [] vs [3]int should be rejected, got: $msg")
  }
}
