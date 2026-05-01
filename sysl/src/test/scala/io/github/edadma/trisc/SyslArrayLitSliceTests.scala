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
