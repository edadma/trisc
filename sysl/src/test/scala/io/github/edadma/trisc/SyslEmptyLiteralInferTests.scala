package io.github.edadma.trisc

class SyslEmptyLiteralInferTests extends SyslTestHelpers {

  // ===== Empty slice literal — element type inferred from currentExpected =====
  //
  // Mirrors the expected-type-from-context rule for variant constructors with
  // phantom type parameters (`None`, etc.). Without this, every empty-slice
  // initializer would have to be hoisted to a typed helper.

  "var initializer (slice)" in {
    eval(
      """main() -> int
        |    var xs: []int = []
        |    len(xs)
        |""".stripMargin) shouldBe 0
  }

  "val initializer (slice)" in {
    eval(
      """main() -> int
        |    val xs: []string = []
        |    len(xs)
        |""".stripMargin) shouldBe 0
  }

  "function return type (slice)" in {
    eval(
      """f() -> []int = []
        |
        |main() -> int = len(f())
        |""".stripMargin) shouldBe 0
  }

  "function call argument with declared slice type" in {
    eval(
      """count(xs: []int) -> int = len(xs)
        |
        |main() -> int = count([])
        |""".stripMargin) shouldBe 0
  }

  "struct field with declared slice type" in {
    eval(
      """struct Bag
        |    items: []int
        |    n: int
        |
        |main() -> int
        |    val b = Bag([], 7)
        |    len(b.items) + b.n
        |""".stripMargin) shouldBe 7
  }

  "nominal-alias cast wrapping an empty slice" in {
    eval(
      """type Wrap[A] = new []A
        |
        |empty() -> Wrap[int] = Wrap[int]([])
        |
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Empty fixed-array literal with size 0 =====

  "var initializer (fixed array of size 0)" in {
    eval(
      """main() -> int
        |    var xs: [0]int = []
        |    sizeof(xs)
        |""".stripMargin) shouldBe 0
  }

  // ===== Composition: paren context + closure body return type =====
  //
  // The original parsyl-style use case: the closure body returns an empty
  // slice, expected = []A from the alias's underlying function type.

  "empty slice as closure return inside generic-alias cast" in {
    eval(
      """type Producer[A] = new (int) -> int
        |
        |make() -> Producer[int] = Producer[int]((n: int) -> n)
        |
        |main() -> int
        |    val p = make()
        |    p(42)
        |""".stripMargin) shouldBe 42
  }

  // ===== Negative cases =====

  "untyped val with empty literal still errors" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    val xs = []
          |    len(xs)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.toLowerCase.contains("infer") || msg.toLowerCase.contains("empty"),
      s"untyped empty literal should fail to infer, got: $msg")
  }

  "fixed-array of non-zero arity rejects empty literal" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var xs: [3]int = []
          |    sizeof(xs)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.toLowerCase.contains("empty") || msg.toLowerCase.contains("fixed-array") || msg.toLowerCase.contains("element"),
      s"3-arity array shouldn't accept [], got: $msg")
  }

  "expected non-collection type rejects empty literal" in {
    val ex = intercept[Exception] {
      eval(
        """main() -> int
          |    var x: int = []
          |    x
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(msg.nonEmpty, s"int = [] should fail, got: $msg")
  }
}
