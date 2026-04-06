package io.github.edadma.trisc

class SyslUnderscoreBindingTests extends SyslTestHelpers {

  // `_` is a write-only discard binding (Go/Rust style).
  // You can bind to it; you cannot reference it; multiple `_`s in the
  // same scope don't collide.

  "val _ = expr evaluates for side effects" in {
    // The initializer must be evaluated even though the result is discarded.
    eval(
      """inc(p: *int) -> int
        |    *p = *p + 1
        |    0
        |
        |main() -> int
        |    var counter = 0
        |    val _ = inc(&counter)
        |    val _ = inc(&counter)
        |    counter
        |""".stripMargin) shouldBe 2
  }

  "var _ = expr also discards" in {
    eval(
      """side(p: *int, n: int) -> int
        |    *p = *p + n
        |    n
        |
        |main() -> int
        |    var sum = 0
        |    var _ = side(&sum, 10)
        |    var _ = side(&sum, 5)
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "multiple val _ in same scope do not collide" in {
    // Two `val _` in one block must NOT trigger a duplicate-binding error.
    eval(
      """main() -> int
        |    val _ = 1
        |    val _ = 2
        |    val _ = 3
        |    42
        |""".stripMargin) shouldBe 42
  }

  "val _ with type annotation" in {
    eval(
      """main() -> int
        |    val _: int = 7
        |    val _: int = 8
        |    100
        |""".stripMargin) shouldBe 100
  }

  "val _ does not shadow outer name" in {
    eval(
      """main() -> int
        |    var x = 5
        |    val _ = 999
        |    x
        |""".stripMargin) shouldBe 5
  }

  // ===== Destructuring with _ =====

  "destructure with _ discards first field" in {
    eval(
      """pair() -> (int, int)
        |    (10, 20)
        |
        |main() -> int
        |    val _, y = pair()
        |    y
        |""".stripMargin) shouldBe 20
  }

  "destructure with _ discards second field" in {
    eval(
      """pair() -> (int, int)
        |    (10, 20)
        |
        |main() -> int
        |    val x, _ = pair()
        |    x
        |""".stripMargin) shouldBe 10
  }

  "destructure discard all with _" in {
    eval(
      """pair() -> (int, int)
        |    (10, 20)
        |
        |main() -> int
        |    val _, _ = pair()
        |    42
        |""".stripMargin) shouldBe 42
  }

  "bare destructure with _ as declaration" in {
    eval(
      """pair() -> (int, int)
        |    (100, 200)
        |
        |main() -> int
        |    _, y = pair()
        |    y
        |""".stripMargin) shouldBe 200
  }

  "bare destructure assignment with _" in {
    eval(
      """pair() -> (int, int)
        |    (100, 200)
        |
        |main() -> int
        |    var x = 0
        |    _, x = pair()
        |    x
        |""".stripMargin) shouldBe 200
  }

  "triple destructure with _ in middle" in {
    eval(
      """triple() -> (int, int, int)
        |    (1, 2, 3)
        |
        |main() -> int
        |    val a, _, c = triple()
        |    a * 10 + c
        |""".stripMargin) shouldBe 13
  }
}
