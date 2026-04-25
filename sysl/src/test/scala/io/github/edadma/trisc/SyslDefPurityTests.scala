package io.github.edadma.trisc

/** `def name(...) = expr` is an expression function in the SPARK/Ada sense — a
  * proof-friendly abstraction that is implicitly `#pure` and usable as an abstraction
  * predicate in `require` / `ensure` / `invariant` / `assume`. These tests cover the
  * implicit-purity tightening: def bodies are restricted, but the attribute itself does
  * not need to be written. */
class SyslDefPurityTests extends SyslTestHelpers {

  "def with a pure expression body compiles" in {
    eval(
      """def sq(x: int) -> int = x * x
        |main() -> int
        |    sq(7)
        |""".stripMargin) shouldBe 49
  }

  "def with a block body of pure statements compiles" in {
    eval(
      """def clamp(x: int, lo: int, hi: int) -> int
        |    if x < lo then return lo
        |    if x > hi then return hi
        |    return x
        |main() -> int
        |    clamp(15, 0, 10)
        |""".stripMargin) shouldBe 10
  }

  "zero-arg def auto-calls at a bare reference" in {
    eval(
      """def answer -> int = 42
        |main() -> int
        |    answer
        |""".stripMargin) shouldBe 42
  }

  "def predicate can be used in a require clause" in {
    eval(
      """def is_positive(x: int) -> bool = x > 0
        |f(n: int) -> int
        |    require is_positive(n)
        |    return n * 2
        |main() -> int
        |    f(5)
        |""".stripMargin) shouldBe 10
  }

  "def predicate in require traps when false" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """def is_positive(x: int) -> bool = x > 0
          |f(n: int) -> int
          |    require is_positive(n), "n must be positive"
          |    return n * 2
          |main() -> int
          |    f(-3)
          |""".stripMargin)
    }
    thrown.getMessage should include("n must be positive")
  }

  "def predicate can be used in an ensure clause" in {
    eval(
      """def is_even(x: int) -> bool = x % 2 == 0
        |twice(n: int) -> int
        |    ensure is_even(result)
        |    return n * 2
        |main() -> int
        |    twice(11)
        |""".stripMargin) shouldBe 22
  }

  "def predicate can be used in a loop invariant" in {
    eval(
      """def non_negative(x: int) -> bool = x >= 0
        |main() -> int
        |    var s = 0
        |    for i = 0; i < 5; i++
        |        invariant non_negative(s)
        |        s = s + i
        |    s
        |""".stripMargin) shouldBe 10
  }

  "def calling another def is allowed" in {
    eval(
      """def sq(x: int) -> int = x * x
        |def sum_of_sq(a: int, b: int) -> int = sq(a) + sq(b)
        |main() -> int
        |    sum_of_sq(3, 4)
        |""".stripMargin) shouldBe 25
  }

  "def rejects writing to a module-level var" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """var counter: int = 0
          |def tick -> int
          |    counter = counter + 1
          |    return counter
          |main() -> int
          |    tick
          |""".stripMargin)
    }
    thrown.getMessage should include("def function")
  }

  "def rejects calling an impure function" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """var global_state: int = 0
          |impure_helper() -> int
          |    global_state = global_state + 1
          |    return global_state
          |def wrapped -> int = impure_helper()
          |main() -> int
          |    wrapped
          |""".stripMargin)
    }
    thrown.getMessage should include("def function")
  }

  "def rejects heap allocation with `new`" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """struct Box
          |    value: int
          |def make_box -> &Box = new Box(42)
          |main() -> int
          |    var b = make_box
          |    b.value
          |""".stripMargin)
    }
    thrown.getMessage should include("def function")
  }

  "def with explicit #reads attribute is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """var g: int = 5
          |#reads(g)
          |def peek -> int = g
          |main() -> int
          |    peek
          |""".stripMargin)
    }
    thrown.getMessage should include("def")
    thrown.getMessage.toLowerCase should include("#reads")
  }

  "def with explicit #writes attribute is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """var g: int = 5
          |#writes(g)
          |def poke -> int
          |    g = 7
          |    return g
          |main() -> int
          |    poke
          |""".stripMargin)
    }
    thrown.getMessage should include("def")
    thrown.getMessage.toLowerCase should include("#writes")
  }

  "def with explicit #pure attribute compiles (redundant but allowed)" in {
    eval(
      """#pure
        |def sq(x: int) -> int = x * x
        |main() -> int
        |    sq(6)
        |""".stripMargin) shouldBe 36
  }

  "def can read module-level consts" in {
    eval(
      """const PI = 355
        |def pi_over_x(x: int) -> int = PI / x
        |main() -> int
        |    pi_over_x(113)
        |""".stripMargin) shouldBe 3
  }

  "def may recurse" in {
    eval(
      """def fact(n: int) -> int
        |    if n <= 1 then return 1
        |    return n * fact(n - 1)
        |main() -> int
        |    fact(5)
        |""".stripMargin) shouldBe 120
  }

  "def reject writing through a pointer parameter" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """def zero_out(p: *int) -> int
          |    *p = 0
          |    return 0
          |main() -> int
          |    var x = 5
          |    zero_out(&x)
          |""".stripMargin)
    }
    thrown.getMessage should include("def function")
  }

  "regular function with expression body stays unaffected (can still have effects)" in {
    // Regular functions (no `def`) remain unrestricted — tick() writes a module var freely.
    eval(
      """var state: int = 0
        |tick() -> int
        |    state = state + 1
        |    return state
        |main() -> int
        |    tick()
        |    tick()
        |""".stripMargin) shouldBe 2
  }
}
