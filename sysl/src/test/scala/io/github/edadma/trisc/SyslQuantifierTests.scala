package io.github.edadma.trisc

/** Ada-style universal / existential quantifier expressions over an integer range:
  *
  *   for all x in lo..hi  => P(x)        // inclusive,  ∀ x ∈ [lo, hi]    : P(x)
  *   for all x in lo..<hi => P(x)        // exclusive, ∀ x ∈ [lo, hi)    : P(x)
  *   for some x in lo..hi  => P(x)       // inclusive, ∃ x ∈ [lo, hi]    : P(x)
  *
  * Bool-typed; short-circuits on first counterexample (`for all`) or first witness
  * (`for some`). Empty ranges give `true` for `all` (vacuous truth) and `false` for
  * `some`. Useful in require/ensure/invariant/assume contracts. */
class SyslQuantifierTests extends SyslTestHelpers {

  // ===== for all — universal quantifier =====

  "for all: holds when every element satisfies the predicate" in {
    eval(
      """main() -> int
        |    if for all x in 0..10 => x >= 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "for all: fails when some element violates the predicate" in {
    eval(
      """main() -> int
        |    if for all x in 0..10 => x < 5 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "for all: vacuously true on an empty range" in {
    eval(
      """main() -> int
        |    if for all x in 5..<5 => false then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "for all: exclusive upper bound (..< excludes hi)" in {
    eval(
      """main() -> int
        |    if for all x in 0..<10 => x < 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "for all: inclusive upper bound (.. includes hi)" in {
    eval(
      """main() -> int
        |    if for all x in 0..10 => x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== for some — existential quantifier =====

  "for some: true when at least one element satisfies the predicate" in {
    eval(
      """main() -> int
        |    if for some x in 0..10 => x == 7 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "for some: false when no element satisfies the predicate" in {
    eval(
      """main() -> int
        |    if for some x in 0..10 => x > 100 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "for some: false on an empty range (no witness possible)" in {
    eval(
      """main() -> int
        |    if for some x in 5..<5 => true then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Captures: predicate may reference outer scope =====

  "predicate references outer-scope value" in {
    eval(
      """main() -> int
        |    var threshold = 7
        |    if for all x in 0..6 => x < threshold then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "predicate references outer-scope array via index" in {
    eval(
      """main() -> int
        |    var a: [5]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    a[4] = 5
        |    if for all i in 0..<5 => a[i] > 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Use in contracts =====

  "for all in require precondition" in {
    eval(
      """allPositive(a: *int, n: int) -> int
        |    require for all i in 0..<n => a[i] > 0
        |    var sum = 0
        |    for j = 0; j < n; j++
        |        sum = sum + a[j]
        |    sum
        |
        |main() -> int
        |    var arr: [4]int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    arr[3] = 4
        |    allPositive(&arr[0], 4)
        |""".stripMargin) shouldBe 10
  }

  "for all in require: traps when violated" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """allPositive(a: *int, n: int) -> int
          |    require for all i in 0..<n => a[i] > 0
          |    a[0]
          |
          |main() -> int
          |    var arr: [3]int
          |    arr[0] = 1
          |    arr[1] = 0 - 5
          |    arr[2] = 3
          |    allPositive(&arr[0], 3)
          |""".stripMargin)
    }
    thrown.getMessage should include("precondition")
  }

  "for all in loop invariant" in {
    eval(
      """main() -> int
        |    var a: [5]int
        |    var i = 0
        |    while i < 5
        |        invariant for all j in 0..<i => a[j] == 0
        |        a[i] = 0
        |        i = i + 1
        |    a[2]
        |""".stripMargin) shouldBe 0
  }

  "for some in assume" in {
    eval(
      """main() -> int
        |    var a: [4]int
        |    a[0] = 0
        |    a[1] = 0
        |    a[2] = 7
        |    a[3] = 0
        |    assume for some i in 0..<4 => a[i] == 7, "must contain a 7"
        |    a[2]
        |""".stripMargin) shouldBe 7
  }

  "for some in assume: traps when no witness exists" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var a: [4]int
          |    a[0] = 0
          |    a[1] = 0
          |    a[2] = 0
          |    a[3] = 0
          |    assume for some i in 0..<4 => a[i] == 7
          |    a[0]
          |""".stripMargin)
    }
    thrown.getMessage should include("assume")
  }

  // ===== Nested quantifiers =====

  "nested for all (2D)" in {
    eval(
      """main() -> int
        |    if for all i in 0..3 => for all j in 0..3 => i + j >= 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "nested mixed: for all of for some" in {
    eval(
      """main() -> int
        |    if for all i in 0..3 => for some j in 0..3 => i + j == 3 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Type checking =====

  "non-bool predicate is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    var n = for all x in 0..3 => x + 1
          |    n
          |""".stripMargin)
    }
    thrown.getMessage should include("quantifier predicate must be bool")
  }

  "non-integral range bound is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    if for all x in 0.0..3.0 => x >= 0.0 then 1 else 0
          |""".stripMargin)
    }
    thrown.getMessage should include("integral")
  }

  // ===== `all` and `some` still usable as identifiers =====

  "`all` is still a valid identifier outside quantifier context" in {
    eval(
      """main() -> int
        |    var all = 42
        |    all
        |""".stripMargin) shouldBe 42
  }

  "`some` is still a valid identifier outside quantifier context" in {
    eval(
      """main() -> int
        |    var some = 7
        |    some + 1
        |""".stripMargin) shouldBe 8
  }
}
