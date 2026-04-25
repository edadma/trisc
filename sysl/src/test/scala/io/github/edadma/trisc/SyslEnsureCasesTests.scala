package io.github.edadma.trisc

/** `ensure cases` — SPARK-style Contract_Cases sugar. Each `guard => postcondition` row
  * is a guarded implication: if `guard` held at entry, `postcondition` must hold at exit.
  * Desugared by the parser to one `require` (completeness: some guard must match on entry)
  * plus N `ensure`s of the form `!old(guard) || postcondition`. */
class SyslEnsureCasesTests extends SyslTestHelpers {

  "ensure cases passes when matching case's postcondition holds" in {
    eval(
      """classify(x: int) -> int
        |    ensure cases
        |        x > 0  => result == 1
        |        x == 0 => result == 0
        |        x < 0  => result == -1
        |    if x > 0 then return 1
        |    if x < 0 then return -1
        |    return 0
        |main() -> int
        |    classify(42)
        |""".stripMargin) shouldBe 1
  }

  "ensure cases: zero branch" in {
    eval(
      """classify(x: int) -> int
        |    ensure cases
        |        x > 0  => result == 1
        |        x == 0 => result == 0
        |        x < 0  => result == -1
        |    if x > 0 then return 1
        |    if x < 0 then return -1
        |    return 0
        |main() -> int
        |    classify(0)
        |""".stripMargin) shouldBe 0
  }

  "ensure cases: negative branch" in {
    eval(
      """classify(x: int) -> int
        |    ensure cases
        |        x > 0  => result == 1
        |        x == 0 => result == 0
        |        x < 0  => result == -1
        |    if x > 0 then return 1
        |    if x < 0 then return -1
        |    return 0
        |main() -> int
        |    classify(-7)
        |""".stripMargin) shouldBe -1
  }

  "ensure cases traps when matching case's postcondition is violated" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """buggy(x: int) -> int
          |    ensure cases
          |        x > 0  => result == 1
          |        x == 0 => result == 0
          |        x < 0  => result == -1
          |    return 99
          |main() -> int
          |    buggy(5)
          |""".stripMargin)
    }
    thrown.getMessage.toLowerCase should include("case")
  }

  "ensure cases traps at completeness violation when no guard matches" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """narrow(x: int) -> int
          |    ensure cases
          |        x > 0  => result == 1
          |        x < 0  => result == -1
          |    return 0
          |main() -> int
          |    narrow(0)
          |""".stripMargin)
    }
    thrown.getMessage should include("no guard matched")
  }

  "ensure cases with a single case works like a guarded ensure" in {
    eval(
      """f(x: int) -> int
        |    ensure cases
        |        x > 0 => result > 0
        |    return x + 1
        |main() -> int
        |    f(3)
        |""".stripMargin) shouldBe 4
  }

  "ensure cases with a single case fails completeness when guard is false" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """f(x: int) -> int
          |    ensure cases
          |        x > 0 => result > 0
          |    return x + 1
          |main() -> int
          |    f(-5)
          |""".stripMargin)
    }
    thrown.getMessage should include("no guard matched")
  }

  "ensure cases composes with plain ensure clauses" in {
    eval(
      """clamp(x: int, lo: int, hi: int) -> int
        |    require lo <= hi
        |    ensure result >= lo
        |    ensure result <= hi
        |    ensure cases
        |        x < lo => result == lo
        |        x > hi => result == hi
        |        lo <= x && x <= hi => result == x
        |    if x < lo then return lo
        |    if x > hi then return hi
        |    return x
        |main() -> int
        |    clamp(15, 0, 10)
        |""".stripMargin) shouldBe 10
  }

  "ensure cases uses entry-state guards via implicit old()" in {
    // Guard is evaluated at entry. If the body mutates the snapshot source, the guard's
    // decision is fixed by what held on entry — the `old(x)` injection is what makes this
    // correct inside an ensure clause.
    eval(
      """bump(x: int) -> int
        |    ensure cases
        |        x > 0 => result == x + 1
        |        x == 0 => result == 1
        |        x < 0 => result == x - 1
        |    if x > 0 then return x + 1
        |    if x < 0 then return x - 1
        |    return 1
        |main() -> int
        |    bump(10)
        |""".stripMargin) shouldBe 11
  }

  "ensure cases allows overlapping guards (both postconditions must hold)" in {
    // We do NOT enforce disjointness at runtime — that's a static obligation for a
    // future prover. Overlapping guards mean both postconditions must hold; here they
    // agree so we pass.
    eval(
      """f(x: int) -> int
        |    ensure cases
        |        x >= 0 => result >= 0
        |        x >= 1 => result >= 0
        |        x < 0  => result == -1
        |    if x < 0 then return -1
        |    return x
        |main() -> int
        |    f(5)
        |""".stripMargin) shouldBe 5
  }

  "ensure cases traps when overlapping guards conflict and current case fails" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """f(x: int) -> int
          |    ensure cases
          |        x >= 0 => result == x
          |        x >= 1 => result == x + 1
          |    return x
          |main() -> int
          |    f(3)
          |""".stripMargin)
    }
    thrown.getMessage.toLowerCase should include("case")
  }

  "ensure cases with per-case message" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """f(x: int) -> int
          |    ensure cases
          |        x > 0 => result > 0, "positive branch broke"
          |        x <= 0 => result <= 0
          |    return -1
          |main() -> int
          |    f(5)
          |""".stripMargin)
    }
    thrown.getMessage should include("positive branch broke")
  }

  "ensure cases inside a function returning via fall-through expression" in {
    eval(
      """sign(x: int) -> int
        |    ensure cases
        |        x > 0 => result == 1
        |        x == 0 => result == 0
        |        x < 0 => result == -1
        |    if x > 0 then 1 else if x < 0 then -1 else 0
        |main() -> int
        |    sign(-100)
        |""".stripMargin) shouldBe -1
  }
}
