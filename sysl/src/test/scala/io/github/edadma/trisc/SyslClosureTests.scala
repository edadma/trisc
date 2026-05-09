package io.github.edadma.trisc

class SyslClosureTests extends SyslTestHelpers {

  // ===== Basic closures (no captures) =====

  "zero-capture closure" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  "multi-param closure" in {
    eval(
      """apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((x, y) -> x + y, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  "zero-param closure" in {
    eval(
      """call(f: () -> int) -> int = f()
        |
        |main() -> int = call(() -> 42)
        |""".stripMargin) shouldBe 42
  }

  "closure assigned to variable" in {
    eval(
      """main() -> int
        |    val f: (int) -> int = x -> x * 2
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Capture by value =====

  "capture local variable" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    apply(x -> x + a, 32)
        |""".stripMargin) shouldBe 42
  }

  "capture is frozen (by value)" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    var a = 10
        |    val f: (int) -> int = x -> x + a
        |    a = 100
        |    f(32)
        |""".stripMargin) shouldBe 42
  }

  "capture multiple variables" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val a = 10
        |    val b = 20
        |    apply(x -> x + a + b, 12)
        |""".stripMargin) shouldBe 42
  }

  // ===== Higher-order functions =====

  "closure passed to higher-order function" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x * 2, 21)
        |""".stripMargin) shouldBe 42
  }

  "closure as return value" in {
    eval(
      """make_adder(n: int) -> (int) -> int
        |    val captured = n
        |    x -> x + captured
        |
        |main() -> int
        |    val add10 = make_adder(10)
        |    add10(32)
        |""".stripMargin) shouldBe 42
  }

  // ===== Type-annotated parameters =====

  "closure with typed parameters" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply((x: int) -> x + 1, 41)
        |""".stripMargin) shouldBe 42
  }

  // ===== Expressions =====

  "closure in arithmetic expression" in {
    eval(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 20) + apply(x -> x + 1, 20)
        |""".stripMargin) shouldBe 42
  }

  // ===== Nested closure declarations =====

  "closure-typed local declared inside another closure body resolves correctly" in {
    // Regression: bare `name = (...) -> ...` inside a function body parses as
    // AssignStmtAST, which the analyzer used to lower to TAssignStmt regardless of
    // whether the name was a fresh binding. When it occurred inside another closure's
    // body, the capture-detection pass walked TAssignStmt as a write to an outer name
    // and added the freshly-created `inner` to the outer closure's capture list. At
    // runtime the outer closure tried to look up `inner` in its captured environment
    // and failed with "undefined variable: inner". Fix: emit TVarStmt for fresh-local
    // creation so capture detection sees the binding.
    eval(
      """main() -> int
        |    outer = (a: int) ->
        |        inner = (b: int) ->
        |            a + b
        |        inner(5)
        |    outer(2)
        |""".stripMargin) shouldBe 7
  }

  // ===== Inner def declarations (recursive named local closures) =====

  "inner def with self-recursion (factorial)" in {
    eval(
      """outer() -> int
        |    def fact(n: int) -> int
        |        if n == 0 then return 1
        |        n * fact(n - 1)
        |    fact(5)
        |
        |main() -> int = outer()
        |""".stripMargin) shouldBe 120
  }

  "inner def captures outer parameter" in {
    eval(
      """outer(base: int) -> int
        |    def add_base(n: int) -> int
        |        n + base
        |    add_base(7)
        |
        |main() -> int = outer(35)
        |""".stripMargin) shouldBe 42
  }

  "inner def with self-recursion uses captured outer local" in {
    // Combines self-recursion with an outer-scope capture: the recursive helper
    // sums n down to 0, adding `bonus` (captured from outer) each step.
    eval(
      """outer(bonus: int) -> int
        |    def sum_with_bonus(n: int) -> int
        |        if n == 0 then return 0
        |        n + bonus + sum_with_bonus(n - 1)
        |    sum_with_bonus(3)
        |
        |main() -> int = outer(10)
        |""".stripMargin) shouldBe 36 // (3+10) + (2+10) + (1+10) + 0 = 36
  }

  "inner def with zero parameters" in {
    eval(
      """outer() -> int
        |    def constant() -> int = 42
        |    constant()
        |
        |main() -> int = outer()
        |""".stripMargin) shouldBe 42
  }

  // ===== Inner def: contract clauses rejected (audit item #26) =====
  //
  // `defDecl` parses the same `funBlockBody` that top-level fns do, so a `require`
  // or `ensure` clause on an inner def is syntactically accepted. But inner defs
  // lower to closures, and the closure analyzer has no contract-emission path —
  // before this fix the clauses were silently dropped, which is a footgun. The
  // analyzer now rejects them with a clear "not supported on inner defs yet"
  // diagnostic. Promote to a top-level fn (where contracts work) or hand-inline
  // the check via `assert(...)`.

  "inner def with require clause is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """outer() -> int
          |    def helper(n: int) -> int
          |        require n >= 0
          |        n + 1
          |    helper(5)
          |
          |main() -> int = outer()
          |""".stripMargin)
    }
    thrown.getMessage should include("inner def 'helper'")
    thrown.getMessage should include("require/ensure")
  }

  "inner def with ensure clause is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """outer() -> int
          |    def doubled(x: int) -> int
          |        ensure result == x * 2
          |        x + x
          |    doubled(21)
          |
          |main() -> int = outer()
          |""".stripMargin)
    }
    thrown.getMessage should include("inner def 'doubled'")
    thrown.getMessage should include("require/ensure")
  }

  // ===== Inner def: mutual recursion (audit Tier 4 followup #3) =====
  //
  // `def f` then `def g` where `f` calls `g` (and/or vice versa) used to fail
  // with "g is not in scope" — the inner-def arm bound each name only as
  // its own body was about to be analyzed, so the earlier sibling couldn't
  // see the later one. The analyzer now does a two-pass scope walk: every
  // sibling inner-def name in a block is pre-bound BEFORE any body is
  // analyzed, so cross-references type-check.
  //
  // Capture-by-value semantics (closures snapshot the enclosing scope at
  // construction) would still leave a forward reference reading garbage at
  // runtime. To make this work end-to-end without per-backend changes, the
  // analyzer detects clusters of inner defs that cross-reference each other
  // and lifts them to top-level synthesized fns — every backend already
  // handles `TIndirectCall` of a `TFuncRef` (top-level fn pointer + null env).
  //
  // Single self-recursive inner defs are NOT lifted; they keep the existing
  // TClosure + selfName path. Clusters that capture outer-scope variables
  // are rejected with a clear "promote to top-level fn" diagnostic — the
  // lift cannot preserve those captures.

  "two-way mutual recursion (is_even / is_odd)" in {
    eval(
      """main() -> int
        |    def is_even(n: int) -> bool
        |        if n == 0 then true
        |        else is_odd(n - 1)
        |    def is_odd(n: int) -> bool
        |        if n == 0 then false
        |        else is_even(n - 1)
        |    if is_even(10) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "two-way mutual recursion — odd input lands on false branch" in {
    eval(
      """main() -> int
        |    def is_even(n: int) -> bool
        |        if n == 0 then true
        |        else is_odd(n - 1)
        |    def is_odd(n: int) -> bool
        |        if n == 0 then false
        |        else is_even(n - 1)
        |    if is_even(7) then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "two-way mutual recursion — both predicates exercised" in {
    eval(
      """main() -> int
        |    def is_even(n: int) -> bool
        |        if n == 0 then true
        |        else is_odd(n - 1)
        |    def is_odd(n: int) -> bool
        |        if n == 0 then false
        |        else is_even(n - 1)
        |    var hits = 0
        |    if is_even(4) then hits = hits + 1
        |    if is_odd(5) then hits = hits + 10
        |    if !is_even(3) then hits = hits + 100
        |    if !is_odd(6) then hits = hits + 1000
        |    hits
        |""".stripMargin) shouldBe 1111
  }

  "three-way mutual recursion (a→b→c→a)" in {
    // Each predicate descends by 1; bottoms out at 0 returning 1. Computes
    // a chain of (n-1)*1 calls — for n=9 the result is 1 because 9 → 8 → 7
    // → 6 → 5 → 4 → 3 → 2 → 1 → 0 = 1.
    eval(
      """main() -> int
        |    def a(n: int) -> int
        |        if n == 0 then 1
        |        else b(n - 1)
        |    def b(n: int) -> int
        |        if n == 0 then 1
        |        else c(n - 1)
        |    def c(n: int) -> int
        |        if n == 0 then 1
        |        else a(n - 1)
        |    a(9)
        |""".stripMargin) shouldBe 1
  }

  "forward-only sibling chain (no back edge)" in {
    // f → g → h → leaf. No cycle, but f and g still cross-reference forward
    // siblings. The cluster-lift logic should still pick this up: g is
    // captured by f, h is captured by g; all three end up in the cluster.
    eval(
      """main() -> int
        |    def f(n: int) -> int = g(n) + 1
        |    def g(n: int) -> int = h(n) + 10
        |    def h(n: int) -> int = n + 100
        |    f(5)
        |""".stripMargin) shouldBe 116 // 5 + 100 + 10 + 1
  }

  "self + sibling refs combined" in {
    // is_even is self-recursive (decreases by 2) AND calls sibling. Single
    // inner-defs with only-self-refs use the TClosure path; mixing self with
    // a sibling ref pulls the def into the cluster and lifts it.
    eval(
      """main() -> int
        |    def is_even(n: int) -> bool
        |        if n == 0 then true
        |        else if n == 1 then is_zero_helper(0)
        |        else is_even(n - 2)
        |    def is_zero_helper(n: int) -> bool
        |        n == 0
        |    if is_even(8) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "single self-recursive inner def still works (no regression)" in {
    // Pre-existing test; pinned so the cluster-lift doesn't disturb the
    // single-self-ref path.
    eval(
      """outer() -> int
        |    def fact(n: int) -> int
        |        if n == 0 then return 1
        |        n * fact(n - 1)
        |    fact(5)
        |
        |main() -> int = outer()
        |""".stripMargin) shouldBe 120
  }

  "captures outer-scope var inside cluster is rejected" in {
    val thrown = intercept[RuntimeException] {
      eval(
        """main() -> int
          |    val bonus = 10
          |    def is_even(n: int) -> bool
          |        if n == 0 then true
          |        else is_odd(n - 1)
          |    def is_odd(n: int) -> bool
          |        if n == 0 then false
          |        else is_even(n - 1 + bonus - bonus)
          |    if is_even(10) then 1 else 0
          |""".stripMargin)
    }
    thrown.getMessage should include("is_odd")
    thrown.getMessage should include("cluster")
    thrown.getMessage should include("bonus")
  }

  "non-cross-referencing inner defs are NOT lifted (sibling not in cluster)" in {
    // f and g both exist as inner defs but neither references the other.
    // Cluster detection finds no cross-refs → no lift. Both keep their
    // existing TClosure paths; a third def `h` that captures `bonus` is
    // allowed because it's not part of any cluster.
    eval(
      """main() -> int
        |    val bonus = 7
        |    def f(n: int) -> int = n + 1
        |    def g(n: int) -> int = n * 2
        |    def h(n: int) -> int = n + bonus
        |    f(10) + g(5) + h(20) // 11 + 10 + 27 = 48
        |""".stripMargin) shouldBe 48
  }
}
