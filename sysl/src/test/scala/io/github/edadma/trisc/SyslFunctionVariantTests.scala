package io.github.edadma.trisc

/** Function-level `variant <expr>` clause — termination witness for recursive functions.
  * The expression is snapshotted at entry and re-evaluated (with parameters substituted by
  * call args) at every direct recursive call site; the call-site value must be strictly
  * less than the snapshot AND ≥ 0. Stripped under `--no-contracts`. */
class SyslFunctionVariantTests extends SyslTestHelpers {

  // ===== Happy path =====

  "fact with variant n returns the right value" in {
    eval("""
      |fact(n: int) -> int
      |    variant n
      |    if n <= 1 then return 1
      |    return n * fact(n - 1)
      |main() -> int = fact(5)
      |""".stripMargin) shouldBe 120
  }

  "void recursive function with variant terminates" in {
    eval("""
      |var counter = 0
      |countdown(n: int)
      |    variant n
      |    if n <= 0 then return
      |    counter = counter + 1
      |    countdown(n - 1)
      |main() -> int
      |    countdown(7)
      |    return counter
      |""".stripMargin) shouldBe 7
  }

  "variant survives recursion at multiple call sites" in {
    eval("""
      |fib(n: int) -> int
      |    variant n
      |    if n <= 1 then return n
      |    return fib(n - 1) + fib(n - 2)
      |main() -> int = fib(8)
      |""".stripMargin) shouldBe 21
  }

  "variant with multi-parameter substitution works" in {
    // gcd(a, b) — variant decreases on b
    eval("""
      |gcd(a: int, b: int) -> int
      |    variant b
      |    if b == 0 then return a
      |    return gcd(b, a % b)
      |main() -> int = gcd(48, 18)
      |""".stripMargin) shouldBe 6
  }

  // ===== Trap: variant doesn't decrease =====

  "variant that doesn't decrease traps" in {
    val t = intercept[RuntimeException] {
      eval("""
        |bad(n: int) -> int
        |    variant n
        |    if n <= 0 then return 0
        |    return bad(n)
        |main() -> int = bad(3)
        |""".stripMargin)
    }
    t.getMessage should include("variant")
  }

  "variant that goes negative traps" in {
    val t = intercept[RuntimeException] {
      eval("""
        |bad(n: int) -> int
        |    variant n
        |    if n < -10 then return 0
        |    return bad(n - 1)
        |main() -> int = bad(2)
        |""".stripMargin)
    }
    t.getMessage should include("variant")
  }

  "variant that increases on recursion traps" in {
    val t = intercept[RuntimeException] {
      eval("""
        |bad(n: int) -> int
        |    variant n
        |    if n > 100 then return 0
        |    return bad(n + 1)
        |main() -> int = bad(0)
        |""".stripMargin)
    }
    t.getMessage should include("variant")
  }

  // ===== Validation =====

  "variant with non-integer expression is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |bad(n: int) -> int
        |    variant n > 0
        |    return n
        |main() -> int = bad(5)
        |""".stripMargin)
    }
    t.getMessage should include("variant")
  }

  "two variant clauses on one function is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |bad(n: int) -> int
        |    variant n
        |    variant n - 1
        |    return n
        |main() -> int = bad(5)
        |""".stripMargin)
    }
    t.getMessage should include("variant")
  }

  // ===== Coexistence with require/ensure =====

  "variant alongside require and ensure" in {
    eval("""
      |fact(n: int) -> int
      |    require n >= 0
      |    variant n
      |    ensure result >= 1
      |    if n <= 1 then return 1
      |    return n * fact(n - 1)
      |main() -> int = fact(6)
      |""".stripMargin) shouldBe 720
  }

  // ===== Variant on a non-recursive function =====

  "variant on a non-recursive function compiles (no checks fire)" in {
    eval("""
      |inc(n: int) -> int
      |    variant n
      |    return n + 1
      |main() -> int = inc(41)
      |""".stripMargin) shouldBe 42
  }

  // ===== Wider integer types =====

  "variant on i64 parameter works" in {
    eval("""
      |sumDown(n: i64) -> i64
      |    variant n
      |    if n <= 0i64 then return 0i64
      |    return n + sumDown(n - 1i64)
      |main() -> i64 = sumDown(10i64)
      |""".stripMargin) shouldBe 55
  }

  // ===== Variant referencing a parameter expression =====

  "variant computed from a parameter expression" in {
    // remaining = upper - lower; decreases as lower grows
    eval("""
      |scan(lower: int, upper: int) -> int
      |    variant upper - lower
      |    if lower >= upper then return lower
      |    return scan(lower + 1, upper)
      |main() -> int = scan(0, 5)
      |""".stripMargin) shouldBe 5
  }

  // ===== Mutual recursion (NOT checked at runtime — direct calls only) =====

  "mutual recursion with variants compiles (note: only direct self-calls are checked)" in {
    // even(n) calls odd(n-1), odd(n) calls even(n-1). Each function's variant only
    // catches DIRECT recursive calls, so this passes runtime even though correctness
    // depends on the indirect chain. (A future verifier would catch the cross-fn case.)
    eval("""
      |isEven(n: int) -> bool
      |    variant n
      |    if n == 0 then return true
      |    return isOdd(n - 1)
      |isOdd(n: int) -> bool
      |    variant n
      |    if n == 0 then return false
      |    return isEven(n - 1)
      |main() -> int
      |    if isEven(10) then return 1 else return 0
      |""".stripMargin) shouldBe 1
  }
}
