package io.github.edadma.trisc

/** `#reads(...)` / `#writes(...)` effect annotations — SPARK Global aspect equivalent.
  * The analyzer enforces three rules: (1) body conformance — every read of a module-level
  * var must be in `#reads ∪ #writes` and every write must be in `#writes`; (2) call-site
  * subset — a callee's effects must be contained in the caller's; (3) strict closure —
  * annotated functions may only call other annotated (or `#pure`) functions, with no
  * indirect/interface dispatch and no allocation. */
class SyslEffectsTests extends SyslTestHelpers {

  // ===== Body conformance =====

  "reads(x) body that reads x compiles" in {
    eval("""
      |var x = 7
      |#reads(x)
      |getX() -> int = x
      |main() -> int = getX()
      |""".stripMargin) shouldBe 7
  }

  "reads(x) body that writes x is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 7
        |#reads(x)
        |bumpX() -> int
        |    x = x + 1
        |    return x
        |main() -> int = bumpX()
        |""".stripMargin)
    }
    t.getMessage should include("writes")
    t.getMessage should include("'x'")
  }

  "writes(x) body that writes x compiles" in {
    eval("""
      |var x = 0
      |#writes(x)
      |setX(v: int)
      |    x = v
      |main() -> int
      |    setX(42)
      |    return x
      |""".stripMargin) shouldBe 42
  }

  "writes(x) body that writes y is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 0
        |var y = 0
        |#writes(x)
        |bad()
        |    y = 1
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("'y'")
  }

  "body that reads y not in reads is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 0
        |var y = 0
        |#reads(x)
        |bad() -> int = y
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("'y'")
  }

  "reads(x,y) writes(x) body that reads y, reads x, writes x compiles" in {
    eval("""
      |var x = 1
      |var y = 10
      |#reads(x, y)
      |#writes(x)
      |advance() -> int
      |    x = x + y
      |    return x
      |main() -> int = advance()
      |""".stripMargin) shouldBe 11
  }

  // ===== Name-list validation =====

  "reads(unknown) is rejected at validation time" in {
    val t = intercept[Exception] {
      eval("""
        |#reads(does_not_exist)
        |bad() -> int = 0
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("unknown global")
    t.getMessage should include("does_not_exist")
  }

  "reads(K) where K is an immutable val is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |val K = 99
        |#reads(K)
        |bad() -> int = K
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("not mutable")
  }

  "reads(LIMIT) where LIMIT is const is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |const LIMIT = 100
        |#reads(LIMIT)
        |bad() -> int = LIMIT
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("not mutable")
  }

  // ===== Call-site subset =====

  "annotated caller may call annotated callee whose writes are a subset" in {
    eval("""
      |var x = 0
      |var y = 0
      |#writes(x)
      |setX(v: int)
      |    x = v
      |#writes(x, y)
      |setBoth(a: int, b: int)
      |    setX(a)
      |    y = b
      |main() -> int
      |    setBoth(3, 4)
      |    return x + y
      |""".stripMargin) shouldBe 7
  }

  "annotated caller rejects calling annotated callee whose writes exceed caller's" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 0
        |var y = 0
        |#writes(x, y)
        |setBoth()
        |    x = 1
        |    y = 2
        |#writes(x)
        |bad()
        |    setBoth()
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("writes")
    t.getMessage should include("setBoth")
  }

  "annotated caller rejects calling unannotated function" in {
    val t = intercept[Exception] {
      eval("""
        |unannotated() -> int = 1
        |#reads()
        |#writes()
        |bad() -> int = unannotated()
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("unannotated")
    t.getMessage should include("#reads/#writes")
  }

  "annotated function rejects indirect (function-pointer) call" in {
    val t = intercept[Exception] {
      eval("""
        |#reads()
        |#writes()
        |add(a: int, b: int) -> int = a + b
        |#reads()
        |#writes()
        |bad() -> int
        |    var f = &add
        |    return f(1, 2)
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("indirect")
  }

  // ===== Mutual recursion =====

  "mutually recursive annotated functions resolve correctly" in {
    eval("""
      |var counter = 0
      |#writes(counter)
      |bumpA(n: int)
      |    if n <= 0 then return
      |    counter = counter + 1
      |    bumpB(n - 1)
      |#writes(counter)
      |bumpB(n: int)
      |    if n <= 0 then return
      |    counter = counter + 1
      |    bumpA(n - 1)
      |main() -> int
      |    bumpA(5)
      |    return counter
      |""".stripMargin) shouldBe 5
  }

  // ===== Interaction with pure =====

  "pure and reads(x) together is rejected (redundant)" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 0
        |#pure
        |#reads(x)
        |bad() -> int = x
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("#pure")
    t.getMessage should include("#reads")
  }

  "pure function may be called from a reads/writes function" in {
    eval("""
      |#pure
      |sq(x: int) -> int = x * x
      |#reads()
      |#writes()
      |callsq(x: int) -> int = sq(x)
      |main() -> int = callsq(7)
      |""".stripMargin) shouldBe 49
  }

  // ===== Contracts in annotated functions =====

  "require clause that reads a global not in reads is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var threshold = 10
        |#reads()
        |#writes()
        |bad(x: int) -> int
        |    require x > threshold
        |    return x
        |main() -> int = bad(20)
        |""".stripMargin)
    }
    t.getMessage should include("threshold")
  }

  "require clause that reads a global in reads compiles" in {
    eval("""
      |var threshold = 10
      |#reads(threshold)
      |bad(x: int) -> int
      |    require x > threshold
      |    return x
      |main() -> int = bad(20)
      |""".stripMargin) shouldBe 20
  }

  "for all quantifier reading a global not in reads is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var lo = 0
        |#reads()
        |#writes()
        |allPositive(n: int) -> bool
        |    return for all i in 0..<n => i >= lo
        |main() -> int
        |    if allPositive(5) then return 1 else return 0
        |""".stripMargin)
    }
    t.getMessage should include("lo")
  }

  // ===== Empty effect sets =====

  "reads/writes empty body using only locals compiles" in {
    eval("""
      |#reads()
      |#writes()
      |compute(x: int) -> int
      |    var y = x + 1
      |    var z = y * 2
      |    return z
      |main() -> int = compute(3)
      |""".stripMargin) shouldBe 8
  }

  "reads/writes empty body that calls new (heap) is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |struct Point
        |    x: int
        |    y: int
        |#reads()
        |#writes()
        |bad() -> int
        |    var p = new Point(1, 2)
        |    return p.x
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("heap-allocate")
  }

  "pure body that calls new (heap) is still rejected" in {
    val t = intercept[Exception] {
      eval("""
        |struct Point
        |    x: int
        |    y: int
        |#pure
        |bad() -> int
        |    var p = new Point(1, 2)
        |    return p.x
        |main() -> int = bad()
        |""".stripMargin)
    }
    t.getMessage should include("allocate")
  }

  // ===== Read+write overlap =====

  "compound assignment counts as both read and write" in {
    eval("""
      |var counter = 10
      |#reads(counter)
      |#writes(counter)
      |bump(n: int)
      |    counter += n
      |main() -> int
      |    bump(5)
      |    return counter
      |""".stripMargin) shouldBe 15
  }

  "compound assignment with writes-only declaration compiles (implicit read covered by writes)" in {
    // Rule 1: a read of V is allowed when V ∈ reads ∪ writes. Compound assignment is
    // a read-then-write of the same var; the writes-set covers the implicit read, so
    // declaring `#writes(counter)` alone is sufficient. (Stricter SPARK Output-vs-In_Out
    // distinction is deliberately not modeled in v1.)
    eval("""
      |var counter = 0
      |#writes(counter)
      |bump(n: int)
      |    counter += n
      |main() -> int
      |    bump(5)
      |    return counter
      |""".stripMargin) shouldBe 5
  }

  "plain write to a global with #writes declaration compiles" in {
    eval("""
      |var counter = 0
      |#writes(counter)
      |reset()
      |    counter = 0
      |main() -> int
      |    counter = 99
      |    reset()
      |    return counter
      |""".stripMargin) shouldBe 0
  }
}
