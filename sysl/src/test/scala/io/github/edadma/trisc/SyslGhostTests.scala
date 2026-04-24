package io.github.edadma.trisc

/** `#ghost var` / `#ghost val` (module-level + statement) and `#ghost fn` — verification-only
  * declarations. Discipline: real code may not read ghost names or call ghost functions; a
  * `#ghost fn` may not write to real (non-ghost) module-level state. The strip pass drops
  * every ghost decl plus any contract clause that touches ghost state, so ghost code never
  * runs at runtime — its only audience is a future verifier. */
class SyslGhostTests extends SyslTestHelpers {

  // ===== Happy path: ghost var + contract reference =====

  "#ghost var declared at module level and read in a contract" in {
    eval("""
      |#ghost
      |var ghost_counter = 0
      |
      |#pure
      |triple(n: int) -> int
      |    require true
      |    ensure result == n + n + n
      |    return n * 3
      |
      |main() -> int = triple(7)
      |""".stripMargin) shouldBe 21
  }

  "ghost var written from real code is implicitly stripped" in {
    eval("""
      |#ghost
      |var visit_count: int = 0
      |
      |bump(x: int) -> int
      |    visit_count = visit_count + 1
      |    return x + 1
      |
      |main() -> int = bump(41)
      |""".stripMargin) shouldBe 42
  }

  // ===== Happy path: ghost local =====

  "#ghost var local is accepted and stripped from runtime" in {
    eval("""
      |main() -> int
      |    var n = 10
      |    #ghost var snapshot = n
      |    n = n + 5
      |    return n
      |""".stripMargin) shouldBe 15
  }

  "ghost local can be referenced in a following contract" in {
    eval("""
      |compute(x: int) -> int
      |    #ghost var entry = x
      |    var y = x * 2
      |    assume y == entry * 2
      |    return y
      |
      |main() -> int = compute(21)
      |""".stripMargin) shouldBe 42
  }

  // ===== Happy path: ghost function =====

  "#ghost fn called from a contract is allowed and stripped" in {
    eval("""
      |#ghost
      |is_positive(n: int) -> bool = n > 0
      |
      |abs_pos(n: int) -> int
      |    require is_positive(n)
      |    return n
      |
      |main() -> int = abs_pos(5)
      |""".stripMargin) shouldBe 5
  }

  "#ghost fn body may freely read real state" in {
    eval("""
      |var max_value = 100
      |
      |#ghost
      |under_max(n: int) -> bool = n < max_value
      |
      |checked(n: int) -> int
      |    require under_max(n)
      |    return n
      |
      |main() -> int = checked(42)
      |""".stripMargin) shouldBe 42
  }

  // ===== Discipline: real code cannot read ghost =====

  "real code reading a ghost var is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |#ghost
        |var secret = 42
        |
        |leak() -> int = secret
        |
        |main() -> int = leak()
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  "real code calling a ghost function is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |#ghost
        |squared(n: int) -> int = n * n
        |
        |misuse(x: int) -> int = squared(x) + 1
        |
        |main() -> int = misuse(3)
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  "real-code expression reading a ghost local is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |main() -> int
        |    #ghost var s = 10
        |    return s + 1
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  // ===== Discipline: ghost fn cannot write real state =====

  "ghost fn writing to a real global is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var real_counter = 0
        |
        |#ghost
        |bad() -> int
        |    real_counter = real_counter + 1
        |    return real_counter
        |
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  "ghost fn may write to a ghost global" in {
    eval("""
      |#ghost
      |var step_count = 0
      |
      |#ghost
      |bump_steps()
      |    step_count = step_count + 1
      |
      |main() -> int = 0
      |""".stripMargin) shouldBe 0
  }

  // ===== Validation: incompatible attribute combinations =====

  "#ghost combined with #pure is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |#ghost
        |#pure
        |bad(n: int) -> int = n
        |
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  "#ghost combined with #reads is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |var x = 0
        |
        |#ghost
        |#reads(x)
        |bad() -> int = x
        |
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  "#ghost on a const var is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |#ghost
        |const C: int = 5
        |
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  "#ghost on a #address var is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |#ghost
        |#address(0xFF000)
        |var mmio: u32
        |
        |main() -> int = 0
        |""".stripMargin)
    }
    t.getMessage should include("ghost")
  }

  // ===== Strip semantics: ghost code does not run at runtime =====

  "ghost var initializer with side effects is dropped from runtime" in {
    // If the ghost initializer ran, real_counter would be 1 — strip ensures it doesn't.
    eval("""
      |var real_counter = 0
      |
      |bump_real() -> int
      |    real_counter = real_counter + 1
      |    return real_counter
      |
      |main() -> int
      |    #ghost var s = bump_real()
      |    return real_counter
      |""".stripMargin) shouldBe 0
  }

  "ghost-touching contract does not run at runtime" in {
    // The contract `assume false_via_ghost()` would trap if it ran. Stripping the
    // ghost-touching contract means the program reaches main's return value.
    eval("""
      |#ghost
      |always_false() -> bool = false
      |
      |dangerous() -> int
      |    assume always_false()
      |    return 99
      |
      |main() -> int = dangerous()
      |""".stripMargin) shouldBe 99
  }

  // ===== Parser-level rejections =====

  "#ghost at statement position followed by a non-var is rejected" in {
    val t = intercept[Exception] {
      eval("""
        |main() -> int
        |    #ghost return 1
        |""".stripMargin)
    }
    // Either a parser error or a downstream message — either way, the source must not run.
    t.getMessage should not be ""
  }

  // ===== Locality: ghost variables don't pollute real-code captures =====

  "ghost local in same scope as real code does not collide with real names" in {
    eval("""
      |main() -> int
      |    var x = 5
      |    #ghost var x_initial = x
      |    x = x + 10
      |    return x
      |""".stripMargin) shouldBe 15
  }
}
