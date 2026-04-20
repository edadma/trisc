package io.github.edadma.trisc

class SyslContractTests extends SyslTestHelpers {

  // ===== require (preconditions) =====

  "require passes when true" in {
    eval("""
      |abs(x: int) -> int
      |    require x > -1000000
      |    if x < 0 then -x else x
      |main() -> int = abs(-5)
      |""".stripMargin) shouldBe 5
  }

  "require traps when false" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |pos(x: int) -> int
        |    require x >= 0
        |    return x
        |main() -> int = pos(-1)
        |""".stripMargin)
    }
    thrown.getMessage should include("precondition")
  }

  "multiple require clauses checked in order" in {
    eval("""
      |divide(a: int, b: int) -> int
      |    require a >= 0
      |    require b > 0
      |    return a / b
      |main() -> int = divide(20, 4)
      |""".stripMargin) shouldBe 5
  }

  "second require traps when violated" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |divide(a: int, b: int) -> int
        |    require a >= 0
        |    require b > 0
        |    return a / b
        |main() -> int = divide(20, 0)
        |""".stripMargin)
    }
    thrown.getMessage should include("precondition")
  }

  // ===== ensure (postconditions) with result =====

  "ensure passes when true" in {
    eval("""
      |twice(x: int) -> int
      |    ensure result == x * 2
      |    x + x
      |main() -> int = twice(7)
      |""".stripMargin) shouldBe 14
  }

  "ensure traps when false" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |broken(x: int) -> int
        |    ensure result == x * 2
        |    return x + 1
        |main() -> int = broken(5)
        |""".stripMargin)
    }
    thrown.getMessage should include("postcondition")
  }

  "ensure also fires on trailing-expression return" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |alwaysFive() -> int
        |    ensure result == 5
        |    42
        |main() -> int = alwaysFive()
        |""".stripMargin)
    }
    thrown.getMessage should include("postcondition")
  }

  "ensure can reference parameters" in {
    eval("""
      |clamp(x: int) -> int
      |    ensure result >= 0
      |    ensure result <= 100
      |    if x < 0 then return 0
      |    if x > 100 then return 100
      |    return x
      |main() -> int = clamp(42)
      |""".stripMargin) shouldBe 42
  }

  "ensure violation from one of many returns" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |abs(x: int) -> int
        |    ensure result >= 0
        |    if x < 0 then return x   // buggy: forgot the negation
        |    return x
        |main() -> int = abs(-5)
        |""".stripMargin)
    }
    thrown.getMessage should include("postcondition")
  }

  // ===== mixed require + ensure =====

  "require and ensure together" in {
    eval("""
      |sqrtApprox(x: int) -> int
      |    require x >= 0
      |    ensure result >= 0
      |    var r = 0
      |    while r * r < x
      |        r = r + 1
      |    return r
      |main() -> int = sqrtApprox(25)
      |""".stripMargin) shouldBe 5
  }

  // ===== result is a normal identifier outside ensure =====

  "result can be a local var outside ensure" in {
    eval("""
      |compute(x: int) -> int
      |    var result = x * 2
      |    return result + 1
      |main() -> int = compute(10)
      |""".stripMargin) shouldBe 21
  }

  "require/ensure do not leak result outside ensure expression" in {
    eval("""
      |twice(x: int) -> int
      |    require x > 0
      |    ensure result == x * 2
      |    var result = x + x
      |    return result
      |main() -> int = twice(7)
      |""".stripMargin) shouldBe 14
  }
}
