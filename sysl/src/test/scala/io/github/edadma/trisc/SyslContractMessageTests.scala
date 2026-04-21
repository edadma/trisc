package io.github.edadma.trisc

class SyslContractMessageTests extends SyslTestHelpers {

  "require with message passes silently" in {
    eval("""
      |pos(x: int) -> int
      |    require x >= 0, "x must be non-negative"
      |    return x + 1
      |main() -> int = pos(41)
      |""".stripMargin) shouldBe 42
  }

  "require failure surfaces custom message" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |pos(x: int) -> int
        |    require x >= 0, "x must be non-negative"
        |    return x
        |main() -> int = pos(-1)
        |""".stripMargin)
    }
    thrown.getMessage should include("x must be non-negative")
  }

  "ensure failure surfaces custom message" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |twice(x: int) -> int
        |    ensure result == x * 2, "off by one in twice()"
        |    return x + x + 1
        |main() -> int = twice(5)
        |""".stripMargin)
    }
    thrown.getMessage should include("off by one in twice()")
  }

  "bare require (no message) still works" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |foo(x: int) -> int
        |    require x > 0
        |    return x
        |main() -> int = foo(-1)
        |""".stripMargin)
    }
    thrown.getMessage should include("precondition")
  }

  "multiple contracts with mixed messages" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |f(x: int, y: int) -> int
        |    require x >= 0
        |    require y > 0, "y must be positive"
        |    ensure result >= 0, "result must be non-negative"
        |    return x / y
        |main() -> int = f(10, 0)
        |""".stripMargin)
    }
    thrown.getMessage should include("y must be positive")
  }
}
