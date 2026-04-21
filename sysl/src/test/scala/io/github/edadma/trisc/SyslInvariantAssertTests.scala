package io.github.edadma.trisc

class SyslInvariantAssertTests extends SyslTestHelpers {

  // ===== Loop invariants =====

  "loop invariant holds across iterations" in {
    eval("""
      |main() -> int =
      |    var sum = 0
      |    var i = 0
      |    while i < 5
      |        invariant i >= 0
      |        invariant sum >= 0
      |        sum = sum + i
      |        i = i + 1
      |    sum
      |""".stripMargin) shouldBe 10
  }

  "loop invariant traps when broken" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |main() -> int =
        |    var i = 0
        |    while i < 5
        |        invariant i >= 0
        |        i = i + 1
        |        i = 0 - 1
        |        invariant i >= 0
        |    i
        |""".stripMargin)
    }
    thrown.getMessage should include("invariant")
  }

  "invariant also works as a general assertion" in {
    eval("""
      |main() -> int =
      |    var x = 42
      |    invariant x > 0
      |    x
      |""".stripMargin) shouldBe 42
  }

  // ===== Static assertions =====

  "static_assert passes when true" in {
    eval("""
      |static_assert(sizeof(int) == 4)
      |main() -> int = 0
      |""".stripMargin) shouldBe 0
  }

  "static_assert with message passes" in {
    eval("""
      |static_assert(sizeof(i64) == 8, "i64 must be 8 bytes")
      |main() -> int = 0
      |""".stripMargin) shouldBe 0
  }

  "static_assert fails at compile time" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |static_assert(sizeof(int) == 100)
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage should include("static_assert failed")
  }

  "static_assert with message reports the message" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |static_assert(sizeof(int) == 100, "int must be 100 bytes")
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage should include("int must be 100 bytes")
  }

  "static_assert on struct layout" in {
    eval("""
      |struct Header
      |    magic: u32
      |    version: u32
      |    flags: u64
      |
      |static_assert(sizeof(Header) == 16, "Header layout fixed by protocol")
      |main() -> int = 0
      |""".stripMargin) shouldBe 0
  }

  "static_assert on const value" in {
    eval("""
      |const PAGE_SIZE = 4096
      |static_assert(PAGE_SIZE % 4096 == 0)
      |main() -> int = 0
      |""".stripMargin) shouldBe 0
  }

  "static_assert rejects non-constant condition" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |get() -> int = 42
        |static_assert(get() == 42)
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage should include("not compile-time evaluable")
  }
}
