package io.github.edadma.trisc

class SyslOldTests extends SyslTestHelpers {

  "old(param) in ensure passes" in {
    eval("""
      |increment(x: int) -> int
      |    ensure result == old(x) + 1
      |    return x + 1
      |main() -> int = increment(41)
      |""".stripMargin) shouldBe 42
  }

  "old(param) in ensure traps on violation" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |brokenIncrement(x: int) -> int
        |    ensure result == old(x) + 1
        |    return x + 2
        |main() -> int = brokenIncrement(5)
        |""".stripMargin)
    }
    thrown.getMessage should include("postcondition")
  }

  "old(param) snapshots before mutation" in {
    // Even though we modify the local `x` param, `old(x)` still sees the entry value.
    eval("""
      |compute(x: int) -> int
      |    ensure result == old(x) * 2
      |    x = 99
      |    return 10
      |main() -> int = compute(5)
      |""".stripMargin) shouldBe 10
  }

  "old(pointee) captures dereferenced value at entry" in {
    eval("""
      |bumpAndReturnOld(p: *int) -> int
      |    ensure *p == old(*p) + 1
      |    val prev = *p
      |    *p = *p + 1
      |    return prev
      |main() -> int
      |    var n = 41
      |    return bumpAndReturnOld(&n)
      |""".stripMargin) shouldBe 41
  }

  "old(pointee) postcondition violation traps" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |badBump(p: *int)
        |    ensure *p == old(*p) + 1
        |    *p = *p + 2
        |main() -> int
        |    var n = 10
        |    badBump(&n)
        |    return n
        |""".stripMargin)
    }
    thrown.getMessage should include("postcondition")
  }

  "multiple old() calls get distinct snapshots" in {
    eval("""
      |sumEntry(a: int, b: int) -> int
      |    ensure result == old(a) + old(b)
      |    a = 999
      |    b = 999
      |    return 3 + 4
      |main() -> int = sumEntry(3, 4)
      |""".stripMargin) shouldBe 7
  }

  "old() outside ensure is undefined" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |foo(x: int) -> int
        |    return old(x)
        |main() -> int = foo(1)
        |""".stripMargin)
    }
    thrown.getMessage should (include("undefined") or include("old"))
  }

  "old() with compound expression" in {
    eval("""
      |f(x: int, y: int) -> int
      |    ensure result == old(x + y)
      |    x = 0
      |    y = 0
      |    return 10
      |main() -> int = f(3, 7)
      |""".stripMargin) shouldBe 10
  }
}
