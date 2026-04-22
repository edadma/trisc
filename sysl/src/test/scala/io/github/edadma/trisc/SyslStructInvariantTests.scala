package io.github.edadma.trisc

class SyslStructInvariantTests extends SyslTestHelpers {

  // ===== Basic struct invariants — success cases =====

  "struct with single invariant passes on valid mutation" in {
    eval("""
      |struct Account
      |    balance: int
      |    limit: int
      |    invariant balance >= -limit
      |
      |main() -> int
      |    var a: Account = Account(10, 100)
      |    a.balance = -50
      |    a.balance
      |""".stripMargin) shouldBe -50
  }

  "struct with multiple invariants all checked" in {
    eval("""
      |struct Range
      |    lo: int
      |    hi: int
      |    invariant lo <= hi
      |    invariant hi - lo <= 100
      |
      |main() -> int
      |    var r: Range = Range(0, 10)
      |    r.hi = 50
      |    r.hi
      |""".stripMargin) shouldBe 50
  }

  // ===== Invariant violation traps =====

  "struct invariant traps on violating field assignment" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Account
        |    balance: int
        |    limit: int
        |    invariant balance >= -limit
        |
        |main() -> int
        |    var a: Account = Account(10, 100)
        |    a.balance = -200
        |    a.balance
        |""".stripMargin)
    }
    thrown.getMessage should include("Account invariant")
  }

  "struct invariant traps on second invariant failure" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Range
        |    lo: int
        |    hi: int
        |    invariant lo <= hi
        |    invariant hi - lo <= 100
        |
        |main() -> int
        |    var r: Range = Range(0, 10)
        |    r.hi = 200
        |    r.hi
        |""".stripMargin)
    }
    thrown.getMessage should include("Range invariant")
  }

  // ===== Compound assignment also checks =====

  "struct invariant fires on compound assignment" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Account
        |    balance: int
        |    limit: int
        |    invariant balance >= -limit
        |
        |main() -> int
        |    var a: Account = Account(10, 100)
        |    a.balance -= 500
        |    a.balance
        |""".stripMargin)
    }
    thrown.getMessage should include("Account invariant")
  }

  "struct invariant passes on valid compound assignment" in {
    eval("""
      |struct Counter
      |    n: int
      |    max: int
      |    invariant n <= max
      |
      |main() -> int
      |    var c: Counter = Counter(0, 100)
      |    c.n += 25
      |    c.n
      |""".stripMargin) shouldBe 25
  }

  // ===== Invariant on struct via pointer =====

  "struct invariant via pointer field assignment fires" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Account
        |    balance: int
        |    limit: int
        |    invariant balance >= -limit
        |
        |update(p: *Account)
        |    (*p).balance = -999
        |
        |main() -> int
        |    var a: Account = Account(10, 100)
        |    update(&a)
        |    a.balance
        |""".stripMargin)
    }
    thrown.getMessage should include("Account invariant")
  }

  // ===== Structs without invariants compile and run as before =====

  "plain struct without invariants still works" in {
    eval("""
      |struct Point
      |    x: int
      |    y: int
      |
      |main() -> int
      |    var p: Point = Point(3, 4)
      |    p.x = 10
      |    p.x
      |""".stripMargin) shouldBe 10
  }

  // ===== Struct invariants can reference constants / globals =====

  "struct invariant can reference global constants" in {
    eval("""
      |const MAX = 100
      |
      |struct Counter
      |    n: int
      |    invariant n <= MAX
      |
      |main() -> int
      |    var c: Counter = Counter(50)
      |    c.n = 100
      |    c.n
      |""".stripMargin) shouldBe 100
  }

  "struct invariant referencing constant traps on violation" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |const MAX = 100
        |
        |struct Counter
        |    n: int
        |    invariant n <= MAX
        |
        |main() -> int
        |    var c: Counter = Counter(50)
        |    c.n = 101
        |    c.n
        |""".stripMargin)
    }
    thrown.getMessage should include("Counter invariant")
  }

  // ===== Non-bool invariant rejected =====

  "non-bool invariant expression fails analysis" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Bad
        |    x: int
        |    invariant x
        |
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage should include("invariant must be bool")
  }
}
