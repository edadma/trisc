package io.github.edadma.trisc

/** Struct invariants fire at construction sites (var init + whole-struct assignment),
 *  not only on field mutations. */
class SyslStructInvariantConstructionTests extends SyslTestHelpers {

  "invariant passes on valid var-init construction" in {
    eval("""
      |struct Account
      |    balance: int
      |    limit: int
      |    invariant balance >= -limit
      |
      |main() -> int
      |    var a: Account = Account(10, 100)
      |    a.balance
      |""".stripMargin) shouldBe 10
  }

  "invariant traps on invalid construction via var init" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Account
        |    balance: int
        |    limit: int
        |    invariant balance >= -limit
        |
        |main() -> int
        |    var a: Account = Account(-500, 100)
        |    a.balance
        |""".stripMargin)
    }
    thrown.getMessage should include("Account invariant")
  }

  "invariant traps on invalid re-assignment to a struct-typed variable" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |struct Account
        |    balance: int
        |    limit: int
        |    invariant balance >= -limit
        |
        |main() -> int
        |    var a: Account = Account(10, 100)
        |    a = Account(-500, 100)
        |    a.balance
        |""".stripMargin)
    }
    thrown.getMessage should include("Account invariant")
  }

  "invariant passes on valid re-assignment" in {
    eval("""
      |struct Account
      |    balance: int
      |    limit: int
      |    invariant balance >= -limit
      |
      |main() -> int
      |    var a: Account = Account(10, 100)
      |    a = Account(-50, 100)
      |    a.balance
      |""".stripMargin) shouldBe -50
  }
}
