package io.github.edadma.trisc

class SyslConstDeclTests extends SyslTestHelpers {

  // ===== Function-scope const =====

  "function-scope const folds literal" in {
    eval("""
      |main() -> int =
      |    const X = 42
      |    X + 1
      |""".stripMargin) shouldBe 43
  }

  "function-scope const folds arithmetic" in {
    eval("""
      |main() -> int =
      |    const A = 3
      |    const B = 4
      |    A * B + 1
      |""".stripMargin) shouldBe 13
  }

  "function-scope const rejects runtime initializer" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |compute(x: int) -> int = x + 1
        |main() -> int =
        |    var x = 5
        |    const BAD = compute(x)
        |    BAD
        |""".stripMargin)
    }
    thrown.getMessage should include("not compile-time evaluable")
  }

  "function-scope const with explicit type" in {
    eval("""
      |main() -> int =
      |    const X: int = 100
      |    X
      |""".stripMargin) shouldBe 100
  }

  // ===== Module-scope const =====

  "module-scope const works in function body" in {
    eval("""
      |const MAX = 150
      |main() -> int = MAX
      |""".stripMargin) shouldBe 150
  }

  "module-scope const can reference earlier const" in {
    eval("""
      |const A = 10
      |const B = A * 2
      |main() -> int = B + A
      |""".stripMargin) shouldBe 30
  }

  "module-scope const accepts a float type" in {
    output("""
      |const PI: f64 = 3.14
      |main() -> int
      |    println(PI)
      |    return 0
      |""".stripMargin) shouldBe "3.14\n"
  }

  "module-scope const still rejects string and other unsupported types" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |const NAME: string = "x"
        |main() -> int = 0
        |""".stripMargin)
    }
    thrown.getMessage.toLowerCase should include("integer, float, array, or struct type")
  }

  "module-scope const rejects runtime initializer" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |compute() -> int = 42
        |const BAD = compute()
        |main() -> int = BAD
        |""".stripMargin)
    }
    thrown.getMessage should include("not compile-time evaluable")
  }

  // ===== Const references in `within` bounds =====

  "const as range upper bound" in {
    eval("""
      |const MAX_AGE = 150
      |type Age = int within 0..MAX_AGE
      |main() -> int =
      |    var a: Age = 42
      |    a
      |""".stripMargin) shouldBe 42
  }

  "const range bound catches compile-time out-of-range literal" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |const MAX_AGE = 150
        |type Age = int within 0..MAX_AGE
        |main() -> int =
        |    var a: Age = 999
        |    a
        |""".stripMargin)
    }
    thrown.getMessage should include("out of range")
  }

  "const range bound enforced at runtime" in {
    val thrown = intercept[RuntimeException] {
      eval("""
        |const LO = 0
        |const HI = 100
        |type Score = int within LO..HI
        |main() -> int =
        |    var x = 200
        |    var s: Score = x
        |    s
        |""".stripMargin)
    }
    thrown.getMessage should include("range check failed")
  }

  "negative const as range lower bound" in {
    eval("""
      |const MIN = -50
      |const MAX = 50
      |type Temp = int within MIN..MAX
      |main() -> int =
      |    var t: Temp = -30
      |    t + 1
      |""".stripMargin) shouldBe -29
  }
}
