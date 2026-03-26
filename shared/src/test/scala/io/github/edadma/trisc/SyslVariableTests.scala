package io.github.edadma.trisc

class SyslVariableTests extends SyslTestHelpers {

  // ===== Variables =====

  "local variable with type annotation" in {
    eval(
      """main() -> int
        |    x: int = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "local variable inferred" in {
    eval(
      """main() -> int
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    eval(
      """main() -> int
        |    x = 1
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    eval(
      """main() -> int
        |    a = 10
        |    b = 20
        |    return a + b
        |""".stripMargin) shouldBe 30
  }

  // ===== Global variables =====

  "global variable" in {
    eval(
      """counter = 0
        |
        |increment()
        |    counter = counter + 1
        |
        |main() -> int
        |    increment()
        |    increment()
        |    increment()
        |    return counter
        |""".stripMargin) shouldBe 3
  }

  "global variable with expression initializer" in {
    eval(
      """x = 2 + 3
        |
        |main() -> int = x
        |""".stripMargin) shouldBe 5
  }

  // ===== Syntactic edge cases: variables =====

  "typed variable declaration" in {
    eval(
      """main() -> int
        |    x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "untyped variable then reassign" in {
    eval(
      """main() -> int
        |    x = 1
        |    x = 2
        |    x = 3
        |    x
        |""".stripMargin) shouldBe 3
  }

  "variable used in its own initialization expression" in {
    eval(
      """main() -> int
        |    x = 10
        |    x = x + 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  // ===== Error cases =====

  "undefined variable error" in {
    val result = (new SyslParser).parseProgram("main() -> int = x\n")
    result match
      case Right(program) =>
        val interp = new SyslInterpreter()
        an[Exception] should be thrownBy interp.run(program)
      case Left(_) => fail("should parse")
  }
}
