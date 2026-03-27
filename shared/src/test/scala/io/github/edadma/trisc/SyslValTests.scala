package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslValTests extends SyslTestHelpers {

  // ===== val works =====

  "val local" in {
    eval(
      """main() -> int
        |    val x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "val local typed" in {
    eval(
      """main() -> int
        |    val x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "val global" in {
    eval(
      """val x = 42
        |main() -> int = x
        |""".stripMargin) shouldBe 42
  }

  "val in expression" in {
    eval(
      """main() -> int
        |    val a = 10
        |    val b = 32
        |    a + b
        |""".stripMargin) shouldBe 42
  }

  // ===== val rejects reassignment =====

  "reject assignment to val local" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    val x = 42
        |    x = 10
        |    x
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "reject compound assignment to val local" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    val x = 42
        |    x += 1
        |    x
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "reject assignment to val global" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """val x = 42
        |main() -> int
        |    x = 10
        |    x
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== var still works =====

  "var allows reassignment" in {
    eval(
      """main() -> int
        |    var x = 10
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "default (no keyword) allows reassignment" in {
    eval(
      """main() -> int
        |    x = 10
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ===== val and var mixed =====

  "val and var mixed" in {
    eval(
      """main() -> int
        |    val a = 10
        |    var b = 20
        |    b = 32
        |    a + b
        |""".stripMargin) shouldBe 42
  }

  // ===== private val =====

  "private val global" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """private val PI = 3
        |main() -> int = PI
        |""".stripMargin): @unchecked
    val decl = ast.decls(0).asInstanceOf[VarDeclAST]
    decl.isPrivate shouldBe true
    decl.isMutable shouldBe false
  }
}
