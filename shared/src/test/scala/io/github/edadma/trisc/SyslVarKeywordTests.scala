package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslVarKeywordTests extends SyslTestHelpers {

  // ===== Global declarations =====

  "var global inferred" in {
    eval(
      """var x = 42
        |main() -> int = x
        |""".stripMargin) shouldBe 42
  }

  "var global typed" in {
    eval(
      """var x: int = 42
        |main() -> int = x
        |""".stripMargin) shouldBe 42
  }

  "var global array" in {
    eval(
      """var buf: [3]int
        |main() -> int = 0
        |""".stripMargin) shouldBe 0
  }

  "global without var still works" in {
    eval(
      """x = 42
        |main() -> int = x
        |""".stripMargin) shouldBe 42
  }

  // ===== Local declarations =====

  "var local inferred" in {
    eval(
      """main() -> int
        |    var x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "var local typed" in {
    eval(
      """main() -> int
        |    var x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "var local array" in {
    eval(
      """main() -> int
        |    var arr: [3]int
        |    arr[0] = 10
        |    arr[0]
        |""".stripMargin) shouldBe 10
  }

  "local without var still works" in {
    eval(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ===== Mixed =====

  "var and non-var mixed" in {
    eval(
      """main() -> int
        |    var x = 10
        |    y = 20
        |    var z: int = 12
        |    x + y + z
        |""".stripMargin) shouldBe 42
  }

  // ===== Private var =====

  "private var global" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """private var secret = 42
        |main() -> int = secret
        |""".stripMargin): @unchecked
    val decl = ast.decls(0).asInstanceOf[VarDeclAST]
    decl.isPrivate shouldBe true
    decl.name shouldBe "secret"
  }
}
