package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslModuleSyntaxTests extends AnyFreeSpec with Matchers {

  private def parse(source: String): ProgramAST =
    (new SyslParser).parseProgram(source) match
      case Right(ast) => ast
      case Left(err) => fail(s"parse error: $err")

  // ===== import =====

  "import declaration" in {
    val ast = parse(
      """import "math"
        |main() -> int = 0
        |""".stripMargin)
    ast.decls.head shouldBe a[ImportDeclAST]
    ast.decls.head.asInstanceOf[ImportDeclAST].path shouldBe "math"
  }

  "import with path" in {
    val ast = parse(
      """import "std/io"
        |main() -> int = 0
        |""".stripMargin)
    ast.decls.head.asInstanceOf[ImportDeclAST].path shouldBe "std/io"
  }

  "multiple imports" in {
    val ast = parse(
      """import "math"
        |import "io"
        |main() -> int = 0
        |""".stripMargin)
    ast.decls(0).asInstanceOf[ImportDeclAST].path shouldBe "math"
    ast.decls(1).asInstanceOf[ImportDeclAST].path shouldBe "io"
  }

  // ===== private =====

  "private function" in {
    val ast = parse(
      """private helper(x: int) -> int = x + 1
        |main() -> int = helper(5)
        |""".stripMargin)
    val helper = ast.decls(0).asInstanceOf[FunDeclAST]
    helper.name shouldBe "helper"
    helper.isPrivate shouldBe true
    val main = ast.decls(1).asInstanceOf[FunDeclAST]
    main.isPrivate shouldBe false
  }

  "private function with block body" in {
    val ast = parse(
      """private helper(x: int) -> int
        |    x + 1
        |main() -> int = 0
        |""".stripMargin)
    ast.decls(0).asInstanceOf[FunDeclAST].isPrivate shouldBe true
  }

  "private variable" in {
    val ast = parse(
      """private secret = 42
        |main() -> int = secret
        |""".stripMargin)
    val v = ast.decls(0).asInstanceOf[VarDeclAST]
    v.name shouldBe "secret"
    v.isPrivate shouldBe true
  }

  "private typed variable" in {
    val ast = parse(
      """private count: int = 0
        |main() -> int = count
        |""".stripMargin)
    ast.decls(0).asInstanceOf[VarDeclAST].isPrivate shouldBe true
  }

  "private array variable" in {
    val ast = parse(
      """private buf: [10]int
        |main() -> int = 0
        |""".stripMargin)
    ast.decls(0).asInstanceOf[VarDeclAST].isPrivate shouldBe true
  }

  "public by default" in {
    val ast = parse(
      """add(a: int, b: int) -> int = a + b
        |x = 10
        |""".stripMargin)
    ast.decls(0).asInstanceOf[FunDeclAST].isPrivate shouldBe false
    ast.decls(1).asInstanceOf[VarDeclAST].isPrivate shouldBe false
  }

  // ===== import + private together =====

  "import and private declarations" in {
    val ast = parse(
      """import "math"
        |private helper(x: int) -> int = x * 2
        |main() -> int = helper(3)
        |""".stripMargin)
    ast.decls(0) shouldBe a[ImportDeclAST]
    ast.decls(1).asInstanceOf[FunDeclAST].isPrivate shouldBe true
    ast.decls(2).asInstanceOf[FunDeclAST].isPrivate shouldBe false
  }

  // ===== private does not affect execution =====

  "private function still callable within module" in {
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(
      """private helper(x: int) -> int = x + 1
        |main() -> int = helper(5)
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    interp.run(typed) shouldBe 6
  }

  "private variable still accessible within module" in {
    val buf = new StringBuilder
    val Right(ast) = (new SyslParser).parseProgram(
      """private secret = 42
        |main() -> int = secret
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter(s => buf ++= s)
    interp.run(typed) shouldBe 42
  }
}
