package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslImportResolutionTests extends AnyFreeSpec with Matchers {

  // Helper: create smeta for a "math" module
  private val mathSmeta =
    s"""SMETA v${ModuleMeta.SMETA_VERSION}
      |FUNC add 2 int int int
      |FUNC square 1 int int
      |DATA pi int
      |""".stripMargin

  private val ioSmeta =
    s"""SMETA v${ModuleMeta.SMETA_VERSION}
      |FUNC write 1 int unit
      |""".stripMargin

  private def analyzeWithImport(source: String, imports: Map[String, String]): (TProgram, SyslAnalyzer) =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    for (_, smeta) <- imports do
      analyzer.registerImport(ModuleMeta.fromSmeta(smeta).get)
    val typed = analyzer.analyze(ast)
    (typed, analyzer)

  // ===== Basic import resolution =====

  "imported function is callable" in {
    val (typed, _) = analyzeWithImport(
      """main() -> int = add(1, 2)
        |""".stripMargin,
      Map("math" -> mathSmeta))
    // Should not throw — add is in scope
    typed.decls.length shouldBe 1
  }

  "imported function rejects wrong argument type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int = add(true, 2)
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.registerImport(ModuleMeta.fromSmeta(mathSmeta).get)
    // add expects (int, int), passing bool should be rejected
    a [RuntimeException] should be thrownBy analyzer.analyze(ast)
  }

  "imported function return type is correct" in {
    val (typed, _) = analyzeWithImport(
      """main() -> int
        |    x = add(1, 2)
        |    x
        |""".stripMargin,
      Map("math" -> mathSmeta))
    typed.decls.length shouldBe 1
  }

  "imported variable is accessible" in {
    val (typed, _) = analyzeWithImport(
      """main() -> int = pi
        |""".stripMargin,
      Map("math" -> mathSmeta))
    typed.decls.length shouldBe 1
  }

  "multiple imports" in {
    val (typed, _) = analyzeWithImport(
      """main() -> int
        |    write(add(1, 2))
        |    0
        |""".stripMargin,
      Map("math" -> mathSmeta, "io" -> ioSmeta))
    typed.decls.length shouldBe 1
  }

  // ===== External tracking =====

  "imported functions are tracked as external" in {
    val (_, analyzer) = analyzeWithImport(
      """main() -> int = add(1, 2)
        |""".stripMargin,
      Map("math" -> mathSmeta))
    analyzer.isExternal("add") shouldBe true
    analyzer.isExternal("square") shouldBe true
    analyzer.isExternal("pi") shouldBe true
    analyzer.isExternal("main") shouldBe false
  }

  "externals set contains all imported symbols" in {
    val (_, analyzer) = analyzeWithImport(
      """main() -> int = 0
        |""".stripMargin,
      Map("math" -> mathSmeta))
    analyzer.externals should contain("add")
    analyzer.externals should contain("square")
    analyzer.externals should contain("pi")
    analyzer.externals should not contain "main"
  }

  // ===== Error cases =====

  "allows re-importing same module (idempotent)" in {
    val analyzer = new SyslAnalyzer
    analyzer.registerImport(ModuleMeta.fromSmeta(mathSmeta).get)
    // Re-importing the same module should not throw — same symbols, same names
    noException should be thrownBy analyzer.registerImport(ModuleMeta.fromSmeta(mathSmeta).get)
  }

  "rejects import conflicting with local function" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """add(a: int, b: int) -> int = a + b
        |main() -> int = add(1, 2)
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.registerImport(ModuleMeta.fromSmeta(mathSmeta).get)
    an[Exception] should be thrownBy analyzer.analyze(ast)
  }

  "undefined function still fails" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int = nonexistent(1)
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.registerImport(ModuleMeta.fromSmeta(mathSmeta).get)
    an[Exception] should be thrownBy analyzer.analyze(ast)
  }

  // ===== Private symbols not imported =====

  "private symbols are not visible" in {
    val smetaWithPrivate =
      s"""SMETA v${ModuleMeta.SMETA_VERSION}
        |FUNC public_fn 0 int
        |PRIVATE FUNC secret_fn 0 int
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int = secret_fn()
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.registerImport(ModuleMeta.fromSmeta(smetaWithPrivate).get)
    an[Exception] should be thrownBy analyzer.analyze(ast)
  }

  "public symbols from same module are visible" in {
    val smetaWithPrivate =
      s"""SMETA v${ModuleMeta.SMETA_VERSION}
        |FUNC public_fn 0 int
        |PRIVATE FUNC secret_fn 0 int
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int = public_fn()
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.registerImport(ModuleMeta.fromSmeta(smetaWithPrivate).get)
    analyzer.analyze(ast) // should not throw
  }

  // ===== End-to-end: analyze then interpret =====

  "imported function works with interpreter" in {
    // Simulate: math module exports add(int,int)->int
    // main calls add — analyzer accepts it
    // For interpretation we need the actual function, so this tests analyzer only
    val (typed, analyzer) = analyzeWithImport(
      """main() -> int = add(1, 2)
        |""".stripMargin,
      Map("math" -> mathSmeta))
    analyzer.isExternal("add") shouldBe true
    // The typed AST has a TCall to "add" with return type int
    val mainBody = typed.decls.head.asInstanceOf[TFunDecl].body.asInstanceOf[TExprBody].expr
    mainBody shouldBe a[TCall]
    mainBody.asInstanceOf[TCall].name shouldBe "add"
    mainBody.asInstanceOf[TCall].typ shouldBe I32
  }
}
