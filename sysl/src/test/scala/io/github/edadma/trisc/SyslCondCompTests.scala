package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslCondCompTests extends AnyFreeSpec with Matchers {

  private def parse(source: String): ProgramAST =
    (new SyslParser).parseProgram(source) match
      case Right(ast) => ast
      case Left(err) => fail(s"parse error: $err")

  private def compileAndRun(sources: Map[String, String], config: Map[String, String] = Map.empty): Long =
    val driver = new SyslDriver(config = config)
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged)

  // ===== parsing =====

  "parse #if/#endif" in {
    val ast = parse(
      """#if HAS_FPU
        |sqrt(x: int) -> int = x
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    ast.decls(0) shouldBe a[CondDeclAST]
    ast.decls(1) shouldBe a[FunDeclAST]
  }

  "parse #if/#else/#endif" in {
    val ast = parse(
      """#if HAS_FPU
        |sqrt(x: int) -> int = x
        |#else
        |sqrt(x: int) -> int = 0
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.thenDecls.length shouldBe 1
    cond.elseDecls.get.length shouldBe 1
  }

  "parse condition: symbol" in {
    val ast = parse(
      """#if FOO
        |f() -> int = 1
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.cond shouldBe CondSymbol("FOO")
  }

  "parse condition: negation" in {
    val ast = parse(
      """#if !FOO
        |f() -> int = 1
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.cond shouldBe CondNot(CondSymbol("FOO"))
  }

  "parse condition: equality" in {
    val ast = parse(
      """#if ARCH == "trisc"
        |f() -> int = 1
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.cond shouldBe CondEq("ARCH", "trisc")
  }

  "parse condition: inequality" in {
    val ast = parse(
      """#if ARCH != "x86"
        |f() -> int = 1
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.cond shouldBe CondNeq("ARCH", "x86")
  }

  "parse condition: numeric value" in {
    val ast = parse(
      """#if WORD_SIZE == 64
        |f() -> int = 1
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.cond shouldBe CondEq("WORD_SIZE", "64")
  }

  "parse nested #if" in {
    val ast = parse(
      """#if HAS_FPU
        |#if PRECISION == "double"
        |sqrt(x: int) -> int = x
        |#endif
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val outer = ast.decls(0).asInstanceOf[CondDeclAST]
    outer.thenDecls(0) shouldBe a[CondDeclAST]
  }

  "parse multiple declarations in #if" in {
    val ast = parse(
      """#if HAS_FPU
        |sqrt(x: int) -> int = x
        |cbrt(x: int) -> int = x
        |#endif
        |main() -> int = 0
        |""".stripMargin)
    val cond = ast.decls(0).asInstanceOf[CondDeclAST]
    cond.thenDecls.length shouldBe 2
  }

  // ===== condition evaluation =====

  "true symbol includes then branch" in {
    compileAndRun(
      Map("app" ->
        """#if HAS_FPU
          |f() -> int = 42
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("HAS_FPU" -> "true")
    ) shouldBe 42
  }

  "false symbol excludes then branch" in {
    compileAndRun(
      Map("app" ->
        """#if HAS_FPU
          |f() -> int = 42
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map.empty
    ) shouldBe 0
  }

  "negated symbol" in {
    compileAndRun(
      Map("app" ->
        """#if !BARE_METAL
          |f() -> int = 1
          |#else
          |f() -> int = 2
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("BARE_METAL" -> "true")
    ) shouldBe 2
  }

  "equality match" in {
    compileAndRun(
      Map("app" ->
        """#if ARCH == "trisc"
          |f() -> int = 16
          |#else
          |f() -> int = 64
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("ARCH" -> "trisc")
    ) shouldBe 16
  }

  "equality no match" in {
    compileAndRun(
      Map("app" ->
        """#if ARCH == "trisc"
          |f() -> int = 16
          |#else
          |f() -> int = 64
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("ARCH" -> "x86")
    ) shouldBe 64
  }

  "inequality match" in {
    compileAndRun(
      Map("app" ->
        """#if ARCH != "x86"
          |f() -> int = 1
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("ARCH" -> "trisc")
    ) shouldBe 1
  }

  "numeric equality" in {
    compileAndRun(
      Map("app" ->
        """#if WORD_SIZE == 64
          |f() -> int = 64
          |#else
          |f() -> int = 32
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("WORD_SIZE" -> "64")
    ) shouldBe 64
  }

  "false string is falsy" in {
    compileAndRun(
      Map("app" ->
        """#if DISABLED
          |f() -> int = 1
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("DISABLED" -> "false")
    ) shouldBe 0
  }

  "zero is falsy" in {
    compileAndRun(
      Map("app" ->
        """#if DISABLED
          |f() -> int = 1
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("DISABLED" -> "0")
    ) shouldBe 0
  }

  "nested conditions" in {
    compileAndRun(
      Map("app" ->
        """#if HAS_FPU
          |#if PRECISION == "double"
          |f() -> int = 64
          |#else
          |f() -> int = 32
          |#endif
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map("HAS_FPU" -> "true", "PRECISION" -> "double")
    ) shouldBe 64
  }

  "nested condition outer false" in {
    compileAndRun(
      Map("app" ->
        """#if HAS_FPU
          |#if PRECISION == "double"
          |f() -> int = 64
          |#else
          |f() -> int = 32
          |#endif
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map.empty
    ) shouldBe 0
  }

  "no config means all conditions false" in {
    compileAndRun(
      Map("app" ->
        """#if ANYTHING
          |f() -> int = 1
          |#else
          |f() -> int = 0
          |#endif
          |main() -> int = f()
          |""".stripMargin),
      Map.empty
    ) shouldBe 0
  }

  "conditional with structs" in {
    compileAndRun(
      Map("app" ->
        """#if USE_POINT
          |struct Point
          |    x: int
          |    y: int
          |get_x(p: *Point) -> int = p.x
          |#endif
          |main() -> int
          |    var p = Point(42, 0)
          |    get_x(&p)
          |""".stripMargin),
      Map("USE_POINT" -> "true")
    ) shouldBe 42
  }

  "conditional import" in {
    compileAndRun(
      Map(
        "mathlib" ->
          """add(a: int, b: int) -> int = a + b
            |""".stripMargin,
        "app" ->
          """#if USE_MATH
            |import mathlib.*
            |#endif
            |main() -> int = add(19, 23)
            |""".stripMargin,
      ),
      Map("USE_MATH" -> "true")
    ) shouldBe 42
  }
}
