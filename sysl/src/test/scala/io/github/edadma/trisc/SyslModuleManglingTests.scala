package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslModuleManglingTests extends AnyFreeSpec with Matchers {

  // ===== Basic mangling =====

  "function in module gets mangled name in TFunDecl" in {
    val sources = Map(
      "mymod/helper" ->
        """module mymod
          |helper(x: int) -> int = x + 1
          |""".stripMargin,
      "app" ->
        """import mymod.*
          |main() -> int = helper(41)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val helperUnit = result.units.find(_.name == "mymod/helper").get
    val helperDecl = helperUnit.typed.decls.collectFirst { case f: TFunDecl if f.name.contains("helper") => f }.get
    helperDecl.name shouldBe "mymod__helper"
  }

  "main is never mangled even in a module" in {
    val sources = Map(
      "mymod/app" ->
        """module mymod
          |main() -> int = 42
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val mainDecl = result.units.head.typed.decls.collectFirst { case f: TFunDecl if f.name == "main" => f }
    mainDecl shouldBe defined
  }

  "extern function is never mangled" in {
    val sources = Map(
      "mymod/ext" ->
        """module mymod
          |extern sbrk(n: int) -> *i8
          |main() -> int = 0
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val externDecl = result.units.head.typed.decls.collectFirst { case e: TExternFuncDecl => e }.get
    externDecl.name shouldBe "sbrk"
  }

  "standalone file (no module) has no mangling" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """helper(x: int) -> int = x + 1
        |main() -> int = helper(41)
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val helperDecl = typed.decls.collectFirst { case f: TFunDecl if f.name == "helper" => f }
    helperDecl shouldBe defined
  }

  // ===== Cross-module call resolution =====

  "cross-module call resolves to mangled name" in {
    val sources = Map(
      "mathlib/add" ->
        """module mathlib
          |add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "app" ->
        """import mathlib.*
          |main() -> int = add(19, 23)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 42
  }

  // ===== Same-name functions in different modules don't collide =====

  "same function name in different modules" in {
    val sources = Map(
      "alpha/funcs" ->
        """module alpha
          |helper(x: int) -> int = x + 10
          |""".stripMargin,
      "beta/funcs" ->
        """module beta
          |helper(x: int) -> int = x + 20
          |""".stripMargin,
      "app" ->
        """import alpha.{helper => alpha_helper}
          |import beta.{helper => beta_helper}
          |main() -> int = alpha_helper(1) + beta_helper(1)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 32 // (1+10) + (1+20)
  }

  // ===== Global variable mangling =====

  "global variable in module is mangled" in {
    val sources = Map(
      "mymod/globals" ->
        """module mymod
          |val MAGIC = 42
          |get_magic() -> int = MAGIC
          |""".stripMargin,
      "app" ->
        """import mymod.*
          |main() -> int = get_magic()
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 42
  }

  // ===== Method calls in modules =====

  "method call in module resolves to mangled function" in {
    val sources = Map(
      "mymod/point" ->
        """module mymod
          |struct Point
          |    x: int
          |    y: int
          |
          |Point_sum(__self__: *Point) -> int = self.x + self.y
          |""".stripMargin,
      "app" ->
        """import mymod.*
          |main() -> int
          |    var p = Point(3, 4)
          |    p.sum()
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 7
  }

  // ===== Selective import =====

  "selective import picks correct mangled function" in {
    val sources = Map(
      "mathlib/math" ->
        """module mathlib
          |add(a: int, b: int) -> int = a + b
          |mul(a: int, b: int) -> int = a * b
          |""".stripMargin,
      "app" ->
        """import mathlib.add
          |main() -> int = add(3, 4)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 7
  }

  // ===== Renamed import =====

  "renamed import maps alias to mangled function" in {
    val sources = Map(
      "mathlib/math" ->
        """module mathlib
          |add(a: int, b: int) -> int = a + b
          |""".stripMargin,
      "app" ->
        """import mathlib.{add => plus}
          |main() -> int = plus(10, 20)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val merged = TProgram(result.units.flatMap(_.typed.decls))
    val interp = new SyslInterpreter()
    interp.run(merged) shouldBe 30
  }
}
