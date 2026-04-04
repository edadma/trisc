package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslStructTests extends SyslTestHelpers {

  // ===== Basic struct =====

  "struct declaration and field access" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "struct fields default to zero" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.x
        |""".stripMargin) shouldBe 0
  }

  "struct with three fields" in {
    eval(
      """struct Vec3
        |    x: int
        |    y: int
        |    z: int
        |
        |main() -> int
        |    v: Vec3
        |    v.x = 1
        |    v.y = 2
        |    v.z = 3
        |    v.x + v.y + v.z
        |""".stripMargin) shouldBe 6
  }

  // ===== Struct with different field types =====

  "struct with mixed types" in {
    eval(
      """struct Pair
        |    first: int
        |    second: bool
        |
        |main() -> int
        |    p: Pair
        |    p.first = 42
        |    p.second = true
        |    if p.second then p.first else 0
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple structs =====

  "multiple struct types" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |struct Size
        |    w: int
        |    h: int
        |
        |main() -> int
        |    p: Point
        |    s: Size
        |    p.x = 10
        |    s.w = 100
        |    p.x + s.w
        |""".stripMargin) shouldBe 110
  }

  // ===== Struct as function argument (by value via pointer) =====

  "pass struct pointer to function" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |sum(p: *Point) -> int = p.x + p.y
        |
        |main() -> int
        |    p: Point
        |    p.x = 15
        |    p.y = 27
        |    sum(&p)
        |""".stripMargin) shouldBe 42
  }

  // ===== Field assignment =====

  "field reassignment" in {
    eval(
      """struct Counter
        |    count: int
        |
        |main() -> int
        |    c: Counter
        |    c.count = 10
        |    c.count = 42
        |    c.count
        |""".stripMargin) shouldBe 42
  }

  // ===== Analyzer errors =====

  "analyzer rejects unknown field" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    p.z
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects field access on non-struct" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    x.field
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects duplicate struct" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Foo
        |    x: int
        |
        |struct Foo
        |    y: int
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== Parser =====

  "parse struct declaration" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls(0) shouldBe a[StructDeclAST]
    ast.decls(0).asInstanceOf[StructDeclAST].name shouldBe "Point"
    ast.decls(0).asInstanceOf[StructDeclAST].fields shouldBe List(("x", NamedTypeAST("int")), ("y", NamedTypeAST("int")))
  }

  // ===== Struct in loop =====

  "struct in loop" in {
    eval(
      """struct Accum
        |    total: int
        |    count: int
        |
        |main() -> int
        |    a: Accum
        |    for i = 1; i <= 5; i++
        |        a.total = a.total + i
        |        a.count = a.count + 1
        |    a.total
        |""".stripMargin) shouldBe 15
  }
}
