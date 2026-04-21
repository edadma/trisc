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
    ast.decls(0).asInstanceOf[StructDeclAST].fields shouldBe List(("x", NamedTypeAST("int"), false), ("y", NamedTypeAST("int"), false))
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

  "tangled list.lsysl parses ListNode with next prev anchor value" in {
    val raw = scala.io.Source.fromFile("std/container/list/list.lsysl").mkString
    val tangled = LiterateRenderer.tangle(new LiterateParser().parse(raw))
    val Right(ast) = (new SyslParser).parseProgram(tangled): @unchecked
    val ln = ast.decls.collect { case s: StructDeclAST if s.name == "ListNode" => s }.head
    ln.fields.map(_._1) shouldBe List("next", "prev", "anchor", "value")
  }

  "tangled list.lsysl analyzes (recursive generic field assign)" in {
    val raw = scala.io.Source.fromFile("std/container/list/list.lsysl").mkString
    val tangled = LiterateRenderer.tangle(new LiterateParser().parse(raw))
    val Right(ast) = (new SyslParser).parseProgram(tangled): @unchecked
    (new SyslAnalyzer).analyze(ast)
  }

  // ===== Literate programming: prose can split a single function body =====
  //
  // When an .lsysl file interleaves indented code blocks with markdown prose,
  // a function body declared in one block and continued in the next must
  // tangle into a single function (not two separate declarations or a parse
  // error). The existing mechanism is:
  //   - LiterateParser collects indented code blocks in source order
  //   - LiterateRenderer.tangle joins them with "\n"
  //   - SyslParser's IndentationLexical treats the resulting blank lines as
  //     continuation within the indented block, so the function body stays whole

  "literate prose between indented blocks keeps a single function body" in {
    val raw =
      """# sum of two
        |
        |Declare the first local:
        |
        |    compute() -> int
        |        val x = 40
        |
        |Then the second, after some explanatory prose:
        |
        |        val y = 2
        |        x + y
        |
        |Finally the entry point:
        |
        |    main() -> int = compute()
        |""".stripMargin
    val tangled = LiterateRenderer.tangle(new LiterateParser().parse(raw))
    val Right(ast) = (new SyslParser).parseProgram(tangled): @unchecked
    // Exactly two function decls — compute and main, not three or four
    val funs = ast.decls.collect { case f: FunDeclAST => f.name }
    funs shouldBe List("compute", "main")
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslInterpreter(_ => ())).run(typed) shouldBe 42L
  }

  "literate prose between nested indented blocks preserves if/else body" in {
    val raw =
      """# choose
        |
        |    choose(b: bool) -> int
        |        if b
        |            val hit = 100
        |
        |The `else` branch is separated by a prose paragraph:
        |
        |            hit
        |        else
        |            50
        |
        |    main() -> int = choose(true)
        |""".stripMargin
    val tangled = LiterateRenderer.tangle(new LiterateParser().parse(raw))
    val Right(ast) = (new SyslParser).parseProgram(tangled): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslInterpreter(_ => ())).run(typed) shouldBe 100L
  }
}
