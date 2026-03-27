package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslAnalyzerTests extends AnyFreeSpec with Matchers {

  def analyze(source: String): TProgram =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    (new SyslAnalyzer).analyze(ast)

  def analyzeExprType(source: String): SyslType =
    val program = analyze(source)
    program.decls.collectFirst { case f: TFunDecl if f.name == "main" => f }.get.body match
      case TExprBody(expr) => expr.typ
      case TBlockBody(stmts) => stmts.last match
        case TExprStmt(expr) => expr.typ
        case _ => VoidType

  def shouldFail(source: String): Unit =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)

  // ===== Literal types =====

  "int literal has I64" in {
    analyzeExprType("main() -> int = 42\n") shouldBe I64
  }

  "bool literal has BoolType" in {
    analyzeExprType("main() -> int = true\n") shouldBe BoolType
  }

  "string literal has ArrayType(I8)" in {
    analyzeExprType(
      """main() -> int
        |    s = "hello"
        |    s
        |""".stripMargin) shouldBe ArrayType(I8, 0)
  }

  // ===== Arithmetic type inference =====

  "int + int = int" in {
    analyzeExprType("main() -> int = 1 + 2\n") shouldBe I64
  }

  "int * int = int" in {
    analyzeExprType("main() -> int = 3 * 4\n") shouldBe I64
  }

  "comparison produces bool" in {
    analyzeExprType("main() -> int = 3 < 5\n") shouldBe BoolType
  }

  "logical and produces bool" in {
    analyzeExprType("main() -> int = true && true\n") shouldBe BoolType
  }

  "logical or produces bool" in {
    analyzeExprType("main() -> int = false || true\n") shouldBe BoolType
  }

  "unary minus preserves type" in {
    analyzeExprType("main() -> int = -42\n") shouldBe I64
  }

  "unary not produces bool" in {
    analyzeExprType("main() -> int = !false\n") shouldBe BoolType
  }

  "bitwise and produces int" in {
    analyzeExprType("main() -> int = 0xFF & 0x0F\n") shouldBe I64
  }

  "shift produces int" in {
    analyzeExprType("main() -> int = 1 << 8\n") shouldBe I64
  }

  // ===== Variable type inference =====

  "variable inferred from int literal" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe I64
  }

  "variable with explicit type" in {
    analyzeExprType(
      """main() -> int
        |    x: int = 42
        |    x
        |""".stripMargin) shouldBe I64
  }

  "variable inferred from expression" in {
    analyzeExprType(
      """main() -> int
        |    x = 3 + 4
        |    x
        |""".stripMargin) shouldBe I64
  }

  // ===== Pointer type inference =====

  "address of int is ptr to int" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    p = &x
        |    p
        |""".stripMargin) shouldBe PtrType(I64)
  }

  "deref of ptr to int is int" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe I64
  }

  "double pointer" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    p = &x
        |    pp = &p
        |    pp
        |""".stripMargin) shouldBe PtrType(PtrType(I64))
  }

  // ===== Array type inference =====

  "array declaration" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    a
        |""".stripMargin) shouldBe ArrayType(I64, 5)
  }

  "array indexing produces element type" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    a[0]
        |""".stripMargin) shouldBe I64
  }

  "address of array element" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    p = &a[2]
        |    p
        |""".stripMargin) shouldBe PtrType(I64)
  }

  // ===== Function return type =====

  "function call type is return type" in {
    analyzeExprType(
      """double(x: int) -> int = x * 2
        |main() -> int = double(21)
        |""".stripMargin) shouldBe I64
  }

  "void function call" in {
    analyze(
      """greet()
        |    print(42)
        |
        |main() -> int
        |    greet()
        |    0
        |""".stripMargin) // should not throw
  }

  // ===== If expression type =====

  "if expression type from then branch" in {
    analyzeExprType("main() -> int = if true then 42 else 0\n") shouldBe I64
  }

  // ===== Pointer arithmetic type =====

  "array + int stays array/pointer" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    p = a + 2
        |    p
        |""".stripMargin) shouldBe ArrayType(I64, 5)
  }

  // ===== Error detection =====

  "undefined variable error" in {
    shouldFail("main() -> int = x\n")
  }

  "undefined function error" in {
    shouldFail("main() -> int = unknown()\n")
  }

  "duplicate function error" in {
    shouldFail(
      """f() -> int = 1
        |f() -> int = 2
        |main() -> int = f()
        |""".stripMargin)
  }

  "deref bool error" in {
    shouldFail(
      """main() -> int
        |    x = true
        |    *x
        |""".stripMargin)
  }

  "index bool error" in {
    shouldFail(
      """main() -> int
        |    x = true
        |    x[0]
        |""".stripMargin)
  }

  // ===== Increment/decrement types =====

  "prefix increment preserves type" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    ++x
        |""".stripMargin) shouldBe I64
  }

  "postfix increment preserves type" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    x++
        |""".stripMargin) shouldBe I64
  }

  // ===== Typed AST structure =====

  "typed AST preserves function structure" in {
    val prog = analyze(
      """add(a: int, b: int) -> int = a + b
        |main() -> int = add(1, 2)
        |""".stripMargin)
    prog.decls.length shouldBe 2
    val add = prog.decls.head.asInstanceOf[TFunDecl]
    add.name shouldBe "add"
    add.params.length shouldBe 2
    add.returnType shouldBe I64
  }

  "typed AST has types on all expressions" in {
    val prog = analyze("main() -> int = 1 + 2 * 3\n")
    val main = prog.decls.head.asInstanceOf[TFunDecl]
    val body = main.body.asInstanceOf[TExprBody].expr
    body.typ shouldBe I64
    body match
      case TBinary(_, "+", right, _) =>
        right.typ shouldBe I64
      case _ => fail("expected binary +")
  }

  // ===== Integration: analyze then interpret =====

  "analyze then interpret factorial" in {
    val source =
      """factorial(n: int) -> int
        |    if n <= 1
        |        return 1
        |    n * factorial(n - 1)
        |
        |main() -> int = factorial(5)
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter()
    interp.run(typed) shouldBe 120
  }

  "analyze then interpret bubble sort" in {
    val source =
      """sort(arr: *int, n: int)
        |    i = 0
        |    while i < n - 1
        |        j = 0
        |        while j < n - 1 - i
        |            if arr[j] > arr[j + 1]
        |                tmp = arr[j]
        |                arr[j] = arr[j + 1]
        |                arr[j + 1] = tmp
        |            j += 1
        |        i += 1
        |
        |main() -> int
        |    a: [5]int
        |    a[0] = 5
        |    a[1] = 3
        |    a[2] = 1
        |    a[3] = 4
        |    a[4] = 2
        |    sort(a, 5)
        |    a[0] * 10000 + a[1] * 1000 + a[2] * 100 + a[3] * 10 + a[4]
        |""".stripMargin
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter()
    interp.run(typed) shouldBe 12345
  }
}
