package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslAnalyzerTests extends AnyFreeSpec with Matchers {

  def analyze(source: String): ProgramAST =
    val Right(program) = (new SyslParser).parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.analyze(program)
    program

  def analyzeExpr(source: String): SyslType =
    val program = analyze(source)
    // Find main's body and get the type of the last expression
    program.decls.collectFirst { case f: FunDeclAST if f.name == "main" => f }.get.body match
      case ExprBodyAST(expr) => expr.typ
      case BlockBodyAST(stmts) => stmts.last match
        case ExprStmtAST(expr) => expr.typ
        case _ => VoidType

  def shouldFail(source: String): Unit =
    val Right(program) = (new SyslParser).parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    an[analyzer.AnalysisError] should be thrownBy analyzer.analyze(program)

  // ===== Literal types =====

  "int literal has IntType" in {
    analyzeExpr("main() -> int = 42\n") shouldBe IntType
  }

  "bool literal has BoolType" in {
    analyzeExpr("main() -> int = true\n") shouldBe BoolType
  }

  "string literal has ArrayType(ByteType)" in {
    analyzeExpr(
      """main() -> int
        |    s = "hello"
        |    s
        |""".stripMargin) shouldBe ArrayType(ByteType, 0)
  }

  // ===== Arithmetic type inference =====

  "int + int = int" in {
    analyzeExpr("main() -> int = 1 + 2\n") shouldBe IntType
  }

  "int * int = int" in {
    analyzeExpr("main() -> int = 3 * 4\n") shouldBe IntType
  }

  "comparison produces bool" in {
    analyzeExpr("main() -> int = 3 < 5\n") shouldBe BoolType
  }

  "logical and produces bool" in {
    analyzeExpr("main() -> int = 1 && 1\n") shouldBe BoolType
  }

  "logical or produces bool" in {
    analyzeExpr("main() -> int = 0 || 1\n") shouldBe BoolType
  }

  "unary minus preserves type" in {
    analyzeExpr("main() -> int = -42\n") shouldBe IntType
  }

  "unary not produces bool" in {
    analyzeExpr("main() -> int = !0\n") shouldBe BoolType
  }

  "bitwise and produces int" in {
    analyzeExpr("main() -> int = 0xFF & 0x0F\n") shouldBe IntType
  }

  "shift produces int" in {
    analyzeExpr("main() -> int = 1 << 8\n") shouldBe IntType
  }

  // ===== Variable type inference =====

  "variable inferred from int literal" in {
    analyzeExpr(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe IntType
  }

  "variable with explicit type" in {
    analyzeExpr(
      """main() -> int
        |    x: int = 42
        |    x
        |""".stripMargin) shouldBe IntType
  }

  "variable inferred from expression" in {
    analyzeExpr(
      """main() -> int
        |    x = 3 + 4
        |    x
        |""".stripMargin) shouldBe IntType
  }

  // ===== Pointer type inference =====

  "address of int is ptr to int" in {
    analyzeExpr(
      """main() -> int
        |    x = 42
        |    p = &x
        |    p
        |""".stripMargin) shouldBe PtrType(IntType)
  }

  "deref of ptr to int is int" in {
    analyzeExpr(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe IntType
  }

  "double pointer" in {
    analyzeExpr(
      """main() -> int
        |    x = 42
        |    p = &x
        |    pp = &p
        |    pp
        |""".stripMargin) shouldBe PtrType(PtrType(IntType))
  }

  // ===== Array type inference =====

  "array declaration" in {
    analyzeExpr(
      """main() -> int
        |    a: [5]int
        |    a
        |""".stripMargin) shouldBe ArrayType(IntType, 5)
  }

  "array indexing produces element type" in {
    analyzeExpr(
      """main() -> int
        |    a: [5]int
        |    a[0]
        |""".stripMargin) shouldBe IntType
  }

  "array decays to pointer" in {
    analyzeExpr(
      """main() -> int
        |    a: [5]int
        |    p = a
        |    p
        |""".stripMargin) shouldBe ArrayType(IntType, 5)
  }

  "address of array element" in {
    analyzeExpr(
      """main() -> int
        |    a: [5]int
        |    p = &a[2]
        |    p
        |""".stripMargin) shouldBe PtrType(IntType)
  }

  // ===== Function return type =====

  "function call type is return type" in {
    analyzeExpr(
      """double(x: int) -> int = x * 2
        |main() -> int = double(21)
        |""".stripMargin) shouldBe IntType
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
    analyzeExpr("main() -> int = if 1 then 42 else 0\n") shouldBe IntType
  }

  // ===== Pointer arithmetic type =====

  "array + int stays array/pointer" in {
    analyzeExpr(
      """main() -> int
        |    a: [5]int
        |    p = a + 2
        |    p
        |""".stripMargin) shouldBe ArrayType(IntType, 5)
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
    analyzeExpr(
      """main() -> int
        |    x = 42
        |    ++x
        |""".stripMargin) shouldBe IntType
  }

  "postfix increment preserves type" in {
    analyzeExpr(
      """main() -> int
        |    x = 42
        |    x++
        |""".stripMargin) shouldBe IntType
  }

  // ===== Existing tests still work with analysis =====

  "analyze then interpret factorial" in {
    val source =
      """factorial(n: int) -> int
        |    if n <= 1
        |        return 1
        |    n * factorial(n - 1)
        |
        |main() -> int = factorial(5)
        |""".stripMargin
    val Right(program) = (new SyslParser).parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.analyze(program)
    val interp = new SyslInterpreter()
    interp.run(program) shouldBe 120
  }

  "analyze then interpret bubble sort" in {
    val source =
      """sort(arr: int, n: int)
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
    val Right(program) = (new SyslParser).parseProgram(source): @unchecked
    val analyzer = new SyslAnalyzer
    analyzer.analyze(program)
    val interp = new SyslInterpreter()
    interp.run(program) shouldBe 12345
  }
}
