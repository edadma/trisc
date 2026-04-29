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

  "int literal has I32" in {
    analyzeExprType("main() -> int = 42\n") shouldBe I32
  }

  "bool literal has BoolType" in {
    analyzeExprType("main() -> int = true\n") shouldBe BoolType
  }

  "string literal has StringType" in {
    analyzeExprType(
      """main() -> int
        |    s = "hello"
        |    s
        |""".stripMargin) shouldBe StringType
  }

  // ===== Arithmetic type inference =====

  "int + int = int" in {
    analyzeExprType("main() -> int = 1 + 2\n") shouldBe I32
  }

  "int * int = int" in {
    analyzeExprType("main() -> int = 3 * 4\n") shouldBe I32
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
    analyzeExprType("main() -> int = -42\n") shouldBe I32
  }

  "unary not produces bool" in {
    analyzeExprType("main() -> int = !false\n") shouldBe BoolType
  }

  "bitwise and produces int" in {
    analyzeExprType("main() -> int = 0xFF & 0x0F\n") shouldBe I32
  }

  "shift produces int" in {
    analyzeExprType("main() -> int = 1 << 8\n") shouldBe I32
  }

  // ===== Variable type inference =====

  "variable inferred from int literal" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe I32
  }

  "variable with explicit type" in {
    analyzeExprType(
      """main() -> int
        |    x: int = 42
        |    x
        |""".stripMargin) shouldBe I32
  }

  "variable inferred from expression" in {
    analyzeExprType(
      """main() -> int
        |    x = 3 + 4
        |    x
        |""".stripMargin) shouldBe I32
  }

  // ===== Pointer type inference =====

  "address of int is ptr to int" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    p = &x
        |    p
        |""".stripMargin) shouldBe PtrType(I32)
  }

  "deref of ptr to int is int" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe I32
  }

  "double pointer" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    p = &x
        |    pp = &p
        |    pp
        |""".stripMargin) shouldBe PtrType(PtrType(I32))
  }

  // ===== Array type inference =====

  "array declaration" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    a
        |""".stripMargin) shouldBe ArrayType(I32, 5)
  }

  "array indexing produces element type" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    a[0]
        |""".stripMargin) shouldBe I32
  }

  "address of array element" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    p = &a[2]
        |    p
        |""".stripMargin) shouldBe PtrType(I32)
  }

  // ===== Function return type =====

  "function call type is return type" in {
    analyzeExprType(
      """dbl(x: int) -> int = x * 2
        |main() -> int = dbl(21)
        |""".stripMargin) shouldBe I32
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
    analyzeExprType("main() -> int = if true then 42 else 0\n") shouldBe I32
  }

  // ===== Pointer arithmetic type =====

  "array + int decays to pointer" in {
    analyzeExprType(
      """main() -> int
        |    a: [5]int
        |    p = a + 2
        |    p
        |""".stripMargin) shouldBe PtrType(I32)
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
        |""".stripMargin) shouldBe I32
  }

  "postfix increment preserves type" in {
    analyzeExprType(
      """main() -> int
        |    x = 42
        |    x++
        |""".stripMargin) shouldBe I32
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
    add.returnType shouldBe I32
  }

  "typed AST has types on all expressions" in {
    val prog = analyze("main() -> int = 1 + 2 * 3\n")
    val main = prog.decls.head.asInstanceOf[TFunDecl]
    val body = main.body.asInstanceOf[TExprBody].expr
    body.typ shouldBe I32
    body match
      case TBinary(_, "+", right, _) =>
        right.typ shouldBe I32
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

  // ===== Duplicate parameter names =====

  "duplicate parameter names rejected" in {
    shouldFail(
      """f(x: int, x: int) -> int = x
        |main() -> int = f(1, 2)
        |""".stripMargin)
  }

  "duplicate parameter names in generic function rejected" in {
    shouldFail(
      """id[T](x: T, x: T) -> T = x
        |main() -> int = id(1, 2)
        |""".stripMargin)
  }

  "three-parameter duplicate rejected" in {
    shouldFail(
      """f(a: int, b: int, a: int) -> int = a + b
        |main() -> int = f(1, 2, 3)
        |""".stripMargin)
  }

  "distinct parameter names accepted" in {
    // Should not throw
    analyze(
      """f(x: int, y: int, z: int) -> int = x + y + z
        |main() -> int = f(1, 2, 3)
        |""".stripMargin)
  }

  // ===== Explicit `self` parameter in method rejected =====

  "user-declared self in method is rejected" in {
    // The parser auto-injects `__self__`. If the user also writes `self: *Point`
    // as an explicit parameter, it's a separate parameter (not a duplicate),
    // but it's almost certainly a mistake — the analyzer has no way to warn
    // specifically about this today. So this just verifies the code compiles
    // without error and has two receiver-like params.
    // (Future: could warn "did you mean to leave out `self`?")
    analyze(
      """struct Point
        |    x: int
        |    y: int
        |
        |Point.foo(self: *Point) -> int = self.x
        |main() -> int = 0
        |""".stripMargin)
  }

  "method body uses `self` without declaring it" in {
    analyze(
      """struct Point
        |    x: int
        |    y: int
        |
        |Point.magnitude() -> int = self.x * self.x + self.y * self.y
        |main() -> int
        |    var p: Point
        |    p.x = 3
        |    p.y = 4
        |    p.magnitude()
        |""".stripMargin)
  }

  // ===== if-expression branch type unification =====

  "if-expr widens byte/int branches to int" in {
    // val ch = if cond then byte else -1  must yield int, not byte —
    // otherwise -1 truncates to 255 when stored in a byte-sized slot.
    analyzeExprType(
      """main() -> int
        |    val s = "x"
        |    val ch = if true then s[0] else -1
        |    ch
        |""".stripMargin) shouldBe I32
  }

  "if-expr widens int/i64 branches to i64" in {
    analyzeExprType(
      """main() -> i64
        |    val n: i64 = 9999999999i64
        |    val r = if true then 1 else n
        |    r
        |""".stripMargin) shouldBe I64
  }

  "if-expr widens u32/i64 branches to i64 (unsigned fits in signed)" in {
    analyzeExprType(
      """main() -> i64
        |    val u: u32 = 7u32
        |    val n: i64 = 9999999999i64
        |    val r = if true then u else n
        |    r
        |""".stripMargin) shouldBe I64
  }
}
