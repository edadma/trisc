package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslFuncPointerTests extends SyslTestHelpers {

  // ===== Basic function pointer =====

  "assign function to variable and call" in {
    eval(
      """double(x: int) -> int = x * 2
        |main() -> int
        |    f: func(int) -> int = double
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "function pointer with inferred type" in {
    eval(
      """double(x: int) -> int = x * 2
        |main() -> int
        |    f = double
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Passing function pointers as arguments =====

  "pass function as argument" in {
    eval(
      """double(x: int) -> int = x * 2
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(double, 21)
        |""".stripMargin) shouldBe 42
  }

  "pass different functions" in {
    eval(
      """double(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(double, 10) + apply(triple, 10)
        |""".stripMargin) shouldBe 50
  }

  // ===== Higher-order functions =====

  "map-like: apply function to array elements" in {
    output(
      """double(x: int) -> int = x * 2
        |forEach(arr: *int, n: int, f: func(int) -> int)
        |    for i = 0; i < n; i++
        |        print(f(arr[i]))
        |main() -> int
        |    arr: [3]int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    forEach(&arr[0], 3, double)
        |    0
        |""".stripMargin) shouldBe "246"
  }

  // ===== Function pointer reassignment =====

  "reassign function pointer" in {
    eval(
      """double(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    f = double
        |    a = f(10)
        |    f = triple
        |    b = f(10)
        |    a + b
        |""".stripMargin) shouldBe 50
  }

  // ===== Two-arg function pointer =====

  "two-arg function pointer" in {
    eval(
      """myAdd(a: int, b: int) -> int = a + b
        |apply2(f: func(int, int) -> int, a: int, b: int) -> int = f(a, b)
        |main() -> int = apply2(myAdd, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  // ===== Void function pointer =====

  "void function pointer" in {
    output(
      """greet(x: int)
        |    print(x)
        |main() -> int
        |    f: func(int) = greet
        |    f(42)
        |    0
        |""".stripMargin) shouldBe "42"
  }

  // ===== Analyzer: type checking =====

  "analyzer infers FuncType from function name" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """double(x: int) -> int = x * 2
        |main() -> int
        |    f = double
        |    f(21)
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    // Should not throw
    typed.decls.length shouldBe 2
  }

  "analyzer rejects calling non-function variable" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    x(1)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== Parser: function type syntax =====

  "parse func(int) -> int type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """apply(f: func(int) -> int, x: int) -> int = f(x)
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }

  "parse func() -> int type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """apply(f: func() -> int) -> int = f()
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }

  "parse func(int) void type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """apply(f: func(int), x: int)
        |    f(x)
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }
}
