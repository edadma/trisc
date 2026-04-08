package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslFuncPointerTests extends SyslTestHelpers {

  // ===== Basic function pointer =====

  "assign function to variable and call" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f: (int) -> int = dbl
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "function pointer with inferred type" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f = dbl
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== Passing function pointers as arguments =====

  "pass function as argument" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(dbl, 21)
        |""".stripMargin) shouldBe 42
  }

  "pass different functions" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(dbl, 10) + apply(triple, 10)
        |""".stripMargin) shouldBe 50
  }

  // ===== Higher-order functions =====

  "map-like: apply function to array elements" in {
    output(
      """dbl(x: int) -> int = x * 2
        |forEach(arr: *int, n: int, f: (int) -> int)
        |    for i = 0; i < n; i++
        |        print(f(arr[i]))
        |main() -> int
        |    arr: [3]int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    forEach(&arr[0], 3, dbl)
        |    0
        |""".stripMargin) shouldBe "246"
  }

  // ===== Function pointer reassignment =====

  "reassign function pointer" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    f = dbl
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
        |apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |main() -> int = apply2(myAdd, 20, 22)
        |""".stripMargin) shouldBe 42
  }

  // ===== Void function pointer =====

  "void function pointer" in {
    output(
      """greet(x: int)
        |    print(x)
        |main() -> int
        |    f: (int) -> unit = greet
        |    f(42)
        |    0
        |""".stripMargin) shouldBe "42"
  }

  // ===== Analyzer: type checking =====

  "analyzer infers FuncType from function name" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f = dbl
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

  "parse (int) -> int type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }

  "parse () -> int type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """apply(f: () -> int) -> int = f()
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }

  "parse (int) -> unit type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """apply(f: (int) -> unit, x: int)
        |    f(x)
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }

  // ===== Indirect calls on arbitrary expressions =====

  "call function pointer from array" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    var funcs: [2](int) -> int
        |    funcs[0] = dbl
        |    funcs[1] = triple
        |    funcs[0](10) + funcs[1](10)
        |""".stripMargin) shouldBe 50
  }

  "call function returned by another function" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |getFunc() -> (int) -> int = dbl
        |main() -> int = getFunc()(21)
        |""".stripMargin) shouldBe 42
  }

  "chain: function returning function pointer called immediately" in {
    output(
      """add(a: int, b: int) -> int = a + b
        |getOp() -> (int, int) -> int = add
        |main() -> int
        |    print(getOp()(20, 22))
        |    0
        |""".stripMargin) shouldBe "42"
  }

  "call dereferenced function pointer" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |main() -> int
        |    f: (int) -> int = dbl
        |    fp = &f
        |    (*fp)(21)
        |""".stripMargin) shouldBe 42
  }

  "indirect call on struct field" in {
    eval(
      """struct Ops
        |    apply: (int) -> int
        |dbl(x: int) -> int = x * 2
        |main() -> int
        |    var ops: Ops
        |    ops.apply = dbl
        |    ops.apply(21)
        |""".stripMargin) shouldBe 42
  }

  "call function-typed field on indexed struct" in {
    eval(
      """struct Cmd
        |    handler: (int) -> int
        |dbl(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |main() -> int
        |    var cmds: [2]Cmd
        |    cmds[0].handler = dbl
        |    cmds[1].handler = triple
        |    cmds[0].handler(10) + cmds[1].handler(10)
        |""".stripMargin) shouldBe 50
  }

  "analyzer rejects indirect call on non-function expression" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var arr: [3]int
        |    arr[0](1)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
