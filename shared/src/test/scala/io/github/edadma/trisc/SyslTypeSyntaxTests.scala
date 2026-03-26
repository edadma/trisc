package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTypeSyntaxTests extends SyslTestHelpers {

  // ===== Pointer type in parameters =====

  "function with *int parameter" in {
    eval(
      """set(p: *int)
        |    *p = 42
        |
        |main() -> int
        |    x = 0
        |    set(&x)
        |    x
        |""".stripMargin) shouldBe 42
  }

  "function with *int parameter dereference" in {
    eval(
      """inc(p: *int)
        |    *p = *p + 1
        |
        |main() -> int
        |    x = 0
        |    inc(&x)
        |    inc(&x)
        |    x
        |""".stripMargin) shouldBe 2
  }

  "function with *int array parameter" in {
    eval(
      """sum(arr: *int, n: int) -> int
        |    total = 0
        |    i = 0
        |    while i < n
        |        total = total + arr[i]
        |        i = i + 1
        |    total
        |
        |main() -> int
        |    a: [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    sum(a, 3)
        |""".stripMargin) shouldBe 60
  }

  "swap with *int parameters" in {
    eval(
      """swap(a: *int, b: *int)
        |    tmp = *a
        |    *a = *b
        |    *b = tmp
        |
        |main() -> int
        |    x = 10
        |    y = 20
        |    swap(&x, &y)
        |    x * 100 + y
        |""".stripMargin) shouldBe 2010
  }

  "sort with *int array parameter" in {
    eval(
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
        |""".stripMargin) shouldBe 12345
  }

  // ===== Pointer type in return type =====

  "function returning *int" in {
    eval(
      """get_ptr(arr: *int, i: int) -> *int = arr + i
        |
        |main() -> int
        |    a: [5]int
        |    a[3] = 42
        |    p = get_ptr(a, 3)
        |    *p
        |""".stripMargin) shouldBe 42
  }

  // ===== Double pointer parameter =====

  "function with **int parameter" in {
    eval(
      """set_ptr(pp: **int, target: *int)
        |    *pp = target
        |
        |main() -> int
        |    x = 42
        |    y = 99
        |    p = &x
        |    set_ptr(&p, &y)
        |    *p
        |""".stripMargin) shouldBe 99
  }

  // ===== *byte parameter for string functions =====

  "strlen with *byte parameter" in {
    eval(
      """strlen(s: *byte) -> int
        |    n = 0
        |    while s[n] != 0 do n++
        |    n
        |
        |main() -> int = strlen("Hello")
        |""".stripMargin) shouldBe 5
  }

  "puts with *byte parameter" in {
    output(
      """puts(s: *byte)
        |    i = 0
        |    while s[i] != 0
        |        putchar(s[i])
        |        i += 1
        |
        |main() -> int
        |    puts("Hi!")
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  "strcmp with *byte parameters" in {
    eval(
      """strcmp(a: *byte, b: *byte) -> int
        |    i = 0
        |    while a[i] != 0 && a[i] == b[i] do i++
        |    a[i] - b[i]
        |
        |main() -> int
        |    if strcmp("abc", "abc") == 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Pointer type in variable declarations =====

  "variable with *int type annotation" in {
    eval(
      """main() -> int
        |    x = 42
        |    p: *int = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  // ===== Bool type =====

  "bool variable declaration" in {
    eval(
      """main() -> int
        |    flag: bool = true
        |    if flag then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "bool parameter" in {
    eval(
      """choose(flag: bool, a: int, b: int) -> int = if flag then a else b
        |
        |main() -> int = choose(true, 42, 0)
        |""".stripMargin) shouldBe 42
  }

  "bool return type" in {
    eval(
      """isPositive(x: int) -> bool = x > 0
        |
        |main() -> int = if isPositive(5) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Strong bool checking =====

  "analyzer rejects int in if condition" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    if 1
        |        42
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects int in while condition" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    while 1
        |        0
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects int in && operand" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = 1 && 1\n"): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects int in ! operand" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = !0\n"): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer accepts bool in if condition" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = if true then 1 else 0\n"): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts comparison in if condition" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = if 3 < 5 then 1 else 0\n"): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== Bool/int incompatibility =====

  "analyzer rejects assigning int to bool variable" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x: bool = 42
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects assigning bool to int variable" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x: int = true
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects bool in arithmetic" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = true + 1\n"): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "comparison result is bool not int" in {
    // This should work: comparison produces bool, if accepts bool
    eval("main() -> int = if 3 < 5 then 1 else 0\n") shouldBe 1
  }

  // ===== Analyzer rejects indexing plain int =====

  "analyzer rejects indexing int" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: int) -> int = x[0]
        |main() -> int = f(0)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "analyzer rejects dereferencing int" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: int) -> int = *x
        |main() -> int = f(0)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== Analyzer accepts *int for indexing/deref =====

  "analyzer accepts *int for indexing" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(arr: *int) -> int = arr[0]
        |main() -> int
        |    a: [3]int
        |    a[0] = 42
        |    f(a)
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    // Should not throw
    val interp = new SyslInterpreter()
    interp.run(typed) shouldBe 42
  }

  "analyzer accepts *int for deref" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(p: *int) -> int = *p
        |main() -> int
        |    x = 42
        |    f(&x)
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter()
    interp.run(typed) shouldBe 42
  }

  // ===== Type inference for pointer variables =====

  "analyzer infers *int from &x" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    // p should be inferred as *int
    val main = typed.decls.collectFirst { case f: TFunDecl if f.name == "main" => f }.get
    main.body match
      case TBlockBody(stmts) =>
        stmts(1) match
          case TAssignStmt("p", TAddrOf("x", PtrType(IntType))) => // correct
          case other => fail(s"expected TAssignStmt with PtrType(IntType), got $other")
      case _ => fail("expected block body")
  }
}
