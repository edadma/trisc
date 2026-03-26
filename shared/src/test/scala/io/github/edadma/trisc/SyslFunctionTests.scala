package io.github.edadma.trisc

class SyslFunctionTests extends SyslTestHelpers {

  // ===== Basic programs =====

  "empty main returns 0" in {
    eval(
      """main() -> int
        |    return 0
        |""".stripMargin) shouldBe 0
  }

  "main returns value with return" in {
    eval(
      """main() -> int
        |    return 42
        |""".stripMargin) shouldBe 42
  }

  "main returns last expression" in {
    eval(
      """main() -> int
        |    42
        |""".stripMargin) shouldBe 42
  }

  "block returns last expression after statements" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    x + y
        |""".stripMargin) shouldBe 30
  }

  "recursive function without explicit return" in {
    eval(
      """factorial(n: int) -> int
        |    if n <= 1
        |        return 1
        |    n * factorial(n - 1)
        |
        |main() -> int = factorial(5)
        |""".stripMargin) shouldBe 120
  }

  // ===== Expression functions =====

  "expression function" in {
    eval(
      """double(x: int) -> int = x * 2
        |
        |main() -> int = double(21)
        |""".stripMargin) shouldBe 42
  }

  "expression function inferred return type" in {
    eval(
      """double(x: int) = x * 2
        |
        |main() -> int = double(21)
        |""".stripMargin) shouldBe 42
  }

  "expression function with block body" in {
    eval(
      """compute(x: int) =
        |    y = x * 2
        |    z = y + 1
        |    z
        |
        |main() -> int = compute(20)
        |""".stripMargin) shouldBe 41
  }

  // ===== Functions =====

  "function call" in {
    eval(
      """double(x: int) -> int = x * 2
        |
        |main() -> int = double(21)
        |""".stripMargin) shouldBe 42
  }

  "recursive function" in {
    eval(
      """factorial(n: int) -> int
        |    if n <= 1
        |        return 1
        |    return n * factorial(n - 1)
        |
        |main() -> int = factorial(5)
        |""".stripMargin) shouldBe 120
  }

  "multiple arguments" in {
    eval(
      """add(a: int, b: int, c: int) -> int = a + b + c
        |
        |main() -> int = add(10, 20, 30)
        |""".stripMargin) shouldBe 60
  }

  "void function" in {
    eval(
      """x =0
        |
        |set(v: int)
        |    x = v
        |
        |main() -> int
        |    set(42)
        |    return x
        |""".stripMargin) shouldBe 42
  }

  // ===== Output =====

  "putchar" in {
    output(
      """main() -> int
        |    putchar(72)
        |    putchar(105)
        |    return 0
        |""".stripMargin) shouldBe "Hi"
  }

  "print" in {
    output(
      """main() -> int
        |    print(42)
        |    return 0
        |""".stripMargin) shouldBe "42"
  }

  "println" in {
    output(
      """main() -> int
        |    println(42)
        |    return 0
        |""".stripMargin) shouldBe "42\n"
  }

  // ===== Fibonacci =====

  "fibonacci" in {
    eval(
      """fib(n: int) -> int
        |    if n <= 1
        |        return n
        |    return fib(n - 1) + fib(n - 2)
        |
        |main() -> int = fib(10)
        |""".stripMargin) shouldBe 55
  }

  // ===== Nested calls =====

  "nested function calls" in {
    eval(
      """double(x: int) -> int = x * 2
        |triple(x: int) -> int = x * 3
        |
        |main() -> int = double(triple(7))
        |""".stripMargin) shouldBe 42
  }

  // ===== Multiple functions =====

  "multiple functions calling each other" in {
    eval(
      """isEven(n: int) -> int = if n == 0 then 1 else isOdd(n - 1)
        |isOdd(n: int) -> int = if n == 0 then 0 else isEven(n - 1)
        |
        |main() -> int = isEven(10)
        |""".stripMargin) shouldBe 1
  }

  // ===== Syntactic edge cases: function forms =====

  "function with -> return type, = expression" in {
    eval(
      """f() -> int = 42
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function with -> return type, = block" in {
    eval(
      """f() -> int =
        |    x = 21
        |    x * 2
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function with -> return type, regular block" in {
    eval(
      """f() -> int
        |    return 42
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function with -> return type, block last expression" in {
    eval(
      """f() -> int
        |    42
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function no return type, = expression (inferred)" in {
    eval(
      """f() = 42
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function no return type, = block (inferred)" in {
    eval(
      """f() =
        |    x = 21
        |    x * 2
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function no return type, block void" in {
    output(
      """f()
        |    print(42)
        |
        |main() -> int
        |    f()
        |    0
        |""".stripMargin) shouldBe "42"
  }

  "function no return type, block with last expression" in {
    eval(
      """f()
        |    42
        |
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  // ===== Edge cases: empty-ish programs =====

  "single expression function" in {
    eval("main() -> int = 42\n") shouldBe 42
  }

  "function with no parameters" in {
    eval(
      """f() -> int = 42
        |main() -> int = f()
        |""".stripMargin) shouldBe 42
  }

  "function with four parameters" in {
    eval(
      """f(a: int, b: int, c: int, d: int) -> int = a + b + c + d
        |main() -> int = f(1, 2, 3, 4)
        |""".stripMargin) shouldBe 10
  }

  // ===== Error case: undefined function error =====

  "undefined function error" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = unknown()\n"): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
