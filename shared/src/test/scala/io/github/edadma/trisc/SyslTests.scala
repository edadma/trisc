package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslTests extends AnyFreeSpec with Matchers {

  def run(source: String): (Long, String) =
    val buf = new StringBuilder
    val Right(program) = SyslParser.parseProgram(source): @unchecked
    val interp = new SyslInterpreter(s => buf ++= s)
    val result = interp.run(program)
    (result, buf.toString)

  def eval(source: String): Long = run(source)._1

  def output(source: String): String = run(source)._2

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

  // ===== Arithmetic =====

  "addition" in {
    eval("main() -> int = 3 + 4\n") shouldBe 7
  }

  "subtraction" in {
    eval("main() -> int = 10 - 3\n") shouldBe 7
  }

  "multiplication" in {
    eval("main() -> int = 6 * 7\n") shouldBe 42
  }

  "division" in {
    eval("main() -> int = 42 / 6\n") shouldBe 7
  }

  "modulo" in {
    eval("main() -> int = 17 % 5\n") shouldBe 2
  }

  "operator precedence" in {
    eval("main() -> int = 2 + 3 * 4\n") shouldBe 14
  }

  "parentheses" in {
    eval("main() -> int = (2 + 3) * 4\n") shouldBe 20
  }

  "unary minus" in {
    eval("main() -> int = -42\n") shouldBe -42
  }

  // ===== Comparison =====

  "less than true" in {
    eval("main() -> int = 3 < 5\n") shouldBe 1
  }

  "less than false" in {
    eval("main() -> int = 5 < 3\n") shouldBe 0
  }

  "equal true" in {
    eval("main() -> int = 5 == 5\n") shouldBe 1
  }

  "not equal" in {
    eval("main() -> int = 5 != 3\n") shouldBe 1
  }

  // ===== Logical =====

  "logical and" in {
    eval("main() -> int = 1 && 1\n") shouldBe 1
  }

  "logical and short-circuit" in {
    eval("main() -> int = 0 && 1\n") shouldBe 0
  }

  "logical or" in {
    eval("main() -> int = 0 || 1\n") shouldBe 1
  }

  "logical not" in {
    eval("main() -> int = !0\n") shouldBe 1
  }

  // ===== Variables =====

  "local variable with type annotation" in {
    eval(
      """main() -> int
        |    x: int = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "local variable inferred" in {
    eval(
      """main() -> int
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    eval(
      """main() -> int
        |    x = 1
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    eval(
      """main() -> int
        |    a = 10
        |    b = 20
        |    return a + b
        |""".stripMargin) shouldBe 30
  }

  // ===== If/else =====

  "if true branch" in {
    eval(
      """main() -> int
        |    if 1
        |        return 42
        |    return 0
        |""".stripMargin) shouldBe 42
  }

  "if false branch" in {
    eval(
      """main() -> int
        |    if 0
        |        return 42
        |    return 0
        |""".stripMargin) shouldBe 0
  }

  "if-else" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3
        |        return 1
        |    else
        |        return 0
        |""".stripMargin) shouldBe 1
  }

  "if-else if-else" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        return 3
        |    else if x > 3
        |        return 2
        |    else
        |        return 1
        |""".stripMargin) shouldBe 2
  }

  // ===== If with then (inline) =====

  "if then inline expression" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3 then return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "if then else inline expression" in {
    eval("main() -> int = if 1 then 42 else 0\n") shouldBe 42
  }

  "if then else inline false branch" in {
    eval("main() -> int = if 0 then 42 else 99\n") shouldBe 99
  }

  "if as expression in variable" in {
    eval(
      """main() -> int
        |    x = if 1 then 42 else 0
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if as expression in argument" in {
    eval(
      """double(x: int) -> int = x * 2
        |
        |main() -> int = double(if 1 then 21 else 0)
        |""".stripMargin) shouldBe 42
  }

  "if then with block body" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3 then
        |        return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "if then else with block bodies" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 3 then
        |        42
        |    else
        |        0
        |""".stripMargin) shouldBe 42
  }

  "nested if expressions" in {
    eval("main() -> int = if 1 then if 0 then 1 else 2 else 3\n") shouldBe 2
  }

  // ===== While =====

  "while loop" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 10
        |        sum = sum + i
        |        i = i + 1
        |    return sum
        |""".stripMargin) shouldBe 45
  }

  "while loop never enters" in {
    eval(
      """main() -> int
        |    x = 0
        |    while 0
        |        x = 42
        |    return x
        |""".stripMargin) shouldBe 0
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

  // ===== Global variables =====

  "global variable" in {
    eval(
      """counter =0
        |
        |increment()
        |    counter = counter + 1
        |
        |main() -> int
        |    increment()
        |    increment()
        |    increment()
        |    return counter
        |""".stripMargin) shouldBe 3
  }
}
