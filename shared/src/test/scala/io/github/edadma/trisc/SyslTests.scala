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

  // ===== Basic program =====

  "empty main returns 0" in {
    eval(
      """int main()
        |    return 0
        |""".stripMargin) shouldBe 0
  }

  "main returns value" in {
    eval(
      """int main()
        |    return 42
        |""".stripMargin) shouldBe 42
  }

  // ===== Arithmetic =====

  "addition" in {
    eval(
      """int main()
        |    return 3 + 4
        |""".stripMargin) shouldBe 7
  }

  "subtraction" in {
    eval(
      """int main()
        |    return 10 - 3
        |""".stripMargin) shouldBe 7
  }

  "multiplication" in {
    eval(
      """int main()
        |    return 6 * 7
        |""".stripMargin) shouldBe 42
  }

  "division" in {
    eval(
      """int main()
        |    return 42 / 6
        |""".stripMargin) shouldBe 7
  }

  "modulo" in {
    eval(
      """int main()
        |    return 17 % 5
        |""".stripMargin) shouldBe 2
  }

  "operator precedence" in {
    eval(
      """int main()
        |    return 2 + 3 * 4
        |""".stripMargin) shouldBe 14
  }

  "parentheses" in {
    eval(
      """int main()
        |    return (2 + 3) * 4
        |""".stripMargin) shouldBe 20
  }

  "unary minus" in {
    eval(
      """int main()
        |    return -42
        |""".stripMargin) shouldBe -42
  }

  // ===== Comparison =====

  "less than true" in {
    eval(
      """int main()
        |    return 3 < 5
        |""".stripMargin) shouldBe 1
  }

  "less than false" in {
    eval(
      """int main()
        |    return 5 < 3
        |""".stripMargin) shouldBe 0
  }

  "equal true" in {
    eval(
      """int main()
        |    return 5 == 5
        |""".stripMargin) shouldBe 1
  }

  "not equal" in {
    eval(
      """int main()
        |    return 5 != 3
        |""".stripMargin) shouldBe 1
  }

  // ===== Logical =====

  "logical and" in {
    eval(
      """int main()
        |    return 1 && 1
        |""".stripMargin) shouldBe 1
  }

  "logical and short-circuit" in {
    eval(
      """int main()
        |    return 0 && 1
        |""".stripMargin) shouldBe 0
  }

  "logical or" in {
    eval(
      """int main()
        |    return 0 || 1
        |""".stripMargin) shouldBe 1
  }

  "logical not" in {
    eval(
      """int main()
        |    return !0
        |""".stripMargin) shouldBe 1
  }

  // ===== Variables =====

  "local variable" in {
    eval(
      """int main()
        |    int x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    eval(
      """int main()
        |    int x = 1
        |    x = 42
        |    return x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    eval(
      """int main()
        |    int a = 10
        |    int b = 20
        |    return a + b
        |""".stripMargin) shouldBe 30
  }

  "uninitialized variable defaults to 0" in {
    eval(
      """int main()
        |    int x
        |    return x
        |""".stripMargin) shouldBe 0
  }

  // ===== If/else =====

  "if true branch" in {
    eval(
      """int main()
        |    if 1
        |        return 42
        |    return 0
        |""".stripMargin) shouldBe 42
  }

  "if false branch" in {
    eval(
      """int main()
        |    if 0
        |        return 42
        |    return 0
        |""".stripMargin) shouldBe 0
  }

  "if-else" in {
    eval(
      """int main()
        |    int x = 5
        |    if x > 3
        |        return 1
        |    else
        |        return 0
        |""".stripMargin) shouldBe 1
  }

  "if-else if-else" in {
    eval(
      """int main()
        |    int x = 5
        |    if x > 10
        |        return 3
        |    else if x > 3
        |        return 2
        |    else
        |        return 1
        |""".stripMargin) shouldBe 2
  }

  // ===== While =====

  "while loop" in {
    eval(
      """int main()
        |    int i = 0
        |    int sum = 0
        |    while i < 10
        |        sum = sum + i
        |        i = i + 1
        |    return sum
        |""".stripMargin) shouldBe 45
  }

  "while loop never enters" in {
    eval(
      """int main()
        |    int x = 0
        |    while 0
        |        x = 42
        |    return x
        |""".stripMargin) shouldBe 0
  }

  // ===== Functions =====

  "function call" in {
    eval(
      """int double(int x)
        |    return x * 2
        |
        |int main()
        |    return double(21)
        |""".stripMargin) shouldBe 42
  }

  "recursive function" in {
    eval(
      """int factorial(int n)
        |    if n <= 1
        |        return 1
        |    return n * factorial(n - 1)
        |
        |int main()
        |    return factorial(5)
        |""".stripMargin) shouldBe 120
  }

  "multiple arguments" in {
    eval(
      """int add(int a, int b, int c)
        |    return a + b + c
        |
        |int main()
        |    return add(10, 20, 30)
        |""".stripMargin) shouldBe 60
  }

  "void function" in {
    eval(
      """int x
        |
        |void set(int v)
        |    x = v
        |
        |int main()
        |    set(42)
        |    return x
        |""".stripMargin) shouldBe 42
  }

  // ===== Output =====

  "putchar" in {
    output(
      """int main()
        |    putchar(72)
        |    putchar(105)
        |    return 0
        |""".stripMargin) shouldBe "Hi"
  }

  "print" in {
    output(
      """int main()
        |    print(42)
        |    return 0
        |""".stripMargin) shouldBe "42"
  }

  "println" in {
    output(
      """int main()
        |    println(42)
        |    return 0
        |""".stripMargin) shouldBe "42\n"
  }

  // ===== Fibonacci =====

  "fibonacci" in {
    eval(
      """int fib(int n)
        |    if n <= 1
        |        return n
        |    return fib(n - 1) + fib(n - 2)
        |
        |int main()
        |    return fib(10)
        |""".stripMargin) shouldBe 55
  }

  // ===== Global variables =====

  "global variable" in {
    eval(
      """int counter
        |
        |void increment()
        |    counter = counter + 1
        |
        |int main()
        |    increment()
        |    increment()
        |    increment()
        |    return counter
        |""".stripMargin) shouldBe 3
  }
}
