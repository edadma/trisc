package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslTests extends AnyFreeSpec with Matchers {

  def run(source: String): (Long, String) =
    val buf = new StringBuilder
    val Right(program) = (new SyslParser).parseProgram(source): @unchecked
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

  // ===== elif =====

  "elif basic" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    elif x > 3
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  "elif with then" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10 then 3
        |    elif x > 3 then 2
        |    else 1
        |""".stripMargin) shouldBe 2
  }

  "multiple elifs" in {
    eval(
      """main() -> int
        |    x = 25
        |    if x > 100
        |        5
        |    elif x > 50
        |        4
        |    elif x > 20
        |        3
        |    elif x > 10
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 3
  }

  "elif without else" in {
    eval(
      """main() -> int
        |    x = 5
        |    r = 0
        |    if x > 10
        |        r = 3
        |    elif x > 3
        |        r = 2
        |    r
        |""".stripMargin) shouldBe 2
  }

  "elif first branch taken" in {
    eval(
      """main() -> int
        |    x = 50
        |    if x > 10
        |        1
        |    elif x > 5
        |        2
        |    else
        |        3
        |""".stripMargin) shouldBe 1
  }

  "elif last else taken" in {
    eval(
      """main() -> int
        |    x = 1
        |    if x > 10
        |        3
        |    elif x > 5
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 1
  }

  "elif with then and blocks" in {
    eval(
      """main() -> int
        |    x = 15
        |    if x > 20 then
        |        3
        |    elif x > 10 then
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  "elif mixed with else if" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    elif x > 7
        |        2
        |    else if x > 3
        |        1
        |    else
        |        0
        |""".stripMargin) shouldBe 1
  }

  "else with nested if block" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    else
        |        if x > 3
        |            2
        |        else
        |            1
        |""".stripMargin) shouldBe 2
  }

  "else with deeply nested if blocks" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 100
        |        4
        |    else
        |        if x > 50
        |            3
        |        else
        |            if x > 3
        |                2
        |            else
        |                1
        |""".stripMargin) shouldBe 2
  }

  "else-if (no extra indent) three branches" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 100
        |        4
        |    else if x > 50
        |        3
        |    else if x > 3
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  // ===== Increment/decrement =====

  "prefix increment" in {
    eval(
      """main() -> int
        |    x = 5
        |    ++x
        |""".stripMargin) shouldBe 6
  }

  "prefix increment returns new value" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = ++x
        |    y
        |""".stripMargin) shouldBe 6
  }

  "prefix decrement" in {
    eval(
      """main() -> int
        |    x = 5
        |    --x
        |""".stripMargin) shouldBe 4
  }

  "postfix increment returns old value" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = x++
        |    y
        |""".stripMargin) shouldBe 5
  }

  "postfix increment modifies variable" in {
    eval(
      """main() -> int
        |    x = 5
        |    x++
        |    x
        |""".stripMargin) shouldBe 6
  }

  "postfix decrement returns old value" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = x--
        |    y
        |""".stripMargin) shouldBe 5
  }

  "postfix decrement modifies variable" in {
    eval(
      """main() -> int
        |    x = 5
        |    x--
        |    x
        |""".stripMargin) shouldBe 4
  }

  "increment in while loop" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 5
        |        sum = sum + i
        |        ++i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "postfix in expression" in {
    eval(
      """main() -> int
        |    x = 5
        |    x++ + 10
        |""".stripMargin) shouldBe 15
  }

  "prefix in expression" in {
    eval(
      """main() -> int
        |    x = 5
        |    ++x + 10
        |""".stripMargin) shouldBe 16
  }

  "while with do and postfix increment" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 5 do i++
        |    i
        |""".stripMargin) shouldBe 5
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

  // ===== Compound assignment =====

  "+= basic" in {
    eval(
      """main() -> int
        |    x = 10
        |    x += 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  "-= basic" in {
    eval(
      """main() -> int
        |    x = 10
        |    x -= 3
        |    x
        |""".stripMargin) shouldBe 7
  }

  "*= basic" in {
    eval(
      """main() -> int
        |    x = 6
        |    x *= 7
        |    x
        |""".stripMargin) shouldBe 42
  }

  "/= basic" in {
    eval(
      """main() -> int
        |    x = 42
        |    x /= 6
        |    x
        |""".stripMargin) shouldBe 7
  }

  "%= basic" in {
    eval(
      """main() -> int
        |    x = 17
        |    x %= 5
        |    x
        |""".stripMargin) shouldBe 2
  }

  "+= in while loop" in {
    eval(
      """main() -> int
        |    sum = 0
        |    i = 0
        |    while i < 5
        |        sum += i
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "+= with expression" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 5
        |    x += y * 2
        |    x
        |""".stripMargin) shouldBe 20
  }

  "+= inline with while do" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 10 do i += 1
        |    i
        |""".stripMargin) shouldBe 10
  }

  // ===== While with do =====

  "while do block" in {
    eval(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 10 do
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 45
  }

  "while do inline" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 5 do i = i + 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "while do inline function call" in {
    eval(
      """inc(p: int) -> int
        |    *p = *p + 1
        |    *p
        |
        |main() -> int
        |    i = 0
        |    while i < 5 do inc(&i)
        |    i
        |""".stripMargin) shouldBe 5
  }

  "while do never enters" in {
    eval(
      """main() -> int
        |    x = 0
        |    while 0 do x = 42
        |    x
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
      """counter = 0
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

  "global variable with expression initializer" in {
    eval(
      """x = 2 + 3
        |
        |main() -> int = x
        |""".stripMargin) shouldBe 5
  }

  // ===== Chained comparisons =====

  "chained comparison lower <= x <= upper" in {
    eval(
      """main() -> int
        |    x = 5
        |    if 1 <= x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison out of range" in {
    eval(
      """main() -> int
        |    x = 15
        |    if 1 <= x <= 10 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained comparison three operators" in {
    eval(
      """main() -> int
        |    if 1 < 2 < 3 < 4 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison three operators fails" in {
    eval(
      """main() -> int
        |    if 1 < 2 < 3 < 2 then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  "chained comparison mixed operators" in {
    eval(
      """main() -> int
        |    x = 5
        |    if 0 < x <= 5 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "chained comparison with == and !=" in {
    eval("main() -> int = if 5 == 5 != 0 then 1 else 0\n") shouldBe 1
  }

  "single comparison still works" in {
    eval("main() -> int = if 3 < 5 then 1 else 0\n") shouldBe 1
  }

  // ===== Boolean literals =====

  "true literal" in {
    eval("main() -> int = if true then 1 else 0\n") shouldBe 1
  }

  "false literal" in {
    eval("main() -> int = if false then 1 else 0\n") shouldBe 0
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

  // ===== Complex conditions =====

  "complex if condition" in {
    eval(
      """main() -> int
        |    x = 5
        |    y = 3
        |    if x > 3 && y < 10 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Nested while =====

  "nested while loops" in {
    eval(
      """main() -> int
        |    sum = 0
        |    i = 0
        |    while i < 3
        |        j = 0
        |        while j < 3
        |            sum = sum + 1
        |            j = j + 1
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 9
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

  // ===== Unary precedence =====

  "unary minus with multiplication" in {
    eval("main() -> int = -3 * 2\n") shouldBe -6
  }

  "unary not with comparison" in {
    eval("main() -> int = if !(3 > 5) then 1 else 0\n") shouldBe 1
  }

  // ===== Error cases =====

  "undefined variable error" in {
    val result = (new SyslParser).parseProgram("main() -> int = x\n")
    result match
      case Right(program) =>
        val interp = new SyslInterpreter()
        an[Exception] should be thrownBy interp.run(program)
      case Left(_) => fail("should parse")
  }

  "undefined function error" in {
    val result = (new SyslParser).parseProgram("main() -> int = unknown()\n")
    result match
      case Right(program) =>
        val interp = new SyslInterpreter()
        an[Exception] should be thrownBy interp.run(program)
      case Left(_) => fail("should parse")
  }

  "division by zero error" in {
    val result = (new SyslParser).parseProgram("main() -> int = 42 / 0\n")
    result match
      case Right(program) =>
        val interp = new SyslInterpreter()
        an[Exception] should be thrownBy interp.run(program)
      case Left(_) => fail("should parse")
  }

  // ===== If without else returns 0 =====

  "if without else returns 0" in {
    eval(
      """main() -> int
        |    x = if 0 then 42
        |    x
        |""".stripMargin) shouldBe 0
  }

  // ===== Abs using if expression =====

  "abs function with if expression" in {
    eval(
      """abs(x: int) -> int = if x >= 0 then x else -x
        |
        |main() -> int = abs(-42)
        |""".stripMargin) shouldBe 42
  }

  // ===== Clamp using chained comparison =====

  "clamp with chained comparison" in {
    eval(
      """clamp(x: int, lo: int, hi: int) -> int =
        |    if x < lo then lo
        |    else if x > hi then hi
        |    else x
        |
        |main() -> int
        |    a = clamp(5, 0, 10)
        |    b = clamp(-5, 0, 10)
        |    c = clamp(15, 0, 10)
        |    a + b + c
        |""".stripMargin) shouldBe 15
  }

  // ===== Syntactic edge cases: if/then/else combinations =====

  "if block without then, no else" in {
    eval(
      """main() -> int
        |    x = 0
        |    if 1
        |        x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if block without then, with else block" in {
    eval(
      """main() -> int
        |    if 0
        |        1
        |    else
        |        2
        |""".stripMargin) shouldBe 2
  }

  "if then inline, no else" in {
    eval(
      """main() -> int
        |    x = 0
        |    if 1 then x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if then inline, with inline else" in {
    eval("main() -> int = if 1 then 42 else 0\n") shouldBe 42
  }

  "if then inline, with block else" in {
    eval(
      """main() -> int
        |    if 0 then 42
        |    else
        |        99
        |""".stripMargin) shouldBe 99
  }

  "if then block, no else" in {
    eval(
      """main() -> int
        |    x = 0
        |    if 1 then
        |        x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "if then block, with else block" in {
    eval(
      """main() -> int
        |    if 1 then
        |        42
        |    else
        |        99
        |""".stripMargin) shouldBe 42
  }

  "if then block, with else if block" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10 then
        |        3
        |    else if x > 3 then
        |        2
        |    else
        |        1
        |""".stripMargin) shouldBe 2
  }

  "if then inline, else if inline, else inline" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10 then 3
        |    else if x > 3 then 2
        |    else 1
        |""".stripMargin) shouldBe 2
  }

  "if then inline return" in {
    eval(
      """main() -> int
        |    if 1 then return 42
        |    0
        |""".stripMargin) shouldBe 42
  }

  "if block, else if inline" in {
    eval(
      """main() -> int
        |    x = 5
        |    if x > 10
        |        3
        |    else if x > 3 then 2
        |    else 1
        |""".stripMargin) shouldBe 2
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

  // ===== Syntactic edge cases: variables =====

  "typed variable declaration" in {
    eval(
      """main() -> int
        |    x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "untyped variable then reassign" in {
    eval(
      """main() -> int
        |    x = 1
        |    x = 2
        |    x = 3
        |    x
        |""".stripMargin) shouldBe 3
  }

  "variable used in its own initialization expression" in {
    eval(
      """main() -> int
        |    x = 10
        |    x = x + 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  // ===== Syntactic edge cases: return =====

  "explicit return in middle of block" in {
    eval(
      """main() -> int
        |    x = 10
        |    if x > 5
        |        return 1
        |    0
        |""".stripMargin) shouldBe 1
  }

  "return from nested while" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 100
        |        if i == 42
        |            return i
        |        i = i + 1
        |    0
        |""".stripMargin) shouldBe 42
  }

  "return void (no value)" in {
    output(
      """f()
        |    print(1)
        |    return
        |    print(2)
        |
        |main() -> int
        |    f()
        |    0
        |""".stripMargin) shouldBe "1"
  }

  // ===== Syntactic edge cases: expressions as statements =====

  "bare function call as statement" in {
    output(
      """main() -> int
        |    print(42)
        |    0
        |""".stripMargin) shouldBe "42"
  }

  "bare expression as last statement" in {
    eval(
      """main() -> int
        |    x = 40
        |    x + 2
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

  // ===== Chained comparison edge cases =====

  "single comparison is not chained" in {
    eval("main() -> int = if 3 < 5 then 1 else 0\n") shouldBe 1
  }

  "two comparisons chained" in {
    eval("main() -> int = if 1 < 2 < 3 then 1 else 0\n") shouldBe 1
  }

  "four comparisons chained" in {
    eval("main() -> int = if 1 < 2 <= 3 < 4 <= 5 then 1 else 0\n") shouldBe 1
  }

  "chained comparison short-circuits on first false" in {
    eval("main() -> int = if 1 < 2 > 3 < 4 then 1 else 0\n") shouldBe 0
  }

  "chained == comparison" in {
    eval("main() -> int = if 5 == 5 == 5 then 1 else 0\n") shouldBe 1
  }

  "chained != comparison" in {
    eval("main() -> int = if 1 != 2 != 3 then 1 else 0\n") shouldBe 1
  }

  // ===== Arrays =====

  "array declaration and indexing" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[0] = 42
        |    a[0]
        |""".stripMargin) shouldBe 42
  }

  "array multiple elements" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 60
  }

  "array zero initialized" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 0
  }

  "array decays to pointer" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[0] = 42
        |    p = a
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "pointer from array with indexing" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[2] = 99
        |    p = a
        |    p[2]
        |""".stripMargin) shouldBe 99
  }

  "pointer arithmetic on array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[3] = 77
        |    p = a + 3
        |    *p
        |""".stripMargin) shouldBe 77
  }

  "pointer subtraction on array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[1] = 88
        |    p = a + 3
        |    q = p - 2
        |    *q
        |""".stripMargin) shouldBe 88
  }

  "array pass to function" in {
    eval(
      """sum(arr: int, n: int) -> int
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

  "array modify through function" in {
    eval(
      """fill(arr: int, n: int, val: int)
        |    i = 0
        |    while i < n
        |        arr[i] = val
        |        i = i + 1
        |
        |main() -> int
        |    a: [3]int
        |    fill(a, 3, 42)
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 126
  }

  "p[0] same as *p for scalar pointer" in {
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    p[0]
        |""".stripMargin) shouldBe 42
  }

  "array in while loop" in {
    eval(
      """main() -> int
        |    a: [10]int
        |    i = 0
        |    while i < 10
        |        a[i] = i * i
        |        i = i + 1
        |    a[5]
        |""".stripMargin) shouldBe 25
  }

  "array with pointer arithmetic in expression" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    *(a + 0) + *(a + 1) + *(a + 2)
        |""".stripMargin) shouldBe 6
  }

  "nested array indexing via pointer" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] = 100
        |    a[1] = 200
        |    a[2] = 300
        |    p = a
        |    p[0] + p[1] + p[2]
        |""".stripMargin) shouldBe 600
  }

  // ===== Pointers =====

  "address-of and dereference basic" in {
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "deref assignment changes original variable" in {
    eval(
      """main() -> int
        |    x = 10
        |    p = &x
        |    *p = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "pointer to different variables" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    p = &x
        |    q = &y
        |    *p + *q
        |""".stripMargin) shouldBe 30
  }

  "pointer reassignment" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    p = &x
        |    p = &y
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer as function argument (pass by pointer)" in {
    eval(
      """set_to_42(p: int)
        |    *p = 42
        |
        |main() -> int
        |    x = 0
        |    set_to_42(&x)
        |    x
        |""".stripMargin) shouldBe 42
  }

  "swap via pointers" in {
    eval(
      """swap(a: int, b: int)
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

  "pointer to global variable" in {
    eval(
      """g = 0
        |
        |set_global(p: int)
        |    *p = 99
        |
        |main() -> int
        |    set_global(&g)
        |    g
        |""".stripMargin) shouldBe 99
  }

  "deref in expression" in {
    eval(
      """main() -> int
        |    x = 21
        |    p = &x
        |    *p * 2
        |""".stripMargin) shouldBe 42
  }

  "deref in if condition" in {
    eval(
      """main() -> int
        |    x = 5
        |    p = &x
        |    if *p > 3 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "deref in function call argument" in {
    eval(
      """double(n: int) -> int = n * 2
        |
        |main() -> int
        |    x = 21
        |    p = &x
        |    double(*p)
        |""".stripMargin) shouldBe 42
  }

  "increment via pointer" in {
    eval(
      """inc(p: int)
        |    *p = *p + 1
        |
        |main() -> int
        |    x = 0
        |    inc(&x)
        |    inc(&x)
        |    inc(&x)
        |    x
        |""".stripMargin) shouldBe 3
  }

  "double pointer" in {
    eval(
      """main() -> int
        |    x = 42
        |    p = &x
        |    pp = &p
        |    **pp
        |""".stripMargin) shouldBe 42
  }

  "double pointer write" in {
    eval(
      """main() -> int
        |    x = 0
        |    p = &x
        |    pp = &p
        |    **pp = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "double pointer redirect" in {
    eval(
      """main() -> int
        |    x = 10
        |    y = 20
        |    p = &x
        |    pp = &p
        |    *pp = &y
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer in while loop" in {
    eval(
      """main() -> int
        |    x = 0
        |    p = &x
        |    i = 0
        |    while i < 5
        |        *p = *p + i
        |        i = i + 1
        |    x
        |""".stripMargin) shouldBe 10
  }

  // ===== Pointer/array combination tests =====

  "address of array element" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[2] = 42
        |    p = &a[2]
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "modify array element via address-of" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    p = &a[3]
        |    *p = 77
        |    a[3]
        |""".stripMargin) shouldBe 77
  }

  "pointer offset then index" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[2] = 10
        |    a[3] = 20
        |    p = a + 2
        |    p[0] + p[1]
        |""".stripMargin) shouldBe 30
  }

  "write through pointer arithmetic" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    *(a + 4) = 55
        |    a[4]
        |""".stripMargin) shouldBe 55
  }

  "array element as function argument" in {
    eval(
      """double(x: int) -> int = x * 2
        |
        |main() -> int
        |    a: [3]int
        |    a[1] = 21
        |    double(a[1])
        |""".stripMargin) shouldBe 42
  }

  "function does pointer arithmetic on array arg" in {
    eval(
      """third(arr: int) -> int = *(arr + 2)
        |
        |main() -> int
        |    a: [5]int
        |    a[2] = 99
        |    third(a)
        |""".stripMargin) shouldBe 99
  }

  "multiple arrays" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    b: [3]int
        |    a[0] = 10
        |    b[0] = 20
        |    a[1] = 30
        |    b[1] = 40
        |    a[0] + a[1] + b[0] + b[1]
        |""".stripMargin) shouldBe 100
  }

  "array index with expression" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    a[3] = 42
        |    i = 1
        |    a[i + 2]
        |""".stripMargin) shouldBe 42
  }

  "deref of address-of array element" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[1] = 88
        |    *(&a[1])
        |""".stripMargin) shouldBe 88
  }

  "array decay and pointer are interchangeable" in {
    eval(
      """main() -> int
        |    a: [3]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    p = a
        |    q = &a[0]
        |    *p + *q + p[1] + q[2]
        |""".stripMargin) shouldBe 7
  }

  "bubble sort" in {
    eval(
      """sort(arr: int, n: int)
        |    i = 0
        |    while i < n - 1
        |        j = 0
        |        while j < n - 1 - i
        |            if arr[j] > arr[j + 1]
        |                tmp = arr[j]
        |                arr[j] = arr[j + 1]
        |                arr[j + 1] = tmp
        |            j = j + 1
        |        i = i + 1
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

  "reverse array in place" in {
    eval(
      """reverse(arr: int, n: int)
        |    i = 0
        |    j = n - 1
        |    while i < j
        |        tmp = arr[i]
        |        arr[i] = arr[j]
        |        arr[j] = tmp
        |        i = i + 1
        |        j = j - 1
        |
        |main() -> int
        |    a: [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    reverse(a, 4)
        |    a[0] * 1000 + a[1] * 100 + a[2] * 10 + a[3]
        |""".stripMargin) shouldBe 4321
  }

  "pointer walks array" in {
    eval(
      """main() -> int
        |    a: [5]int
        |    i = 0
        |    while i < 5
        |        a[i] = (i + 1) * 10
        |        i = i + 1
        |    sum = 0
        |    p = a
        |    i = 0
        |    while i < 5
        |        sum = sum + *p
        |        p = p + 1
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 150
  }

  // ===== Char literals =====

  "char literal basic" in {
    eval("main() -> int = 'A'\n") shouldBe 65
  }

  "char literal in arithmetic" in {
    eval("main() -> int = 'A' + 1\n") shouldBe 66
  }

  "char literal comparison" in {
    eval("main() -> int = if 'A' < 'B' then 1 else 0\n") shouldBe 1
  }

  "char literal escape newline" in {
    eval("main() -> int = '\\n'\n") shouldBe 10
  }

  "char literal escape tab" in {
    eval("main() -> int = '\\t'\n") shouldBe 9
  }

  "char literal escape null" in {
    eval("main() -> int = '\\0'\n") shouldBe 0
  }

  "char literal escape backslash" in {
    eval("main() -> int = '\\\\'\n") shouldBe 92
  }

  "putchar with char literal" in {
    output(
      """main() -> int
        |    putchar('H')
        |    putchar('i')
        |    0
        |""".stripMargin) shouldBe "Hi"
  }

  "char conversion digit to int" in {
    eval("main() -> int = '5' - '0'\n") shouldBe 5
  }

  "char in array" in {
    output(
      """main() -> int
        |    msg: [3]int
        |    msg[0] = 'H'
        |    msg[1] = 'i'
        |    msg[2] = '!'
        |    i = 0
        |    while i < 3 do putchar(msg[i++])
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  // ===== String library functions (in sysl) =====

  "strlen implementation" in {
    eval(
      """strlen(s: int) -> int
        |    n = 0
        |    while s[n] != 0 do n++
        |    n
        |
        |main() -> int
        |    str: [6]int
        |    str[0] = 'H'
        |    str[1] = 'e'
        |    str[2] = 'l'
        |    str[3] = 'l'
        |    str[4] = 'o'
        |    str[5] = 0
        |    strlen(str)
        |""".stripMargin) shouldBe 5
  }

  "puts implementation" in {
    output(
      """puts(s: int)
        |    i = 0
        |    while s[i] != 0
        |        putchar(s[i])
        |        i += 1
        |
        |main() -> int
        |    str: [4]int
        |    str[0] = 'H'
        |    str[1] = 'i'
        |    str[2] = '!'
        |    str[3] = 0
        |    puts(str)
        |    0
        |""".stripMargin) shouldBe "Hi!"
  }

  "strcmp implementation" in {
    eval(
      """strcmp(a: int, b: int) -> int
        |    i = 0
        |    while a[i] != 0 && a[i] == b[i] do i++
        |    a[i] - b[i]
        |
        |main() -> int
        |    s1: [4]int
        |    s2: [4]int
        |    s3: [4]int
        |    s1[0] = 'a'
        |    s1[1] = 'b'
        |    s1[2] = 'c'
        |    s1[3] = 0
        |    s2[0] = 'a'
        |    s2[1] = 'b'
        |    s2[2] = 'c'
        |    s2[3] = 0
        |    s3[0] = 'a'
        |    s3[1] = 'b'
        |    s3[2] = 'd'
        |    s3[3] = 0
        |    eq = strcmp(s1, s2)
        |    lt = strcmp(s1, s3)
        |    if eq == 0 && lt < 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }
}
