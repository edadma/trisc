package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslTriscCodegenTests extends AnyFreeSpec with Matchers {

  def compile(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  def compileAndRun(source: String, memSize: Int = 0x1000): Long =
    val asm = compile(source)
    // println(asm) // uncomment to debug
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(tof))
    val mem = new Memory("Memory", new RAM(0, memSize))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    // Set up stack pointer
    cpu.r(7).write(memSize - 8)
    cpu.run()
    cpu.r(1).read

  // ===== Constants =====

  "return constant 0" in {
    compileAndRun("main() -> int = 0\n") shouldBe 0
  }

  "return constant 42" in {
    compileAndRun("main() -> int = 42\n") shouldBe 42
  }

  "return constant 255" in {
    compileAndRun("main() -> int = 255\n") shouldBe 255
  }

  "return large constant" in {
    compileAndRun("main() -> int = 1000\n") shouldBe 1000
  }

  // ===== Arithmetic =====

  "addition" in {
    compileAndRun("main() -> int = 3 + 4\n") shouldBe 7
  }

  "subtraction" in {
    compileAndRun("main() -> int = 10 - 3\n") shouldBe 7
  }

  "multiplication" in {
    compileAndRun("main() -> int = 6 * 7\n") shouldBe 42
  }

  "division" in {
    compileAndRun("main() -> int = 42 / 6\n") shouldBe 7
  }

  "modulo" in {
    compileAndRun("main() -> int = 17 % 5\n") shouldBe 2
  }

  "operator precedence" in {
    compileAndRun("main() -> int = 2 + 3 * 4\n") shouldBe 14
  }

  "parentheses" in {
    compileAndRun("main() -> int = (2 + 3) * 4\n") shouldBe 20
  }

  "unary minus" in {
    compileAndRun("main() -> int = -42\n") shouldBe -42
  }

  "complex expression" in {
    compileAndRun("main() -> int = (10 + 20) * 2 - 5\n") shouldBe 55
  }

  // ===== Variables =====

  "local variable" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "variable assignment" in {
    compileAndRun(
      """main() -> int
        |    x = 1
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "multiple variables" in {
    compileAndRun(
      """main() -> int
        |    a = 10
        |    b = 20
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  "variable in expression" in {
    compileAndRun(
      """main() -> int
        |    x = 21
        |    x * 2
        |""".stripMargin) shouldBe 42
  }

  // ===== Comparison =====

  "equal true" in {
    compileAndRun("main() -> int = 5 == 5\n") shouldBe 1
  }

  "equal false" in {
    compileAndRun("main() -> int = 5 == 3\n") shouldBe 0
  }

  "not equal true" in {
    compileAndRun("main() -> int = 5 != 3\n") shouldBe 1
  }

  "less than true" in {
    compileAndRun("main() -> int = 3 < 5\n") shouldBe 1
  }

  "less than false" in {
    compileAndRun("main() -> int = 5 < 3\n") shouldBe 0
  }

  "greater than true" in {
    compileAndRun("main() -> int = 5 > 3\n") shouldBe 1
  }

  // ===== If expression =====

  "if true branch" in {
    compileAndRun(
      """main() -> int
        |    if true
        |        return 42
        |    0
        |""".stripMargin) shouldBe 42
  }

  "if false branch" in {
    compileAndRun(
      """main() -> int
        |    if false
        |        return 42
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Function calls =====

  "function call" in {
    compileAndRun(
      """double(x: int) -> int = x * 2
        |
        |main() -> int = double(21)
        |""".stripMargin) shouldBe 42
  }

  // ===== While loop =====

  "while loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    sum = 0
        |    while i < 5
        |        sum = sum + i
        |        i = i + 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  // ===== Comparisons (full set) =====

  "less than or equal true" in {
    compileAndRun("main() -> int = 3 <= 5\n") shouldBe 1
  }

  "less than or equal equal" in {
    compileAndRun("main() -> int = 5 <= 5\n") shouldBe 1
  }

  "less than or equal false" in {
    compileAndRun("main() -> int = 7 <= 5\n") shouldBe 0
  }

  "greater than or equal true" in {
    compileAndRun("main() -> int = 5 >= 3\n") shouldBe 1
  }

  "greater than or equal equal" in {
    compileAndRun("main() -> int = 5 >= 5\n") shouldBe 1
  }

  "greater than or equal false" in {
    compileAndRun("main() -> int = 3 >= 5\n") shouldBe 0
  }

  // ===== Logical operators =====

  "and true true" in {
    compileAndRun("main() -> int = true && true\n") shouldBe 1
  }

  "and true false" in {
    compileAndRun("main() -> int = true && false\n") shouldBe 0
  }

  "and false short-circuits" in {
    compileAndRun("main() -> int = false && true\n") shouldBe 0
  }

  "or false false" in {
    compileAndRun("main() -> int = false || false\n") shouldBe 0
  }

  "or true short-circuits" in {
    compileAndRun("main() -> int = true || false\n") shouldBe 1
  }

  "or false true" in {
    compileAndRun("main() -> int = false || true\n") shouldBe 1
  }

  // ===== Compound assignment =====

  "plus equals" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    x += 5
        |    x
        |""".stripMargin) shouldBe 15
  }

  "minus equals" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    x -= 3
        |    x
        |""".stripMargin) shouldBe 7
  }

  "times equals" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x *= 4
        |    x
        |""".stripMargin) shouldBe 20
  }

  // ===== Pre/post increment/decrement =====

  "pre-increment" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    ++x
        |""".stripMargin) shouldBe 6
  }

  "post-increment returns old value" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x++
        |""".stripMargin) shouldBe 5
  }

  "post-increment modifies variable" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    x++
        |    x
        |""".stripMargin) shouldBe 6
  }

  "pre-decrement" in {
    compileAndRun(
      """main() -> int
        |    x = 5
        |    --x
        |""".stripMargin) shouldBe 4
  }

  // ===== For loop =====

  "for loop sum" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "for loop factorial" in {
    compileAndRun(
      """main() -> int
        |    result = 1
        |    for i = 1; i <= 5; i++
        |        result *= i
        |    result
        |""".stripMargin) shouldBe 120
  }

  // ===== Do/while =====

  "do/while loop" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    do
        |        i += 1
        |    while i < 5
        |    i
        |""".stripMargin) shouldBe 5
  }

  "do/while executes at least once" in {
    compileAndRun(
      """main() -> int
        |    x = 0
        |    do
        |        x = 42
        |    while false
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ===== Break/continue =====

  "break in while" in {
    compileAndRun(
      """main() -> int
        |    i = 0
        |    while true
        |        if i == 5 then break
        |        i += 1
        |    i
        |""".stripMargin) shouldBe 5
  }

  "break in for" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 100; i++
        |        if i == 5 then break
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "continue in for" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for i = 0; i < 10; i++
        |        if i % 2 == 0 then continue
        |        sum += i
        |    sum
        |""".stripMargin) shouldBe 25
  }

  // ===== Casts =====

  "bool cast from nonzero" in {
    compileAndRun("main() -> int = bool(42)\n") shouldBe 1
  }

  "bool cast from zero" in {
    compileAndRun("main() -> int = bool(0)\n") shouldBe 0
  }

  "byte cast truncates" in {
    compileAndRun("main() -> int = byte(256)\n") shouldBe 0
  }

  "byte cast preserves low bits" in {
    compileAndRun("main() -> int = byte(0xff)\n") shouldBe 255
  }

  // ===== Arrays =====

  "array declaration and indexing" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[1]
        |""".stripMargin) shouldBe 20
  }

  "array sum" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = i * 10
        |    arr[0] + arr[1] + arr[2] + arr[3] + arr[4]
        |""".stripMargin) shouldBe 100
  }

  "array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = i + 1
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 15
  }

  // ===== Pointers =====

  "address-of and dereference" in {
    compileAndRun(
      """main() -> int
        |    x = 42
        |    p = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "write through pointer" in {
    compileAndRun(
      """main() -> int
        |    x = 10
        |    p = &x
        |    *p = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "pointer to array element" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[1]
        |    *p
        |""".stripMargin) shouldBe 200
  }

  // ===== Char cast =====

  "char cast truncates to 32 bits" in {
    compileAndRun("main() -> int = char(65)\n") shouldBe 65
  }

  // ===== Multi-arg function calls =====

  "function with two args" in {
    compileAndRun(
      """myAdd(a: int, b: int) -> int = a + b
        |main() -> int = myAdd(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "function with three args" in {
    compileAndRun(
      """sum3(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = sum3(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  "function with four args" in {
    compileAndRun(
      """sum4(a: int, b: int, c: int, d: int) -> int = a + b + c + d
        |main() -> int = sum4(10, 11, 12, 9)
        |""".stripMargin) shouldBe 42
  }

  // ===== Global variables =====

  "global variable with initializer" in {
    compileAndRun(
      """x = 42
        |main() -> int = x
        |""".stripMargin, memSize = 0x2000) shouldBe 42
  }

  "global variable bool initializer" in {
    compileAndRun(
      """flag = true
        |main() -> int = flag
        |""".stripMargin, memSize = 0x2000) shouldBe 1
  }
}
