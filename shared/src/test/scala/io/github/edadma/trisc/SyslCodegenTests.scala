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

  // ===== Function pointers =====

  "function pointer call" in {
    compileAndRun(
      """myDouble(x: int) -> int = x * 2
        |main() -> int
        |    f = myDouble
        |    f(21)
        |""".stripMargin) shouldBe 42
  }

  "pass function pointer as argument" in {
    compileAndRun(
      """myDouble(x: int) -> int = x * 2
        |apply(f: func(int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(myDouble, 21)
        |""".stripMargin) shouldBe 42
  }

  "pointer increment" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "function pointer reassignment" in {
    compileAndRun(
      """myDouble(x: int) -> int = x * 2
        |myTriple(x: int) -> int = x * 3
        |main() -> int
        |    f = myDouble
        |    a = f(10)
        |    f = myTriple
        |    b = f(10)
        |    a + b
        |""".stripMargin) shouldBe 50
  }

  // ===== Width-Aware Stack Layout Tests =====

  // --- Locals at each width ---

  "i8 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "i16 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 1000
        |    x
        |""".stripMargin) shouldBe 1000
  }

  "i32 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 50000
        |    x
        |""".stripMargin) shouldBe 50000
  }

  "i64 local store and load" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 50000
        |    x
        |""".stripMargin) shouldBe 50000
  }

  // --- Multiple locals of different widths ---

  "mixed width locals" in {
    compileAndRun(
      """main() -> int
        |    var a: byte = 10
        |    var b: i16 = 20
        |    var c: int = 30
        |    var d: i64 = 40
        |    a + b + c + d
        |""".stripMargin) shouldBe 100
  }

  "mixed width locals reverse order" in {
    compileAndRun(
      """main() -> int
        |    var d: i64 = 40
        |    var c: int = 30
        |    var b: i16 = 20
        |    var a: byte = 10
        |    a + b + c + d
        |""".stripMargin) shouldBe 100
  }

  "mixed width locals with reassignment" in {
    compileAndRun(
      """main() -> int
        |    var a: byte = 1
        |    var b: int = 2
        |    a = 10
        |    b = 20
        |    a + b
        |""".stripMargin) shouldBe 30
  }

  // --- Arrays at each element width ---

  "i8 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]byte
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  "i16 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i16
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 600
  }

  "i32 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 1000
        |    arr[1] = 2000
        |    arr[2] = 3000
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 6000
  }

  "i64 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 10000
        |    arr[1] = 20000
        |    arr[2] = 30000
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60000
  }

  "i32 array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = (i + 1) * 10
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 150
  }

  // --- Pointer dereference at each width ---

  "pointer deref i8" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 42
        |    var p: *byte = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "pointer deref i16" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 1234
        |    var p: *i16 = &x
        |    *p
        |""".stripMargin) shouldBe 1234
  }

  "pointer deref i32" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 56789
        |    var p: *int = &x
        |    *p
        |""".stripMargin) shouldBe 56789
  }

  "write through i32 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 0
        |    var p: *int = &x
        |    *p = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  "write through i8 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 0
        |    var p: *byte = &x
        |    *p = 77
        |    x
        |""".stripMargin) shouldBe 77
  }

  // --- Pointer arithmetic at each width ---

  "pointer increment i32 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer increment i8 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]byte
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 20
  }

  "pointer increment i64 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 200
  }

  "pointer arithmetic add i32" in {
    compileAndRun(
      """main() -> int
        |    arr: [4]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    p = &arr[0]
        |    val q: *int = p + 2
        |    *q
        |""".stripMargin) shouldBe 30
  }

  "pointer decrement i32 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[2]
        |    p--
        |    *p
        |""".stripMargin) shouldBe 20
  }

  // --- Global variables at each width ---

  "i32 global variable" in {
    compileAndRun(
      """var g: int = 42
        |main() -> int = g
        |""".stripMargin) shouldBe 42
  }

  "i32 global compound assign" in {
    compileAndRun(
      """var g: int = 10
        |main() -> int
        |    g += 5
        |    g
        |""".stripMargin) shouldBe 15
  }

  "bool global variable" in {
    compileAndRun(
      """var g: bool = true
        |main() -> int
        |    if g then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // --- Function args with different widths ---

  "i32 function arg preserved" in {
    compileAndRun(
      """double(x: int) -> int = x * 2
        |main() -> int = double(21)
        |""".stripMargin) shouldBe 42
  }

  "two i32 args" in {
    compileAndRun(
      """myAdd(a: int, b: int) -> int = a + b
        |main() -> int = myAdd(20, 22)
        |""".stripMargin) shouldBe 42
  }

  "three i32 args" in {
    compileAndRun(
      """mySum(a: int, b: int, c: int) -> int = a + b + c
        |main() -> int = mySum(10, 20, 12)
        |""".stripMargin) shouldBe 42
  }

  "four i32 args" in {
    compileAndRun(
      """mySum4(a: int, b: int, c: int, d: int) -> int = a + b + c + d
        |main() -> int = mySum4(10, 11, 12, 9)
        |""".stripMargin) shouldBe 42
  }

  // --- addr-of with typed index ---

  "addr-of i32 array element" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[2]
        |    *p
        |""".stripMargin) shouldBe 300
  }

  // --- Compound assign on narrow locals ---

  "compound assign i8 local" in {
    compileAndRun(
      """main() -> int
        |    var x: byte = 5
        |    x += 3
        |    x
        |""".stripMargin) shouldBe 8
  }

  "compound assign i16 local" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 100
        |    x += 50
        |    x
        |""".stripMargin) shouldBe 150
  }

  "compound assign i32 local" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 100
        |    x *= 3
        |    x
        |""".stripMargin) shouldBe 300
  }

  "multiple compound assigns mixed widths" in {
    compileAndRun(
      """main() -> int
        |    var a: byte = 10
        |    var b: int = 20
        |    a += 5
        |    b += 10
        |    a + b
        |""".stripMargin) shouldBe 45
  }

  // --- Narrow globals ---

  "i8 global read" in {
    compileAndRun(
      """var g: byte = 42
        |main() -> int = g
        |""".stripMargin) shouldBe 42
  }

  "i16 global read" in {
    compileAndRun(
      """var g: i16 = 1000
        |main() -> int = g
        |""".stripMargin) shouldBe 1000
  }

  "i64 global read" in {
    compileAndRun(
      """var g: i64 = 50000
        |main() -> int = g
        |""".stripMargin) shouldBe 50000
  }

  "i8 global compound assign" in {
    compileAndRun(
      """var g: byte = 10
        |main() -> int
        |    g += 5
        |    g
        |""".stripMargin) shouldBe 15
  }

  "i16 global compound assign" in {
    compileAndRun(
      """var g: i16 = 100
        |main() -> int
        |    g += 50
        |    g
        |""".stripMargin) shouldBe 150
  }

  // --- Pointer deref write at each width ---

  "write through i16 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: i16 = 0
        |    var p: *i16 = &x
        |    *p = 999
        |    x
        |""".stripMargin) shouldBe 999
  }

  "write through i64 pointer" in {
    compileAndRun(
      """main() -> int
        |    var x: i64 = 0
        |    var p: *i64 = &x
        |    *p = 12345
        |    x
        |""".stripMargin) shouldBe 12345
  }

  // --- Narrow loop counter ---

  "i8 loop counter" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for var i: byte = 0; i < 10; i++
        |        sum += 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  // --- Narrow array with loop ---

  "i8 array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]byte
        |    for i = 0; i < 5; i++
        |        arr[i] = i + 1
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "i16 array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]i16
        |    for i = 0; i < 5; i++
        |        arr[i] = (i + 1) * 100
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 1500
  }

  // --- Pointer arithmetic i16 ---

  "pointer increment i16 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i16
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[0]
        |    p++
        |    *p
        |""".stripMargin) shouldBe 200
  }

  // --- Call chain with narrow return ---

  "function stores result in narrow local" in {
    compileAndRun(
      """triple(x: int) -> int = x * 3
        |main() -> int
        |    var r: int = triple(14)
        |    r
        |""".stripMargin) shouldBe 42
  }

  "chained function calls" in {
    compileAndRun(
      """inc(x: int) -> int = x + 1
        |double(x: int) -> int = x * 2
        |main() -> int
        |    var x: int = 10
        |    x = double(inc(x))
        |    x
        |""".stripMargin) shouldBe 22
  }

  // --- Pre-inc/dec on narrow locals ---

  "pre-increment i32 local" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 41
        |    ++x
        |""".stripMargin) shouldBe 42
  }

  "pre-decrement i32 local" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 43
        |    --x
        |""".stripMargin) shouldBe 42
  }

  "post-increment i32 returns old value" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 42
        |    x++
        |""".stripMargin) shouldBe 42
  }

  "post-increment i32 modifies variable" in {
    compileAndRun(
      """main() -> int
        |    var x: int = 41
        |    x++
        |    x
        |""".stripMargin) shouldBe 42
  }
}
