package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslTriscCodegenTests extends AnyFreeSpec with Matchers {

  def compile(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  def compileAndRun(source: String, memSize: Int = 0): Long =
    val asm = compile(source)
    // println(asm) // uncomment to debug
    val tof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof, tof, Runtime.ioTof))
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read

  /** Compile multiple Sysl source files together and run via CPU. */
  def compileMultiAndRun(sources: Map[String, String]): Long =
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof) ++ tofs ++ Seq(Runtime.ioTof))
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.reset()
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

  "if-then-else expression" in {
    compileAndRun(
      """main() -> int
        |    x = -42
        |    if x < 0 then -x else x
        |""".stripMargin) shouldBe 42
  }

  "abs function" in {
    compileAndRun(
      """abs_val(x: int) -> int
        |    if x < 0 then -x else x
        |
        |main() -> int = abs_val(-42)
        |""".stripMargin) shouldBe 42
  }

  "multifile abs function" in {
    val sources = Map(
      "math" ->
        """abs_val(x: int) -> int
          |    if x < 0 then -x else x
          |""".stripMargin,
      "main" ->
        """import "math"
          |
          |main() -> int = abs_val(-42)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    val linked = Linker.link(Runtime.bootTof +: tofs :+ Runtime.ioTof)
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  "multifile abs function two-stage link" in {
    val sources = Map(
      "math" ->
        """abs_val(x: int) -> int
          |    if x < 0 then -x else x
          |""".stripMargin,
      "main" ->
        """import "math"
          |
          |main() -> int = abs_val(-42)
          |""".stripMargin,
    )
    val driver = new SyslDriver
    val result = driver.compile(sources)
    val codegen = new SyslTriscCodegen
    val tofs = for unit <- result.units yield
      val asm = codegen.generate(unit.typed)
      assemble(asm, relocatable = true)
    // Stage 1: link user modules into single relocatable TOF
    val partial = Linker.link(tofs, relocatable = true)
    // Stage 2: link with runtime (what trisc run does)
    val linked = Linker.link(Seq(Runtime.bootTof, partial, Runtime.ioTof))
    val stdout = new Stdout(Runtime.stdoutAddress)
    val ram = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem = new Memory("Memory", ram, stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 100000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  // ===== Function calls =====

  "function call" in {
    compileAndRun(
      """dbl(x: int) -> int = x * 2
        |
        |main() -> int = dbl(21)
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
      """dbl(x: int) -> int = x * 2
        |main() -> int = dbl(21)
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
        |dbl(x: int) -> int = x * 2
        |main() -> int
        |    var x: int = 10
        |    x = dbl(inc(x))
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

  "sieve of eratosthenes" in {
    compileAndRun(
      """main() -> int
        |    arr: [101]byte
        |    for i = 0; i <= 100; i++
        |        arr[i] = 0
        |    arr[0] = 1
        |    arr[1] = 1
        |    for i = 2; i * i <= 100; i++
        |        if arr[i] == 0 then
        |            for j = i * i; j <= 100; j += i
        |                arr[j] = 1
        |    count = 0
        |    for i = 2; i <= 100; i++
        |        if arr[i] == 0 then count += 1
        |    count
        |""".stripMargin) shouldBe 25
  }

  // ===== Bug fix: global arrays must reserve full size =====

  "global array reserves correct space" in {
    compileAndRun(
      """var arr: [4]int
        |
        |main() -> int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[3] = 40
        |    arr[0] + arr[1] + arr[2] + arr[3]
        |""".stripMargin) shouldBe 100
  }

  "global array does not overlap next global" in {
    compileAndRun(
      """var arr: [4]int
        |var sentinel = 99
        |
        |main() -> int
        |    arr[0] = 1
        |    arr[1] = 2
        |    arr[2] = 3
        |    arr[3] = 4
        |    sentinel
        |""".stripMargin) shouldBe 99
  }

  // ===== Bug fix: global array/struct returns address, not value =====

  "global array address used in addr-of-index" in {
    compileAndRun(
      """var arr: [4]int
        |
        |main() -> int
        |    arr[2] = 42
        |    var p: *int = &arr[2]
        |    *p
        |""".stripMargin) shouldBe 42
  }

  // ===== Bug fix: struct field access and assignment =====

  "struct field write and read through pointer" in {
    compileAndRun(
      """struct Point
        |    x: int
        |    y: int
        |
        |var p: Point
        |
        |main() -> int
        |    var pp: *Point = &p
        |    pp.x = 10
        |    pp.y = 32
        |    pp.x + pp.y
        |""".stripMargin) shouldBe 42
  }

  "struct field access second field" in {
    compileAndRun(
      """struct Pair
        |    first: int
        |    second: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.first = 100
        |    pp.second = 200
        |    pp.second
        |""".stripMargin) shouldBe 200
  }

  "struct array with field access" in {
    compileAndRun(
      """struct Entry
        |    key: int
        |    value: int
        |
        |var entries: [3]Entry
        |
        |main() -> int
        |    var e: *Entry = &entries[0]
        |    e.key = 1
        |    e.value = 10
        |    e = &entries[1]
        |    e.key = 2
        |    e.value = 20
        |    e = &entries[2]
        |    e.key = 3
        |    e.value = 30
        |    var sum = 0
        |    e = &entries[0]
        |    sum = sum + e.value
        |    e = &entries[1]
        |    sum = sum + e.value
        |    e = &entries[2]
        |    sum = sum + e.value
        |    sum
        |""".stripMargin) shouldBe 60
  }

  // ===== Bug fix: linker segment alignment =====

  "multi-module globals remain aligned after linking" in {
    compileMultiAndRun(Map(
      "lib" ->
        """helper() -> int = 42
          |""".stripMargin,
      "app" ->
        """import "lib"
          |
          |val MAGIC = 12345
          |
          |main() -> int
          |    val v = MAGIC
          |    helper() + v
          |""".stripMargin
    )) shouldBe 12387
  }

  // ===== Bug fix: struct field compound assignment =====

  "struct field compound assign +=" in {
    compileAndRun(
      """struct Counter
        |    value: int
        |
        |var c: Counter
        |
        |main() -> int
        |    var p: *Counter = &c
        |    p.value = 10
        |    p.value += 32
        |    p.value
        |""".stripMargin) shouldBe 42
  }

  "struct field compound assign -=" in {
    compileAndRun(
      """struct Counter
        |    value: int
        |
        |var c: Counter
        |
        |main() -> int
        |    var p: *Counter = &c
        |    p.value = 50
        |    p.value -= 8
        |    p.value
        |""".stripMargin) shouldBe 42
  }

  "struct field compound assign *= on first field" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.a = 7
        |    pp.a *= 6
        |    pp.a
        |""".stripMargin) shouldBe 42
  }

  "struct field compound assign *= on second field" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.a = 5
        |    pp.b = 7
        |    pp.b *= 6
        |    pp.b
        |""".stripMargin) shouldBe 42
  }

  // ===== Bug fix: addr-of global variable =====

  "addr-of global scalar" in {
    compileAndRun(
      """var x = 42
        |
        |main() -> int
        |    var p: *int = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "addr-of global and write through pointer" in {
    compileAndRun(
      """var x = 0
        |
        |main() -> int
        |    var p: *int = &x
        |    *p = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  // ===== Global variable operations =====

  "global compound assign +=" in {
    compileAndRun(
      """var counter = 10
        |
        |main() -> int
        |    counter += 32
        |    counter
        |""".stripMargin) shouldBe 42
  }

  "global compound assign += multiple times" in {
    compileAndRun(
      """var counter = 0
        |
        |inc()
        |    counter += 1
        |
        |main() -> int
        |    inc()
        |    inc()
        |    inc()
        |    counter
        |""".stripMargin) shouldBe 3
  }

  "global assign from local" in {
    compileAndRun(
      """var result = 0
        |
        |main() -> int
        |    var x = 42
        |    result = x
        |    result
        |""".stripMargin) shouldBe 42
  }

  "global assign from expression" in {
    compileAndRun(
      """var current = 0
        |var count = 5
        |var result = 0
        |
        |main() -> int
        |    var next = current + 1
        |    if next >= count
        |        next = 0
        |    result = next
        |    result
        |""".stripMargin) shouldBe 1
  }

  "global read in arithmetic" in {
    compileAndRun(
      """var base = 40
        |
        |main() -> int
        |    var x = base + 2
        |    x
        |""".stripMargin) shouldBe 42
  }

  "compare local to global" in {
    compileAndRun(
      """var limit = 5
        |
        |main() -> int
        |    var i = 0
        |    var sum = 0
        |    while i < limit
        |        sum += 1
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 5
  }

  "compare local >= global with wrap" in {
    compileAndRun(
      """var thread_count = 3
        |
        |main() -> int
        |    var next = 2
        |    next += 1
        |    if next >= thread_count
        |        next = 0
        |    next
        |""".stripMargin) shouldBe 0
  }

  // ===== Struct pointer sequences =====

  "multiple field writes then read all" in {
    compileAndRun(
      """struct TCB
        |    ssp: int
        |    state: int
        |    priority: int
        |
        |var tcb: TCB
        |
        |main() -> int
        |    var p: *TCB = &tcb
        |    p.ssp = 100
        |    p.state = 200
        |    p.priority = 300
        |    p.ssp + p.state + p.priority
        |""".stripMargin) shouldBe 600
  }

  "write field then overwrite same field" in {
    compileAndRun(
      """struct Pair
        |    a: int
        |    b: int
        |
        |var p: Pair
        |
        |main() -> int
        |    var pp: *Pair = &p
        |    pp.a = 99
        |    pp.a = 42
        |    pp.a
        |""".stripMargin) shouldBe 42
  }

  "function returns struct field" in {
    compileAndRun(
      """struct Entry
        |    value: int
        |
        |var e: Entry
        |
        |get_value() -> int
        |    var p: *Entry = &e
        |    p.value
        |
        |main() -> int
        |    var p: *Entry = &e
        |    p.value = 42
        |    get_value()
        |""".stripMargin) shouldBe 42
  }

  // ===== Array index + struct field (MUL interaction) =====

  "index struct array then read field" in {
    compileAndRun(
      """struct Item
        |    x: int
        |    y: int
        |
        |var items: [4]Item
        |
        |main() -> int
        |    var p: *Item = &items[0]
        |    p.x = 10
        |    p.y = 20
        |    p = &items[1]
        |    p.x = 30
        |    p.y = 40
        |    p = &items[2]
        |    p.x = 50
        |    p.y = 60
        |    var q: *Item = &items[1]
        |    q.x + q.y
        |""".stripMargin) shouldBe 70
  }

  "index struct array with variable index" in {
    compileAndRun(
      """struct Slot
        |    data: int
        |
        |var slots: [4]Slot
        |
        |main() -> int
        |    var i = 0
        |    while i < 4
        |        var p: *Slot = &slots[i]
        |        p.data = (i + 1) * 10
        |        i += 1
        |    var p: *Slot = &slots[2]
        |    p.data
        |""".stripMargin) shouldBe 30
  }

  "index struct array with global index" in {
    compileAndRun(
      """struct Slot
        |    data: int
        |
        |var slots: [4]Slot
        |var idx = 0
        |
        |main() -> int
        |    var p: *Slot = &slots[0]
        |    p.data = 10
        |    p = &slots[1]
        |    p.data = 20
        |    p = &slots[2]
        |    p.data = 30
        |    idx = 2
        |    p = &slots[idx]
        |    p.data
        |""".stripMargin) shouldBe 30
  }

  // ===== Full scheduler pattern =====

  "scheduler pattern: index, write fields, pick next, return field" in {
    compileAndRun(
      """struct Task
        |    ssp: int
        |    state: int
        |
        |var tasks: [3]Task
        |var current = 0
        |var count = 3
        |var ticks = 0
        |
        |schedule(cur_ssp: int) -> int
        |    ticks += 1
        |    var cur: *Task = &tasks[current]
        |    cur.ssp = cur_ssp
        |    cur.state = 0
        |    var next = current + 1
        |    if next >= count
        |        next = 0
        |    current = next
        |    var nxt: *Task = &tasks[next]
        |    nxt.state = 1
        |    nxt.ssp
        |
        |main() -> int
        |    // Initialize: thread 0 ssp=100, thread 1 ssp=200, thread 2 ssp=300
        |    var p: *Task = &tasks[0]
        |    p.ssp = 100
        |    p = &tasks[1]
        |    p.ssp = 200
        |    p = &tasks[2]
        |    p.ssp = 300
        |
        |    // Simulate: current=0, call schedule(111)
        |    // Should save 111 to tasks[0].ssp, pick next=1, return tasks[1].ssp=200
        |    var r1 = schedule(111)
        |
        |    // Now current=1, call schedule(222)
        |    // Should save 222 to tasks[1].ssp, pick next=2, return tasks[2].ssp=300
        |    var r2 = schedule(222)
        |
        |    // Now current=2, call schedule(333)
        |    // Should save 333 to tasks[2].ssp, pick next=0 (wrap), return tasks[0].ssp=111
        |    var r3 = schedule(333)
        |
        |    // Verify: r1=200, r2=300, r3=111, ticks=3
        |    if r1 != 200 then return 1
        |    if r2 != 300 then return 2
        |    if r3 != 111 then return 3
        |    if ticks != 3 then return 4
        |    0
        |""".stripMargin) shouldBe 0
  }
}
