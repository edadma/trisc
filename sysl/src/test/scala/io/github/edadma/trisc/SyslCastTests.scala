package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslCastTests extends SyslTestHelpers {

  // ===== int() cast =====

  "int from char" in {
    eval("main() -> int = int('A')\n") shouldBe 65
  }

  "int from bool true" in {
    eval("main() -> int = int(true)\n") shouldBe 1
  }

  "int from bool false" in {
    eval("main() -> int = int(false)\n") shouldBe 0
  }

  "int from comparison" in {
    eval("main() -> int = int(3 < 5)\n") shouldBe 1
  }

  "int from byte" in {
    eval("main() -> int = int(byte(200))\n") shouldBe 200  // byte is unsigned u8
  }

  // ===== char() cast =====

  "char from int" in {
    eval("main() -> int = char(65)\n") shouldBe 65
  }

  "char truncates to 32 bits" in {
    eval("main() -> int = char(0x100000041)\n") shouldBe 0x41
  }

  // ===== byte() cast =====

  "byte from int" in {
    eval("main() -> int = byte(0x1FF)\n") shouldBe 255  // byte is unsigned u8: 0xFF → 255
  }

  "byte truncates to 8 bits" in {
    eval("main() -> int = byte(256)\n") shouldBe 0
  }

  // ===== bool() cast =====

  "bool from zero is false" in {
    eval("main() -> int = if bool(0) then 1 else 0\n") shouldBe 0
  }

  "bool from nonzero is true" in {
    eval("main() -> int = if bool(42) then 1 else 0\n") shouldBe 1
  }

  "bool from negative is true" in {
    eval("main() -> int = if bool(-1) then 1 else 0\n") shouldBe 1
  }

  // ===== Cast in expressions =====

  "cast in arithmetic" in {
    eval("main() -> int = int(true) + int(true)\n") shouldBe 2
  }

  "cast in condition" in {
    eval(
      """main() -> int
        |    x = 42
        |    if bool(x) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "cast in function argument" in {
    eval(
      """dbl(x: int) -> int = x * 2
        |main() -> int = dbl(int('!'))
        |""".stripMargin) shouldBe 66
  }

  "chained casts" in {
    eval("main() -> int = int(bool(42))\n") shouldBe 1
  }

  // ===== Cast type checking =====

  "analyzer accepts int to bool cast" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = if bool(42) then 1 else 0\n"): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts bool to int cast" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = int(true)\n"): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "pointer to bool cast is null check" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    p = &x
        |    if bool(p) then 1 else 0
        |""".stripMargin): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val interp = new SyslInterpreter()
    interp.run(typed) shouldBe 1
  }

  "analyzer infers correct type from cast" in {
    val Right(ast) = (new SyslParser).parseProgram("main() -> int = int(true)\n"): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    val main = typed.decls.collectFirst { case f: TFunDecl if f.name == "main" => f }.get
    main.body match
      case TExprBody(TCast(_, SyslType.IntType(32))) => // correct
      case other => fail(s"expected TCast to I32, got $other")
  }

  // ===== Practical usage =====

  "convert bool flag to int for output" in {
    output(
      """main() -> int
        |    flag = 3 > 2
        |    print(int(flag))
        |    0
        |""".stripMargin) shouldBe "1"
  }

  "use bool cast for C-style truthiness" in {
    eval(
      """main() -> int
        |    values: [3]int
        |    values[0] = 0
        |    values[1] = 42
        |    values[2] = 0
        |    count = 0
        |    i = 0
        |    while i < 3
        |        count = count + int(bool(values[i]))
        |        i += 1
        |    count
        |""".stripMargin) shouldBe 1
  }

  // ===== array decay in casts =====

  "i64 cast of array decays to address" in {
    // i64(arr) should get the address of the first element, not try to cast the array value
    eval("""
        |main() -> int
        |    var arr: [4]i64
        |    arr[0] = 42
        |    var p: *i64 = *i64(i64(arr))
        |    int(p[0])
        |""".stripMargin) shouldBe 42
  }

  "*byte cast of array decays to pointer" in {
    eval("""
        |main() -> int
        |    var arr: [4]byte
        |    arr[0] = 65
        |    arr[1] = 66
        |    var p: *byte = *byte(arr)
        |    int(p[0]) + int(p[1])
        |""".stripMargin) shouldBe (65 + 66)
  }

  "*i64 cast of array decays to pointer" in {
    eval("""
        |main() -> int
        |    var arr: [4]i64
        |    arr[0] = 100
        |    arr[1] = 200
        |    var p: *i64 = *i64(arr)
        |    int(p[0] + p[1])
        |""".stripMargin) shouldBe 300
  }

  "string from array" in {
    output("""
        |main()
        |    var buf: [5]byte
        |    buf[0] = 72
        |    buf[1] = 101
        |    buf[2] = 108
        |    buf[3] = 108
        |    buf[4] = 111
        |    val s = string(buf, 5)
        |    puts(s)
        |""".stripMargin) shouldBe "Hello"
  }
}
