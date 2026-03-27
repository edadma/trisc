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
    eval("main() -> int = int(byte(200))\n") shouldBe 200
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
    eval("main() -> int = byte(0x1FF)\n") shouldBe 0xFF
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
      """double(x: int) -> int = x * 2
        |main() -> int = double(int('!'))
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

  "analyzer rejects cast from pointer to bool" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    p = &x
        |    if bool(p) then 1 else 0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
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
}
