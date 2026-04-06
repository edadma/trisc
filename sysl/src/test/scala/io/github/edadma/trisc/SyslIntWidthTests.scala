package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslIntWidthTests extends SyslTestHelpers {

  // ===== i8/i16/i32/i64 type names in declarations =====

  "i8 variable" in {
    eval(
      """main() -> int
        |    var x: i8 = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "i16 variable" in {
    eval(
      """main() -> int
        |    var x: i16 = 1000
        |    x
        |""".stripMargin) shouldBe 1000
  }

  "i32 variable" in {
    eval(
      """main() -> int
        |    var x: i32 = 100000
        |    x
        |""".stripMargin) shouldBe 100000
  }

  "i64 variable" in {
    eval(
      """main() -> int
        |    var x: i64 = 100000
        |    x
        |""".stripMargin) shouldBe 100000
  }

  "int is alias for i32" in {
    eval(
      """main() -> int
        |    var x: int = 42
        |    var y: i32 = x
        |    y
        |""".stripMargin) shouldBe 42
  }

  "byte is alias for u8" in {
    eval(
      """main() -> int
        |    var x: byte = 42
        |    var y: u8 = x
        |    y
        |""".stripMargin) shouldBe 42
  }

  "char is alias for u32" in {
    eval(
      """main() -> int
        |    var x: char = 65
        |    var y: u32 = x
        |    int(y)
        |""".stripMargin) shouldBe 65
  }

  // ===== i16 as function parameter and return =====

  "i16 parameter" in {
    eval(
      """dbl(x: i16) -> i16 = x * 2
        |main() -> int = dbl(21)
        |""".stripMargin) shouldBe 42
  }

  "i32 return type" in {
    eval(
      """get32() -> i32 = 100000
        |main() -> int = get32()
        |""".stripMargin) shouldBe 100000
  }

  // ===== Compound types with iN =====

  "pointer to i32" in {
    eval(
      """main() -> int
        |    var x: i32 = 42
        |    var p: *i32 = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "array of i16" in {
    eval(
      """main() -> int
        |    var arr: [3]i16
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  "pointer to i8" in {
    eval(
      """main() -> int
        |    var x: i8 = 99
        |    var p: *i8 = &x
        |    *p
        |""".stripMargin) shouldBe 99
  }

  // ===== sizeof with all widths =====

  "sizeof(i8)" in {
    eval("main() -> int = sizeof(i8)\n") shouldBe 1
  }

  "sizeof(i16)" in {
    eval("main() -> int = sizeof(i16)\n") shouldBe 2
  }

  "sizeof(i32)" in {
    eval("main() -> int = sizeof(i32)\n") shouldBe 4
  }

  "sizeof(i64)" in {
    eval("main() -> int = sizeof(i64)\n") shouldBe 8
  }

  "sizeof(int) equals sizeof(i32)" in {
    eval("main() -> int = sizeof(int) == sizeof(i32)\n") shouldBe 1
  }

  "sizeof(byte) equals sizeof(i8)" in {
    eval("main() -> int = sizeof(byte) == sizeof(i8)\n") shouldBe 1
  }

  "sizeof(char) equals sizeof(i32)" in {
    eval("main() -> int = sizeof(char) == sizeof(i32)\n") shouldBe 1
  }

  "sizeof(*i32)" in {
    eval("main() -> int = sizeof(*i32)\n") shouldBe 8
  }

  "sizeof([5]i16)" in {
    eval("main() -> int = sizeof([5]i16)\n") shouldBe 10
  }

  "sizeof([10]i8)" in {
    eval("main() -> int = sizeof([10]i8)\n") shouldBe 10
  }

  // ===== sizeof struct with mixed widths =====

  "sizeof struct with mixed field widths" in {
    eval(
      """struct Mixed
        |    a: i8
        |    b: i16
        |    c: i32
        |    d: i64
        |
        |main() -> int = sizeof(Mixed)
        |""".stripMargin) shouldBe 16  // 1 + (1 pad) + 2 + 4 + 8, aligned to 8
  }

  "sizeof struct all i8" in {
    eval(
      """struct Bytes4
        |    a: i8
        |    b: i8
        |    c: i8
        |    d: i8
        |
        |main() -> int = sizeof(Bytes4)
        |""".stripMargin) shouldBe 4
  }

  // ===== Cast syntax with iN types =====

  "i8 cast" in {
    eval("main() -> int = i8(256)\n") shouldBe 0
  }

  "i8 cast preserves low bits" in {
    eval("main() -> int = i8(0xFF)\n") shouldBe -1  // i8 is signed: 0xFF → -1
  }

  "i16 cast" in {
    eval("main() -> int = i16(0x10000)\n") shouldBe 0
  }

  "i16 cast preserves low bits" in {
    eval("main() -> int = i16(0xFFFF)\n") shouldBe -1  // i16 is signed: 0xFFFF → -1
  }

  "i32 cast" in {
    eval("main() -> int = i32(65)\n") shouldBe 65
  }

  "i64 cast is identity" in {
    eval("main() -> int = i64(42)\n") shouldBe 42
  }

  "bool cast from i32" in {
    eval("main() -> int = bool(i32(1))\n") shouldBe 1
  }

  "i8 cast truncation" in {
    eval("main() -> int = i8(300)\n") shouldBe 44  // 300 & 0xFF = 44
  }

  "i16 cast truncation" in {
    eval("main() -> int = i16(70000)\n") shouldBe 4464  // 70000 & 0xFFFF = 4464
  }

  // ===== Type compatibility / cross-width assignment =====

  "assign i8 to i64 variable" in {
    eval(
      """main() -> int
        |    var x: i8 = 42
        |    var y: i64 = x
        |    y
        |""".stripMargin) shouldBe 42
  }

  "assign i16 to i32 variable" in {
    eval(
      """main() -> int
        |    var x: i16 = 1000
        |    var y: i32 = x
        |    y
        |""".stripMargin) shouldBe 1000
  }

  "assign i64 to i8 variable is rejected (narrowing)" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var x: i64 = 42
        |    var y: i8 = x
        |    y
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "assign i64 to i8 with explicit cast works" in {
    eval(
      """main() -> int
        |    var x: i64 = 42
        |    var y: i8 = i8(x)
        |    y
        |""".stripMargin) shouldBe 42
  }

  // ===== Prefix notation round-trip =====

  "i8 prefix round-trip" in {
    SyslType.fromPrefix("i8") shouldBe IntType(8)
  }

  "i16 prefix round-trip" in {
    SyslType.fromPrefix("i16") shouldBe IntType(16)
  }

  "i32 prefix round-trip" in {
    SyslType.fromPrefix("i32") shouldBe IntType(32)
  }

  "i64 prefix round-trip" in {
    SyslType.fromPrefix("i64") shouldBe IntType(64)
  }

  "ptr i32 prefix round-trip" in {
    val t = PtrType(IntType(32))
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "arr 5 i16 prefix round-trip" in {
    val t = ArrayType(IntType(16), 5)
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "legacy prefix names still parse" in {
    SyslType.fromPrefix("int") shouldBe IntType(32)
    SyslType.fromPrefix("byte") shouldBe UIntType(8)
    SyslType.fromPrefix("char") shouldBe UIntType(32)
  }

  // ===== i16 toPrefix =====

  "i16 toPrefix" in {
    IntType(16).toPrefix shouldBe "i16"
  }

  "i32 toPrefix" in {
    IntType(32).toPrefix shouldBe "i32"
  }

  // ===== Arithmetic with different widths =====

  "i16 arithmetic" in {
    eval(
      """main() -> int
        |    var a: i16 = 100
        |    var b: i16 = 200
        |    a + b
        |""".stripMargin) shouldBe 300
  }

  "i32 arithmetic" in {
    eval(
      """main() -> int
        |    var a: i32 = 100000
        |    var b: i32 = 200000
        |    a + b
        |""".stripMargin) shouldBe 300000
  }

  // ===== Parser: iN in various positions =====

  "parse i16 as function param type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: i16) -> i32 = x * 2
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 2
  }

  "parse *i16 type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var p: *i16 = 0
        |    0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 1
  }

  "parse [3]i32 type" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var arr: [3]i32
        |    0
        |""".stripMargin): @unchecked
    ast.decls.length shouldBe 1
  }

  // ===== Narrowing rejection tests =====

  "i32 to i16 narrowing is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var x: i32 = 42
        |    var y: i16 = x
        |    y
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "i16 to i8 narrowing is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var x: i16 = 42
        |    var y: i8 = x
        |    y
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "u64 to u32 narrowing is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var x: u64 = 42
        |    var y: u32 = x
        |    int(y)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "i64 to u32 narrowing across sign is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var x: i64 = 42
        |    var y: u32 = x
        |    int(y)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "i8 to u64 widening across sign is allowed" in {
    eval(
      """main() -> int
        |    var x: i8 = 5
        |    var y: u64 = x
        |    int(y)
        |""".stripMargin) shouldBe 5
  }

  "narrowing in function arg is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: i8) -> int = x
        |main() -> int
        |    var a: i32 = 42
        |    f(a)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
