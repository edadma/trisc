package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslUnsignedTests extends SyslTestHelpers {

  // ===== Type system =====

  "u8 type exists" in {
    U8 shouldBe UIntType(8)
  }

  "u16 type exists" in {
    U16 shouldBe UIntType(16)
  }

  "u32 type exists" in {
    U32 shouldBe UIntType(32)
  }

  "u64 type exists" in {
    U64 shouldBe UIntType(64)
  }

  "char is u32" in {
    Char shouldBe U32
  }

  "unsigned sizeOf" in {
    U8.sizeOf shouldBe 1
    U16.sizeOf shouldBe 2
    U32.sizeOf shouldBe 4
    U64.sizeOf shouldBe 8
  }

  "unsigned isIntegral" in {
    U32.isIntegral shouldBe true
    U32.isNumeric shouldBe true
    U32.isUnsigned shouldBe true
    U32.isSigned shouldBe false
  }

  "signed isSigned" in {
    I32.isSigned shouldBe true
    I32.isUnsigned shouldBe false
  }

  "unsigned toString" in {
    U8.toString shouldBe "u8"
    U16.toString shouldBe "u16"
    U32.toString shouldBe "u32"
    U64.toString shouldBe "u64"
  }

  "unsigned toPrefix round-trip" in {
    SyslType.fromPrefix(U8.toPrefix) shouldBe U8
    SyslType.fromPrefix(U16.toPrefix) shouldBe U16
    SyslType.fromPrefix(U32.toPrefix) shouldBe U32
    SyslType.fromPrefix(U64.toPrefix) shouldBe U64
  }

  "ptr to unsigned round-trip" in {
    val t = PtrType(U32)
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "array of unsigned round-trip" in {
    val t = ArrayType(U16, 10)
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  // ===== Variable declaration and literal coercion =====

  "u32 variable with literal" in {
    eval(
      """main() -> int
        |    var x: u32 = 42
        |    int(x)
        |""".stripMargin) shouldBe 42
  }

  "u8 variable with literal" in {
    eval(
      """main() -> int
        |    var x: u8 = 200
        |    int(x)
        |""".stripMargin) shouldBe 200
  }

  "u64 variable with literal" in {
    eval(
      """main() -> int
        |    var x: u64 = 100
        |    int(x)
        |""".stripMargin) shouldBe 100
  }

  "char variable with char literal" in {
    eval(
      """main() -> int
        |    var c: char = 'A'
        |    int(c)
        |""".stripMargin) shouldBe 65
  }

  // ===== Unsigned arithmetic =====

  "u32 addition" in {
    eval(
      """main() -> int
        |    var a: u32 = 10
        |    var b: u32 = 20
        |    int(a + b)
        |""".stripMargin) shouldBe 30
  }

  "u32 subtraction" in {
    eval(
      """main() -> int
        |    var a: u32 = 50
        |    var b: u32 = 20
        |    int(a - b)
        |""".stripMargin) shouldBe 30
  }

  "u32 multiplication" in {
    eval(
      """main() -> int
        |    var a: u32 = 7
        |    var b: u32 = 6
        |    int(a * b)
        |""".stripMargin) shouldBe 42
  }

  "u64 division is unsigned" in {
    // 0xFFFFFFFFFFFFFFFF as unsigned / 2 = 0x7FFFFFFFFFFFFFFF
    // As signed, -1 / 2 = 0 — different result
    eval(
      """main() -> int
        |    var a: u64 = u64(i64(-1))
        |    var b: u64 = 2
        |    i64(a / b)
        |""".stripMargin) shouldBe Long.MaxValue
  }

  "u64 remainder is unsigned" in {
    eval(
      """main() -> int
        |    var a: u64 = u64(i64(-1))
        |    var b: u64 = 10
        |    i64(a % b)
        |""".stripMargin) shouldBe 5 // 18446744073709551615 % 10 = 5
  }

  // ===== Unsigned comparisons =====

  "u32 less than" in {
    eval(
      """main() -> int
        |    var a: u32 = 5
        |    var b: u32 = 10
        |    if a < b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 greater than" in {
    eval(
      """main() -> int
        |    var a: u32 = 10
        |    var b: u32 = 5
        |    if a > b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u64 comparison treats high bit as value, not sign" in {
    // 0xFFFFFFFFFFFFFFFF unsigned > 0 (as signed it would be -1 < 0)
    eval(
      """main() -> int
        |    var a: u64 = u64(i64(-1))
        |    var b: u64 = 0
        |    if a > b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 less than or equal" in {
    eval(
      """main() -> int
        |    var a: u32 = 5
        |    var b: u32 = 5
        |    if a <= b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 greater than or equal" in {
    eval(
      """main() -> int
        |    var a: u32 = 10
        |    var b: u32 = 10
        |    if a >= b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 equality" in {
    eval(
      """main() -> int
        |    var a: u32 = 42
        |    var b: u32 = 42
        |    if a == b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u32 inequality" in {
    eval(
      """main() -> int
        |    var a: u32 = 42
        |    var b: u32 = 43
        |    if a != b then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Unsigned bitwise operations =====

  "u32 bitwise and" in {
    eval(
      """main() -> int
        |    var a: u32 = 0xFF
        |    var b: u32 = 0x0F
        |    int(a & b)
        |""".stripMargin) shouldBe 0x0F
  }

  "u32 bitwise or" in {
    eval(
      """main() -> int
        |    var a: u32 = 0xF0
        |    var b: u32 = 0x0F
        |    int(a | b)
        |""".stripMargin) shouldBe 0xFF
  }

  "u32 bitwise xor" in {
    eval(
      """main() -> int
        |    var a: u32 = 0xFF
        |    var b: u32 = 0x0F
        |    int(a ^ b)
        |""".stripMargin) shouldBe 0xF0
  }

  "u32 left shift" in {
    eval(
      """main() -> int
        |    var a: u32 = 1
        |    var b: u32 = 8
        |    int(a << b)
        |""".stripMargin) shouldBe 256
  }

  "u64 right shift is logical, not arithmetic" in {
    // Signed: -1 >> 1 = -1 (arithmetic shift preserves sign)
    // Unsigned: 0xFFFFFFFFFFFFFFFF >>> 1 = 0x7FFFFFFFFFFFFFFF
    eval(
      """main() -> int
        |    var a: u64 = u64(i64(-1))
        |    var b: u64 = 1
        |    i64(a >> b)
        |""".stripMargin) shouldBe Long.MaxValue
  }

  // ===== Width promotion =====

  "u8 + u16 promotes to u16" in {
    eval(
      """main() -> int
        |    var a: u8 = 10
        |    var b: u16 = 20
        |    int(a + b)
        |""".stripMargin) shouldBe 30
  }

  "u16 + u32 promotes to u32" in {
    eval(
      """main() -> int
        |    var a: u16 = 100
        |    var b: u32 = 200
        |    int(a + b)
        |""".stripMargin) shouldBe 300
  }

  // ===== Casts =====

  "u8 cast zero-extends" in {
    eval("main() -> int = int(u8(0xFF))\n") shouldBe 255
  }

  "u16 cast zero-extends" in {
    eval("main() -> int = int(u16(0xFFFF))\n") shouldBe 65535
  }

  "u32 cast zero-extends" in {
    eval("main() -> int = i64(u32(0xFFFFFFFF))\n") shouldBe 0xFFFFFFFFL
  }

  "i8 cast sign-extends" in {
    eval("main() -> int = i32(i8(0xFF))\n") shouldBe -1
  }

  "i16 cast sign-extends" in {
    eval("main() -> int = i32(i16(0xFFFF))\n") shouldBe -1
  }

  "cast signed to unsigned" in {
    eval(
      """main() -> int
        |    var x: int = -1
        |    var y: u32 = u32(x)
        |    i64(y)
        |""".stripMargin) shouldBe 0xFFFFFFFFL
  }

  "cast unsigned to signed" in {
    eval(
      """main() -> int
        |    var x: u8 = 200
        |    i8(x)
        |""".stripMargin) shouldBe -56
  }

  // ===== Mixed signed/unsigned errors =====

  "mixed signed/unsigned addition is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var a: int = 5
        |    var b: u32 = 10
        |    a + b
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "mixed signed/unsigned comparison is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var a: int = 5
        |    var b: u32 = 10
        |    if a < b then 1 else 0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "mixed signed/unsigned bitwise is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var a: int = 0xFF
        |    var b: u32 = 0x0F
        |    a & b
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "assigning signed var to unsigned var is allowed" in {
    eval(
      """main() -> int
        |    var a: int = 5
        |    var b: u32 = a
        |    int(b)
        |""".stripMargin) shouldBe 5
  }

  "assigning unsigned var to signed var is allowed" in {
    eval(
      """main() -> int
        |    var a: u32 = 5
        |    var b: int = a
        |    b
        |""".stripMargin) shouldBe 5
  }

  // ===== Integer literal coercion =====

  "literal coerces to u32 in declaration" in {
    eval(
      """main() -> int
        |    var x: u32 = 100
        |    int(x)
        |""".stripMargin) shouldBe 100
  }

  "literal coerces to u8 in declaration" in {
    eval(
      """main() -> int
        |    var x: u8 = 42
        |    int(x)
        |""".stripMargin) shouldBe 42
  }

  // ===== Sizeof =====

  "sizeof u8 is 1" in {
    eval("main() -> int = sizeof(u8)\n") shouldBe 1
  }

  "sizeof u16 is 2" in {
    eval("main() -> int = sizeof(u16)\n") shouldBe 2
  }

  "sizeof u32 is 4" in {
    eval("main() -> int = sizeof(u32)\n") shouldBe 4
  }

  "sizeof u64 is 8" in {
    eval("main() -> int = sizeof(u64)\n") shouldBe 8
  }

  // ===== Unsigned in arrays =====

  "u32 array" in {
    eval(
      """main() -> int
        |    var arr: [3]u32
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    int(arr[0] + arr[1] + arr[2])
        |""".stripMargin) shouldBe 60
  }

  // ===== Unsigned in structs =====

  "struct with unsigned fields" in {
    eval(
      """struct Point
        |    x: u32
        |    y: u32
        |
        |main() -> int
        |    p: Point
        |    p.x = 10
        |    p.y = 20
        |    int(p.x + p.y)
        |""".stripMargin) shouldBe 30
  }

  // ===== Unsigned in loops =====

  "u32 for loop counter" in {
    eval(
      """main() -> int
        |    var sum: u32 = 0
        |    var i: u32 = 0
        |    while i < 10
        |        sum = sum + i
        |        i = i + 1
        |    int(sum)
        |""".stripMargin) shouldBe 45
  }

  // ===== Unsigned function parameter and return =====

  "unsigned function parameter" in {
    eval(
      """double_it(x: u32) -> u32 = x * 2
        |main() -> int
        |    int(double_it(21))
        |""".stripMargin) shouldBe 42
  }
}
