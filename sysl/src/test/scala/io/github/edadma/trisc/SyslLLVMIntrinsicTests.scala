package io.github.edadma.trisc

class SyslLLVMIntrinsicTests extends SyslLLVMTestHelpers {

  "wrapping_add: u8 overflow wraps" in {
    llvmExit(
      """main() -> int
        |    var a: u8 = 200
        |    var b: u8 = 100
        |    int(wrapping_add(a, b))
        |""".stripMargin) shouldBe 44
  }

  "wrapping_sub: u8 underflow wraps" in {
    llvmExit(
      """main() -> int
        |    var a: u8 = 0
        |    var b: u8 = 1
        |    int(wrapping_sub(a, b))
        |""".stripMargin) shouldBe 255
  }

  "wrapping_mul: i32 overflow wraps" in {
    llvmExit(
      """main() -> int
        |    var a: i32 = 100000
        |    var b: i32 = 100000
        |    var r = wrapping_mul(a, b)
        |    if r == 1410065408 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "saturating_add: u8 clamps to 255" in {
    llvmExit(
      """main() -> int
        |    var a: u8 = 200
        |    var b: u8 = 100
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 255
  }

  "saturating_add: i8 clamps to MAX (127)" in {
    llvmExit(
      """main() -> int
        |    var a: i8 = 100
        |    var b: i8 = 50
        |    int(saturating_add(a, b))
        |""".stripMargin) shouldBe 127
  }

  "saturating_add: i8 clamps to MIN (-128)" in {
    // Exit code is 8-bit; -128 → 128 unsigned, so check via if/then.
    llvmExit(
      """main() -> int
        |    var a: i8 = i8(-100)
        |    var b: i8 = i8(-50)
        |    if int(saturating_add(a, b)) == -128 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "saturating_sub: u8 clamps to 0" in {
    llvmExit(
      """main() -> int
        |    var a: u8 = 5
        |    var b: u8 = 10
        |    int(saturating_sub(a, b))
        |""".stripMargin) shouldBe 0
  }

  "saturating_mul: u16 clamps to 65535" in {
    // Exit codes are 8-bit on UNIX, so check via if/then to avoid truncation.
    llvmExit(
      """main() -> int
        |    var a: u16 = 1000
        |    var b: u16 = 1000
        |    if int(saturating_mul(a, b)) == 65535 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "saturating_mul: i16 negative clamps to MIN" in {
    // Exit code is 8-bit; -32768 won't fit. Use if/then.
    llvmExit(
      """main() -> int
        |    var a: i16 = i16(-1000)
        |    var b: i16 = 100
        |    if int(saturating_mul(a, b)) == -32768 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ===== IR verification =====

  "wrapping_add lowers to plain add (no overflow check)" in {
    val ir = compileLLVM(
      """main() -> int
        |    var a: i32 = 1
        |    var b: i32 = 2
        |    wrapping_add(a, b)
        |""".stripMargin)
    // The wrapping form should NOT introduce sat/overflow intrinsics in the body
    val body = ir.split("define.*@main")(1)
    assert(!body.contains(".sat.") && !body.contains(".with.overflow."),
           s"wrapping_add should compile to plain add, but IR contains sat/overflow:\n$body")
  }

  "saturating_add uses llvm.uadd.sat for unsigned" in {
    val ir = compileLLVM(
      """main() -> int
        |    var a: u8 = 1
        |    var b: u8 = 2
        |    int(saturating_add(a, b))
        |""".stripMargin)
    assert(ir.contains("llvm.uadd.sat.i8"),
           s"expected llvm.uadd.sat.i8 in IR:\n$ir")
  }

  "saturating_add uses llvm.sadd.sat for signed" in {
    val ir = compileLLVM(
      """main() -> int
        |    var a: i16 = 1
        |    var b: i16 = 2
        |    int(saturating_add(a, b))
        |""".stripMargin)
    assert(ir.contains("llvm.sadd.sat.i16"),
           s"expected llvm.sadd.sat.i16 in IR:\n$ir")
  }
}
