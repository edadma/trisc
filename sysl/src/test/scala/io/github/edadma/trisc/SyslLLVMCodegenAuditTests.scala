package io.github.edadma.trisc

class SyslLLVMCodegenAuditTests extends SyslLLVMTestHelpers {

  // =============================================================================
  // Issue 1: emitSextIfNeeded always uses sext — unsigned values with high bit set
  //          get sign-extended instead of zero-extended when widened
  // =============================================================================

  // --- u8 → i32 widening (function argument) ---

  "u8 200 passed to int param should be 200 not -56" in {
    llvmExit(
      """id(x: int) -> int = x
        |
        |main() -> int
        |    var b: u8 = 200
        |    val r = id(b)
        |    if r == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "u8 255 passed to int param should be 255 not -1" in {
    llvmExit(
      """id(x: int) -> int = x
        |
        |main() -> int
        |    var b: u8 = 255
        |    val r = id(b)
        |    if r == 255 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- u8 → i32 widening (variable assignment) ---

  "u8 200 assigned to int var should be 200" in {
    llvmExit(
      """main() -> int
        |    var b: u8 = 200
        |    var x: int = b
        |    if x == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- u8 → i32 widening (return value) ---

  "function returning u8 200 to int should be 200" in {
    llvmExit(
      """getval() -> u8
        |    200
        |
        |main() -> int
        |    val r: int = getval()
        |    if r == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- u16 → i32 widening ---

  "u16 40000 passed to int param should be 40000" in {
    llvmExit(
      """id(x: int) -> int = x
        |
        |main() -> int
        |    var s: u16 = 40000
        |    val r = id(s)
        |    if r == 40000 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "u16 65535 assigned to int var should be 65535" in {
    llvmExit(
      """main() -> int
        |    var s: u16 = 65535
        |    var x: int = s
        |    if x == 65535 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- u32 → i64 widening (the critical case for the OS kernel) ---

  "u32 with high bit set widened to i64 should zero-extend" in {
    val (exit, output) = runLLVM(
      """main() -> int
        |    var u: u32 = 0x80000000
        |    var wide: i64 = i64(u)
        |    if wide == i64(0x80000000) then 0 else 1
        |""".stripMargin)
    exit shouldBe 0
  }

  "u32 0xFFFFFFFF widened to i64 should be 4294967295" in {
    val (exit, output) = runLLVM(
      """main() -> int
        |    var u: u32 = 0xFFFFFFFF
        |    var wide: i64 = i64(u)
        |    if wide > i64(0) then 0 else 1
        |""".stripMargin)
    exit shouldBe 0
  }

  "u32 passed to i64 param should zero-extend" in {
    llvmExit(
      """check(x: i64) -> int
        |    if x > i64(0) then 0 else 1
        |
        |main() -> int
        |    var u: u32 = 0x80000000
        |    check(i64(u))
        |""".stripMargin) shouldBe 0
  }

  // --- u32 → i64 widening in arithmetic context ---

  "u32 high-bit in addition with i64 should produce correct result" in {
    llvmExit(
      """main() -> int
        |    var u: u32 = 0xC0000000
        |    var wide: i64 = i64(u) + i64(0x100)
        |    if wide == i64(0xC0000100) then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- Compound assignment with unsigned RHS ---

  "compound assign u8 to int should zero-extend" in {
    llvmExit(
      """main() -> int
        |    var x: int = 0
        |    var b: u8 = 200
        |    x += b
        |    if x == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- Unsigned return value widening ---

  "u8 returned where int expected should zero-extend" in {
    llvmExit(
      """getbyte() -> u8 = 200
        |
        |wrapper() -> int
        |    getbyte()
        |
        |main() -> int
        |    val r = wrapper()
        |    if r == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- if-expr branch widening ---

  "u8 in if-expr then branch widened to int" in {
    llvmExit(
      """main() -> int
        |    var b: u8 = 200
        |    val r: int = if true then b else 0
        |    if r == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // --- Field compound assign with unsigned ---

  "struct field compound assign with u8 should zero-extend" in {
    llvmExit(
      """struct Accum
        |    total: int
        |
        |main() -> int
        |    var a = Accum(0)
        |    var b: u8 = 200
        |    a.total += b
        |    if a.total == 200 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // =============================================================================
  // Issue 2: Missing TPreInc / TPreDec codegen — would silently return 0
  // =============================================================================

  "pre-increment should work in LLVM codegen" in {
    llvmExit(
      """main() -> int
        |    var x = 41
        |    val r = ++x
        |    r
        |""".stripMargin) shouldBe 42
  }

  "pre-decrement should work in LLVM codegen" in {
    llvmExit(
      """main() -> int
        |    var x = 43
        |    val r = --x
        |    r
        |""".stripMargin) shouldBe 42
  }

  "pre-increment side effect should persist" in {
    llvmExit(
      """main() -> int
        |    var x = 41
        |    ++x
        |    x
        |""".stripMargin) shouldBe 42
  }

  // =============================================================================
  // Issue 3: Missing TSizeof codegen — would silently return 0
  // =============================================================================

  "sizeof int should be 4" in {
    llvmExit(
      """main() -> int
        |    sizeof(int)
        |""".stripMargin) shouldBe 4
  }

  "sizeof i64 should be 8" in {
    llvmExit(
      """main() -> int
        |    sizeof(i64)
        |""".stripMargin) shouldBe 8
  }

  "sizeof u8 should be 1" in {
    llvmExit(
      """main() -> int
        |    sizeof(u8)
        |""".stripMargin) shouldBe 1
  }

  // =============================================================================
  // Issue 4: Signed comparison used for unsigned types in binary ops
  // =============================================================================

  "u32 comparison should be unsigned" in {
    llvmExit(
      """main() -> int
        |    var a: u32 = 0xFFFFFFFF
        |    var b: u32 = 1
        |    if a > b then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  "u8 comparison should be unsigned" in {
    llvmExit(
      """main() -> int
        |    var a: u8 = 255
        |    var b: u8 = 1
        |    if a > b then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // =============================================================================
  // Issue 5: IR verification — check sext vs zext in generated IR
  // =============================================================================

  "u8 to int widening should emit zext not sext in IR" in {
    val ir = compileLLVM(
      """id(x: int) -> int = x
        |
        |main() -> int
        |    var b: u8 = 200
        |    id(b)
        |""".stripMargin)
    // The widening from i8 to i32 should use zext for unsigned types
    // If we find sext i8 ... to i32 where a zext should be, that's the bug
    val hasCorrectExtend = ir.contains("zext i8") || !ir.contains("sext i8")
    // At minimum, the generated code should contain a widening
    // The actual check: if the IR has "sext i8" but NO "zext i8", that's wrong
    if ir.contains("sext i8") && !ir.contains("zext i8") then
      fail(s"IR uses sext instead of zext for u8→i32 widening:\n$ir")
  }

  "u32 to i64 widening should emit zext not sext in IR" in {
    val ir = compileLLVM(
      """check(x: i64) -> int = 0
        |
        |main() -> int
        |    var u: u32 = 0x80000000
        |    check(i64(u))
        |""".stripMargin)
    // Cast from u32 to i64 should use zext
    if ir.contains("sext i32") && !ir.contains("zext i32") then
      fail(s"IR uses sext instead of zext for u32→i64 widening:\n$ir")
  }
}
