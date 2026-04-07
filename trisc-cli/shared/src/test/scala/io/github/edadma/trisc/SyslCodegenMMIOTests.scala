package io.github.edadma.trisc

class SyslCodegenMMIOTests extends SyslCodegenHelpers {

  "deref assign through *u32 cast emits stw" in {
    // Verify the assembly contains stw, not std
    val asm = compile(
      """main() -> int
        |    val addr: i64 = 0x1000
        |    *(*u32(addr)) = u32(42)
        |    0
        |""".stripMargin)
    asm should include("stw")
    asm should not include regex("std r[0-7], r[0-7], r0\n.*# deref assign")
  }

  "deref assign through *u16 cast emits sts" in {
    val asm = compile(
      """main() -> int
        |    val addr: i64 = 0x1000
        |    *(*u16(addr)) = u16(42)
        |    0
        |""".stripMargin)
    asm should include("sts")
  }

  "deref assign through *u8 cast emits stb" in {
    val asm = compile(
      """main() -> int
        |    val addr: i64 = 0x1000
        |    *(*byte(addr)) = byte(42)
        |    0
        |""".stripMargin)
    asm should include("stb")
  }

  "deref read through *u32 cast emits ldw" in {
    val asm = compile(
      """main() -> int
        |    val addr: i64 = 0x1000
        |    val v = *(*u32(addr))
        |    0
        |""".stripMargin)
    asm should include("ldw")
  }

  "MMIO write and read back through *u32" in {
    // Write 42 to a memory location via *u32 pointer, read it back
    compileAndRun(
      """main() -> int
        |    var storage: i64 = 0
        |    val addr = i64(&storage)
        |    *(*u32(addr)) = u32(42)
        |    i64(*(*u32(addr)))
        |""".stripMargin) shouldBe 42
  }

  "MMIO write through *byte" in {
    compileAndRun(
      """main() -> int
        |    var storage: i64 = 0
        |    val addr = i64(&storage)
        |    *(*byte(addr)) = byte(0xFF)
        |    i64(*(*byte(addr)))
        |""".stripMargin) shouldBe 255
  }
}
