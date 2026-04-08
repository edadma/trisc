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

  "struct pointer cast and field access" in {
    compileAndRun(
      """struct Pair
        |    a: u32
        |    b: u32
        |
        |main() -> int
        |    var data: [8]byte
        |    var p: *Pair = *Pair(i64(&data[0]))
        |    p.a = 100u32
        |    p.b = 200u32
        |    i64(p.a) + i64(p.b)
        |""".stripMargin) shouldBe 300
  }

  "struct pointer with mixed field sizes" in {
    val asm = compile(
      """struct Regs
        |    word: u32
        |    half: u16
        |    flag: byte
        |    cmd: byte
        |
        |main() -> int
        |    var r: *Regs = *Regs(0x1000)
        |    r.word = 42u32
        |    r.half = 7u16
        |    r.flag = 1u8
        |    0
        |""".stripMargin)
    asm should include("stw")
    asm should include("sts")
    asm should include("stb")
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
