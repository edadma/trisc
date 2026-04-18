package io.github.edadma.trisc

class SyslLLVMU64FieldTests extends SyslLLVMTestHelpers {

  "u64 struct field stores and reads value > 2^32" in {
    llvmExit(
      """struct W
        |    v: u64
        |
        |main() -> int
        |    w = W(0x1_0000_0000)
        |    if w.v == 0x1_0000_0000 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u64 struct field assignment > 2^32" in {
    llvmExit(
      """struct W
        |    v: u64
        |
        |main() -> int
        |    var w = W(0)
        |    w.v = 0x1_0000_0000
        |    if w.v == 0x1_0000_0000 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "u64 struct field passed to function" in {
    llvmExit(
      """struct W
        |    v: u64
        |
        |check(x: u64) -> int
        |    if x == 0x1_0000_0000 then 1 else 0
        |
        |main() -> int
        |    w = W(0x1_0000_0000)
        |    check(w.v)
        |""".stripMargin) shouldBe 1
  }

  "ulong struct field stores and reads value > 2^32" in {
    llvmExit(
      """struct W
        |    v: ulong
        |
        |main() -> int
        |    w = W(0x1_0000_0000)
        |    if w.v == 0x1_0000_0000 then 1 else 0
        |""".stripMargin) shouldBe 1
  }
}
