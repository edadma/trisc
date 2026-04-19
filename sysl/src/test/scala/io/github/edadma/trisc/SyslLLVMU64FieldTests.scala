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

  // ===== Struct array stride with alignment padding =====

  "struct array write does not corrupt adjacent element" in {
    // Struct with i32 followed by i64 requires padding — LLVM stride > naive field sum.
    // Writing to arr[1] must not corrupt arr[0].
    llvmExit(
      """struct Padded
        |    tag: int
        |    big: u64
        |    flag: int
        |
        |main() -> int
        |    var arr: [4]Padded
        |    arr[0] = Padded(0xAA, 0, 0)
        |    arr[1] = Padded(0xBB, u64(-1), 0xFF)
        |    if arr[0].tag != 0xAA then return 0
        |    if arr[0].big != 0 then return 0
        |    if arr[0].flag != 0 then return 0
        |    if arr[1].tag != 0xBB then return 0
        |    if arr[1].big != u64(-1) then return 0
        |    if arr[1].flag != 0xFF then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }

  "struct array index 10 not corrupted by index 11" in {
    // Regression test for the Process struct corruption bug
    llvmExit(
      """struct Proc
        |    state: int
        |    ptbr: u64
        |    parent: int
        |    exit_code: int
        |    main_tid: int
        |    allow_lo: i64
        |    allow_hi: i64
        |    mask: int
        |
        |main() -> int
        |    var procs: [16]Proc
        |    procs[10] = Proc(1, 0, 0, 0, 0, 0, 0, 0)
        |    procs[11] = Proc(2, 0, 0, 0, 0, i64(-1), i64(-1), 0)
        |    if procs[10].state != 1 then return 0
        |    if procs[11].state != 2 then return 0
        |    if procs[11].allow_lo != i64(-1) then return 0
        |    1
        |""".stripMargin) shouldBe 1
  }
}
