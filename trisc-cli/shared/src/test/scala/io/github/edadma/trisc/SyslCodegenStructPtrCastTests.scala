package io.github.edadma.trisc

class SyslCodegenStructPtrCastTests extends SyslCodegenHelpers {

  "cast integer to *Struct and read field" in {
    compileAndRun(
      """struct Regs
        |    status: int
        |    data: int
        |
        |main() -> int
        |    var regs: Regs
        |    regs.status = 42
        |    val addr = i64(&regs)
        |    val p = *Regs(addr)
        |    p.status
        |""".stripMargin) shouldBe 42
  }

  "cast integer to *Struct and write field" in {
    compileAndRun(
      """struct Regs
        |    status: int
        |    data: int
        |
        |main() -> int
        |    var regs: Regs
        |    val addr = i64(&regs)
        |    val p = *Regs(addr)
        |    p.data = 99
        |    regs.data
        |""".stripMargin) shouldBe 99
  }

  "MMIO pattern: *Struct(constant) field access" in {
    compileAndRun(
      """struct HWRegs
        |    ctrl: int
        |    status: int
        |    data: int
        |
        |main() -> int
        |    var hw: HWRegs
        |    hw.ctrl = 1
        |    hw.status = 2
        |    hw.data = 3
        |    val base = i64(&hw)
        |    val r = *HWRegs(base)
        |    r.ctrl + r.status + r.data
        |""".stripMargin) shouldBe 6
  }
}
