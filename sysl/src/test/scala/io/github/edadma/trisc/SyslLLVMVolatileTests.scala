package io.github.edadma.trisc

class SyslLLVMVolatileTests extends SyslLLVMTestHelpers {

  // ===== Volatile variable — IR verification =====

  "volatile var emits volatile load" in {
    val ir = compileLLVM(
      """main() -> int
        |    volatile var x: int = 42
        |    x
        |""".stripMargin)
    ir should include("load volatile i32")
  }

  "volatile var emits volatile store on assign" in {
    val ir = compileLLVM(
      """main() -> int
        |    volatile var x: int = 0
        |    x = 42
        |    x
        |""".stripMargin)
    ir should include("store volatile i32")
    ir should include("load volatile i32")
  }

  // ===== Volatile struct field — IR verification =====

  "volatile struct field emits volatile load" in {
    val ir = compileLLVM(
      """struct Regs
        |    volatile status: u32
        |    data: int
        |
        |main() -> int
        |    var r = Regs(0, 0)
        |    r.status
        |""".stripMargin)
    ir should include("load volatile i32")
  }

  "volatile struct field emits volatile store" in {
    val ir = compileLLVM(
      """struct Regs
        |    volatile status: u32
        |    data: int
        |
        |main() -> int
        |    var r = Regs(0, 0)
        |    r.status = 1
        |    0
        |""".stripMargin)
    ir should include("store volatile i32")
  }

  "non-volatile field does not emit volatile" in {
    val ir = compileLLVM(
      """struct Regs
        |    volatile status: u32
        |    data: int
        |
        |main() -> int
        |    var r = Regs(0, 0)
        |    r.data = 42
        |    r.data
        |""".stripMargin)
    // The data field loads/stores should NOT be volatile
    val lines = ir.linesIterator.toList
    val dataLoads = lines.filter(l => l.contains("load") && !l.contains("volatile") && l.contains("i32") && !l.contains("getelementptr"))
    dataLoads should not be empty
  }

  // ===== Volatile variable — runtime correctness =====

  "volatile var reads and writes correctly" in {
    llvmExit(
      """main() -> int
        |    volatile var x: int = 42
        |    x
        |""".stripMargin) shouldBe 42
  }

  "volatile var assign and read back" in {
    llvmExit(
      """main() -> int
        |    volatile var x: int = 0
        |    x = 42
        |    x
        |""".stripMargin) shouldBe 42
  }
}
