package io.github.edadma.trisc

class SyslCodegenCrossUnitValTests extends SyslCodegenHelpers {

  "module-level val integer constant visible across units" in {
    compileMultiAndRun(Map(
      "mymod/consts" ->
        """module mymod
          |val MAGIC = 0x1234
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = MAGIC
          |""".stripMargin,
    )) shouldBe 0x1234
  }

  "module-level val used in function across units" in {
    compileMultiAndRun(Map(
      "mymod/consts" ->
        """module mymod
          |val BASE = 100
          |get_offset(n: int) -> int = BASE + n
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = get_offset(42)
          |""".stripMargin,
    )) shouldBe 142
  }

  "module-level val across units in same module" in {
    compileMultiAndRun(Map(
      "mymod/consts" ->
        """module mymod
          |val ADDR = 0x100
          |""".stripMargin,
      "mymod/funcs" ->
        """module mymod
          |use_addr() -> int = ADDR
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = use_addr()
          |""".stripMargin,
    )) shouldBe 0x100
  }

  "module-level val large hex constant" in {
    compileMultiAndRun(Map(
      "mymod/hw" ->
        """module mymod
          |val HW_BASE = 0x10000
          |get_base() -> int = HW_BASE
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = get_base()
          |""".stripMargin,
    )) shouldBe 0x10000
  }

  "val defined in module A, used by function in module B" in {
    compileMultiAndRun(Map(
      "hw/config" ->
        """module hw
          |val SHA_BASE = 0x100160
          |""".stripMargin,
      "crypto/sha" ->
        """module crypto
          |import hw.*
          |process() -> int = SHA_BASE
          |""".stripMargin,
      "main" ->
        """import crypto.*
          |main() -> int = process()
          |""".stripMargin,
    )) shouldBe 0x100160
  }

  "val from module A used in arithmetic in module B" in {
    compileMultiAndRun(Map(
      "hw/config" ->
        """module hw
          |val BASE = 0x1000
          |val STATUS_OFF = 4
          |""".stripMargin,
      "drv/dev" ->
        """module drv
          |import hw.*
          |get_status_addr() -> int = BASE + STATUS_OFF
          |""".stripMargin,
      "main" ->
        """import drv.*
          |main() -> int = get_status_addr()
          |""".stripMargin,
    )) shouldBe 0x1004
  }

  "multiple vals from module A used in module B function" in {
    compileMultiAndRun(Map(
      "hw/regs" ->
        """module hw
          |val REG_A = 10
          |val REG_B = 20
          |val REG_C = 30
          |""".stripMargin,
      "app/calc" ->
        """module app
          |import hw.*
          |sum_regs() -> int = REG_A + REG_B + REG_C
          |""".stripMargin,
      "main" ->
        """import app.*
          |main() -> int = sum_regs()
          |""".stripMargin,
    )) shouldBe 60
  }
}
