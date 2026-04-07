package io.github.edadma.trisc

class SyslCodegenModuleValExprTests extends SyslCodegenHelpers {

  "module val with literal" in {
    compileMultiAndRun(Map(
      "mymod/consts" ->
        """module mymod
          |val BASE = 100
          |get_base() -> int = BASE
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = get_base()
          |""".stripMargin,
    )) shouldBe 100
  }

  "module val with expression initializer" in {
    compileMultiAndRun(Map(
      "mymod/consts" ->
        """module mymod
          |val BASE = 100
          |val OFFSET = BASE + 4
          |get_offset() -> int = OFFSET
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = get_offset()
          |""".stripMargin,
    )) shouldBe 104
  }

  "module val referencing another val" in {
    compileMultiAndRun(Map(
      "mymod/consts" ->
        """module mymod
          |val A = 10
          |val B = A * 2
          |val C = A + B
          |get_c() -> int = C
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = get_c()
          |""".stripMargin,
    )) shouldBe 30
  }

  "module val hex expression" in {
    compileMultiAndRun(Map(
      "mymod/hw" ->
        """module mymod
          |val RD_BASE = 0x1000
          |val RD_STATUS = RD_BASE + 4
          |val RD_DATA = RD_BASE + 8
          |get_data_addr() -> int = RD_DATA
          |""".stripMargin,
      "main" ->
        """import mymod.*
          |main() -> int = get_data_addr()
          |""".stripMargin,
    )) shouldBe 0x1008
  }

  // Cross-module val expressions (val X = IMPORTED_CONST + 4) don't work yet
  // because constEval can't resolve globals from other compilation units.
  // Workaround: use a function that computes the value, or duplicate the constant.
  "cross-module val expression via function" in {
    compileMultiAndRun(Map(
      "hw/base" ->
        """module hw
          |val BASE = 0x1000
          |get_base() -> int = BASE
          |""".stripMargin,
      "drv/regs" ->
        """module drv
          |import hw.*
          |get_status() -> int = get_base() + 4
          |""".stripMargin,
      "main" ->
        """import drv.*
          |main() -> int = get_status()
          |""".stripMargin,
    )) shouldBe 0x1004
  }
}
