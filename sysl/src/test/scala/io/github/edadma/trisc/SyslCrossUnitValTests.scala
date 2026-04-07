package io.github.edadma.trisc

class SyslCrossUnitValTests extends SyslTestHelpers {

  "module-level val integer constant visible across units" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |val MAGIC = 0x1234
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = MAGIC
        |""".stripMargin
    ) shouldBe 0x1234
  }

  "module-level val used in function across units" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |val BASE = 100
            |get_offset(n: int) -> int = BASE + n
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = get_offset(42)
        |""".stripMargin
    ) shouldBe 142
  }

  "module-level val across units in same module" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |val ADDR = 0x100160
            |""".stripMargin,
        "mymod/funcs" ->
          """module mymod
            |use_addr() -> int = ADDR
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = use_addr()
        |""".stripMargin
    ) shouldBe 0x100160
  }
}
