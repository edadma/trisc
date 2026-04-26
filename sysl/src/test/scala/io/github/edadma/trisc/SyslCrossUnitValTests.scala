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

  // ===== const cross-file visibility (regression: pre-fix consts were visible
  // only within the file that declared them, even though val/fn/struct/enum all
  // were module-wide visible) =====

  "module-level const integer visible across units" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |const MAGIC: int = 0x1234
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = MAGIC
        |""".stripMargin
    ) shouldBe 0x1234
  }

  "module-level const used in expression across units" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |const BASE: int = 100
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = BASE + 42
        |""".stripMargin
    ) shouldBe 142
  }

  "const declared in sibling file of same module is visible" in {
    // The original bug: `const X` in fileA, used in fileB of the same module.
    // ModuleMeta.fromProgram skipped TConstDecl, so fileB's analyzer never saw it.
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |const LINE_H: int = 18
            |const PAD_Y: int = 6
            |""".stripMargin,
        "mymod/funcs" ->
          """module mymod
            |row_height() -> int = LINE_H + 2 * PAD_Y
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = row_height()
        |""".stripMargin
    ) shouldBe 30
  }
}
