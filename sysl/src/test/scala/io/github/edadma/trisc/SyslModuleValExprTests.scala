package io.github.edadma.trisc

class SyslModuleValExprTests extends SyslTestHelpers {

  "module val with expression initializer" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |val BASE = 100
            |val OFFSET = BASE + 4
            |get_offset() -> int = OFFSET
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = get_offset()
        |""".stripMargin
    ) shouldBe 104
  }

  "module val referencing another val" in {
    evalWithLibs(
      Map(
        "mymod/consts" ->
          """module mymod
            |val A = 10
            |val B = A * 2
            |val C = A + B
            |get_c() -> int = C
            |""".stripMargin,
      ),
      """import mymod.*
        |main() -> int = get_c()
        |""".stripMargin
    ) shouldBe 30
  }
}
