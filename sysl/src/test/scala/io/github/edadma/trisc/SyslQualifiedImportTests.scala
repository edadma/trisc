package io.github.edadma.trisc

class SyslQualifiedImportTests extends SyslTestHelpers {

  "qualified import: call function" in {
    evalWithLibs(
      Map(
        "mylib/math/math" ->
          """module mylib.math
            |add(a: int, b: int) -> int = a + b
            |""".stripMargin,
      ),
      """import mylib.math
        |main() -> int = math.add(20, 22)
        |""".stripMargin
    ) shouldBe 42
  }

  "qualified import: access global variable" in {
    evalWithLibs(
      Map(
        "mylib/consts/consts" ->
          """module mylib.consts
            |val MAGIC = 42
            |""".stripMargin,
      ),
      """import mylib.consts
        |main() -> int = consts.MAGIC
        |""".stripMargin
    ) shouldBe 42
  }

  "qualified import: multiple calls from same module" in {
    evalWithLibs(
      Map(
        "mylib/math/math" ->
          """module mylib.math
            |add(a: int, b: int) -> int = a + b
            |mul(a: int, b: int) -> int = a * b
            |""".stripMargin,
      ),
      """import mylib.math
        |main() -> int = math.add(math.mul(2, 10), 22)
        |""".stripMargin
    ) shouldBe 42
  }

  "qualified and wildcard imports coexist" in {
    evalWithLibs(
      Map(
        "mylib/math/math" ->
          """module mylib.math
            |add(a: int, b: int) -> int = a + b
            |""".stripMargin,
        "mylib/consts/consts" ->
          """module mylib.consts
            |val BASE = 20
            |""".stripMargin,
      ),
      """import mylib.math
        |import mylib.consts.*
        |main() -> int = math.add(BASE, 22)
        |""".stripMargin
    ) shouldBe 42
  }

  "qualified import: function reference" in {
    evalWithLibs(
      Map(
        "mylib/math/math" ->
          """module mylib.math
            |dbl(x: int) -> int = x * 2
            |""".stripMargin,
      ),
      """import mylib.math
        |apply(f: (int) -> int, x: int) -> int = f(x)
        |main() -> int = apply(math.dbl, 21)
        |""".stripMargin
    ) shouldBe 42
  }
}
