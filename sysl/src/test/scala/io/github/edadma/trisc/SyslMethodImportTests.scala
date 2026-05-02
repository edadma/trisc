package io.github.edadma.trisc

class SyslMethodImportTests extends SyslTestHelpers {

  // ===== Selective import of a constructor pulls in the type's methods =====
  //
  // `import some.module.{factory_fn}` should let the user call methods on the
  // value returned by `factory_fn` without also having to name the type in
  // the selector list. Methods belong to the type's defining module, not to
  // the importer's namespace; the analyzer derives the type set transitively
  // from the imported function's signature and pulls in matching `Type_method`
  // functions automatically.
  //
  // Regression: previously `import std.builder.{new_builder}` gave a clean
  // analyzer error on `b.write_rune(...)` because `StrBuilder_write_rune`
  // wasn't in the local function table.

  private val builderLib =
    """module gear.builder
      |
      |struct GBuf
      |    n: int
      |
      |new_buf() -> GBuf = GBuf(0)
      |
      |GBuf.add(n: int)
      |    self.n = self.n + n
      |
      |GBuf.value() -> int = self.n
      |""".stripMargin

  "method calls on the type returned by an imported function resolve without naming the type" in {
    runWithLibs(
      Map("gear/builder/builder.lsysl" -> builderLib),
      """import gear.builder.{new_buf}
        |
        |main() -> int
        |    val b = new_buf()
        |    b.add(7)
        |    b.add(35)
        |    b.value()
        |""".stripMargin,
    )._1 shouldBe 42
  }

  "explicit type import still works (sanity: methods not pulled in twice)" in {
    runWithLibs(
      Map("gear/builder/builder.lsysl" -> builderLib),
      """import gear.builder.{GBuf, new_buf}
        |
        |main() -> int
        |    val b = new_buf()
        |    b.add(40)
        |    b.add(2)
        |    b.value()
        |""".stripMargin,
    )._1 shouldBe 42
  }

  // The "type referenced in a parameter pulls methods too" shape — exercises
  // both directions of the signature, not just the return slot.
  "method calls work when the type appears only in a parameter of the imported function" in {
    val withParamFn =
      builderLib +
        """|
           |inc_buf(b: *GBuf, n: int)
           |    b.add(n)
           |""".stripMargin
    runWithLibs(
      Map("gear/builder/builder.lsysl" -> withParamFn),
      """import gear.builder.{new_buf, inc_buf}
        |
        |main() -> int
        |    val b = new_buf()
        |    inc_buf(&b, 19)
        |    b.add(23)
        |    b.value()
        |""".stripMargin,
    )._1 shouldBe 42
  }
}
