package io.github.edadma.trisc

class SyslSelectiveImportTests extends SyslLLVMTestHelpers {

  // ===== Selective imports filter generic functions =====
  //
  // `import std.option.{Option, Some, None}` should NOT bring in `std.option.expect`
  // (a 2-arg generic). The 3-arg testing-builtin `expect` should remain callable.
  // Before the fix, generic templates were registered unconditionally on import,
  // and `expect[T]` shadowed the testing-builtin → arity error at first use.

  "selective import doesn't leak generic expect from std.option" in {
    val (exit, _) = runLLVMWithStd(
      """import std.option.{Option, Some, None}
        |
        |main() -> int
        |    var x: Option[int] = Some(42)
        |    x match
        |        Some(v) -> v
        |        None -> 0
        |""".stripMargin)
    exit shouldBe 42
  }

  "selectively-imported names still resolve" in {
    val (exit, _) = runLLVMWithStd(
      """import std.result.{Result, Ok, Err, is_ok, unwrap}
        |
        |main() -> int
        |    val r: Result[int, int] = Ok(42)
        |    if is_ok(r) then unwrap(r) else 0
        |""".stripMargin)
    exit shouldBe 42
  }

  "non-selected generic function is not visible" in {
    // std.result has `unwrap_err[T, E]` but we don't list it in the selective
    // import. The compiler should treat unwrap_err as undefined here.
    val ex = intercept[Exception] {
      runLLVMWithStd(
        """import std.result.{Result, Ok, Err, is_err, unwrap}
          |
          |main() -> int
          |    val r: Result[int, int] = Err(99)
          |    unwrap_err(r)
          |""".stripMargin)
    }
    val msg = ex.getMessage
    assert(
      msg.toLowerCase.contains("undefined") || msg.toLowerCase.contains("unknown") || msg.toLowerCase.contains("unwrap_err"),
      s"non-selected name should not resolve, got: $msg",
    )
  }

  "wildcard import still brings in everything" in {
    val (exit, _) = runLLVMWithStd(
      """import std.result.*
        |
        |main() -> int
        |    val r: Result[int, int] = Err(99)
        |    if is_err(r) then unwrap_err(r) - 57 else 0
        |""".stripMargin)
    exit shouldBe 42
  }

  "selective import of generic enum + variants — None resolves with right expected type" in {
    // The user's parsyl bug: with selective import {Option, Some, None}, `None`
    // inside a parent variant arg should resolve via the field's expected type,
    // not the parent's. This exercises both the selective-import filter and the
    // variant-arg expected-type propagation fix.
    val (exit, _) = runLLVMWithStd(
      """import std.option.{Option, Some, None}
        |
        |enum Wrap[A]
        |    Just(o: Option[A])
        |    Empty
        |
        |main() -> int
        |    val w: Wrap[int] = Just(None)
        |    w match
        |        Just(o) ->
        |            o match
        |                Some(v) -> v
        |                None -> 42
        |        Empty -> 0
        |""".stripMargin)
    exit shouldBe 42
  }
}
