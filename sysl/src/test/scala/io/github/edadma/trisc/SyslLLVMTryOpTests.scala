package io.github.edadma.trisc

/** LLVM coverage for the `?` (try) postfix operator on `Option` / `Result`.
  *
  * The audit (project_sysl_audit_bugs.md, item #7) flagged that `?` had no
  * dedicated LLVM gap-test even though the parser produces `TryAST` and the
  * analyzer desugars it to an early-return match. These tests pin that the
  * desugared form lowers to LLVM correctly — both the unwrap path and the
  * early-return path, including chained / mid-expression / cross-function
  * uses that the interpreter already covers in `SyslTryOpTests`.
  */
class SyslLLVMTryOpTests extends SyslLLVMTestHelpers {

  private val optionEnum =
    """enum Option[T]
      |    Some(value: T)
      |    None
      |""".stripMargin

  private val resultEnum =
    """enum Result[T, E]
      |    Ok(value: T)
      |    Err(error: E)
      |""".stripMargin

  // ===== Option[T] =====

  "? on Some unwraps value" in {
    llvmExit(
      optionEnum +
      """first(o: Option[int]) -> Option[int]
        |    val v = o?
        |    Some(v * 2)
        |
        |main() -> int
        |    val r = first(Some(21))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 42
  }

  "? on None early-returns None" in {
    llvmExit(
      optionEnum +
      """first(o: Option[int]) -> Option[int]
        |    val v = o?
        |    Some(v * 2)
        |
        |main() -> int
        |    val arg: Option[int] = None
        |    val r = first(arg)
        |    r match
        |        Some(v) -> v
        |        None -> 99
        |""".stripMargin) shouldBe 99
  }

  "? short-circuits in middle of computation" in {
    llvmExit(
      optionEnum +
      """pairSum(a: Option[int], b: Option[int]) -> Option[int]
        |    val x = a?
        |    val y = b?
        |    Some(x + y)
        |
        |main() -> int
        |    val bad: Option[int] = None
        |    val r = pairSum(Some(10), bad)
        |    r match
        |        Some(v) -> v
        |        None -> 7
        |""".stripMargin) shouldBe 7
  }

  "chained ? across function calls" in {
    llvmExit(
      optionEnum +
      """doub(o: Option[int]) -> Option[int]
        |    val v = o?
        |    Some(v * 2)
        |
        |quad(o: Option[int]) -> Option[int]
        |    val a = doub(o)?
        |    doub(Some(a))
        |
        |main() -> int
        |    val r = quad(Some(5))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 20
  }

  // ===== Result[T, E] =====

  "? on Ok unwraps value" in {
    llvmExit(
      resultEnum +
      """doBoth(a: Result[int, int], b: Result[int, int]) -> Result[int, int]
        |    val x = a?
        |    val y = b?
        |    Ok(x + y)
        |
        |main() -> int
        |    val r = doBoth(Ok(10), Ok(32))
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe 42
  }

  "? on Err early-returns Err preserving payload" in {
    llvmExit(
      resultEnum +
      """doBoth(a: Result[int, int], b: Result[int, int]) -> Result[int, int]
        |    val x = a?
        |    val y = b?
        |    Ok(x + y)
        |
        |main() -> int
        |    val bad: Result[int, int] = Err(13)
        |    val r = doBoth(Ok(10), bad)
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> e
        |""".stripMargin) shouldBe 13
  }
}
