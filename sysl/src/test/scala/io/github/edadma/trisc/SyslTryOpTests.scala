package io.github.edadma.trisc

class SyslTryOpTests extends SyslTestHelpers {

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
    eval(
      optionEnum +
      """first(o: Option[int]) -> Option[int]
        |    v = o?
        |    Some(v * 2)
        |
        |main() -> int
        |    r = first(Some(21))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 42
  }

  "? on None early-returns None (via typed var)" in {
    eval(
      optionEnum +
      """first(o: Option[int]) -> Option[int]
        |    v = o?
        |    Some(v * 2)
        |
        |main() -> int
        |    arg: Option[int] = None
        |    r = first(arg)
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe -1
  }

  "chained ? calls" in {
    eval(
      optionEnum +
      """doub(o: Option[int]) -> Option[int]
        |    v = o?
        |    Some(v * 2)
        |
        |quad(o: Option[int]) -> Option[int]
        |    a = doub(o)?
        |    doub(Some(a))
        |
        |main() -> int
        |    r = quad(Some(5))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 20
  }

  "? inside combined computation" in {
    eval(
      optionEnum +
      """pairSum(a: Option[int], b: Option[int]) -> Option[int]
        |    x = a?
        |    y = b?
        |    Some(x + y)
        |
        |main() -> int
        |    r = pairSum(Some(10), Some(32))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 42
  }

  "? short-circuits in middle" in {
    eval(
      optionEnum +
      """pairSum(a: Option[int], b: Option[int]) -> Option[int]
        |    x = a?
        |    y = b?
        |    Some(x + y)
        |
        |main() -> int
        |    bad: Option[int] = None
        |    r = pairSum(Some(10), bad)
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe -1
  }

  // ===== Result[T, E] =====

  "? on Ok unwraps value" in {
    eval(
      resultEnum +
      """doBoth(a: Result[int, int], b: Result[int, int]) -> Result[int, int]
        |    x = a?
        |    y = b?
        |    Ok(x + y)
        |
        |main() -> int
        |    r = doBoth(Ok(10), Ok(32))
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe 42
  }

  "? on Err early-returns Err" in {
    eval(
      resultEnum +
      """doBoth(a: Result[int, int], b: Result[int, int]) -> Result[int, int]
        |    x = a?
        |    y = b?
        |    Ok(x + y)
        |
        |main() -> int
        |    r = doBoth(Ok(10), Err(7))
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe -7
  }

  "? short-circuits first Err" in {
    eval(
      resultEnum +
      """doBoth(a: Result[int, int], b: Result[int, int]) -> Result[int, int]
        |    x = a?
        |    y = b?
        |    Ok(x + y)
        |
        |main() -> int
        |    r = doBoth(Err(5), Ok(100))
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe -5
  }

  // ===== Errors =====

  "? on non-enum type is error" in {
    an[Exception] should be thrownBy eval(
      """first() -> int
        |    v = 42?
        |    v
        |main() -> int = first()
        |""".stripMargin)
  }

  "? without matching return type is error" in {
    an[Exception] should be thrownBy eval(
      optionEnum +
      """mismatched(o: Option[int]) -> int
        |    v = o?
        |    v
        |main() -> int = mismatched(Some(10))
        |""".stripMargin)
  }
}
