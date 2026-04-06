package io.github.edadma.trisc

class SyslCodegenGenericEnumTests extends SyslCodegenHelpers {

  "Option Some path" in {
    compileAndRun(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |main() -> int
        |    s = Some(42)
        |    s match
        |        Some(v) -> v
        |        None -> 0
        |""".stripMargin) shouldBe 42
  }

  "Option None with expected type" in {
    compileAndRun(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |safeDiv(a: int, b: int) -> Option[int]
        |    if b == 0 then None
        |    else Some(a / b)
        |
        |main() -> int
        |    r = safeDiv(20, 0)
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe -1
  }

  "Result type with function return context" in {
    compileAndRun(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |safeDiv(a: int, b: int) -> Result[int, int]
        |    if b == 0 then Err(99)
        |    else Ok(a / b)
        |
        |main() -> int
        |    r = safeDiv(20, 4)
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe 5
  }
}
