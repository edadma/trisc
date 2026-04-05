package io.github.edadma.trisc

class SyslCodegenTryOpTests extends SyslCodegenHelpers {

  "? on Option unwraps Some" in {
    compileAndRun(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |doub(o: Option[int]) -> Option[int]
        |    v = o?
        |    Some(v * 2)
        |
        |main() -> int
        |    r = doub(Some(21))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 42
  }

  "? early-returns None" in {
    compileAndRun(
      """enum Option[T]
        |    Some(value: T)
        |    None
        |
        |doub(o: Option[int]) -> Option[int]
        |    v = o?
        |    Some(v * 2)
        |
        |main() -> int
        |    arg: Option[int] = None
        |    r = doub(arg)
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe -1
  }

  "? on Result with single unwrap" in {
    // Note: chaining multiple ?s on Result (and other data-carrying variants) is
    // currently blocked by a pre-existing codegen bug around struct-return when
    // two match-as-expr forms both contain early-return(StructConstruct) arms.
    // Interpreter handles this correctly. Single-? Result works fine.
    compileAndRun(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |addOne(a: Result[int, int]) -> Result[int, int]
        |    x = a?
        |    Ok(x + 1)
        |
        |main() -> int
        |    r = addOne(Ok(10))
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe 11
  }

  "? early-returns Err (single unwrap)" in {
    compileAndRun(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |addOne(a: Result[int, int]) -> Result[int, int]
        |    x = a?
        |    Ok(x + 1)
        |
        |main() -> int
        |    r = addOne(Err(7))
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe -7
  }
}
