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

  // ===== `?` reads the function return type, not currentExpected =====
  //
  // Reads from `currentReturnType`, not `currentExpected`. The latter is the
  // *immediate* expected type and gets overridden by inner contexts (var-decl
  // LHS, field-assign LHS, closure body expected, etc). The `?` operator's
  // contract is about where it returns to — the enclosing function — so it
  // must look at the function-level return type independently of whatever
  // immediate context we're in.

  "? inside RHS of var-decl with explicit `: T` annotation" in {
    // The var's annotated type sets `currentExpected = T` for the RHS, which
    // is *not* the enclosing function's return type. Reading from
    // `currentReturnType` keeps `?` working.
    eval(
      optionEnum +
      """unwrap_or_zero(o: Option[int]) -> Option[int]
        |    val v: int = o?
        |    Some(v + 1)
        |
        |main() -> int
        |    val r = unwrap_or_zero(Some(41))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 42
  }

  "? inside RHS of plain assignment to typed local" in {
    eval(
      optionEnum +
      """double_or_short_circuit(o: Option[int]) -> Option[int]
        |    var n: int = 0
        |    n = o?
        |    Some(n * 2)
        |
        |main() -> int
        |    val r = double_or_short_circuit(Some(21))
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 42
  }

  "? inside RHS of field assign on a typed field" in {
    // FieldAssignStmtAST forwards the field's declared type as the RHS expected.
    // `?` must still see the function's return type, not the field type.
    eval(
      optionEnum +
      """struct Box
        |    n: int
        |
        |store(o: Option[int], b: *Box) -> Option[int]
        |    b.n = o?
        |    Some(b.n)
        |
        |main() -> int
        |    var b = Box(0)
        |    val r = store(Some(99), &b)
        |    r match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 99
  }
}
