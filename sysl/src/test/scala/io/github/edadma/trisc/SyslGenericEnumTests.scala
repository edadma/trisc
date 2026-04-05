package io.github.edadma.trisc

class SyslGenericEnumTests extends SyslTestHelpers {

  // ===== Option-style enum =====

  private val optionEnum =
    """enum Option[T]
      |    Some(value: T)
      |    None
      |""".stripMargin

  "construct Some and match" in {
    eval(
      optionEnum +
      """main() -> int
        |    s = Some(42)
        |    s match
        |        Some(v) -> v
        |        None -> 0
        |""".stripMargin) shouldBe 42
  }

  "construct Some(int) then Some(int) again reuses instantiation" in {
    eval(
      optionEnum +
      """main() -> int
        |    a = Some(5)
        |    b = Some(10)
        |    sum = 0
        |    a match
        |        Some(v) -> sum = sum + v
        |        None -> sum = sum - 1
        |    b match
        |        Some(v) -> sum = sum + v
        |        None -> sum = sum - 1
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "Option[int] as typed variable works" in {
    eval(
      optionEnum +
      """main() -> int
        |    x: Option[int] = Some(7)
        |    x match
        |        Some(v) -> v * 3
        |        None -> -1
        |""".stripMargin) shouldBe 21
  }

  "Option[int] passed to function" in {
    eval(
      optionEnum +
      """unwrapOr(o: Option[int], def_: int) -> int
        |    o match
        |        Some(v) -> v
        |        None -> def_
        |
        |main() -> int
        |    a = unwrapOr(Some(100), 999)
        |    a
        |""".stripMargin) shouldBe 100
  }

  "Option[i64] independent from Option[int]" in {
    eval(
      optionEnum +
      """main() -> int
        |    a = Some(3)
        |    b: Option[i64] = Some(50i64)
        |    var total = 0
        |    a match
        |        Some(v) -> total = total + v
        |        None -> total = -1
        |    b match
        |        Some(v) -> total = total + i32(v)
        |        None -> total = -1
        |    total
        |""".stripMargin) shouldBe 53
  }

  // ===== Result-style enum with 2 type params =====

  "Result[T, E] construct and match" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |main() -> int
        |    r: Result[int, int] = Ok(42)
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe 42
  }

  "Result err path" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |main() -> int
        |    r: Result[int, int] = Err(5)
        |    r match
        |        Ok(v) -> v
        |        Err(e) -> 0 - e
        |""".stripMargin) shouldBe -5
  }

  "Result via function return type" in {
    eval(
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

  // ===== Returning generic enum =====

  "function returning Option[int]" in {
    eval(
      optionEnum +
      """safeDiv(a: int, b: int) -> Option[int]
        |    if b == 0 then None
        |    else Some(a / b)
        |
        |main() -> int
        |    a = safeDiv(10, 2)
        |    a match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe 5
  }

  "function returning None when divisor zero" in {
    eval(
      optionEnum +
      """safeDiv(a: int, b: int) -> Option[int]
        |    if b == 0 then None
        |    else Some(a / b)
        |
        |main() -> int
        |    a = safeDiv(10, 0)
        |    a match
        |        Some(v) -> v
        |        None -> -1
        |""".stripMargin) shouldBe -1
  }

  // ===== Nesting / composition =====

  "Option[Point] with struct" in {
    eval(
      optionEnum +
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Option[Point] = Some(Point(3, 4))
        |    p match
        |        Some(pt) -> pt.x * 10 + pt.y
        |        None -> -1
        |""".stripMargin) shouldBe 34
  }
}
