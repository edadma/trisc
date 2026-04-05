package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslEnumTests extends SyslTestHelpers {

  // ===== Basic enum =====

  "enum member access" in {
    eval(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color.Red
        |""".stripMargin) shouldBe 0
  }

  "enum auto-increment" in {
    eval(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int = Color.Blue
        |""".stripMargin) shouldBe 2
  }

  "enum explicit values" in {
    eval(
      """enum Status
        |    Ok = 200
        |    NotFound = 404
        |    Error = 500
        |
        |main() -> int = Status.NotFound
        |""".stripMargin) shouldBe 404
  }

  "enum mixed auto and explicit" in {
    eval(
      """enum Flags
        |    A
        |    B
        |    C = 10
        |    D
        |    E
        |
        |main() -> int = Flags.D
        |""".stripMargin) shouldBe 11
  }

  // ===== Enum in expressions =====

  "enum in arithmetic" in {
    eval(
      """enum Op
        |    Add = 1
        |    Sub = 2
        |    Mul = 3
        |
        |main() -> int = Op.Add + Op.Mul
        |""".stripMargin) shouldBe 4
  }

  "enum in comparison" in {
    eval(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |main() -> int
        |    x = Color.Green
        |    if x == 1 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "enum as variable initializer" in {
    eval(
      """enum Dir
        |    Up
        |    Down
        |    Left
        |    Right
        |
        |main() -> int
        |    d = Dir.Left
        |    d
        |""".stripMargin) shouldBe 2
  }

  "enum in function argument" in {
    eval(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |
        |is_green(c: int) -> int
        |    if c == Color.Green then 1 else 0
        |
        |main() -> int = is_green(Color.Green)
        |""".stripMargin) shouldBe 1
  }

  "enum in switch-like if chain" in {
    eval(
      """enum Op
        |    Add = 1
        |    Sub = 2
        |    Mul = 3
        |
        |compute(op: int, a: int, b: int) -> int
        |    if op == Op.Add then a + b
        |    elif op == Op.Sub then a - b
        |    elif op == Op.Mul then a * b
        |    else 0
        |
        |main() -> int = compute(Op.Mul, 6, 7)
        |""".stripMargin) shouldBe 42
  }

  // ===== Error cases =====

  "enum unknown member is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """enum Color
        |    Red
        |    Green
        |
        |main() -> int = Color.Blue
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "duplicate enum is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """enum Color
        |    Red
        |
        |enum Color
        |    Blue
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== Simple enum usable as a distinct type =====

  "simple enum usable as generic type argument" in {
    eval(
      """enum Result[T, E]
        |    Ok(value: T)
        |    Err(error: E)
        |
        |enum ParseError
        |    EmptyInput
        |    BadDigit
        |    Overflow
        |
        |is_ok[T, E](r: Result[T, E]) -> bool
        |    r match
        |        Ok(_) -> true
        |        Err(_) -> false
        |
        |main() -> int
        |    val ok: Result[int, ParseError] = Ok(42)
        |    val err: Result[int, ParseError] = Err(BadDigit)
        |    var n = 0
        |    if is_ok(ok)  then n = n + 1
        |    if is_ok(err) then n = n + 100
        |    n
        |""".stripMargin) shouldBe 1
  }

  "simple enum bare variant constructs value of enum type" in {
    eval(
      """enum Status
        |    Active
        |    Pending
        |    Closed
        |
        |describe(s: Status) -> int
        |    s match
        |        Active -> 1
        |        Pending -> 2
        |        Closed -> 3
        |
        |main() -> int
        |    describe(Pending)
        |""".stripMargin) shouldBe 2
  }

  "qualified simple enum access still returns int constant" in {
    // Backward compat: TestError.BadDigit still works as an int value.
    eval(
      """enum ParseError
        |    EmptyInput
        |    BadDigit
        |    Overflow
        |
        |main() -> int
        |    ParseError.BadDigit
        |""".stripMargin) shouldBe 1
  }
}
