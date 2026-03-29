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
}
