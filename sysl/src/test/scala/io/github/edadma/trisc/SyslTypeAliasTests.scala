package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTypeAliasTests extends SyslTestHelpers {

  // ===== Basic type aliases =====

  "type alias for primitive" in {
    eval(
      """type Size = u32
        |
        |main() -> int
        |    var s: Size = 42
        |    int(s)
        |""".stripMargin) shouldBe 42
  }

  "type alias for signed int" in {
    eval(
      """type Score = i32
        |
        |main() -> int
        |    var s: Score = -10
        |    s
        |""".stripMargin) shouldBe -10
  }

  "type alias for pointer" in {
    eval(
      """type IntPtr = *int
        |
        |main() -> int
        |    x = 42
        |    var p: IntPtr = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  "type alias in function signature" in {
    eval(
      """type Num = i32
        |
        |double_it(x: Num) -> Num = x * 2
        |
        |main() -> int = double_it(21)
        |""".stripMargin) shouldBe 42
  }

  "multiple type aliases" in {
    eval(
      """type Width = u32
        |type Height = u32
        |
        |area(w: Width, h: Height) -> u32 = w * h
        |
        |main() -> int = int(area(6, 7))
        |""".stripMargin) shouldBe 42
  }

  "chained type aliases" in {
    eval(
      """type A = i32
        |type B = A
        |
        |main() -> int
        |    var x: B = 99
        |    x
        |""".stripMargin) shouldBe 99
  }

  // ===== Uninitialized declarations via type alias =====

  "type alias for array — uninitialized" in {
    eval(
      """type Vec3 = [3]int
        |
        |main() -> int
        |    v: Vec3
        |    v[0] = 10
        |    v[1] = 20
        |    v[2] = 30
        |    v[0] + v[1] + v[2]
        |""".stripMargin) shouldBe 60
  }

  "type alias for struct — uninitialized" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |type Pos = Point
        |
        |main() -> int
        |    p: Pos
        |    p.x = 10
        |    p.y = 20
        |    p.x + p.y
        |""".stripMargin) shouldBe 30
  }

  "type alias for scalar — uninitialized" in {
    eval(
      """type Count = i32
        |
        |main() -> int
        |    c: Count
        |    c
        |""".stripMargin) shouldBe 0
  }

  // ===== Error cases =====

  "duplicate type alias is error" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """type Foo = i32
        |type Foo = u32
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
