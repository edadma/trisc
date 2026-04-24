package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Optional Scala-3-style `end <kw>` terminators on every block construct.
  * The end marker is always optional — these tests pin both that the marker
  * parses and that omitting it still works (no regression). */
class SyslEndMarkerTests extends SyslTestHelpers {

  // ===== Control-flow statements =====

  "end if (block form)" in {
    eval(
      """main() -> int
        |    x = 0
        |    if true
        |        x = 42
        |    end if
        |    x
        |""".stripMargin) shouldBe 42
  }

  "end if (if/elif/else chain)" in {
    eval(
      """main() -> int
        |    x = 0
        |    if false
        |        x = 1
        |    elif false
        |        x = 2
        |    else
        |        x = 3
        |    end if
        |    x
        |""".stripMargin) shouldBe 3
  }

  "end while" in {
    eval(
      """main() -> int
        |    i = 0
        |    while i < 5
        |        i += 1
        |    end while
        |    i
        |""".stripMargin) shouldBe 5
  }

  "end for" in {
    eval(
      """main() -> int
        |    sum = 0
        |    for i = 1; i <= 4; i++
        |        sum += i
        |    end for
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "end match (statement context)" in {
    eval(
      """main() -> int
        |    x = 2
        |    r = x match
        |        1 -> 10
        |        2 -> 20
        |        else -> 0
        |    end match
        |    r
        |""".stripMargin) shouldBe 20
  }

  "end loop" in {
    eval(
      """main() -> int
        |    i = 0
        |    loop
        |        if i == 7 then break
        |        i += 1
        |    end loop
        |    i
        |""".stripMargin) shouldBe 7
  }

  // ===== Declarations =====

  "end struct" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |end struct
        |
        |main() -> int
        |    p: Point
        |    p.x = 3
        |    p.y = 4
        |    p.x + p.y
        |""".stripMargin) shouldBe 7
  }

  "end enum (simple)" in {
    eval(
      """enum Color
        |    Red
        |    Green
        |    Blue
        |end enum
        |
        |main() -> int = Color.Green
        |""".stripMargin) shouldBe 1
  }

  "end interface" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """interface Closer
        |    close() -> int
        |end interface
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.head shouldBe a[InterfaceDeclAST]
  }

  "end trait" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """trait Show[T]
        |    show(self: T) -> string
        |end trait
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls.head shouldBe a[TraitDeclAST]
  }

  "end impl" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """trait Show[T]
        |    show(self: T) -> int
        |end trait
        |
        |impl Show[int]
        |    show(self: int) -> int = self
        |end impl
        |
        |main() -> int = 0
        |""".stripMargin): @unchecked
    ast.decls(1) shouldBe a[ImplDeclAST]
  }

  // ===== Omitted form (regression: marker truly optional) =====

  "all without end markers (regression)" in {
    eval(
      """struct Box
        |    n: int
        |
        |enum Mode
        |    On
        |    Off
        |
        |main() -> int
        |    b: Box
        |    b.n = 0
        |    if true
        |        b.n = 1
        |    while b.n < 3
        |        b.n += 1
        |    for i = 0; i < 2; i++
        |        b.n += i
        |    loop
        |        if b.n >= 10 then break
        |        b.n += 1
        |    b.n
        |""".stripMargin) shouldBe 10
  }

  // ===== Mismatched end keyword (parser must reject if marker present) =====

  "wrong end keyword fails to parse" in {
    val res = (new SyslParser).parseProgram(
      """main() -> int
        |    while true
        |        break
        |    end for
        |    0
        |""".stripMargin)
    res.isLeft shouldBe true
  }
}
