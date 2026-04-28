package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Unit-level coverage for `SyslSVMCodegen`.
  *
  * The `sysl` cross-project does not depend on `svm`, so we cannot link and
  * execute SVM bytecode here — that path lives in `sysl-cli`. What this suite
  * does cover is the codegen contract: every `TExpr` / `TStmt` produced by the
  * analyzer for a representative input must be lowerable to assembly without
  * tripping the unhandled-node fall-throughs in `genExpr` / `genStmt`.
  *
  * Both fall-throughs were silent miscompiles before sysl@3ada8ece (genExpr
  * pushed `0`, genStmt did nothing). They now `sys.error`. These tests pin
  * the happy path so any future TExpr/TStmt that the analyzer starts emitting
  * — and codegen forgets — breaks the build immediately rather than silently
  * generating wrong code.
  */
class SyslSVMCodegenTests extends AnyFreeSpec with Matchers {

  private def codegen(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslSVMCodegen).generate(typed)

  "arithmetic and comparisons lower without hitting genExpr fallback" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    val a = 2 + 3 * 4
        |    val b = a - 1
        |    val c = b / 2
        |    val d = c % 3
        |    if a < b || c >= d then 0 else 1
        |""".stripMargin)
  }

  "unsigned arithmetic and bitwise ops lower" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    var x: u32 = 0xDEADBEEF
        |    var y: u32 = x & 0xFFFF
        |    var z: u32 = (x >> 16) | y
        |    if z == x then 0 else 1
        |""".stripMargin)
  }

  "control-flow stmts lower without hitting genStmt fallback" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    var sum = 0
        |    for i in 1..10
        |        sum = sum + i
        |    var n = 0
        |    while sum > 0
        |        sum = sum - 1
        |        n = n + 1
        |    n
        |""".stripMargin)
  }

  "do-while + break + continue lower" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    var i = 0
        |    do
        |        i = i + 1
        |        if i == 3 then continue
        |        if i == 7 then break
        |    while i < 100
        |    i
        |""".stripMargin)
  }

  "match on tagged union lowers" in {
    noException should be thrownBy codegen(
      """enum Shape
        |    Circle(radius: int)
        |    Square(side: int)
        |
        |area(s: Shape) -> int
        |    s match
        |        Circle(r) -> r * r * 3
        |        Square(side) -> side * side
        |
        |main() -> int = area(Circle(5))
        |""".stripMargin)
  }

  "struct construction, field access, field assignment lower" in {
    noException should be thrownBy codegen(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    var p = Point(3, 4)
        |    p.x = p.x + 1
        |    p.x + p.y
        |""".stripMargin)
  }

  "pre/post inc/dec on locals and fields lower" in {
    noException should be thrownBy codegen(
      """struct Box
        |    n: int
        |
        |main() -> int
        |    var i = 0
        |    var b = Box(10)
        |    ++i
        |    i++
        |    --b.n
        |    b.n--
        |    i + b.n
        |""".stripMargin)
  }

  "chained comparison desugar lowers cleanly" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    val x = 5
        |    if 1 <= x <= 10 then 0 else 1
        |""".stripMargin)
  }

  "string literals and interpolation lower" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    val name = "world"
        |    val greeting = s"hello, ${name}"
        |    len(greeting)
        |""".stripMargin)
  }

  "function calls including recursion lower" in {
    noException should be thrownBy codegen(
      """fact(n: int) -> int
        |    if n <= 1 then 1 else n * fact(n - 1)
        |
        |main() -> int = fact(5)
        |""".stripMargin)
  }

  "closures and higher-order calls lower" in {
    noException should be thrownBy codegen(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int
        |    val adder: (int) -> int = y -> y + 100
        |    apply(adder, 7)
        |""".stripMargin)
  }

  "sizeof and bool literals lower" in {
    noException should be thrownBy codegen(
      """main() -> int
        |    val s = sizeof(int)
        |    val t = sizeof(i64)
        |    val b: bool = true
        |    if b then s + t else 0
        |""".stripMargin)
  }
}
