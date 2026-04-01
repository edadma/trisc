package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTypeCheckTests extends SyslTestHelpers {

  // ===== Pointer cast syntax (analyzer-level — interpreter doesn't support ptr casts) =====

  "pointer cast *i64 on int expression passes analysis" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var buf: [2]i64
        |    val addr: int = int(&buf)
        |    var p: *i64 = *i64(addr)
        |    *p = 42
        |    buf[0]
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "pointer cast *i8 on int expression passes analysis" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var buf: [8]i8
        |    val addr: int = int(&buf)
        |    var p: *i8 = *i8(addr)
        |    *p = 7
        |    buf[0]
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== Argument type checking =====

  "function arg type mismatch is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: int) -> int = x
        |main() -> int = f(true)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "function arity mismatch too few is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: int, y: int) -> int = x + y
        |main() -> int = f(1)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "function arity mismatch too many is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(x: int) -> int = x
        |main() -> int = f(1, 2)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "function arg with matching type is accepted" in {
    eval(
      """f(x: int) -> int = x * 2
        |main() -> int = f(21)
        |""".stripMargin) shouldBe 42
  }

  "integer literal coerced to match parameter type" in {
    eval(
      """f(x: i64) -> int = int(x)
        |main() -> int = f(42)
        |""".stripMargin) shouldBe 42
  }

  // ===== Char vs string literal parsing =====

  "single-quoted char is an integer" in {
    eval("main() -> int = 'A'\n") shouldBe 65
  }

  "double-quoted single char is a string not a char" in {
    output(
      """main() -> int
        |    puts("a")
        |    0
        |""".stripMargin) shouldBe "a"
  }

  "single-quoted escape char works" in {
    eval("main() -> int = '\\n'\n") shouldBe 10
  }

  "single-quoted null char works" in {
    eval("main() -> int = '\\0'\n") shouldBe 0
  }

  "single-quoted backslash works" in {
    eval("main() -> int = '\\\\'\n") shouldBe 92
  }

  // ===== Null pointer coercion =====

  "literal 0 accepted as pointer argument" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(p: *int) -> int = 0
        |main() -> int = f(0)
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "non-zero literal rejected as pointer argument" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(p: *int) -> int = 0
        |main() -> int = f(42)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== String → *byte compatibility =====

  "string accepted where *i8 expected in analysis" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(s: *i8) -> int = 0
        |main() -> int = f("hello")
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "string rejected where *i32 expected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """f(s: *i32) -> int = 0
        |main() -> int = f("hello")
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== FuncType → int compatibility =====

  "function ref accepted where int expected in analysis" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """target() -> int = 42
        |take_addr(addr: int) -> int = addr
        |main() -> int = take_addr(target)
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "function ref rejected where bool expected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """target() -> int = 42
        |take_flag(b: bool) -> int = 0
        |main() -> int = take_flag(target)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  // ===== Signed ↔ unsigned compatibility =====

  "int assigned to u32 variable is allowed" in {
    eval(
      """main() -> int
        |    var a: int = 5
        |    var b: u32 = a
        |    int(b)
        |""".stripMargin) shouldBe 5
  }

  "u32 assigned to int variable is allowed" in {
    eval(
      """main() -> int
        |    var a: u32 = 5
        |    var b: int = a
        |    b
        |""".stripMargin) shouldBe 5
  }

  // ===== Int ↔ pointer rejection =====

  "implicit int to pointer is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var p: *int = 42
        |    0
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "implicit pointer to int is rejected" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    var n: int = &x
        |    n
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "explicit int(ptr) cast passes analysis" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    val addr = int(&x)
        |    addr
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "explicit *i64(int) cast passes analysis" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    var x: i64 = 99
        |    val addr = int(&x)
        |    var p: *i64 = *i64(addr)
        |    *p
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }
}
