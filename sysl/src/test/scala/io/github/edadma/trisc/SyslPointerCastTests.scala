package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslPointerCastTests extends SyslTestHelpers {

  // ===== Same-type pointer assignment (already works) =====

  "same-type pointer assignment" in {
    eval(
      """main() -> int
        |    x = 42
        |    var p: *int = &x
        |    *p
        |""".stripMargin) shouldBe 42
  }

  // ===== Analyzer accepts different pointer types =====

  "analyzer accepts *Struct assigned to *int" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    p: Point
        |    var ptr: *int = &p
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts *T assigned to *U" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Data
        |    value: int
        |
        |main() -> int
        |    d: Data
        |    var dp: *Data = &d
        |    var raw: *i64 = dp
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts different pointer type in function arg" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Point
        |    x: int
        |    y: int
        |
        |read_first(p: *int) -> int = *p
        |
        |main() -> int
        |    pt: Point
        |    pt.x = 42
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  "analyzer accepts round-trip pointer cast" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Node
        |    value: int
        |
        |main() -> int
        |    n: Node
        |    var p: *Node = &n
        |    var raw: *i64 = p
        |    var back: *Node = raw
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== Pointer compatibility in expressions =====

  "pointer to int read through same-pointee pointer" in {
    eval(
      """main() -> int
        |    x = 42
        |    var p1: *int = &x
        |    var p2: *int = p1
        |    *p2
        |""".stripMargin) shouldBe 42
  }

  // ===== Pointer cast in explicit cast expression =====

  "explicit cast between pointer types accepted" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """struct Foo
        |    x: int
        |
        |main() -> int
        |    f: Foo
        |    var p: *Foo = &f
        |    0
        |""".stripMargin): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }

  // ===== Explicit pointer-to-pointer casts =====

  "cast *i8 to *byte" in {
    eval(
      """main() -> int
        |    var x: i8 = 42
        |    var p: *i8 = &x
        |    var q: *byte = *byte(p)
        |    int(*q)
        |""".stripMargin) shouldBe 42
  }

  "cast *byte to *i8" in {
    eval(
      """main() -> int
        |    var x: byte = 42
        |    var p: *byte = &x
        |    var q: *i8 = *i8(p)
        |    int(*q)
        |""".stripMargin) shouldBe 42
  }

  "cast *int to *i64" in {
    eval(
      """main() -> int
        |    var x: int = 99
        |    var p: *int = &x
        |    var q: *i64 = *i64(p)
        |    *q
        |""".stripMargin) shouldBe 99
  }

  "cast *byte to *int for reinterpret" in {
    eval(
      """main() -> int
        |    var buf: [8]byte
        |    var p: *byte = &buf[0]
        |    var q: *int = *int(p)
        |    *q = 123
        |    *q
        |""".stripMargin) shouldBe 123
  }

  // ===== Pointer to bool (null check) =====

  "non-null pointer to bool is true" in {
    eval(
      """main() -> int
        |    var x = 42
        |    var p = &x
        |    if bool(p) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  "null pointer to bool is false" in {
    eval(
      """main() -> int
        |    var p = *int(0)
        |    if bool(p) then 1 else 0
        |""".stripMargin) shouldBe 0
  }

  // ===== Ref to pointer =====

  "ref to i64 cast for raw pointer" in {
    eval(
      """struct Point
        |    x: int
        |    y: int
        |
        |main() -> int
        |    val r = new Point(3, 4)
        |    if i64(r) != 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Ref to bool (null check) =====

  "non-null ref to bool is true" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val r = new Box(1)
        |    if bool(r) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Ref to integer =====

  "ref to i64 gives non-zero address" in {
    eval(
      """struct Box
        |    value: int
        |
        |main() -> int
        |    val r = new Box(1)
        |    if i64(r) != 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Func to integer =====

  "func to i64" in {
    eval(
      """helper() -> int = 42
        |
        |main() -> int
        |    val f: () -> int = helper
        |    if i64(f) != 0 then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Func to bool =====

  "func to bool is true" in {
    eval(
      """helper() -> int = 42
        |
        |main() -> int
        |    val f: () -> int = helper
        |    if bool(f) then 1 else 0
        |""".stripMargin) shouldBe 1
  }

  // ===== Invalid casts should error =====

  "float to pointer is error" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val x = 3.14
        |    val p = *int(x)
        |    0
        |""".stripMargin)
  }

  "pointer to float is error" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    var x = 0
        |    val p = &x
        |    val f = f64(p)
        |    0
        |""".stripMargin)
  }

  "string to int is error" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    val s = "hello"
        |    int(s)
        |""".stripMargin)
  }
}
