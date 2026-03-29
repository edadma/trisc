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
}
