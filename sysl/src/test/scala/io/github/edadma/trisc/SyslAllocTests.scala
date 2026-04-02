package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SyslAllocTests extends AnyFreeSpec with Matchers {

  private val allocSource = scala.io.Source.fromFile("sysl/lib/alloc.sysl").mkString

  "allocator parses" in {
    val result = (new SyslParser).parseProgram(allocSource)
    result shouldBe a[Right[_, _]]
  }

  "allocator analyzes" in {
    val Right(ast) = (new SyslParser).parseProgram(allocSource): @unchecked
    (new SyslAnalyzer).analyze(ast) // should not throw
  }
}
