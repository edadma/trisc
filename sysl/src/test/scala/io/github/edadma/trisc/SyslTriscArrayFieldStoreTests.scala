package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pin for fixed-size array as a struct field on TRISC.
  *
  * Bug: emitStore had no ArrayType case, so any path that copied a struct
  * containing a fixed-size array field (e.g. `BufReader { buf: [1024]byte }`)
  * threw `emitStore: unexpected type [N]u8`. Fixed by adding an ArrayType
  * branch that delegates to emitAggregateCopy with the array's total size and
  * element-derived alignment.
  */
class SyslTriscArrayFieldStoreTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "struct with fixed-size byte array field codegens" in {
    val out = asm(
      """struct Buf
        |    data: [16]byte
        |    pos: int
        |
        |make() -> Buf
        |    var b: Buf
        |    b.pos = 0
        |    return b
        |
        |use() -> int
        |    val b = make()
        |    return b.pos
        |""".stripMargin)
    out should include("# function: make")
    out should include("# function: use")
  }

  "struct with fixed-size int array field codegens" in {
    val out = asm(
      """struct Histogram
        |    counts: [8]int
        |    total: int
        |
        |make() -> Histogram
        |    var h: Histogram
        |    h.total = 0
        |    return h
        |
        |use() -> int
        |    val h = make()
        |    return h.total
        |""".stripMargin)
    out should include("# function: make")
  }
}
