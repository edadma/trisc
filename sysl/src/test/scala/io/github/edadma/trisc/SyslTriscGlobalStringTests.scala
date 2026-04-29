package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Pin for module-level string and array-of-string globals on TRISC.
  *
  * Bug: emitDataDirective threw `unexpected type string` when a module-level
  * `val x = "foo"` or `val xs: [N]string = [...]` reached the data segment.
  * Fixed by:
  *  - intercepting TStringLit init (scalar) and emitting a 16-byte
  *    `{ptr, len}` descriptor pointing at an interned string-literal blob;
  *  - intercepting `[N]string` array literals and emitting one `dl/dl` pair
  *    per element with per-element interning.
  *
  * Cluster: ~46 std/time test failures referenced in the post-item-#34
  * handoff (MONTHS_SHORT/LONG and WEEKDAYS_*).
  */
class SyslTriscGlobalStringTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "module-level scalar string val codegens" in {
    val out = asm(
      """val GREETING = "hello"
        |
        |use() -> int
        |    return len(GREETING)
        |""".stripMargin)
    out should include("# global: GREETING")
    // descriptor: ptr label + length
    out should include regex """dl __str_\d+""".r
    out should include("dl 5")
    // String body in rodata
    out should include("db 104") // 'h'
  }

  "module-level array of strings codegens" in {
    val out = asm(
      """val MONTHS: [3]string = ["Jan", "Feb", "Mar"]
        |
        |use() -> int
        |    return 0
        |""".stripMargin)
    out should include("# global: MONTHS")
    // 3 descriptors = 6 dl entries (ptr+len each)
    out should include("dl 3") // length of "Jan"/"Feb"/"Mar"
    out should include("db 74") // 'J' (first char of "Jan")
  }

  "module-level empty string val goes to bss" in {
    val out = asm(
      """val EMPTY = ""
        |
        |use() -> int
        |    return len(EMPTY)
        |""".stripMargin)
    out should include("segment bss")
    out should include("# global: EMPTY")
    out should include("rb 16") // 16-byte zero descriptor
  }
}
