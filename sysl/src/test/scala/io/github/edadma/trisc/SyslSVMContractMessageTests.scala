package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Audit item #17 — SVM contract checks (require/ensure/invariant/type
 *  predicate/type attribute/variant/assume) used to compile to a bare
 *  `halt` opcode, discarding both the kind and the message. The fix at
 *  sysl@<TBD> emits `; <kind>: <message>` as an asm comment immediately
 *  above the failure path, then traps with `trap 1`. The `trap u8`
 *  opcode (0x6A) calls `handleTrap(num)`; the default impl halts on
 *  any non-zero number, but a debugging harness can override it to
 *  recover the kind. The asm comment carries the message verbatim
 *  for postmortem inspection.
 *
 *  These tests pin the asm shape so a future SVM codegen change can't
 *  silently revert to bare-halt without a failing test.
 */
class SyslSVMContractMessageTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslSVMCodegen).generate(typed)

  "require message survives in asm comment + trap 1 emitted" in {
    val out = asm(
      """f(x: int) -> int
        |    require x > 0, "x must be positive"
        |    x + 1
        |
        |main() -> int = f(5)
        |""".stripMargin)
    out should include("; precondition: x must be positive")
    out should include("trap 1")
  }

  "ensure message survives in asm comment + trap 1 emitted" in {
    val out = asm(
      """f(x: int) -> int
        |    ensure result > x, "result must exceed x"
        |    x + 1
        |
        |main() -> int = f(5)
        |""".stripMargin)
    out should include("; postcondition: result must exceed x")
    out should include("trap 1")
  }

  "kind without custom message: comment is bare kind" in {
    val out = asm(
      """f(x: int) -> int
        |    require x > 0
        |    x + 1
        |
        |main() -> int = f(5)
        |""".stripMargin)
    out should include("; precondition")
    out should include("trap 1")
  }

  "no bare halt remains in contract path (regression guard)" in {
    val out = asm(
      """f(x: int) -> int
        |    require x > 0, "msg"
        |    x
        |
        |main() -> int = f(1)
        |""".stripMargin)
    // The contract should now use trap 1, not halt. (main itself ends
    // with halt as the program-exit instruction; this check just
    // confirms the require lowering doesn't include halt anymore.
    // We do this by checking that the precondition comment is followed
    // by trap, not halt.)
    val lines = out.linesIterator.toList
    val msgIdx = lines.indexWhere(_.contains("; precondition: msg"))
    msgIdx should be >= 0
    val nextNonEmpty = lines.drop(msgIdx + 1).find(_.trim.nonEmpty).getOrElse("")
    nextNonEmpty should include("trap")
    nextNonEmpty should not include "halt"
  }
}
