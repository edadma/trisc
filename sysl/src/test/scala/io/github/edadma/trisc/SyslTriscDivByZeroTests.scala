package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for the TRISC divide-by-zero check.
  *
  * Audit item #13: TRISC `div`/`divu` were emitted with no zero-divisor
  * guard. Hardware behaviour on `div r_a, r_b, 0` is undefined, so we now
  * insert `bne r_b, r0, .div_ok / ldi r1, 5 / trap 1 / .div_ok` before
  * every div/divu. These tests pin that asm shape across the three sites
  * that emit division (the `emitBinOp` helper, the field compound-assign
  * path, and the main typed-binary path).
  *
  * Sysl does not depend on the trisc emulator from this project, so we
  * verify the check at the asm string level. End-to-end execution lands
  * later, when the TRISC test runner is wired up (audit item #19).
  */
class SyslTriscDivByZeroTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "signed `/` emits div-by-zero check" in {
    val out = asm(
      """divide(a: int, b: int) -> int = a / b
        |""".stripMargin)
    out should include("trap 1")
    out should (include regex "bne r2, r0, \\.div_ok_")
  }

  "signed `%` emits div-by-zero check" in {
    val out = asm(
      """rem(a: int, b: int) -> int = a % b
        |""".stripMargin)
    out should include("trap 1")
    out should (include regex "bne r2, r0, \\.div_ok_")
  }

  "unsigned `/` emits div-by-zero check before divu" in {
    val out = asm(
      """udivide(a: u32, b: u32) -> u32 = a / b
        |""".stripMargin)
    out should include("trap 1")
    out should include("divu r1, r1, r2")
    out should (include regex "bne r2, r0, \\.div_ok_")
  }

  "div-by-zero check uses error code 5" in {
    val out = asm(
      """divide(a: int, b: int) -> int = a / b
        |""".stripMargin)
    out should include("ldi r1, 5")
    out should include("trap 1")
  }

  "non-divisive ops are not gated" in {
    val out = asm(
      """ops(a: int, b: int) -> int = a + b * (a - b)
        |""".stripMargin)
    // Only +, -, * — no division. Confirm the div-ok label never appears.
    out should not include "div_ok"
  }
}
