package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for `saturating_mul` on `u32` in the TRISC backend.
  *
  * Audit item #11: codegen previously threw "saturating_mul on u32 is not yet
  * supported" because the full u64 product `(2^32 - 1)^2` overflows the
  * signed-i64 clamp logic. Now u32*u32 uses `mul` to produce the low u64 of
  * the product — and because both inputs are zero-extended u32 in r1/r2,
  * the low 64 bits hold the full mathematical product. We then
  * unsigned-compare against u32 max. Post Stage-2 ISA there is no separate
  * `mulu` opcode — `mul`-low is identical signed/unsigned.
  *
  * Sysl does not depend on the trisc emulator from this project, so we pin
  * the asm shape rather than the runtime result. End-to-end semantics
  * verification waits on audit item #19 (TRISC test runner wire-up).
  */
class SyslTriscSaturatingMulU32Tests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "saturating_mul u32 no longer throws" in {
    noException should be thrownBy asm(
      """smul(a: u32, b: u32) -> u32 = saturating_mul(a, b)
        |""".stripMargin)
  }

  "saturating_mul u32 emits mul + unsigned compare against u32 max" in {
    val out = asm(
      """smul(a: u32, b: u32) -> u32 = saturating_mul(a, b)
        |""".stripMargin)
    out should include("mul r1, r1, r2")
    out should include("sltu r4, r3, r1")
    // u32 max = 0xFFFFFFFF = 4294967295. The codegen uses `movi` for
    // 32-bit unsigned constants (zero-extended).
    out should (include("movi r3, 4294967295") or include("ldc r3, 4294967295"))
  }

  "saturating_mul u32 has no signed slt clamp" in {
    val out = asm(
      """smul(a: u32, b: u32) -> u32 = saturating_mul(a, b)
        |""".stripMargin)
    // The u32-specific path uses sltu only — no signed slt on the saturating
    // branch (other ops elsewhere in the program may still have slt).
    val sat = out.linesIterator.dropWhile(!_.contains("mul r1, r1, r2"))
      .takeWhile(!_.contains("jalr r0, r6"))
      .mkString("\n")
    sat should not include "slt r4"
  }

  "signed saturating_mul i32 still uses signed slt clamp (regression guard)" in {
    val out = asm(
      """smul(a: int, b: int) -> int = saturating_mul(a, b)
        |""".stripMargin)
    // Signed path: still uses `mul` and signed slt clamp.
    out should include("mul r1, r1, r2")
    out should include("slt r4")
  }

  "saturating_add u32 still uses signed slt clamp (no regression on add)" in {
    val out = asm(
      """sadd(a: u32, b: u32) -> u32 = saturating_add(a, b)
        |""".stripMargin)
    // u32+u32 fits in signed i64 positive range; signed slt clamp is correct.
    out should include("add r1, r1, r2")
    out should include("slt r4")
  }
}
