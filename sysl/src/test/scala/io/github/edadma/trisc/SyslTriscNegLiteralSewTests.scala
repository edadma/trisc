package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression for the TRISC sign-extension bug surfaced while wiring contracts
 *  on inner defs (Tier 4 followup #2). Pre-existing on TRISC backend.
 *
 *  TRISC's `movi r1, $imm` takes a 32-bit unsigned immediate and writes it to
 *  the 64-bit register zero-extended. The codegen for `TIntLit(-3, IntType(32))`
 *  used to emit `movi r1, 4294967293` (the i32 bit pattern of -3 read as
 *  unsigned), giving r1 = `0x00000000FFFFFFFD` — NOT the sign-extended -3
 *  (`0xFFFFFFFFFFFFFFFD`) that the rest of the codegen expects.
 *
 *  Symptom: any 64-bit-wide compare that mixes a negative i32 literal with a
 *  value loaded from memory (which `ldw` sign-extends correctly) reads them
 *  as unequal. Showed up first as `ensure result == x + x` trapping for x<0
 *  in a top-level fn — `result` was loaded via ldw (sign-extended), `x + x`
 *  was loaded the same way and added (correct in 64-bit), but the value of
 *  `x` passed at the call site came from a negative-literal `movi` that
 *  zero-extended.
 *
 *  Fix: emit `sew r1, r1` (sign-extend word) after `movi` when the literal
 *  value is negative. Non-negative values in 0..0xFFFFFFFF skip the sew —
 *  that range covers both unsigned u32 max and positive i32 values, which
 *  both want movi's zero-extended representation.
 *
 *  Static check: scan the generated asm; verify every `movi r1, N` that
 *  represents a negative i32 literal is followed by a `sew r1, r1`.
 *  Runtime check would require the full TRISC harness; the asm regression
 *  is enough to pin the discipline. */
class SyslTriscNegLiteralSewTests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  /** Return true iff `out` contains a `movi r1, $unsigned` immediately
   *  followed by `sew r1, r1`, where `$unsigned` is the 32-bit unsigned
   *  representation of the given negative `Int` value (e.g. -3 → 4294967293). */
  private def hasMoviSewPair(out: String, negI32Value: Int): Boolean =
    val unsigned = negI32Value.toLong & 0xFFFFFFFFL
    val lines = out.linesIterator.toIndexedSeq
    val moviPat = s"^\\s*movi\\s+r1,\\s+$unsigned\\s*$$".r
    val sewPat = "^\\s*sew\\s+r1,\\s+r1\\s*$".r
    val moviIdxs = lines.zipWithIndex.collect { case (l, i) if moviPat.matches(l) => i }
    moviIdxs.exists(i => i + 1 < lines.length && sewPat.matches(lines(i + 1)))

  "negative i32 literal -1 emits movi + sew pair" in {
    val out = asm(
      """main() -> int
        |    val x: int = -1
        |    x
        |""".stripMargin)
    hasMoviSewPair(out, -1) shouldBe true
  }

  "negative i32 literal -3 emits movi + sew pair" in {
    val out = asm(
      """main() -> int
        |    val x: int = -3
        |    x
        |""".stripMargin)
    hasMoviSewPair(out, -3) shouldBe true
  }

  "negative i32 literal at MIN_INT emits movi + sew pair" in {
    val out = asm(
      """main() -> int
        |    val x: int = -2147483648
        |    x
        |""".stripMargin)
    hasMoviSewPair(out, Int.MinValue) shouldBe true
  }

  "negative literal as call arg emits sew at call site" in {
    val out = asm(
      """twice(x: int) -> int = x + x
        |
        |main() -> int
        |    twice(-3)
        |""".stripMargin)
    hasMoviSewPair(out, -3) shouldBe true
  }

  "small positive literal does NOT emit sew (uses ldi)" in {
    // Values 0..255 use `ldi`, no sew involved at all.
    val out = asm(
      """main() -> int
        |    val x: int = 5
        |    x
        |""".stripMargin)
    out should not include "sew r1, r1"
  }

  "u32 max (0xFFFFFFFF) does NOT emit sew (zero-extension is correct)" in {
    // 0xFFFFFFFF is u32 max — Long.MaxValue says positive; we want zero-ext.
    val out = asm(
      """main() -> int
        |    val x: u32 = u32(0xFFFFFFFF)
        |    int(x)
        |""".stripMargin)
    val sewLines = out.linesIterator.count(_.matches("^\\s*sew\\s+r1,\\s+r1\\s*$"))
    // Allow zero or more sew lines unrelated to this literal, but the literal
    // 0xFFFFFFFF (as u32) must NOT trigger sew. Safer assertion: the asm has
    // a `movi r1, 4294967295` that's NOT followed by sew.
    val lines = out.linesIterator.toIndexedSeq
    val moviIdxs = lines.zipWithIndex.collect {
      case (l, i) if l.matches("^\\s*movi\\s+r1,\\s+4294967295\\s*$") => i
    }
    moviIdxs should not be empty
    moviIdxs.foreach { i =>
      if i + 1 < lines.length then
        lines(i + 1) should not include "sew r1, r1"
    }
  }

  "positive int 1000 (above ldi range) emits movi without sew" in {
    val out = asm(
      """main() -> int
        |    val x: int = 1000
        |    x
        |""".stripMargin)
    val lines = out.linesIterator.toIndexedSeq
    val moviIdxs = lines.zipWithIndex.collect {
      case (l, i) if l.matches("^\\s*movi\\s+r1,\\s+1000\\s*$") => i
    }
    moviIdxs should not be empty
    moviIdxs.foreach { i =>
      if i + 1 < lines.length then
        lines(i + 1) should not include "sew r1, r1"
    }
  }
}
