package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for `saturating_add`/`sub`/`mul` on 64-bit types in the
  * TRISC backend.
  *
  * Audit item #12: codegen previously threw "saturating_* on 64-bit types is
  * not yet supported". The narrow-width signed-clamp path doesn't work for i64
  * or u64 because operands already span the full i64 range, so we use overflow
  * detection on the wrapped result instead. Six implementations: i64/u64 ×
  * add/sub/mul.
  *
  * Sysl does not depend on the trisc emulator from this project, so we pin the
  * asm shape rather than runtime semantics. End-to-end execution waits on
  * audit item #19 (TRISC test runner wire-up).
  */
class SyslTriscSaturating64Tests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  "saturating_* on i64/u64 no longer throws" - {
    "u64 add" in {
      noException should be thrownBy asm("f(a: u64, b: u64) -> u64 = saturating_add(a, b)\n")
    }
    "u64 sub" in {
      noException should be thrownBy asm("f(a: u64, b: u64) -> u64 = saturating_sub(a, b)\n")
    }
    "u64 mul" in {
      noException should be thrownBy asm("f(a: u64, b: u64) -> u64 = saturating_mul(a, b)\n")
    }
    "i64 add" in {
      noException should be thrownBy asm("f(a: i64, b: i64) -> i64 = saturating_add(a, b)\n")
    }
    "i64 sub" in {
      noException should be thrownBy asm("f(a: i64, b: i64) -> i64 = saturating_sub(a, b)\n")
    }
    "i64 mul" in {
      noException should be thrownBy asm("f(a: i64, b: i64) -> i64 = saturating_mul(a, b)\n")
    }
  }

  "u64 saturating_add emits add + unsigned overflow detect" in {
    val out = asm("f(a: u64, b: u64) -> u64 = saturating_add(a, b)\n")
    out should include("add r1, r1, r2")
    // overflow check: sum < a unsigned
    out should include("sltu r4, r1, r3")
    // overflow → MAX_U64 (loaded as -1 via constant pool)
    out should include("ldc r1, -1")
  }

  "u64 saturating_sub emits sub + unsigned underflow detect" in {
    val out = asm("f(a: u64, b: u64) -> u64 = saturating_sub(a, b)\n")
    out should include("sub r1, r1, r2")
    // underflow check happens before the sub
    out should include("sltu r4, r1, r2")
    // underflow → 0
    out should include("ldi r1, 0")
  }

  "u64 saturating_mul emits mulhu/mul and tests high-half non-zero" in {
    val out = asm("f(a: u64, b: u64) -> u64 = saturating_mul(a, b)\n")
    // Post-Stage-2 ISA: mulu removed; compute high (mulhu, destructive)
    // and low (mul) separately. r3 is the high-half temp.
    out should include("mov r3, r1")
    out should include("mulhu r3, r2")
    out should include("mul r1, r1, r2")
    // overflow iff r3 != 0
    out should (include regex "beq r3, r0, \\.sat_noof_")
    out should include("ldc r1, -1")
  }

  "i64 saturating_add emits add + signed XOR overflow detect" in {
    val out = asm("f(a: i64, b: i64) -> i64 = saturating_add(a, b)\n")
    out should include("add r1, r1, r2")
    // XOR-trick signature. The second XOR reuses r2 (held b but no longer
    // needed) — NOT r5, which is the frame pointer. Earlier codegen wrote
    // r5 here and corrupted FP; the regression of that bug is the
    // saturating_64bit_and_u32mul.lsysl runtime tests.
    out should include("xor r4, r3, r1")
    out should include("xor r2, r2, r1")
    out should include("and r4, r4, r2")
    // signed compare against r0 (= 0) — must NOT use sltu
    out should include("slt r4, r4, r0")
    // Guard against future regression: the second XOR's destination must
    // not be r5 (the frame pointer). The old buggy form was
    // `xor r5, r2, r1` / `xor r5, r3, r1` — explicitly forbid those.
    out shouldNot include("xor r5, r2, r1")
    out shouldNot include("xor r5, r3, r1")
    // both saturation directions present
    out should include regex "ldc r1, 9223372036854775807"
    out should include regex "ldc r1, -9223372036854775808"
  }

  "i64 saturating_sub emits sub + signed XOR overflow detect" in {
    val out = asm("f(a: i64, b: i64) -> i64 = saturating_sub(a, b)\n")
    out should include("sub r1, r1, r2")
    // XOR trick for sub: ((a ^ b) & (a ^ result)). Second XOR target is
    // r2, not r5. Same FP-preservation as saturating_add.
    out should include("xor r4, r3, r2")
    out should include("xor r2, r3, r1")
    out should include("and r4, r4, r2")
    out should include("slt r4, r4, r0")
    // Guard against future regression: the second XOR's destination must
    // not be r5 (the frame pointer). Old buggy form was `xor r5, r3, r1`.
    out shouldNot include("xor r5, r3, r1")
    out shouldNot include("xor r5, r2, r1")
    out should include regex "ldc r1, 9223372036854775807"
    out should include regex "ldc r1, -9223372036854775808"
  }

  "i64 saturating_mul emits mulh/mul + sign-extension high-half compare" in {
    val out = asm("f(a: i64, b: i64) -> i64 = saturating_mul(a, b)\n")
    // Post-Stage-2 ISA: compute high (mulh, destructive) and low (mul) separately.
    out should include("mov r3, r1")
    out should include("mulh r3, r2")
    out should include("mul r1, r1, r2")
    // expected high = asr(low, 63), into r4 (using r4 as temp for the literal)
    out should include("ldi r4, 63")
    out should include("asr r4, r1, r4")
    // compare actual high (r3) against expected high (r4)
    out should (include regex "beq r4, r3, \\.sat_noof_")
    // signed direction probe on the actual high
    out should include("slt r4, r3, r0")
    out should include regex "ldc r1, 9223372036854775807"
    out should include regex "ldc r1, -9223372036854775808"
  }

  "i64 paths use signed slt — never sltu — for sign tests" in {
    // Carve out just the i64-mul saturating section so we don't pick up sltu
    // from elsewhere in the program.
    val out = asm("f(a: i64, b: i64) -> i64 = saturating_mul(a, b)\n")
    val sat = out.linesIterator.dropWhile(!_.contains("mulh r3, r2"))
      .takeWhile(!_.contains("jalr r0, r6"))
      .mkString("\n")
    sat should not include "sltu"
  }

  "u64 paths use unsigned sltu — never slt — for the overflow tests" in {
    val out = asm("f(a: u64, b: u64) -> u64 = saturating_add(a, b)\n")
    val sat = out.linesIterator.dropWhile(!_.contains("add r1, r1, r2"))
      .takeWhile(!_.contains("jalr r0, r6"))
      .mkString("\n")
    // u64 sat_add uses sltu r4, r1, r3 — and shouldn't have any `slt r4` (signed)
    sat should not include "slt r4"
  }

  "narrow signed saturating_mul i32 still uses signed slt clamp (regression guard)" in {
    val out = asm("smul(a: int, b: int) -> int = saturating_mul(a, b)\n")
    out should include("mul r1, r1, r2")
    out should include("slt r4")
  }

  "narrow saturating_add u32 still uses signed slt clamp (regression guard)" in {
    val out = asm("sadd(a: u32, b: u32) -> u32 = saturating_add(a, b)\n")
    out should include("add r1, r1, r2")
    out should include("slt r4")
  }
}
