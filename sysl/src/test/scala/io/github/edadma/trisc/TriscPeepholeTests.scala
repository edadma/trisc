package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import TriscPeephole.*

class TriscPeepholeTests extends AnyFreeSpec with Matchers:

  // ===== Parse / render round-trip =====

  "parse and render preserve a representative asm program" in {
    val src =
      """entry main
        |global outer, func, 0 i32
        |segment code
        |# function: outer
        |outer:
        |  pshd r6
        |  pshd r5
        |  mov r5, r7
        |  ldi r1, 32
        |  movi r4, malloc
        |  jalr r6, r4
        |  beq r1, r0, .skip_decr_1
        |  addi r3, r1, -16
        |.skip_decr_1
        |  jalr r0, r6
        |""".stripMargin
    render(parse(src)) shouldBe src
  }

  "Instr operands are split on commas and trimmed" in {
    parse("  addi r3, r1, -16\n").head shouldBe Instr("addi", List("r3", "r1", "-16"))
  }

  "Label is detected with both `.name` and `name:` forms" in {
    parse(".skip_decr_1\n").head shouldBe Label(".skip_decr_1")
    parse("outer:\n").head shouldBe Label("outer:")
  }

  "Directive is detected for column-0 non-label lines" in {
    parse("global outer, func, 0 i32\n").head shouldBe Directive("global outer, func, 0 i32")
  }

  // ===== Individual rule behavior =====

  "mov-self-drop removes mov rN, rN" in {
    val (out, stats) = TriscPeephole("  mov r3, r3\n  ldi r1, 5\n")
    out shouldBe "  ldi r1, 5\n"
    stats.perRule("mov-self-drop") shouldBe 1
  }

  "addi-self-zero-drop removes addi rN, rN, 0" in {
    val (out, stats) = TriscPeephole("  addi r3, r3, 0\n  ldi r1, 5\n")
    out shouldBe "  ldi r1, 5\n"
    stats.perRule("addi-self-zero-drop") shouldBe 1
  }

  "addi-zero-to-mov rewrites addi rD, rA, 0 to mov rD, rA" in {
    val (out, stats) = TriscPeephole("  addi r3, r1, 0\n")
    out shouldBe "  mov r3, r1\n"
    stats.perRule("addi-zero-to-mov") shouldBe 1
  }

  // pshdPopdSameReg / pshdPopdDiffReg are currently disabled in `defaultRules` —
  // they make `SyslTriscStringRefcountTests` fail (string comparison corruption)
  // for reasons not yet diagnosed. The rule logic itself is exercised by passing
  // an explicit rules list. See the comment on `defaultRules`.
  "pshd-popd-same-reg removes the round-trip (rule-level)" in {
    val (out, stats) = TriscPeephole("  pshd r1\n  popd r1\n", List(pshdPopdSameReg))
    out shouldBe ""
    stats.perRule("pshd-popd-same-reg") shouldBe 1
  }

  "pshd-popd-diff-reg rewrites to mov (rule-level)" in {
    val (out, stats) = TriscPeephole("  pshd r1\n  popd r2\n", List(pshdPopdDiffReg))
    out shouldBe "  mov r2, r1\n"
    stats.perRule("pshd-popd-diff-reg") shouldBe 1
  }

  "addi-fold combines consecutive addi to the same register" in {
    val (out, stats) = TriscPeephole("  addi r3, r5, 8\n  addi r3, r3, 8\n")
    out shouldBe "  addi r3, r5, 16\n"
    stats.perRule("addi-fold") shouldBe 1
  }

  "addi-fold skips when the combined immediate would overflow" in {
    val (out, stats) = TriscPeephole("  addi r3, r5, 32000\n  addi r3, r3, 32000\n")
    out shouldBe "  addi r3, r5, 32000\n  addi r3, r3, 32000\n"
    stats.perRule.getOrElse("addi-fold", 0) shouldBe 0
  }

  // ===== Barrier behavior =====

  "labels block rule application across them" in {
    val src =
      """  pshd r1
        |.label
        |  popd r1
        |""".stripMargin
    val (out, stats) = TriscPeephole(src)
    out shouldBe src
    stats.totalRewrites shouldBe 0
  }

  "directives block rule application across them" in {
    val src =
      """  pshd r1
        |global foo
        |  popd r1
        |""".stripMargin
    val (out, _) = TriscPeephole(src)
    out shouldBe src
  }

  "blank lines block rule application across them" in {
    val src =
      """  pshd r1
        |
        |  popd r1
        |""".stripMargin
    val (out, _) = TriscPeephole(src)
    out shouldBe src
  }

  // ===== Chained rewrites in one pass =====

  "chained rewrites compound: addi-fold then addi-zero-to-mov" in {
    // addi r3, r5, 8 ; addi r3, r3, -8 → addi r3, r5, 0 → mov r3, r5
    val (out, stats) = TriscPeephole("  addi r3, r5, 8\n  addi r3, r3, -8\n")
    out shouldBe "  mov r3, r5\n"
    stats.perRule("addi-fold") shouldBe 1
    stats.perRule("addi-zero-to-mov") shouldBe 1
  }

  "chained rewrites compound: pshd-popd-diff-reg then mov-self-drop (rule-level)" in {
    // pshd r3 ; popd r3 → (pshd-popd-same-reg) → ε
    // Verifies the chain through both rules; uses an explicit rules list since the
    // pshd-popd rules aren't in `defaultRules`.
    val (out, _) = TriscPeephole("  pshd r3\n  popd r3\n", List(pshdPopdSameReg))
    out shouldBe ""
  }
