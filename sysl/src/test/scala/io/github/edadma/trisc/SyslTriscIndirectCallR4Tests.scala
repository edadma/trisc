package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Audit item #31 (Tier 4): TRISC indirect calls preserve r4 between
 *  the function-pointer load and the `jalr r6, r4`.
 *
 *  TRISC's calling convention reserves r4 for the call target — both
 *  direct (`movi r4, name`) and indirect (`ldd r4, r1, r0`) flavors
 *  end with `jalr r6, r4`. Anything that writes r4 between those two
 *  points would silently call the wrong address and crash at runtime.
 *
 *  Static check approach: compile a representative indirect-call program,
 *  scan the emitted asm, and verify that for every `ldd r4` that
 *  precedes a `jalr r6, r4`, the only intervening writes to r4 are
 *  themselves part of the same call setup (which there should be none of —
 *  the codegen path between load and call only touches r1 and r7).
 *
 *  This pins the discipline so a future refactor that accidentally clobbers
 *  r4 between load and call (e.g., emitting an `add r4, ...` when it meant
 *  `add r2, ...`) is caught by the test suite, not at runtime in the OS.
 */
class SyslTriscIndirectCallR4Tests extends AnyFreeSpec with Matchers {

  private def asm(source: String): String =
    val Right(ast) = (new SyslParser).parseProgram(source): @unchecked
    val typed = (new SyslAnalyzer).analyze(ast)
    (new SyslTriscCodegen).generate(typed)

  /** Find each (loadIdx, callIdx) pair where an `ldd r4, …` is followed by a
   *  `jalr r6, r4` with no other `jalr r6, r4` in between. Returns the line
   *  ranges in instruction-stream order. */
  private def indirectCallSites(out: String): List[(Int, Int)] =
    val lines = out.linesIterator.toIndexedSeq
    val sites = scala.collection.mutable.ListBuffer.empty[(Int, Int)]
    var pendingLoad = -1
    val ldR4 = "^\\s*ldd\\s+r4,\\s+r1,\\s+r0\\s*$".r
    val callR4 = "^\\s*jalr\\s+r6,\\s+r4\\s*$".r
    for (line, idx) <- lines.zipWithIndex do
      if ldR4.matches(line) then pendingLoad = idx
      else if callR4.matches(line) && pendingLoad >= 0 then
        sites += ((pendingLoad, idx))
        pendingLoad = -1
    sites.toList

  /** Returns true iff `line` is an instruction whose destination register
   *  is r4. Catches `add r4, …`, `addi r4, …`, `mov r4, …`, `ldd r4, …`,
   *  `ldw r4, …`, `ldi r4, …`, `movi r4, …`, etc. We're conservative: any
   *  three-letter or four-letter mnemonic followed by `r4,` counts as a write. */
  private def writesR4(line: String): Boolean =
    val trimmed = line.trim
    if trimmed.isEmpty || trimmed.startsWith(";") || trimmed.startsWith("#") then false
    else
      // Match `<mnemonic> r4,` — destination is always first operand on TRISC.
      val parts = trimmed.split("\\s+", 3)
      parts.length >= 2 && parts(1).startsWith("r4,")

  // ===== Smoke test: indirect call detection =====

  "indirect call program emits an `ldd r4 / jalr r6, r4` pair" in {
    val out = asm(
      """call_it(f: () -> int) -> int = f()
        |
        |main() -> int
        |    val r = call_it(() -> 42)
        |    r
        |""".stripMargin)
    val sites = indirectCallSites(out)
    sites should not be empty
  }

  // ===== Static check: r4 not clobbered between load and jalr =====

  "scalar-result indirect call: r4 preserved load → jalr" in {
    val out = asm(
      """apply(f: (int) -> int, x: int) -> int = f(x)
        |
        |main() -> int = apply(x -> x + 1, 41)
        |""".stripMargin)
    val lines = out.linesIterator.toIndexedSeq
    val sites = indirectCallSites(out)
    sites should not be empty
    for (loadIdx, callIdx) <- sites do
      val between = lines.slice(loadIdx + 1, callIdx)
      val clobber = between.find(writesR4)
      withClue(s"between ldd r4 (line $loadIdx) and jalr r6, r4 (line $callIdx): clobber=$clobber\n${between.mkString("\n")}\n") {
        clobber shouldBe None
      }
  }

  "no-arg indirect call: r4 preserved" in {
    val out = asm(
      """call_it(f: () -> int) -> int = f()
        |
        |main() -> int = call_it(() -> 42)
        |""".stripMargin)
    val lines = out.linesIterator.toIndexedSeq
    val sites = indirectCallSites(out)
    sites should not be empty
    for (loadIdx, callIdx) <- sites do
      val between = lines.slice(loadIdx + 1, callIdx)
      val clobber = between.find(writesR4)
      withClue(s"between ldd r4 (line $loadIdx) and jalr r6, r4 (line $callIdx): clobber=$clobber\n${between.mkString("\n")}\n") {
        clobber shouldBe None
      }
  }

  "multi-arg indirect call: r4 preserved" in {
    val out = asm(
      """apply2(f: (int, int) -> int, a: int, b: int) -> int = f(a, b)
        |
        |main() -> int = apply2((x, y) -> x + y, 20, 22)
        |""".stripMargin)
    val lines = out.linesIterator.toIndexedSeq
    val sites = indirectCallSites(out)
    sites should not be empty
    for (loadIdx, callIdx) <- sites do
      val between = lines.slice(loadIdx + 1, callIdx)
      val clobber = between.find(writesR4)
      withClue(s"between ldd r4 (line $loadIdx) and jalr r6, r4 (line $callIdx): clobber=$clobber\n${between.mkString("\n")}\n") {
        clobber shouldBe None
      }
  }

  "captured-state closure indirect call: r4 preserved through env-load" in {
    // Captures matter because r3 = env_ptr is loaded *between* r4 and jalr;
    // the code there must not accidentally use r4 for env staging.
    val out = asm(
      """make_adder(n: int) -> (int) -> int =
        |    x -> x + n
        |
        |main() -> int =
        |    val add5 = make_adder(5)
        |    add5(37)
        |""".stripMargin)
    val lines = out.linesIterator.toIndexedSeq
    val sites = indirectCallSites(out)
    sites should not be empty
    for (loadIdx, callIdx) <- sites do
      val between = lines.slice(loadIdx + 1, callIdx)
      val clobber = between.find(writesR4)
      withClue(s"between ldd r4 (line $loadIdx) and jalr r6, r4 (line $callIdx): clobber=$clobber\n${between.mkString("\n")}\n") {
        clobber shouldBe None
      }
  }

  // ===== Sentinel: a hand-injected `add r4, ...` would be detected =====
  //
  // Sanity-check that `writesR4` actually fires on a clobber. This protects
  // the test against a silent regression where the detector itself stops
  // recognizing the patterns.

  "writesR4 detector recognises common r4 writes" in {
    writesR4("  add r4, r1, r2") shouldBe true
    writesR4("  addi r4, r1, 8") shouldBe true
    writesR4("  mov r4, r0")     shouldBe true
    writesR4("  ldd r4, r1, r0") shouldBe true
    writesR4("  ldw r4, r1, r0") shouldBe true
    writesR4("  ldi r4, 0")      shouldBe true
    writesR4("  movi r4, foo")   shouldBe true
  }

  "writesR4 detector ignores reads / unrelated regs" in {
    writesR4("  add r1, r4, r2") shouldBe false  // r4 is source, not dest
    writesR4("  jalr r6, r4")    shouldBe false  // jalr dest is r6
    writesR4("  ldd r3, r1, r0") shouldBe false  // different dest
    writesR4("  pshd r4")        shouldBe false  // pshd writes memory, not r4
    writesR4("")                 shouldBe false  // blank line
    writesR4("  ; comment")      shouldBe false  // comment
  }
}
