package io.github.edadma.trisc

/** End-to-end tests for `for all` / `for some` quantifier expressions through the TRISC
  * backend (compile → assemble → link → run on the CPU emulator). Mirrors the interpreter
  * coverage in SyslQuantifierTests and the LLVM coverage in SyslLLVMQuantifierTests. */
class SyslTriscQuantifierTests extends SyslCodegenHelpers {

  "for all: true on satisfied range" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 0..10 => x >= 0 then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "for all: false when violated" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 0..10 => x < 5 then 1 else 0
        |""".stripMargin) shouldBe 0L
  }

  "for all: vacuously true on exclusive empty range" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 5..<5 => false then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "for all: vacuously true on backward range" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 7..3 => false then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "for some: true with witness" in {
    compileAndRun(
      """main() -> int
        |    if for some x in 0..10 => x == 7 then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "for some: false without witness" in {
    compileAndRun(
      """main() -> int
        |    if for some x in 0..10 => x > 100 then 1 else 0
        |""".stripMargin) shouldBe 0L
  }

  "for some: false on empty range" in {
    compileAndRun(
      """main() -> int
        |    if for some x in 5..<5 => true then 1 else 0
        |""".stripMargin) shouldBe 0L
  }

  "for all: inclusive range includes the upper bound" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 0..5 => x <= 5 then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "for all: exclusive range stops short of the upper bound" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 0..<5 => x < 5 then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "for some: exclusive range does not see the upper endpoint" in {
    compileAndRun(
      """main() -> int
        |    if for some x in 0..<5 => x == 5 then 1 else 0
        |""".stripMargin) shouldBe 0L
  }

  "outer-scope capture in predicate" in {
    compileAndRun(
      """main() -> int
        |    var threshold = 7
        |    if for all x in 0..6 => x < threshold then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "nested: for all of for some" in {
    compileAndRun(
      """main() -> int
        |    if for all i in 0..3 => for some j in 0..3 => i + j == 3 then 1 else 0
        |""".stripMargin) shouldBe 1L
  }

  "outer-scope name shadowed by bound variable, restored after" in {
    compileAndRun(
      """main() -> int
        |    var x = 99
        |    var ok = if for all x in 0..3 => x >= 0 then 1 else 0
        |    ok + x
        |""".stripMargin) shouldBe 100L
  }

  "for all: counterexample at the first element bails out immediately" in {
    compileAndRun(
      """main() -> int
        |    if for all x in 0..100 => x < 0 then 1 else 0
        |""".stripMargin) shouldBe 0L
  }

  "for some: witness at the last element" in {
    compileAndRun(
      """main() -> int
        |    if for some x in 0..5 => x == 5 then 1 else 0
        |""".stripMargin) shouldBe 1L
  }
}
