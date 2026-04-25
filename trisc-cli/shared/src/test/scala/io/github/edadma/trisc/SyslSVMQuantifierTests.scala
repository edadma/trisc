package io.github.edadma.trisc

class SyslSVMQuantifierTests extends SyslSVMCodegenHelpers {

  "for all: holds when every element satisfies predicate" in {
    compileAndRun(
      """main() -> i64
        |    if for all x in 0..10 => x >= 0 then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }

  "for all: fails when some element violates" in {
    compileAndRun(
      """main() -> i64
        |    if for all x in 0..10 => x < 5 then 1i64 else 0i64
        |""".stripMargin) shouldBe 0
  }

  "for all: vacuously true on empty range" in {
    compileAndRun(
      """main() -> i64
        |    if for all x in 5..<5 => false then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }

  "for all: exclusive upper bound" in {
    compileAndRun(
      """main() -> i64
        |    if for all x in 0..<10 => x < 10 then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }

  "for all: inclusive upper bound" in {
    compileAndRun(
      """main() -> i64
        |    if for all x in 0..10 => x <= 10 then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }

  "for some: true when at least one element satisfies" in {
    compileAndRun(
      """main() -> i64
        |    if for some x in 0..10 => x == 7 then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }

  "for some: false when no element satisfies" in {
    compileAndRun(
      """main() -> i64
        |    if for some x in 0..10 => x > 100 then 1i64 else 0i64
        |""".stripMargin) shouldBe 0
  }

  "for some: false on empty range" in {
    compileAndRun(
      """main() -> i64
        |    if for some x in 5..<5 => true then 1i64 else 0i64
        |""".stripMargin) shouldBe 0
  }

  "predicate references outer-scope value" in {
    compileAndRun(
      """main() -> i64
        |    var threshold = 7
        |    if for all x in 0..6 => x < threshold then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }

  "for some captures outer array via index" in {
    compileAndRun(
      """main() -> i64
        |    var a: [5]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    a[4] = 5
        |    if for some i in 0..<5 => a[i] == 3 then 1i64 else 0i64
        |""".stripMargin) shouldBe 1
  }
}
