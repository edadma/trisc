package io.github.edadma.trisc

class SyslSliceOpsTests extends SyslTestHelpers {

  // ===== Sub-slice basics =====

  "sub-slice [lo:hi]" in {
    eval(
      """main() -> int
        |    a = new [5]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[3] = 40
        |    a[4] = 50
        |    s = a[1:4]
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 90
  }

  "sub-slice [:hi]" in {
    eval(
      """main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[:2]
        |    s[0] + s[1]
        |""".stripMargin) shouldBe 30
  }

  "sub-slice [lo:]" in {
    eval(
      """main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[1:]
        |    s[0] + s[1]
        |""".stripMargin) shouldBe 50
  }

  "sub-slice [:]" in {
    eval(
      """main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[:]
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 60
  }

  // ===== Sub-slice shares backing array =====

  "sub-slice shares backing array" in {
    eval(
      """main() -> int
        |    a = new [3]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    s = a[1:]
        |    s[0] = 99
        |    a[1]
        |""".stripMargin) shouldBe 99
  }

  // ===== Sub-slice len and cap =====

  "sub-slice len" in {
    eval(
      """main() -> int
        |    a = new [5]int
        |    s = a[1:4]
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "sub-slice cap" in {
    eval(
      """main() -> int
        |    a = new [5]int
        |    s = a[1:4]
        |    cap(s)
        |""".stripMargin) shouldBe 4
  }

  // ===== Sub-slice bounds checking =====

  "sub-slice lo > hi" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    a = new [3]int
        |    s = a[2:1]
        |    0
        |""".stripMargin)
  }

  "sub-slice hi > len" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    a = new [3]int
        |    s = a[0:4]
        |    0
        |""".stripMargin)
  }

  "sub-slice negative lo" in {
    an[Exception] should be thrownBy eval(
      """main() -> int
        |    a = new [3]int
        |    s = a[-1:2]
        |    0
        |""".stripMargin)
  }

  // ===== Sub-slice of fixed array =====

  "sub-slice of fixed array" in {
    eval(
      """main() -> int
        |    var a: [4]int
        |    a[0] = 1
        |    a[1] = 2
        |    a[2] = 3
        |    a[3] = 4
        |    s = a[1:3]
        |    s[0] + s[1]
        |""".stripMargin) shouldBe 5
  }

  // ===== Sub-slice index assignment =====

  "index assign through sub-slice" in {
    eval(
      """main() -> int
        |    a = new [4]int
        |    a[0] = 10
        |    a[1] = 20
        |    a[2] = 30
        |    a[3] = 40
        |    s = a[1:3]
        |    s[0] = 99
        |    s[1] = 88
        |    a[1] + a[2]
        |""".stripMargin) shouldBe 187
  }

  // ===== Append basics =====

  "append to slice with capacity" in {
    eval(
      """main() -> int
        |    a = new [5]int
        |    a[0] = 10
        |    s = a[:1]
        |    s = append(s, 20)
        |    s = append(s, 30)
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 60
  }

  "append grows beyond capacity" in {
    eval(
      """main() -> int
        |    a = new [2]int
        |    a[0] = 10
        |    a[1] = 20
        |    s = a[:]
        |    s = append(s, 30)
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 60
  }

  "append len updates" in {
    eval(
      """main() -> int
        |    a = new [5]int
        |    s = a[:0]
        |    s = append(s, 1)
        |    s = append(s, 2)
        |    s = append(s, 3)
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "append to empty slice grows" in {
    eval(
      """main() -> int
        |    a = new [1]int
        |    s = a[:0]
        |    s = append(s, 42)
        |    s[0]
        |""".stripMargin) shouldBe 42
  }

  "append preserves old slice (Go semantics)" in {
    eval(
      """main() -> int
        |    a = new [2]int
        |    a[0] = 10
        |    a[1] = 20
        |    s = a[:]
        |    s2 = append(s, 30)
        |    len(s)
        |""".stripMargin) shouldBe 2
  }

  "append in loop" in {
    eval(
      """main() -> int
        |    a = new [10]int
        |    s = a[:0]
        |    i = 0
        |    while i < 5
        |        s = append(s, i * 10)
        |        i += 1
        |    s[0] + s[1] + s[2] + s[3] + s[4]
        |""".stripMargin) shouldBe 100
  }

  // ===== cap() on &[]T =====

  "cap on ref slice" in {
    eval(
      """main() -> int
        |    a = new [7]int
        |    cap(a)
        |""".stripMargin) shouldBe 7
  }

  // ===== Analyzer errors =====

  "append rejects non-slice" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    append(x, 1)
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }

  "sub-slice rejects non-sliceable" in {
    val Right(ast) = (new SyslParser).parseProgram(
      """main() -> int
        |    x = 42
        |    x[0:1]
        |""".stripMargin): @unchecked
    an[Exception] should be thrownBy (new SyslAnalyzer).analyze(ast)
  }
}
