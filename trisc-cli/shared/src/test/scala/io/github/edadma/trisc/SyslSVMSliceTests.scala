package io.github.edadma.trisc

class SyslSVMSliceTests extends SyslSVMCodegenHelpers {

  "new slice then len" in {
    compileAndRun(
      """main() -> i64
        |    var s = new [5]i64
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "new slice write and read" in {
    compileAndRun(
      """main() -> i64
        |    var s = new [3]i64
        |    s[0] = 10
        |    s[1] = 20
        |    s[2] = 30
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 60
  }

  "array full slice" in {
    compileAndRun(
      """main() -> i64
        |    var arr = [10, 20, 30, 40, 50]
        |    var s = arr[:]
        |    len(s)
        |""".stripMargin) shouldBe 5
  }

  "array partial slice" in {
    compileAndRun(
      """main() -> i64
        |    var arr = [10, 20, 30, 40, 50]
        |    var s = arr[1:4]
        |    len(s)
        |""".stripMargin) shouldBe 3
  }

  "array slice sum" in {
    compileAndRun(
      """main() -> i64
        |    var arr = [10, 20, 30, 40, 50]
        |    var s = arr[1:4]
        |    s[0] + s[1] + s[2]
        |""".stripMargin) shouldBe 90
  }

  "pass slice to function" in {
    compileAndRun(
      """sum(xs: []int) -> i64
        |    var i = 0
        |    var total: i64 = 0
        |    while i < len(xs)
        |        total += xs[i]
        |        i += 1
        |    total
        |
        |main() -> i64
        |    var arr = [1, 2, 3, 4, 5]
        |    sum(arr[:])
        |""".stripMargin) shouldBe 15
  }

  "new byte slice" in {
    compileAndRun(
      """main() -> i64
        |    var s = new [10]u8
        |    len(s)
        |""".stripMargin) shouldBe 10
  }
}
