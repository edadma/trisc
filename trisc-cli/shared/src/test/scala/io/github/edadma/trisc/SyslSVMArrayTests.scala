package io.github.edadma.trisc

class SyslSVMArrayTests extends SyslSVMCodegenHelpers {

  "array declaration and indexing" in {
    val src = """main() -> int
        |    arr: [3]i64
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[1]
        |""".stripMargin
    compileAndRun(src) shouldBe 20
  }

  "array sum" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]i64
        |    for var i: i64 = 0; i < 5; i += 1
        |        arr[i] = i * 10
        |    arr[0] + arr[1] + arr[2] + arr[3] + arr[4]
        |""".stripMargin) shouldBe 100
  }

  "array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]i64
        |    for var i: i64 = 0; i < 5; i += 1
        |        arr[i] = i + 1
        |    var sum: i64 = 0
        |    for var j: i64 = 0; j < 5; j += 1
        |        sum += arr[j]
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "array literal" in {
    compileAndRun(
      """main() -> i64
        |    var arr = [10, 20, 12]
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 42
  }

  "array passed to function" in {
    compileAndRun(
      """sum(p: *i64, n: i64) -> i64
        |    var s: i64 = 0
        |    for var i: i64 = 0; i < n; i += 1
        |        s += *(p + i)
        |    s
        |
        |main() -> int
        |    arr: [3]i64
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 12
        |    sum(&arr[0], 3)
        |""".stripMargin) shouldBe 42
  }
}
