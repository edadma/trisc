package io.github.edadma.trisc

class SyslCodegenArrayTests extends SyslCodegenHelpers {

  "array declaration and indexing" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[1]
        |""".stripMargin) shouldBe 20
  }

  "array sum" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = i * 10
        |    arr[0] + arr[1] + arr[2] + arr[3] + arr[4]
        |""".stripMargin) shouldBe 100
  }

  "array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = i + 1
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "i8 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]byte
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60
  }

  "i16 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i16
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 600
  }

  "i32 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]int
        |    arr[0] = 1000
        |    arr[1] = 2000
        |    arr[2] = 3000
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 6000
  }

  "i64 array" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 10000
        |    arr[1] = 20000
        |    arr[2] = 30000
        |    arr[0] + arr[1] + arr[2]
        |""".stripMargin) shouldBe 60000
  }

  "i32 array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]int
        |    for i = 0; i < 5; i++
        |        arr[i] = (i + 1) * 10
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 150
  }

  "i8 array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]byte
        |    for i = 0; i < 5; i++
        |        arr[i] = i + 1
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 15
  }

  "i8 loop counter" in {
    compileAndRun(
      """main() -> int
        |    sum = 0
        |    for var i: byte = 0; i < 10; i++
        |        sum += 1
        |    sum
        |""".stripMargin) shouldBe 10
  }

  "i16 array with loop" in {
    compileAndRun(
      """main() -> int
        |    arr: [5]i16
        |    for i = 0; i < 5; i++
        |        arr[i] = (i + 1) * 100
        |    sum = 0
        |    for i = 0; i < 5; i++
        |        sum += arr[i]
        |    sum
        |""".stripMargin) shouldBe 1500
  }

  "local array literal indexing" in {
    compileAndRun(
      """main() -> int
        |    a = [5, 15, 25]
        |    a[0] + a[1] + a[2]
        |""".stripMargin) shouldBe 45
  }

  "array literal passed to function" in {
    compileAndRun(
      """sum3(p: *int) -> int = p[0] + p[1] + p[2]
        |main() -> int
        |    a = [100, 200, 300]
        |    sum3(a)
        |""".stripMargin) shouldBe 600
  }
}
