package io.github.edadma.trisc

class SyslCodegenLargeImmTests extends SyslCodegenHelpers {

  // Array sizes > 255 triggered ldi with out-of-byte-range values

  "large i8 array: index read" in {
    compileAndRun(
      """main() -> int
        |    arr: [512]i8
        |    arr[0] = 42
        |    arr[511] = 99
        |    arr[0] + arr[511]
        |""".stripMargin) shouldBe 141
  }

  "large i8 array: index write" in {
    compileAndRun(
      """main() -> int
        |    arr: [300]i8
        |    arr[299] = 77
        |    arr[299]
        |""".stripMargin) shouldBe 77
  }

  "large int array: index read/write" in {
    compileAndRun(
      """main() -> int
        |    arr: [100]int
        |    arr[99] = 12345
        |    arr[99]
        |""".stripMargin) shouldBe 12345
  }

  "len of large array" in {
    compileAndRun(
      """main() -> int
        |    arr: [512]i8
        |    len(arr)
        |""".stripMargin) shouldBe 512
  }

  "large array loop write and read" in {
    compileAndRun(
      """main() -> int
        |    arr: [300]i8
        |    var i = 0
        |    while i < 100
        |        arr[i + 200] = i
        |        i += 1
        |    arr[299]
        |""".stripMargin) shouldBe 99
  }

  "pointer arithmetic with large struct" in {
    compileAndRun(
      """struct Entry
        |    a: i64
        |    b: i64
        |    c: i64
        |    d: i64
        |
        |main() -> int
        |    arr: [4]Entry
        |    arr[3].a = 42
        |    arr[3].a
        |""".stripMargin) shouldBe 42
  }
}
