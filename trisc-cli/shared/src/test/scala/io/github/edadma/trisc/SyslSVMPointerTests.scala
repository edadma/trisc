package io.github.edadma.trisc

class SyslSVMPointerTests extends SyslSVMCodegenHelpers {

  "pointer to array element" in {
    compileAndRun(
      """main() -> i64
        |    arr: [3]i64
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    var p = &arr[1]
        |    *p
        |""".stripMargin) shouldBe 200
  }

  "write through pointer" in {
    compileAndRun(
      """main() -> i64
        |    arr: [3]i64
        |    arr[0] = 10
        |    var p = &arr[0]
        |    *p = 42
        |    arr[0]
        |""".stripMargin) shouldBe 42
  }

  "pointer arithmetic" in {
    compileAndRun(
      """main() -> i64
        |    arr: [3]i64
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    var p = &arr[0]
        |    *(p + 2)
        |""".stripMargin) shouldBe 30
  }

  "global pointer" in {
    compileAndRun(
      """var g: i64 = 42
        |
        |read_ptr(p: *i64) -> i64 = *p
        |
        |main() -> i64 = read_ptr(&g)
        |""".stripMargin) shouldBe 42
  }
}
