package io.github.edadma.trisc

class SyslSVMPointerTests extends SyslSVMCodegenHelpers {

  "pointer to array element" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 100
        |    arr[1] = 200
        |    arr[2] = 300
        |    p = &arr[1]
        |    *p
        |""".stripMargin) shouldBe 200
  }

  "write through pointer" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 10
        |    p = &arr[0]
        |    *p = 42
        |    arr[0]
        |""".stripMargin) shouldBe 42
  }

  "pointer arithmetic" in {
    compileAndRun(
      """main() -> int
        |    arr: [3]i64
        |    arr[0] = 10
        |    arr[1] = 20
        |    arr[2] = 30
        |    p = &arr[0]
        |    *(p + 2)
        |""".stripMargin) shouldBe 30
  }

  "global pointer" in {
    compileAndRun(
      """var g: int = 42
        |
        |read_ptr(p: *int) -> int = *p
        |
        |main() -> int = read_ptr(&g)
        |""".stripMargin) shouldBe 42
  }
}
