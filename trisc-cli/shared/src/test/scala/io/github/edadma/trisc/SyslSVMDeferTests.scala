package io.github.edadma.trisc

class SyslSVMDeferTests extends SyslSVMCodegenHelpers {

  "defer runs on normal return" in {
    compileAndRun(
      """var counter: i64 = 0
        |
        |bump()
        |    counter += 1
        |
        |run()
        |    defer bump()
        |    counter = 10
        |
        |main() -> i64
        |    run()
        |    counter
        |""".stripMargin) shouldBe 11
  }

  "multiple defers run LIFO" in {
    compileAndRun(
      """var order: i64 = 0
        |
        |shift()
        |    order = order * 10
        |
        |add(n: i64)
        |    order = order + n
        |
        |run()
        |    defer add(1)
        |    defer add(2)
        |    defer shift()
        |
        |main() -> i64
        |    run()
        |    order
        |""".stripMargin) shouldBe 3
  }
}
