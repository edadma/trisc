package io.github.edadma.trisc

class SyslSplitTestB extends SyslTestHelpers {
  "if block" in {
    eval(
      """main() -> int
        |    if 1
        |        return 42
        |    0
        |""".stripMargin) shouldBe 42
  }
}
