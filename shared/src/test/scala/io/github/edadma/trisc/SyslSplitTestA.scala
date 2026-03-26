package io.github.edadma.trisc

class SyslSplitTestA extends SyslTestHelpers {
  "addition" in {
    eval("main() -> int = 3 + 4\n") shouldBe 7
  }
}
