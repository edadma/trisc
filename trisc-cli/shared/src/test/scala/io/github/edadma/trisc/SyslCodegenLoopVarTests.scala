package io.github.edadma.trisc

class SyslCodegenLoopVarTests extends SyslCodegenHelpers {

  "val inside while loop" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        val x = i * 10
        |        sum += x
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 100
  }

  "var inside while loop with reassign" in {
    compileAndRun(
      """main() -> int
        |    var total = 0
        |    var i = 0
        |    while i < 3
        |        var v = i
        |        v = v * 2
        |        total += v
        |        i += 1
        |    total
        |""".stripMargin) shouldBe 6
  }

  "val inside while with function call" in {
    compileAndRun(
      """dbl(x: int) -> int
        |    x * 2
        |
        |main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 3
        |        val d = dbl(i)
        |        sum += d
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 6
  }

  "var reassigned inside if inside while" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 5
        |        var v = 0
        |        if i > 2
        |            v = 10
        |        sum += v
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 20 // i=3,4 contribute 10 each
  }

  "var reassigned inside if-else inside while" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 4
        |        var v = 0
        |        if i < 2
        |            v = 1
        |        else
        |            v = 10
        |        sum += v
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 22 // 1+1+10+10
  }

  "val and var mixed inside while" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 3
        |        val a = i + 1
        |        var b = a * 2
        |        b += 1
        |        sum += b
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 15 // a=1,b=3; a=2,b=5; a=3,b=7 → 3+5+7 = 15
  }

  "multiple vals inside while" in {
    compileAndRun(
      """main() -> int
        |    var sum = 0
        |    var i = 0
        |    while i < 3
        |        val a = i
        |        val b = a + 1
        |        val c = b * 2
        |        sum += c
        |        i += 1
        |    sum
        |""".stripMargin) shouldBe 12 // (0+1)*2 + (1+1)*2 + (2+1)*2 = 2+4+6
  }

  "var inside nested while" in {
    compileAndRun(
      """main() -> int
        |    var total = 0
        |    var i = 0
        |    while i < 3
        |        var j = 0
        |        while j < 3
        |            val v = i * 3 + j
        |            total += v
        |            j += 1
        |        i += 1
        |    total
        |""".stripMargin) shouldBe 36 // sum of 0..8
  }
}
