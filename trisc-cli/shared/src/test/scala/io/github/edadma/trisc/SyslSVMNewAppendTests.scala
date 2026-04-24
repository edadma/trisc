package io.github.edadma.trisc

class SyslSVMNewAppendTests extends SyslSVMCodegenHelpers {

  "new struct basic" in {
    compileAndRun(
      """struct Point
        |    x: i64
        |    y: i64
        |
        |main() -> i64
        |    var p = new Point(3, 4)
        |    p.x + p.y
        |""".stripMargin) shouldBe 7
  }

  "new struct through pointer" in {
    compileAndRun(
      """struct Box
        |    value: i64
        |
        |main() -> i64
        |    var b = new Box(42)
        |    b.value
        |""".stripMargin) shouldBe 42
  }

  "append to empty slice" in {
    compileAndRun(
      """main() -> i64
        |    var s = (new [4]i64)[:0]
        |    var s2 = append(s, 100)
        |    len(s2)
        |""".stripMargin) shouldBe 1
  }

  "append preserves old elements" in {
    compileAndRun(
      """main() -> i64
        |    var buf = new [8]i64
        |    buf[0] = 10
        |    buf[1] = 20
        |    buf[2] = 30
        |    var s = buf[0:3]
        |    var s2 = append(s, 40)
        |    s2[0] + s2[1] + s2[2] + s2[3]
        |""".stripMargin) shouldBe 100
  }

  "append bytes" in {
    compileAndRun(
      """main() -> i64
        |    var s = (new [8]u8)[:0]
        |    var s1 = append(s, 10u8)
        |    var s2 = append(s1, 20u8)
        |    var s3 = append(s2, 30u8)
        |    len(s3)
        |""".stripMargin) shouldBe 3
  }

  "append byte content" in {
    compileAndRun(
      """main() -> i64
        |    var buf = new [8]u8
        |    buf[0] = 10u8
        |    buf[1] = 20u8
        |    var s = buf[0:2]
        |    var s2 = append(s, 30u8)
        |    s2[0] + s2[1] + s2[2]
        |""".stripMargin) shouldBe 60
  }
}
