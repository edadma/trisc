package io.github.edadma.trisc

class SyslSVMByteCastTests extends SyslSVMCodegenHelpers {

  "string byte indexing" in {
    compileAndRun(
      """main() -> i64
        |    var s = "hello"
        |    var c: i64 = s[0]
        |    c
        |""".stripMargin) shouldBe 104 // 'h'
  }

  "byte slice to string" in {
    compileAndRun(
      """main() -> i64
        |    var buf = new [5]u8
        |    buf[0] = 104u8
        |    buf[1] = 105u8
        |    var slc = buf[0:2]
        |    var s = string(slc)
        |    len(s)
        |""".stripMargin) shouldBe 2
  }
}
