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

  // Repro for the regex-test failures: struct with 3×i32 fields = sizeOf 12 (not a
  // multiple of 8). emitStore for aggregates was rounding sizeOf UP to the next
  // multiple of 8, which made `append(slice_of_struct12, elem)` write 16 bytes
  // into a 12-byte slot — overwriting the next element's first field with junk
  // padding from the source. The bug only surfaces when the next-element slot
  // exists and isn't immediately overwritten.
  "append struct with non-8-aligned size preserves fields" in {
    compileAndRun(
      """struct Inst3
        |    op: int
        |    arg1: int
        |    arg2: int
        |
        |main() -> i64
        |    var s = (new [16]Inst3)[:0]
        |    s = append(s, Inst3(1, 100, 0))
        |    s = append(s, Inst3(2, 200, 0))
        |    s = append(s, Inst3(3, 300, 0))
        |    s = append(s, Inst3(4, 400, 0))
        |    s = append(s, Inst3(5, 500, 0))
        |    i64(s[0].op + s[1].op + s[2].op + s[3].op + s[4].op)
        |""".stripMargin) shouldBe 15  // 1+2+3+4+5
  }

  "append struct field slice (mirrors compile()'s self.insts pattern)" in {
    compileAndRun(
      """struct Inst3
        |    op: int
        |    arg1: int
        |    arg2: int
        |
        |struct Compiler
        |    insts: []Inst3
        |
        |Compiler.emit(op: int, arg1: int, arg2: int)
        |    self.insts = append(self.insts, Inst3(op, arg1, arg2))
        |
        |main() -> i64
        |    var c: Compiler
        |    val _i = new [16]Inst3
        |    c.insts = _i[:0]
        |    c.emit(1, 100, 0)
        |    c.emit(2, 200, 0)
        |    c.emit(3, 300, 0)
        |    c.emit(4, 400, 0)
        |    c.emit(5, 500, 0)
        |    i64(c.insts[0].op + c.insts[1].op + c.insts[2].op + c.insts[3].op + c.insts[4].op)
        |""".stripMargin) shouldBe 15
  }
}
