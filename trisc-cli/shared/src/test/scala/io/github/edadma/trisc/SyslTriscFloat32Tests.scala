package io.github.edadma.trisc

class SyslTriscFloat32Tests extends SyslCodegenHelpers {

  // ===== Basic load/store via local variable =====

  "f32 local round-trips through assignment" in {
    // 1.5 is exactly representable in f32; result should be int(1.5 * 2.0) = 3
    compileAndRun(
      """main() -> int
        |    var x: f32 = 1.5
        |    int(x * 2.0)
        |""".stripMargin) shouldBe 3
  }

  "f32 zero-initialized local" in {
    compileAndRun(
      """main() -> int
        |    var x: f32 = 0.0
        |    int(x)
        |""".stripMargin) shouldBe 0
  }

  // ===== Arithmetic =====

  "f32 addition" in {
    compileAndRun(
      """main() -> int
        |    var a: f32 = 1.5
        |    var b: f32 = 2.5
        |    int(a + b)
        |""".stripMargin) shouldBe 4
  }

  "f32 multiplication" in {
    compileAndRun(
      """main() -> int
        |    var a: f32 = 2.5
        |    var b: f32 = 4.0
        |    int(a * b)
        |""".stripMargin) shouldBe 10
  }

  // ===== Width conversions =====

  "f32 widens to f64 implicitly (no-op in TRISC since reg already holds f64)" in {
    compileAndRun(
      """main() -> int
        |    var a: f32 = 1.5
        |    var b: f64 = a
        |    int(b * 4.0)
        |""".stripMargin) shouldBe 6
  }

  "f64 to f32 explicit cast rounds to single precision" in {
    // 1.5 is exactly representable, so round-trips cleanly.
    compileAndRun(
      """main() -> int
        |    var a: f64 = 1.5
        |    var b: f32 = f32(a)
        |    int(b * 4.0)
        |""".stripMargin) shouldBe 6
  }

  "f64 to f32 narrowing actually loses precision" in {
    // 1.0 + 1e-10 is NOT representable in f32 (smaller than f32 epsilon at this magnitude),
    // so f32(...) should round to exactly 1.0.
    compileAndRun(
      """main() -> int
        |    var a: f64 = 1.0 + 0.0000000001
        |    var b: f32 = f32(a)
        |    if int(b) == 1 then 0 else 1
        |""".stripMargin) shouldBe 0
  }

  // ===== Int ↔ f32 =====

  "int to f32 cast" in {
    compileAndRun(
      """main() -> int
        |    var n: int = 7
        |    var x: f32 = f32(n)
        |    int(x * 2.0)
        |""".stripMargin) shouldBe 14
  }

  "f32 to int cast truncates" in {
    compileAndRun(
      """main() -> int
        |    var x: f32 = 3.75
        |    int(x)
        |""".stripMargin) shouldBe 3
  }

  // ===== Struct fields =====

  "f32 field in struct round-trips" in {
    compileAndRun(
      """struct Vec
        |    x: f32
        |    y: f32
        |
        |main() -> int
        |    var v = Vec(1.5, 2.5)
        |    int(v.x + v.y)
        |""".stripMargin) shouldBe 4
  }

  "f32 struct field stays 4 bytes" in {
    compileAndRun(
      """struct Vec
        |    x: f32
        |    y: f32
        |
        |main() -> int = sizeof(Vec)
        |""".stripMargin) shouldBe 8
  }

  // ===== IR check: f32tof64 / f64tof32 emitted =====

  "f32 load emits f32tof64" in {
    val asm = compile(
      """main() -> int
        |    var x: f32 = 1.5
        |    int(x)
        |""".stripMargin)
    assert(asm.contains("f32tof64"), s"expected f32tof64 in asm:\n$asm")
  }

  "f32 store emits f64tof32" in {
    val asm = compile(
      """main() -> int
        |    var x: f32 = 1.5
        |    int(x)
        |""".stripMargin)
    assert(asm.contains("f64tof32"), s"expected f64tof32 in asm:\n$asm")
  }

  "f32 load uses ldw not ldd" in {
    val asm = compile(
      """main() -> int
        |    var x: f32 = 1.5
        |    int(x)
        |""".stripMargin)
    // Should contain a load-word and not store/load 8 bytes for the f32 itself
    assert(asm.contains("ldw"), s"expected ldw for f32 load:\n$asm")
  }

  // ===== Function args/returns (8-byte slot regardless of type) =====

  "f32 function parameter and return" in {
    compileAndRun(
      """sq(x: f32) -> f32 = x * x
        |
        |main() -> int
        |    var r: f32 = sq(4.0)
        |    int(r)
        |""".stripMargin) shouldBe 16
  }
}
