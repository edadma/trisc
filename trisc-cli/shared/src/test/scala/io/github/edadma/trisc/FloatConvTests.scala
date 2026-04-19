package io.github.edadma.trisc

class FloatConvTests extends TestHelpers {

  // Helper: load a double from a dd constant into a register via ldd
  private def loadDouble(reg: String, label: String): String =
    s"movi $reg, $label\nldd $reg, $reg, r0\n"

  // Helper: load a 32-bit value from a dw constant into a register via ldw
  private def loadWord(reg: String, label: String): String =
    s"movi $reg, $label\nldw $reg, $reg, r0\n"

  private def floatProg(body: String, consts: String): String =
    VECTORS + body + "halt\n" + consts

  // ===== f32tof64 =====

  "f32tof64 converts 1.5f to 1.5" in {
    // IEEE 754 single-precision 1.5 = 0x3FC00000
    val cpu = runCPU(floatProg(
      loadWord("r1", "x") + "f32tof64 r2, r1\n",
      "x dw 0x3FC00000\n"))
    cpu.r(2).readf shouldBe 1.5
  }

  "f32tof64 converts 0.0f to 0.0" in {
    val cpu = runCPU(floatProg(
      loadWord("r1", "x") + "f32tof64 r2, r1\n",
      "x dw 0x00000000\n"))
    cpu.r(2).readf shouldBe 0.0
  }

  "f32tof64 converts -1.0f to -1.0" in {
    // IEEE 754 single -1.0 = 0xBF800000
    val cpu = runCPU(floatProg(
      loadWord("r1", "x") + "f32tof64 r2, r1\n",
      "x dw 0xBF800000\n"))
    cpu.r(2).readf shouldBe -1.0
  }

  "f32tof64 converts 3.14159f to approximately 3.14159" in {
    // IEEE 754 single 3.14159f = 0x40490FD0
    val cpu = runCPU(floatProg(
      loadWord("r1", "x") + "f32tof64 r2, r1\n",
      "x dw 0x40490FD0\n"))
    // Single-precision rounds; expect the exact float->double widening
    val expected = java.lang.Float.intBitsToFloat(0x40490FD0).toDouble
    cpu.r(2).readf shouldBe expected
  }

  // ===== f64tof32 =====

  "f64tof32 converts 1.5 to 1.5f bit pattern" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "x") + "f64tof32 r2, r1\n",
      "x dd 1.5\n"))
    // 1.5f bit pattern = 0x3FC00000; upper 32 bits should be zero
    cpu.r(2).read shouldBe 0x3FC00000L
  }

  "f64tof32 converts 0.0 to 0x00000000" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "x") + "f64tof32 r2, r1\n",
      "x dd 0.0\n"))
    cpu.r(2).read shouldBe 0L
  }

  "f64tof32 converts -2.0 to -2.0f bit pattern" in {
    // -2.0f bit pattern = 0xC0000000
    val cpu = runCPU(floatProg(
      loadDouble("r1", "x") + "f64tof32 r2, r1\n",
      "x dd -2.0\n"))
    cpu.r(2).read shouldBe 0xC0000000L
  }

  "f64tof32 zeroes upper 32 bits" in {
    // Set r1 = 0xFFFFFFFFFFFFFFFF, then load a double into it, convert, check upper is zero
    val cpu = runCPU(floatProg(
      loadDouble("r1", "x") + "f64tof32 r2, r1\n",
      "x dd 1.0\n"))
    // 1.0f bit pattern = 0x3F800000; upper 32 bits zeroed
    cpu.r(2).read shouldBe 0x3F800000L
    (cpu.r(2).read >> 32) shouldBe 0L
  }

  // ===== Round-trip =====

  "f64tof32 then f32tof64 round-trip on a representable value" in {
    val cpu = runCPU(floatProg(
      loadDouble("r1", "x") + "f64tof32 r2, r1\nf32tof64 r3, r2\n",
      "x dd 2.5\n"))
    cpu.r(3).readf shouldBe 2.5
  }

  "f32tof64 then f64tof32 round-trip preserves single-precision bit pattern" in {
    val cpu = runCPU(floatProg(
      loadWord("r1", "x") + "f32tof64 r2, r1\nf64tof32 r3, r2\n",
      "x dw 0x40490FD0\n"))
    cpu.r(3).read shouldBe 0x40490FD0L
  }
}
