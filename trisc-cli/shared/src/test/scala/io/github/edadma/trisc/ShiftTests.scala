package io.github.edadma.trisc

class ShiftTests extends TestHelpers {

  // ===== ASR (arithmetic shift right) =====

  "asr basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x80\nldi r2, 4\nasr r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 8
  }

  "asr preserves sign" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 4
        |asr r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe -1 // -1 >> 4 = -1 (sign-extended)
  }

  "asr by zero is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nasr r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 42
  }

  // ===== LSR (logical shift right) =====

  "lsr basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x80\nldi r2, 4\nlsr r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 8
  }

  "lsr does not preserve sign" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |sub r1, r0, r1
        |ldi r2, 1
        |lsr r3, r1, r2
        |halt
        |""".stripMargin)
    cpu.r(3).read shouldBe Long.MaxValue // -1 >>> 1 = 0x7FFFFFFFFFFFFFFF
  }

  "lsr by zero is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nlsr r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 42
  }

  // ===== LSL (logical shift left) =====

  "lsl basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 1\nldi r2, 8\nlsl r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 256
  }

  "lsl by zero is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nlsl r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 42
  }

  "lsl multiply by power of 2" in {
    val cpu = runCPU(VECTORS + "ldi r1, 5\nldi r2, 3\nlsl r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 40 // 5 * 8
  }
}
