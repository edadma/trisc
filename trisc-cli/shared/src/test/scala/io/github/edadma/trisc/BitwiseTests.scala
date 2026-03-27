package io.github.edadma.trisc

class BitwiseTests extends TestHelpers {

  // ===== AND =====

  "and basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x0F\nldi r2, 0x37\nand r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0x07
  }

  "and with zero clears" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xFF\nand r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 0
  }

  "and with self is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xAB\nand r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 0xAB
  }

  // ===== OR =====

  "or basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x0F\nldi r2, 0x30\nor r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0x3F
  }

  "or with zero is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xAB\nor r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 0xAB
  }

  "or with self is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x55\nor r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 0x55
  }

  // ===== XOR =====

  "xor basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xFF\nldi r2, 0x0F\nxor r3, r1, r2\nhalt\n")
    cpu.r(3).read shouldBe 0xF0
  }

  "xor with self yields zero" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xAB\nxor r2, r1, r1\nhalt\n")
    cpu.r(2).read shouldBe 0
  }

  "xor with zero is identity" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x42\nxor r2, r1, r0\nhalt\n")
    cpu.r(2).read shouldBe 0x42
  }
}
