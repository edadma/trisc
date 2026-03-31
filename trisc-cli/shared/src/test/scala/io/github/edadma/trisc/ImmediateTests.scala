package io.github.edadma.trisc

class ImmediateTests extends TestHelpers {

  // ===== LDI =====

  "ldi loads small immediate" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nhalt\n")
    cpu.r(1).read shouldBe 42
  }

  "ldi loads zero" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\nldi r1, 0\nhalt\n")
    cpu.r(1).read shouldBe 0
  }

  "ldi loads max unsigned byte" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xFF\nhalt\n")
    cpu.r(1).read shouldBe 0xFF
  }

  "ldi works on all registers" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 2
        |ldi r3, 3
        |ldi r4, 4
        |ldi r5, 5
        |ldi r6, 6
        |ldi r7, 7
        |halt
        |""".stripMargin)
    for i <- 1 to 7 do cpu.r(i).read shouldBe i
  }

  // ===== SLI =====

  "sli shifts left 8 and ORs immediate" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0xAB\nsli r1, 0xCD\nhalt\n")
    cpu.r(1).read shouldBe 0xABCD
  }

  "sli chains for 3-byte value" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x12\nsli r1, 0x34\nsli r1, 0x56\nhalt\n")
    cpu.r(1).read shouldBe 0x123456
  }

  "sli chains for 4-byte value" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x12\nsli r1, 0x34\nsli r1, 0x56\nsli r1, 0x78\nhalt\n")
    cpu.r(1).read shouldBe 0x12345678
  }

  "sli with zero immediate just shifts" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x42\nsli r1, 0x00\nhalt\n")
    cpu.r(1).read shouldBe 0x4200
  }

  // ===== STI =====

  "sti writes immediate byte to address" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dd 0xFF0
        |dd 160
        |rb 144
        |movi r1, STDOUT
        |sti r1, 'A'
        |halt
        |""".stripMargin)
    output shouldBe "A"
  }

  "sti writes multiple bytes" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dd 0xFF0
        |dd 160
        |rb 144
        |movi r1, STDOUT
        |sti r1, 'H'
        |sti r1, 'i'
        |halt
        |""".stripMargin)
    output shouldBe "Hi"
  }

  // ===== AUIPC =====

  "auipc loads PC-relative address" in {
    val cpu = runCPU(VECTORS +
      """auipc r1, 0
        |halt
        |""".stripMargin)
    // vector table is 20 slots × 8 bytes = 160 bytes, auipc at address 160
    cpu.r(1).read shouldBe 160
  }

  "auipc with nonzero immediate" in {
    val cpu = runCPU(VECTORS +
      """auipc r1, 1
        |halt
        |""".stripMargin)
    // auipc at address 160, PC-2 + (1<<8) = 160 + 256 = 416
    cpu.r(1).read shouldBe 416
  }

  "auipc followed by ld for PC-relative load" in {
    val cpu = runCPU(VECTORS +
      """auipc r1, 0
        |ld r2, r1, 8
        |halt
        |align 4
        |dw 0x1234
        |""".stripMargin)
    // auipc at 160 → r1=160, ld at 162, halt at 164, align pads to 168, data at 168
    // ld r2, r1, 8 → readInt(160 + 8) = readInt(168)
    cpu.r(2).read shouldBe 0x1234
  }

  // ===== ADDI =====

  "addi basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\naddi r2, r1, 5\nhalt\n")
    cpu.r(2).read shouldBe 15
  }

  "addi negative immediate" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\naddi r2, r1, -3\nhalt\n")
    cpu.r(2).read shouldBe 7
  }

  "addi zero immediate is mov" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\naddi r2, r1, 0\nhalt\n")
    cpu.r(2).read shouldBe 42
  }

  "addi same src and dst" in {
    val cpu = runCPU(VECTORS + "ldi r1, 10\naddi r1, r1, 3\nhalt\n")
    cpu.r(1).read shouldBe 13
  }

  "addi max positive immediate (63)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0\naddi r2, r1, 63\nhalt\n")
    cpu.r(2).read shouldBe 63
  }

  "addi max negative immediate (-64)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 100\naddi r2, r1, -64\nhalt\n")
    cpu.r(2).read shouldBe 36
  }

  // ===== MOVI =====

  "movi 1-byte address" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x42\nhalt\n", addresses = 1)
    cpu.r(1).read shouldBe 0x42
  }

  "movi 2-byte address" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x1234\nhalt\n", addresses = 2)
    cpu.r(1).read shouldBe 0x1234
  }

  "movi 3-byte address" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x123456\nhalt\n", addresses = 3)
    cpu.r(1).read shouldBe 0x123456
  }

  "movi 4-byte address" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x12345678\nhalt\n", addresses = 4)
    cpu.r(1).read shouldBe 0x12345678
  }
}
