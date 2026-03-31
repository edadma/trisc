package io.github.edadma.trisc

class LoadStoreTests extends TestHelpers {

  // ===== LDB / STB =====

  "stb and ldb basic" in {
    val cpu = runCPU(VECTORS + "ldi r1, 0x41\nmovi r2, buf\nstb r1, r2, r0\nldb r3, r2, r0\nhalt\nbuf db 0\n")
    cpu.r(3).read shouldBe 0x41
  }

  "stb and ldb with offset register" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x42
        |movi r2, buf
        |ldi r3, 2
        |stb r1, r2, r3
        |ldb r4, r2, r3
        |halt
        |buf rb 4
        |""".stripMargin)
    cpu.r(4).read shouldBe 0x42
  }

  "ldb reads signed byte" in {
    val cpu = runCPU(VECTORS + "movi r1, buf\nldb r2, r1, r0\nhalt\nbuf db -128\n")
    cpu.r(2).read shouldBe -128
  }

  "stb stores only low byte" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x1234\nmovi r2, buf\nstb r1, r2, r0\nldb r3, r2, r0\nhalt\nbuf db 0\n")
    cpu.r(3).read shouldBe 0x34
  }

  // ===== LDS / STS =====

  "sts and lds basic" in {
    val cpu = runCPU(VECTORS + "movi r1, 0x1234\nmovi r2, buf\nsts r1, r2, r0\nlds r3, r2, r0\nhalt\nalign 2\nbuf ds 0\n")
    cpu.r(3).read shouldBe 0x1234
  }

  "sts and lds with offset register" in {
    val cpu = runCPU(VECTORS +
      """movi r1, 0x5678
        |movi r2, buf
        |ldi r3, 2
        |sts r1, r2, r3
        |lds r4, r2, r3
        |halt
        |align 2
        |buf rb 8
        |""".stripMargin)
    cpu.r(4).read shouldBe 0x5678
  }

  // ===== LDW / STW =====

  "stw and ldw basic" in {
    val cpu = runCPU(VECTORS +
      """movi r1, 0x1234
        |sli r1, 0x56
        |movi r2, buf
        |stw r1, r2, r0
        |ldw r3, r2, r0
        |halt
        |align 4
        |buf rb 4
        |""".stripMargin)
    cpu.r(3).read shouldBe cpu.r(1).read
  }

  "stw and ldw with offset register" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x42
        |movi r2, buf
        |ldi r3, 4
        |stw r1, r2, r3
        |ldw r4, r2, r3
        |halt
        |align 4
        |buf rb 8
        |""".stripMargin)
    cpu.r(4).read shouldBe 0x42
  }

  // ===== LDD / STD =====

  "std and ldd basic" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x12
        |sli r1, 0x34
        |sli r1, 0x56
        |sli r1, 0x78
        |sli r1, 0x9A
        |sli r1, 0xBC
        |sli r1, 0xDE
        |sli r1, 0xF0
        |movi r2, buf
        |std r1, r2, r0
        |ldd r3, r2, r0
        |halt
        |align 8
        |buf rb 8
        |""".stripMargin)
    cpu.r(3).read shouldBe cpu.r(1).read
  }

  "std and ldd with offset register" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |movi r2, buf
        |ldi r3, 8
        |std r1, r2, r3
        |ldd r4, r2, r3
        |halt
        |align 8
        |buf rb 16
        |""".stripMargin)
    cpu.r(4).read shouldBe 42
  }

  // ===== LD / ST (immediate offset) =====

  "ld and st with offset 0" in {
    val cpu = runCPU(VECTORS + "ldi r1, 42\nmovi r2, buf\nst r1, r2, 0\nld r3, r2, 0\nhalt\nalign 4\nbuf rb 4\n")
    cpu.r(3).read shouldBe 42
  }

  "ld and st with nonzero offset" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 11
        |ldi r2, 22
        |ldi r3, 33
        |movi r4, buf
        |st r1, r4, 0
        |st r2, r4, 4
        |st r3, r4, 8
        |ld r5, r4, 0
        |ld r6, r4, 4
        |ld r7, r4, 8
        |halt
        |align 4
        |buf rb 12
        |""".stripMargin)
    cpu.r(5).read shouldBe 11
    cpu.r(6).read shouldBe 22
    cpu.r(7).read shouldBe 33
  }

  "ld and st max aligned offset (60)" in {
    val cpu = runCPU(VECTORS + "ldi r1, 99\nmovi r2, buf\nst r1, r2, 60\nld r3, r2, 60\nhalt\nalign 4\nbuf rb 64\n")
    cpu.r(3).read shouldBe 99
  }
}
