package io.github.edadma.trisc

class MovemTests extends TestHelpers {

  // ===== MOVEM.SAVE =====

  "pshr r1 pushes r1 only" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |movi r7, 0xFF0
        |pshr r1
        |halt
        |""".stripMargin)
    cpu.r(7).read shouldBe (0xFF0 - 8)
    cpu.readLong(0xFF0 - 8) shouldBe 42
  }

  "pshr r3 pushes r1-r3" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 11
        |ldi r2, 22
        |ldi r3, 33
        |movi r7, 0xFF0
        |pshr r3
        |halt
        |""".stripMargin)
    cpu.r(7).read shouldBe (0xFF0 - 24)
    // r1 pushed first (deepest), r3 pushed last (shallowest)
    cpu.readLong(0xFF0 - 8) shouldBe 11
    cpu.readLong(0xFF0 - 16) shouldBe 22
    cpu.readLong(0xFF0 - 24) shouldBe 33
  }

  "pshr r6 pushes r1-r6" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |ldi r2, 20
        |ldi r3, 30
        |ldi r4, 40
        |ldi r5, 50
        |ldi r6, 60
        |movi r7, 0xFF0
        |pshr r6
        |halt
        |""".stripMargin)
    cpu.r(7).read shouldBe (0xFF0 - 48)
    cpu.readLong(0xFF0 - 8) shouldBe 10
    cpu.readLong(0xFF0 - 16) shouldBe 20
    cpu.readLong(0xFF0 - 24) shouldBe 30
    cpu.readLong(0xFF0 - 32) shouldBe 40
    cpu.readLong(0xFF0 - 40) shouldBe 50
    cpu.readLong(0xFF0 - 48) shouldBe 60
  }

  // ===== MOVEM.RESTORE =====

  "popr r1 pops r1 only" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 42
        |movi r7, 0xFF0
        |pshr r1
        |ldi r1, 0
        |popr r1
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(7).read shouldBe 0xFF0
  }

  "pshr/restore r6 round-trips all registers" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |ldi r2, 20
        |ldi r3, 30
        |ldi r4, 40
        |ldi r5, 50
        |ldi r6, 60
        |movi r7, 0xFF0
        |pshr r6
        |ldi r1, 0
        |ldi r2, 0
        |ldi r3, 0
        |ldi r4, 0
        |ldi r5, 0
        |ldi r6, 0
        |popr r6
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 10
    cpu.r(2).read shouldBe 20
    cpu.r(3).read shouldBe 30
    cpu.r(4).read shouldBe 40
    cpu.r(5).read shouldBe 50
    cpu.r(6).read shouldBe 60
    cpu.r(7).read shouldBe 0xFF0
  }

  "pshr/restore r4 round-trips r1-r4" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0xAA
        |ldi r2, 0xBB
        |ldi r3, 0xCC
        |ldi r4, 0xDD
        |ldi r5, 99
        |movi r7, 0xFF0
        |pshr r4
        |ldi r1, 0
        |ldi r2, 0
        |ldi r3, 0
        |ldi r4, 0
        |popr r4
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 0xAA
    cpu.r(2).read shouldBe 0xBB
    cpu.r(3).read shouldBe 0xCC
    cpu.r(4).read shouldBe 0xDD
    cpu.r(5).read shouldBe 99 // unchanged
  }

  "pshr adjusts SP correctly" in {
    val cpu = runCPU(VECTORS +
      """movi r7, 0xFF0
        |pshr r5
        |halt
        |""".stripMargin)
    // 5 registers × 8 bytes = 40
    cpu.r(7).read shouldBe (0xFF0 - 40)
  }
}
