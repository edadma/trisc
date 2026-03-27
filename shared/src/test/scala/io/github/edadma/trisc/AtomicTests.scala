package io.github.edadma.trisc

class AtomicTests extends TestHelpers {

  // ===== LL (Load-Linked) =====

  "ll loads value from memory correctly" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |ll r1, r3
        |halt
        |align 8
        |data dd 42
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }

  "ll sets reservation so subsequent sc succeeds" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |ll r1, r3
        |sc r1, r3
        |halt
        |align 8
        |data dd 42
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
  }

  // ===== SC (Store-Conditional) =====

  "sc succeeds when reservation is valid" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |ll r1, r3
        |ldi r1, 99
        |sc r1, r3
        |ldd r4, r3, r0
        |halt
        |align 8
        |data dd 42
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
    cpu.r(4).read shouldBe 99
  }

  "sc fails when no prior ll" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |ldi r1, 99
        |sc r1, r3
        |halt
        |align 8
        |data dd 42
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  "sc clears reservation so second sc always fails" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |ll r1, r3
        |ldi r1, 99
        |sc r1, r3
        |mov r2, r1
        |ldi r1, 55
        |sc r1, r3
        |halt
        |align 8
        |data dd 42
        |""".stripMargin)
    cpu.r(2).read shouldBe 1
    cpu.r(1).read shouldBe 0
  }

  "sc after trap invalidates reservation" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |resb 56
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  movi r3, data
        |  ll r1, r3
        |  trap 0
        |  ldi r1, 99
        |  sc r1, r3
        |  halt
        |handler
        |  rte
        |align 8
        |data dd 42
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  // ===== LL/SC Patterns =====

  "simple atomic store pattern: ll, modify, sc, check success" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |ll r1, r3
        |ldi r1, 100
        |sc r1, r3
        |ldd r4, r3, r0
        |halt
        |align 8
        |data dd 7
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
    cpu.r(4).read shouldBe 100
  }

  "retry loop succeeds on first try with no interruption" in {
    val cpu = runCPU(VECTORS +
      """movi r3, data
        |retry
        |  ll r1, r3
        |  ldi r1, 55
        |  sc r1, r3
        |  beq r1, r0, retry
        |ldd r4, r3, r0
        |halt
        |align 8
        |data dd 10
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
    cpu.r(4).read shouldBe 55
  }
}
