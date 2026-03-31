package io.github.edadma.trisc

class SystemTests extends TestHelpers {

  // ===== FENCE =====

  "fence executes without error between instructions" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 10
        |fence
        |ldi r2, 20
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 10
    cpu.r(2).read shouldBe 20
    cpu.state shouldBe State.Halt
  }

  "fence does not modify any registers" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |ldi r2, 2
        |ldi r3, 3
        |ldi r4, 4
        |ldi r5, 5
        |ldi r6, 6
        |ldi r7, 7
        |fence
        |halt
        |""".stripMargin)
    cpu.r(0).read shouldBe 0
    cpu.r(1).read shouldBe 1
    cpu.r(2).read shouldBe 2
    cpu.r(3).read shouldBe 3
    cpu.r(4).read shouldBe 4
    cpu.r(5).read shouldBe 5
    cpu.r(6).read shouldBe 6
    cpu.r(7).read shouldBe 7
  }

  "fence at start of program does not crash" in {
    val cpu = runCPU(VECTORS +
      """fence
        |ldi r1, 55
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 55
  }

  "multiple consecutive fences execute without error" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 1
        |fence
        |fence
        |fence
        |ldi r2, 2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 1
    cpu.r(2).read shouldBe 2
  }

  // ===== WFI =====

  "wfi suspends execution" in {
    // Without an interrupt source, wfi should leave the CPU in Wfi state.
    // The instruction after wfi should NOT execute.
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(VECTORS +
      """ldi r1, 10
        |wfi
        |ldi r1, 42
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 100 }
    cpu.reset()
    cpu.run()
    // CPU should be stuck in Wfi state; r1 should still be 10
    cpu.state shouldBe State.Wfi
    cpu.r(1).read shouldBe 10
  }

  "wfi resumes on interrupt" in {
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    var interruptFired = false
    val interruptSource: CPU => Unit = cpu =>
      if !interruptFired && cpu.state == State.Wfi then
        interruptFired = true
        cpu.interrupt()
    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    val tof = assemble(
      """STDOUT = 0xFF8
        |dd 0xFF0
        |dd reset
        |dd isr
        |resb 136
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  wfi
        |  ldi r1, 42
        |  halt
        |isr
        |  movi r3, STDOUT
        |  sti r3, 'W'
        |  rte
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Seq(interruptSource)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    // After wfi resumes via interrupt, isr writes 'W' to stdout and rte returns.
    // Then ldi r1, 42 executes and halt.
    cpu.r(1).read shouldBe 42
    output.toString shouldBe "W"
    cpu.state shouldBe State.Halt
  }

  "wfi does not execute next instruction before interrupt" in {
    // Interrupt fires only when CPU reaches WFI state.
    var interruptFired = false
    val interruptSource: CPU => Unit = cpu =>
      if !interruptFired && cpu.state == State.Wfi then
        interruptFired = true
        cpu.interrupt()
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd isr
        |resb 136
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  ldi r3, 10
        |  wfi
        |  ldi r3, 20
        |  halt
        |isr
        |  rte
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Seq(interruptSource)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    // r3 should be 20 after resuming (not still 10)
    cpu.r(3).read shouldBe 20
  }

  // ===== PRIVILEGE VIOLATION: spsr =====

  "spsr in user mode triggers privilege violation" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  ldi r1, 5
        |  spsr r1
        |  ldi r2, 99
        |  halt
        |privhandler
        |  ldi r2, 42
        |  halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 42
    cpu.r(2).read should not be 99L
  }

  "first spsr in supervisor mode succeeds" in {
    // After reset, CPU is in supervisor mode. spsr should work.
    val cpu = runCPU(VECTORS +
      """ldi r1, 0x0F
        |spsr r1
        |gpsr r2
        |halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 0x0F
  }

  "spsr clears mode bit then second spsr faults" in {
    // Verify the sequence: spsr with Mode=0 clears Mode, next spsr fails.
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  ldi r3, 1
        |  spsr r3
        |  ldi r4, 99
        |  halt
        |privhandler
        |  ldi r4, 11
        |  halt
        |""".stripMargin)
    // r3=1 was loaded before the faulting spsr, but the handler sets r4=11
    cpu.r(4).read shouldBe 11
    cpu.r(4).read should not be 99L
  }

  // ===== PRIVILEGE VIOLATION: rte =====

  "rte in user mode triggers privilege violation" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  rte
        |  ldi r2, 99
        |  halt
        |privhandler
        |  ldi r2, 42
        |  halt
        |""".stripMargin)
    cpu.r(2).read shouldBe 42
    cpu.r(2).read should not be 99L
  }

  "rte in supervisor mode does not trigger privilege violation" in {
    // After reset we are in supervisor mode. rte should work (it returns to saved PC).
    // Use trap to set up a proper return context, then rte in handler (supervisor mode).
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
        |resb 80
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  trap 0
        |  ldi r1, 77
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 77
    cpu.state shouldBe State.Halt
  }

  // ===== GPSR: NOT privileged =====

  "gpsr works in user mode" in {
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
        |dd trap0handler
        |resb 80
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  gpsr r2
        |  trap 0
        |trap0handler
        |  halt
        |""".stripMargin)
    // After spsr r1 (r1=0), Mode is cleared. gpsr should still work.
    // PSR should be 0 (all flags cleared). Trap back to supervisor for halt.
    cpu.r(2).read shouldBe 0
    cpu.state shouldBe State.Halt
  }

  "gpsr reads correct PSR value after clearing mode" in {
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
        |dd trap0handler
        |resb 80
        |reset
        |  ldi r1, 4
        |  spsr r1
        |  gpsr r2
        |  trap 0
        |trap0handler
        |  halt
        |""".stripMargin)
    // spsr r1 with r1=4 sets C flag, clears Mode and Ind.
    // gpsr should read 4. Trap back to supervisor for halt.
    cpu.r(2).read shouldBe 4
  }

  "gpsr in supervisor mode reads mode bit" in {
    // After reset, PSR has Ind=1 and Mode=2 set, so PSR = 3.
    val cpu = runCPU(VECTORS +
      """gpsr r1
        |halt
        |""".stripMargin)
    // Ind (1) + Mode (2) = 3
    cpu.r(1).read shouldBe 3
  }

  // ===== PRIVILEGE VIOLATION: handler receives correct state =====

  "privilege violation handler runs in supervisor mode" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |resb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  spsr r1
        |  halt
        |privhandler
        |  gpsr r3
        |  halt
        |""".stripMargin)
    // The handler should be in supervisor mode (Mode bit set).
    // Exception entry sets Mode(2) and Ind(1), so PSR = 3.
    cpu.r(3).read shouldBe 3
  }

  "privilege violation handler resumes after faulting instruction via rte" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |dd trap0handler
        |resb 80
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  ldi r1, 10
        |  ldi r2, 20
        |  rte
        |  ldi r3, 30
        |  trap 0
        |privhandler
        |  ; rte does not restore registers, handler does not clobber r1, r2
        |  ldi r5, 55
        |  rte
        |trap0handler
        |  halt
        |""".stripMargin)
    // rte in user mode faults. Handler sets r5=55 but does not touch r1,r2.
    // rte in handler pops PC and PSR, resumes after the faulting rte.
    // trap 0 returns to supervisor mode for halt.
    cpu.r(1).read shouldBe 10
    cpu.r(2).read shouldBe 20
    cpu.r(3).read shouldBe 30
    cpu.r(5).read shouldBe 55
    cpu.state shouldBe State.Halt
  }

  // ===== Combined / edge cases =====

  "fence before wfi does not interfere" in {
    var interruptFired = false
    val interruptSource: CPU => Unit = cpu =>
      if !interruptFired && cpu.state == State.Wfi then
        interruptFired = true
        cpu.interrupt()
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd isr
        |resb 136
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  fence
        |  wfi
        |  ldi r1, 99
        |  halt
        |isr
        |  rte
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Seq(interruptSource)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 99
    cpu.state shouldBe State.Halt
  }

  "wfi with interrupts disabled stays suspended" in {
    // If Ind (interrupt disable) is set, wfi should stay suspended forever
    // because the interrupt callback calls cpu.interrupt() but it won't
    // transition state since Ind is set.
    val interruptSource: CPU => Unit = cpu => cpu.interrupt()
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd isr
        |resb 136
        |reset
        |  ldi r1, 3
        |  spsr r1
        |  wfi
        |  ldi r1, 42
        |  halt
        |isr
        |  rte
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Seq(interruptSource)) { limit = 100 }
    cpu.reset()
    cpu.run()
    // Should exhaust the limit while stuck in Wfi
    cpu.r(1).read should not be 42L
  }

  "gpsr does not trigger privilege violation even after mode cleared" in {
    // Explicitly test that gpsr after clearing mode does NOT cause a fault
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |dd trap0handler
        |resb 80
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  gpsr r2
        |  ldi r3, 88
        |  trap 0
        |privhandler
        |  ldi r3, 11
        |  halt
        |trap0handler
        |  halt
        |""".stripMargin)
    // If gpsr triggered a privilege violation, r3 would be 11.
    // It should NOT fault, so r3 should be 88. Trap 0 returns to supervisor for halt.
    cpu.r(3).read shouldBe 88
    cpu.state shouldBe State.Halt
  }

  // ===== CLI (disable interrupts) =====

  "cli sets Ind flag" in {
    val cpu = runCPU(VECTORS +
      """cli
        |gpsr r1
        |halt
        |""".stripMargin)
    (cpu.r(1).read & 1) shouldBe 1
  }

  // ===== STI (enable interrupts) =====

  "sti clears Ind flag" in {
    val cpu = runCPU(VECTORS +
      """cli
        |sti
        |gpsr r1
        |halt
        |""".stripMargin)
    (cpu.r(1).read & 1) shouldBe 0
  }

  // ===== SWSP (swap r7 and usp) =====

  "swsp swaps r7 and usp" in {
    val cpu = runCPU(VECTORS +
      """movi r1, 0x800
        |susp r1
        |swsp
        |gusp r2
        |halt
        |""".stripMargin)
    cpu.r(7).read shouldBe 0x800
    cpu.r(2).read should not be 0x800L
  }

  "swsp twice is identity" in {
    val cpu = runCPU(VECTORS +
      """gusp r1
        |swsp
        |swsp
        |gusp r2
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe cpu.r(2).read
  }

  // ===== TSR (read cycle counter) =====

  "tsr reads nonzero cycle count" in {
    val cpu = runCPU(VECTORS +
      """nop
        |nop
        |nop
        |tsr r1
        |halt
        |""".stripMargin)
    cpu.r(1).read should be > 0L
  }

  "tsr increases with more instructions" in {
    val cpu = runCPU(VECTORS +
      """tsr r1
        |nop
        |nop
        |nop
        |nop
        |nop
        |tsr r2
        |halt
        |""".stripMargin)
    (cpu.r(2).read - cpu.r(1).read) shouldBe 6 // 5 nops + 1 for the first tsr itself
  }
}
