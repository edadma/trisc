package io.github.edadma.trisc

class ExceptionTests extends TestHelpers {

  // ===== CPU Reset =====

  "reset clears registers and sets mode" in {
    val (cpu, _) = mkCPU(VECTORS + "halt\n")
    cpu.state shouldBe State.Reset
    cpu.test(Status.Mode) shouldBe true
    cpu.test(Status.Ind) shouldBe true
    cpu.test(Status.C) shouldBe false
  }

  "reset loads SSP from address 0 and PC from address 8" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd start
        |resb 144
        |halt
        |start
        |  ldi r1, 42
        |  halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }

  // ===== TRAP =====

  "trap 0 dispatches to vector 9" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dd 0xFF0
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
        |  trap 0
        |  halt
        |handler
        |  movi r3, STDOUT
        |  sti r3, 'T'
        |  rte
        |""".stripMargin)
    output shouldBe "T"
  }

  "trap preserves registers when handler does not clobber them" in {
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
        |  ldi r1, 42
        |  ldi r2, 99
        |  trap 0
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(2).read shouldBe 99
  }

  "trap resumes at instruction after trap" in {
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
        |  trap 0
        |  ldi r1, 77
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 77
  }

  // ===== RTE =====

  "rte pops PC and PSR from stack" in {
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
        |  ldi r1, 1
        |  ldi r2, 2
        |  trap 0
        |  ldi r3, 33
        |  halt
        |handler
        |  ; handler clobbers r1 and r2 — rte does NOT restore them
        |  ldi r1, 0
        |  ldi r2, 0
        |  rte
        |""".stripMargin)
    ; // rte only restores PC and PSR, not registers
    cpu.r(1).read shouldBe 0  // clobbered by handler
    cpu.r(2).read shouldBe 0  // clobbered by handler
    cpu.r(3).read shouldBe 33 // resumes after trap
  }

  "rte restores PSR" in {
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
        |  gpsr r1
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    ; // PSR was set to 2 (Mode only), trap saved it, rte restored it
    cpu.r(1).read shouldBe 2
  }

  // ===== Interrupts =====

  "interrupt dispatches to vector 2 when enabled" in {
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
      if !interruptFired then
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
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |loop
        |  bra loop
        |isr
        |  movi r3, STDOUT
        |  sti r3, 'I'
        |  halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, List(interruptSource)) { limit = 10000 }
    cpu.reset()
    cpu.run()
    output.toString shouldBe "I"
  }

  "interrupt is masked when Ind is set" in {
    val interruptSource: CPU => Unit = cpu => cpu.interrupt()
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(VECTORS + "ldi r1, 3\nspsr r1\nldi r1, 42\nhalt\n")
    tof.load(mem)
    val cpu = new CPU(mem, List(interruptSource)) { limit = 100 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
    cpu.state shouldBe State.Halt
  }
}
