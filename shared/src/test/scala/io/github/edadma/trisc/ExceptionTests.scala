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

  "reset vector is loaded from address 0" in {
    val cpu = runCPU(
      """dw start
        |dw 0
        |dw 0
        |dw 0
        |halt
        |start
        |  ldi r1, 42
        |  halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
  }

  // ===== TRAP =====

  "trap 0 dispatches to vector 3" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dw reset
        |dw 0
        |dw 0
        |dw handler
        |reset
        |  ldi r1, 0
        |  spsr r1
        |  trap 0
        |  halt
        |handler
        |  movi r3, STDOUT
        |  sti r3, 'T'
        |  rte
        |""".stripMargin)
    output shouldBe "T"
  }

  "trap preserves and restores registers via rte" in {
    val cpu = runCPU(
      """dw reset
        |dw 0
        |dw 0
        |dw handler
        |reset
        |  ldi r1, 0
        |  spsr r1
        |  ldi r1, 42
        |  ldi r2, 99
        |  trap 0
        |  halt
        |handler
        |  ldi r1, 0
        |  ldi r2, 0
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(2).read shouldBe 99
  }

  "trap resumes at instruction after trap" in {
    val cpu = runCPU(
      """dw reset
        |dw 0
        |dw 0
        |dw handler
        |reset
        |  ldi r1, 0
        |  spsr r1
        |  trap 0
        |  ldi r1, 77
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 77
  }

  // ===== RTE =====

  "rte restores all registers" in {
    val cpu = runCPU(
      """dw reset
        |dw 0
        |dw 0
        |dw handler
        |reset
        |  ldi r1, 0
        |  spsr r1
        |  ldi r1, 1
        |  ldi r2, 2
        |  ldi r3, 3
        |  ldi r4, 4
        |  ldi r5, 5
        |  ldi r6, 6
        |  ldi r7, 7
        |  trap 0
        |  halt
        |handler
        |  ldi r1, 0
        |  ldi r2, 0
        |  ldi r3, 0
        |  ldi r4, 0
        |  ldi r5, 0
        |  ldi r6, 0
        |  ldi r7, 0
        |  rte
        |""".stripMargin)
    for i <- 1 to 7 do cpu.r(i).read shouldBe i
  }

  "rte restores PSR" in {
    val cpu = runCPU(
      """dw reset
        |dw 0
        |dw 0
        |dw handler
        |reset
        |  ldi r1, 0
        |  spsr r1
        |  trap 0
        |  gpsr r1
        |  halt
        |handler
        |  rte
        |""".stripMargin)
    cpu.r(1).read shouldBe 0
  }

  // ===== Interrupts =====

  "interrupt dispatches to vector 1 when enabled" in {
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
        |dw reset
        |dw isr
        |dw 0
        |dw 0
        |reset
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
    val tof = assemble(VECTORS + "ldi r1, 1\nspsr r1\nldi r1, 42\nhalt\n")
    tof.load(mem)
    val cpu = new CPU(mem, List(interruptSource)) { limit = 100 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
    cpu.state shouldBe State.Halt
  }
}
