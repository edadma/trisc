package io.github.edadma.trisc

class MPUTests extends TestHelpers {

  // ===== SimpleMPU unit tests =====

  "SimpleMPU allows all access when disabled" in {
    val mpu = new SimpleMPU()
    mpu.check(0x1000, Access.Read, supervisor = false) shouldBe true
    mpu.check(0x1000, Access.Write, supervisor = false) shouldBe true
    mpu.check(0x1000, Access.Execute, supervisor = false) shouldBe true
  }

  "SimpleMPU denies all access when enabled with no regions configured" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(72, 1) // enable MPU
    mpu.check(0x1000, Access.Read, supervisor = true) shouldBe false
  }

  "SimpleMPU allows supervisor read to configured region" in {
    val mpu = new SimpleMPU()
    // Region 0: base=0x1000, size=0x100, attr=SR|EN (0x41)
    mpu.writeRegister(2, 0x10) // base high byte
    mpu.writeRegister(3, 0x00) // base low byte
    mpu.writeRegister(6, 0x01) // size high byte
    mpu.writeRegister(7, 0x00) // size low byte
    mpu.writeRegister(8, 0x41) // SR | EN
    mpu.writeRegister(72, 1)   // enable
    mpu.check(0x1000, Access.Read, supervisor = true) shouldBe true
    mpu.check(0x1000, Access.Write, supervisor = true) shouldBe false
  }

  "SimpleMPU denies user access when only supervisor permitted" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(2, 0x10)
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(6, 0x01)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(8, 0x47) // SR|SW|SX|EN
    mpu.writeRegister(72, 1)
    mpu.check(0x1000, Access.Read, supervisor = false) shouldBe false
  }

  "SimpleMPU allows user read when UR set" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(2, 0x10)
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(6, 0x01)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(8, 0x49) // SR|UR|EN
    mpu.writeRegister(72, 1)
    mpu.check(0x1000, Access.Read, supervisor = false) shouldBe true
  }

  "SimpleMPU higher region overrides lower" in {
    val mpu = new SimpleMPU()
    // Region 0: base=0x1000, size=0x1000, SR|EN (read-only)
    mpu.writeRegister(2, 0x10)
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(6, 0x10)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(8, 0x41) // SR|EN
    // Region 1: base=0x1000, size=0x100, SR|SW|EN (read-write, overlapping)
    mpu.writeRegister(9 + 2, 0x10)
    mpu.writeRegister(9 + 3, 0x00)
    mpu.writeRegister(9 + 6, 0x01)
    mpu.writeRegister(9 + 7, 0x00)
    mpu.writeRegister(9 + 8, 0x43) // SR|SW|EN
    mpu.writeRegister(72, 1)
    // Address in both regions — region 1 wins (higher number)
    mpu.check(0x1000, Access.Write, supervisor = true) shouldBe true
    // Address only in region 0
    mpu.check(0x1500, Access.Write, supervisor = true) shouldBe false
  }

  "SimpleMPU disabled region is skipped" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(2, 0x10)
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(6, 0x01)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(8, 0x01) // SR but NOT EN
    mpu.writeRegister(72, 1)
    mpu.check(0x1000, Access.Read, supervisor = true) shouldBe false // no enabled region matches
  }

  "SimpleMPU register readback" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(2, 0xAB)
    mpu.writeRegister(3, 0xCD)
    mpu.readRegister(2) shouldBe 0xAB
    mpu.readRegister(3) shouldBe 0xCD
  }

  "SimpleMPU control register readback" in {
    val mpu = new SimpleMPU()
    mpu.readRegister(72) shouldBe 0
    mpu.writeRegister(72, 1)
    mpu.readRegister(72) shouldBe 1
  }

  "SimpleMPU address at region boundary is inside" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(2, 0x10) // base = 0x1000
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(6, 0x01) // size = 0x100
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(8, 0x41) // SR|EN
    mpu.writeRegister(72, 1)
    mpu.check(0x1000, Access.Read, supervisor = true) shouldBe true  // start
    mpu.check(0x10FF, Access.Read, supervisor = true) shouldBe true  // last byte
    mpu.check(0x1100, Access.Read, supervisor = true) shouldBe false // one past end
  }

  "SimpleMPU execute permission" in {
    val mpu = new SimpleMPU()
    mpu.writeRegister(2, 0x10)
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(6, 0x01)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(8, 0x64) // SX|UX|EN (execute only)
    mpu.writeRegister(72, 1)
    mpu.check(0x1000, Access.Execute, supervisor = true) shouldBe true
    mpu.check(0x1000, Access.Execute, supervisor = false) shouldBe true
    mpu.check(0x1000, Access.Read, supervisor = true) shouldBe false
    mpu.check(0x1000, Access.Write, supervisor = true) shouldBe false
  }

  // ===== CPU integration tests =====

  "CPU without MPU works normally" in {
    val output = runProgram(
      s"""${VECTORS}movi r1, 0xFF8
         |sti r1, 'O'
         |sti r1, 'K'
         |halt
         |""".stripMargin)
    output shouldBe "OK"
  }

  "CPU with MPU disabled works normally" in {
    val mpu = new SimpleMPU()
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    val tof = assemble(
      s"""${VECTORS}movi r1, 0xFF8
         |sti r1, 'O'
         |sti r1, 'K'
         |halt
         |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, mpu = Some(mpu), mpuBase = 0xF00) { limit = 10000 }
    cpu.reset()
    cpu.run()
    output.toString shouldBe "OK"
  }

  "MPU registers readable in supervisor mode" in {
    val mpu = new SimpleMPU()
    // Pre-configure region 0 base byte 2 = 0xAB
    mpu.writeRegister(2, 0xAB)
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(
      s"""${VECTORS}; supervisor mode — read MPU register
         |movi r3, 0xF02
         |ldb r1, r3, r0
         |halt
         |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, mpu = Some(mpu), mpuBase = 0xF00) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 0xAB
  }

  "MPU registers writable in supervisor mode" in {
    val mpu = new SimpleMPU()
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(
      s"""${VECTORS}; supervisor mode — write MPU register
         |movi r3, 0xF02
         |ldi r1, 0xCD
         |stb r1, r3, r0
         |halt
         |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, mpu = Some(mpu), mpuBase = 0xF00) { limit = 10000 }
    cpu.reset()
    cpu.run()
    mpu.readRegister(2) shouldBe 0xCD
  }

  "MPU registers trigger privilege violation in user mode" in {
    val mpu = new SimpleMPU()
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    // PrivilegeViolation = state ordinal 6, vector slot 7 (offset 56)
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd privhandler
        |dd 0
        |rb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  ; now in user mode — try to read MPU register
        |  movi r3, 0xF02
        |  ldb r4, r3, r0
        |  ldi r2, 99
        |  halt
        |privhandler
        |  ldi r2, 42
        |  halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, mpu = Some(mpu), mpuBase = 0xF00) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(2).read shouldBe 42
  }

  "MPU blocks user write to protected region" in {
    val mpu = new SimpleMPU()
    // Region 0: base=0x000, size=0x1000, all supervisor + user RX (no user write)
    // SR|SW|SX|UR|UX|EN = 0x01|0x02|0x04|0x08|0x20|0x40 = 0x6F
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(6, 0x10)
    mpu.writeRegister(8, 0x6F)
    mpu.writeRegister(72, 1) // enable

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    // DataAccess = state ordinal 3, vector slot 4 (offset 32)
    val tof = assemble(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd datahandler
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |rb 88
        |reset
        |  movi r1, 0xE00
        |  susp r1
        |  ldi r1, 0
        |  spsr r1
        |  ; now in user mode — try to write
        |  movi r3, 0x800
        |  ldi r1, 0x42
        |  stb r1, r3, r0
        |  ldi r2, 99
        |  halt
        |datahandler
        |  ldi r2, 77
        |  halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, mpu = Some(mpu), mpuBase = 0xF00) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(2).read shouldBe 77
    mem.readByte(0x800) should not be 0x42
  }

  "MPU allows supervisor write to same region" in {
    val mpu = new SimpleMPU()
    // Region 0: base=0x000, size=0x1000, SR|SW|SX|UR|UX|EN = 0x6F
    mpu.writeRegister(3, 0x00)
    mpu.writeRegister(7, 0x00)
    mpu.writeRegister(6, 0x10)
    mpu.writeRegister(8, 0x6F)
    mpu.writeRegister(72, 1)

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    val tof = assemble(
      s"""${VECTORS}; supervisor mode — write should succeed
         |movi r3, 0x800
         |ldi r1, 0x42
         |stb r1, r3, r0
         |halt
         |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, mpu = Some(mpu), mpuBase = 0xF00) { limit = 10000 }
    cpu.reset()
    cpu.run()
    mem.readByte(0x800) shouldBe 0x42
  }
}
