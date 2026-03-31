package io.github.edadma.trisc

class AlignmentTests extends TestHelpers {

  // ===== align directive =====

  "align 4 pads to 4-byte boundary" in {
    val tof = assemble("db 0x01\nalign 4\ndb 0x02\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    // byte at 0, align 4 pads to offset 4, so 0x02 is at offset 4
    ram.readByteUnsigned(4) shouldBe 0x02
  }

  "align 8 pads to 8-byte boundary" in {
    val tof = assemble("db 0x01\nalign 8\ndb 0x02\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    ram.readByteUnsigned(8) shouldBe 0x02
  }

  "align when already aligned adds no padding" in {
    val tof = assemble("dw 0\nalign 4\ndb 0x42\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    // dw is 4 bytes, already 4-byte aligned
    ram.readByteUnsigned(4) shouldBe 0x42
  }

  "align 2 after odd byte count" in {
    val tof = assemble("db 0x01\nalign 2\ndb 0x02\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    ram.readByteUnsigned(2) shouldBe 0x02
  }

  // ===== Misaligned access detection =====

  "misaligned short read triggers exception" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |dd 0
        |rb 88
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  movi r2, buf
        |  addi r2, r2, 1
        |  lds r3, r2, r0
        |  ldi r1, 42
        |  halt
        |handler
        |  rte
        |buf rb 8
        |""".stripMargin)
    // lds faults, handler rte returns to instruction after lds, r1 gets set to 42
    cpu.r(1).read shouldBe 42
    // r3 should be 0 since the faulting load returned 0
    cpu.r(3).read shouldBe 0
  }

  "misaligned word read triggers exception" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |dd 0
        |rb 88
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  movi r2, buf
        |  addi r2, r2, 2
        |  ldw r3, r2, r0
        |  ldi r1, 42
        |  halt
        |handler
        |  rte
        |align 4
        |buf rb 8
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(3).read shouldBe 0
  }

  "misaligned double read triggers exception" in {
    val cpu = runCPU(
      """dd 0xFF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd handler
        |dd 0
        |dd 0
        |dd 0
        |rb 88
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  movi r2, buf
        |  addi r2, r2, 4
        |  ldd r3, r2, r0
        |  ldi r1, 42
        |  halt
        |handler
        |  rte
        |align 8
        |buf rb 16
        |""".stripMargin)
    cpu.r(1).read shouldBe 42
    cpu.r(3).read shouldBe 0
  }

  "aligned accesses do not trigger exception" in {
    val cpu = runCPU(VECTORS +
      """movi r1, buf
        |ldi r2, 0x42
        |stb r2, r1, r0
        |ldb r3, r1, r0
        |halt
        |buf rb 4
        |""".stripMargin)
    cpu.r(3).read shouldBe 0x42
    cpu.state shouldBe State.Halt // no exception
  }

  "aligned word access works normally" in {
    val cpu = runCPU(VECTORS +
      """movi r1, buf
        |ldi r2, 42
        |stw r2, r1, r0
        |ldw r3, r1, r0
        |halt
        |align 4
        |buf rb 4
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
    cpu.state shouldBe State.Halt
  }

  "aligned double access works normally" in {
    val cpu = runCPU(VECTORS +
      """movi r1, buf
        |ldi r2, 42
        |std r2, r1, r0
        |ldd r3, r1, r0
        |halt
        |align 8
        |buf rb 8
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
    cpu.state shouldBe State.Halt
  }

  // ===== Reserve directive auto-alignment =====

  "rs auto-aligns to 2-byte boundary" in {
    val tof = assemble("db 0x01\nrs 1\ndb 0x42\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    // db at 0, pad byte at 1, rs 1 = 2 bytes at 2-3, db at 4
    ram.readByteUnsigned(4) shouldBe 0x42
  }

  "rw auto-aligns to 4-byte boundary" in {
    val tof = assemble("db 0x01\nrw 1\ndb 0x42\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    // db at 0, pad 3 bytes, rw 1 = 4 bytes at 4-7, db at 8
    ram.readByteUnsigned(8) shouldBe 0x42
  }

  "rl auto-aligns to 8-byte boundary" in {
    val tof = assemble("db 0x01\nrl 1\ndb 0x42\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    // db at 0, pad 7 bytes, rl 1 = 8 bytes at 8-15, db at 16
    ram.readByteUnsigned(16) shouldBe 0x42
  }

  "rb does not auto-align" in {
    val tof = assemble("db 0x01\nrb 2\ndb 0x42\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    // db at 0, rb 2 at 1-2, db at 3
    ram.readByteUnsigned(3) shouldBe 0x42
  }

  "rw when already aligned adds no padding" in {
    val tof = assemble("dw 0\nrw 1\ndb 0x42\n")
    val ram = new RAM(0, 256)
    tof.load(ram)
    // dw at 0-3, rw 1 at 4-7 (already aligned), db at 8
    ram.readByteUnsigned(8) shouldBe 0x42
  }

  "rw auto-align updates preceding label" in {
    val tof = assemble("db 0x01\nbuf rw 1\ndb 0x42\n", relocatable = true)
    val sym = tof.symbolByName("buf").get
    // db at 0, pad to 4, buf label at 4
    sym.offset shouldBe 4
  }

  "rw usable for aligned word buffer" in {
    val cpu = runCPU(VECTORS +
      """movi r1, buf
        |ldi r2, 42
        |stw r2, r1, r0
        |ldw r3, r1, r0
        |halt
        |buf rw 1
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
    cpu.state shouldBe State.Halt
  }

  "rl usable for aligned double buffer" in {
    val cpu = runCPU(VECTORS +
      """movi r1, buf
        |ldi r2, 42
        |std r2, r1, r0
        |ldd r3, r1, r0
        |halt
        |buf rl 1
        |""".stripMargin)
    cpu.r(3).read shouldBe 42
    cpu.state shouldBe State.Halt
  }
}
