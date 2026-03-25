package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers {

  def mkCPU(program: String, memSize: Int = 0x1000, addresses: Int = 2): (CPU, StringBuilder) =
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = memSize - 8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }
    val mem = new Memory("Memory", new RAM(0, memSize - 8), stdout)
    val tof = assemble(program, addresses = addresses)
    tof.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    (cpu, output)

  def runProgram(program: String, memSize: Int = 0x1000, addresses: Int = 2): String =
    val (cpu, output) = mkCPU(program, memSize, addresses)
    cpu.run()
    output.toString

  // --- Register and basic instructions ---

  "r0 is hardwired to zero" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 42
        |add r1, r1, r0
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(0).read shouldBe 0
    cpu.r(1).read shouldBe 42
  }

  "ldi loads immediate into register" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 42
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  "sli shifts left and ORs immediate" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 0xAB
        |sli r1, 0xCD
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(1).read shouldBe 0xABCD
  }

  "movi loads multi-byte immediate" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r1, 0x1234
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(1).read shouldBe 0x1234
  }

  // --- Arithmetic ---

  "add" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 10
        |ldi r2, 20
        |add r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 30
  }

  "sub" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 50
        |ldi r2, 20
        |sub r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 30
  }

  "mul" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 6
        |ldi r2, 7
        |mul r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 42
  }

  "div" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 42
        |ldi r2, 6
        |div r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 7
  }

  "rem" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 17
        |ldi r2, 5
        |rem r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 2
  }

  "addi" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 10
        |addi r2, r1, 5
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(2).read shouldBe 15
  }

  "addi with negative immediate" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 10
        |addi r2, r1, -3
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(2).read shouldBe 7
  }

  // --- Bitwise ---

  "and" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 0x0F
        |ldi r2, 0x37
        |and r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0x07
  }

  "or" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 0x0F
        |ldi r2, 0x30
        |or r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0x3F
  }

  "xor" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 0xFF
        |ldi r2, 0x0F
        |xor r3, r1, r2
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0xF0
  }

  // --- Branching ---

  "beq taken" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 5
        |ldi r2, 5
        |beq r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 99
  }

  "beq not taken" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 5
        |ldi r2, 6
        |beq r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 1
    cpu.r(4).read shouldBe 99
  }

  "bls taken" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 3
        |ldi r2, 5
        |bls r1, r2, skip
        |ldi r3, 1
        |skip
        |ldi r4, 99
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0
    cpu.r(4).read shouldBe 99
  }

  "bls not taken when equal" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 5
        |ldi r2, 5
        |bls r1, r2, skip
        |ldi r3, 1
        |skip
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 1
  }

  "bra unconditional branch" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |bra skip
        |ldi r1, 1
        |skip
        |ldi r2, 99
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(1).read shouldBe 0
    cpu.r(2).read shouldBe 99
  }

  // --- Pseudo-instructions ---

  "nop does nothing" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 42
        |nop
        |nop
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  "mov copies register" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 77
        |mov r2, r1
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(2).read shouldBe 77
  }

  // --- Load/Store ---

  "stb and ldb" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 0x41
        |movi r2, buf
        |stb r1, r2, r0
        |ldb r3, r2, r0
        |halt
        |buf db 0
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0x41
  }

  "sts and lds" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r1, 0x1234
        |movi r2, buf
        |sts r1, r2, r0
        |lds r3, r2, r0
        |halt
        |buf ds 0
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 0x1234
  }

  "stw and ldw" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r1, 0x1234
        |sli r1, 0x56
        |movi r2, buf
        |stw r1, r2, r0
        |ldw r3, r2, r0
        |halt
        |buf resb 4
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe cpu.r(1).read
  }

  "sti writes byte to memory" in {
    val output = runProgram(
      """
        |STDOUT = 0xFF8
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r1, STDOUT
        |sti r1, 'A'
        |halt
        |""".stripMargin,
    )
    output shouldBe "A"
  }

  "ld and st with offset" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 42
        |movi r2, buf
        |st r1, r2, 0
        |ld r3, r2, 0
        |halt
        |buf resb 4
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(3).read shouldBe 42
  }

  // --- Loops and stdout ---

  "loop counting to 3" in {
    val output = runProgram(
      """
        |STDOUT = 0xFF8
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 1
        |movi r3, STDOUT
        |loop
        |  addi r4, r1, '0'
        |  stb r4, r3, r0
        |  addi r1, r1, 1
        |  ldi r2, 3
        |  bls r2, r1, done
        |  bra loop
        |done
        |  halt
        |""".stripMargin,
    )
    output shouldBe "123"
  }

  // --- jalr ---

  "jalr subroutine call and return" in {
    val output = runProgram(
      """
        |STDOUT = 0xFF8
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r2, 'A'
        |movi r1, printChar
        |jalr r7, r1
        |ldi r2, 'B'
        |movi r1, printChar
        |jalr r7, r1
        |halt
        |printChar
        |  movi r4, STDOUT
        |  stb r2, r4, r0
        |  jalr r0, r7
        |""".stripMargin,
    )
    output shouldBe "AB"
  }

  // --- Traps and interrupt vectors ---

  "trap dispatches to vector" in {
    val output = runProgram(
      """
        |STDOUT = 0xFF8
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
        |""".stripMargin,
    )
    output shouldBe "T"
  }

  // --- Assembler ---

  "assembler rejects duplicate symbol" in {
    an[Exception] should be thrownBy {
      assemble(
        """
          |foo
          |  halt
          |foo
          |  halt
          |""".stripMargin,
      )
    }
  }

  "assembler handles equates" in {
    val (cpu, _) = mkCPU(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |VAL = 42
        |ldi r1, VAL
        |halt
        |""".stripMargin,
    )
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  // --- Decode table ---

  "illegal instruction throws" in {
    an[Exception] should be thrownBy {
      IllegalInstruction(new CPU(new RAM(0, 64), Nil))
    }
  }

  // --- Memory ---

  "RAM read/write" in {
    val ram = new RAM(0, 256)
    ram.writeByte(0, 0x42)
    ram.readByte(0) shouldBe 0x42
  }

  "ROM is not writable" in {
    val rom = new ROM(0, 256)
    rom.loadByte(0, 0x42)
    rom.readByte(0) shouldBe 0x42
    an[Exception] should be thrownBy {
      rom.writeByte(0, 0x43)
    }
  }

  "Memory rejects overlapping blocks" in {
    an[Exception] should be thrownBy {
      new Memory("test", new RAM(0, 256), new RAM(128, 256))
    }
  }

  "Addressable multi-byte read/write" in {
    val ram = new RAM(0, 256)
    ram.writeShort(0, 0x1234)
    ram.readShortUnsigned(0) shouldBe 0x1234
    ram.writeInt(4, 0x12345678)
    ram.readInt(4) shouldBe 0x12345678
  }

  // --- TOF serialize/deserialize ---

  "TOF round-trips through serialize/deserialize" in {
    val tof = assemble(
      """
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |ldi r1, 42
        |halt
        |""".stripMargin,
    )
    val serialized = tof.serialize
    val deserialized = TOF.deserialize(serialized)
    deserialized.serialize shouldBe serialized
  }
}
