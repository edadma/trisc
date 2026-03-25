package io.github.edadma.trisc

class AssemblerTests extends TestHelpers {

  "rejects duplicate symbol" in {
    an[Exception] should be thrownBy {
      assemble("foo\n  halt\nfoo\n  halt\n")
    }
  }

  "handles equates" in {
    val cpu = runCPU(VECTORS + "VAL = 42\nldi r1, VAL\nhalt\n")
    cpu.r(1).read shouldBe 42
  }

  "handles segments with orgs" in {
    val cpu = runCPU(
      """dw reset
        |dw 0
        |dw 0
        |dw 0
        |reset
        |  movi r1, data
        |  ldw r2, r1, r0
        |  halt
        |segment data
        |data dw 0x1234
        |""".stripMargin,
      orgs = Map("data" -> 0x800L))
    cpu.r(2).read shouldBe 0x1234
  }

  "handles local labels" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |foo
        |  addi r1, r1, 1
        |  ldi r2, 3
        |  beq r1, r2, .done
        |  bra foo
        |.done
        |bar
        |  addi r1, r1, 10
        |  ldi r2, 13
        |  beq r1, r2, .done
        |  bra bar
        |.done
        |  halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 13
  }

  "data directive db" in {
    val cpu = runCPU(VECTORS + "movi r1, data\nldb r2, r1, r0\nhalt\ndata db 0x42\n")
    cpu.r(2).read shouldBe 0x42
  }

  "data directive string" in {
    val cpu = runCPU(VECTORS +
      """movi r1, str
        |ldb r2, r1, r0
        |addi r3, r1, 1
        |ldb r4, r3, r0
        |halt
        |str db "AB", 0
        |""".stripMargin)
    cpu.r(2).read shouldBe 'A'
    cpu.r(4).read shouldBe 'B'
  }

  "illegal instruction throws" in {
    an[Exception] should be thrownBy {
      IllegalInstruction(new CPU(new RAM(0, 64), Nil))
    }
  }
}
