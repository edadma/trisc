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

  // ===== Relocatable mode =====

  "relocatable mode exports global labels as symbols" in {
    val tof = assemble("main\n  halt\nhelper\n  halt\n", relocatable = true)
    val syms = tof.segments.head.symbols
    syms.map(_.name) should contain("main")
    syms.map(_.name) should contain("helper")
  }

  "relocatable mode does not export local labels" in {
    val tof = assemble("main\n.loop\n  halt\n", relocatable = true)
    val syms = tof.segments.head.symbols
    syms.map(_.name) should contain("main")
    syms.map(_.name) should not contain "main.loop"
  }

  "relocatable mode emits MOVI2 relocation for unresolved reference" in {
    val tof = assemble("main\n  movi r1, printf\n  halt\n", relocatable = true)
    val seg = tof.segments.head
    seg.externs should contain("printf")
    seg.relocs.length shouldBe 1
    seg.relocs.head.typ shouldBe RelocType.MOVI2
    seg.relocs.head.symbol shouldBe "printf"
  }

  "relocatable mode emits MOVI3 relocation with addresses=3" in {
    val tof = assemble("main\n  movi r1, printf\n  halt\n", relocatable = true, addresses = 3)
    val seg = tof.segments.head
    seg.relocs.head.typ shouldBe RelocType.MOVI3
  }

  "relocatable mode emits MOVI4 relocation with addresses=4" in {
    val tof = assemble("main\n  movi r1, printf\n  halt\n", relocatable = true, addresses = 4)
    val seg = tof.segments.head
    seg.relocs.head.typ shouldBe RelocType.MOVI4
  }

  "relocatable mode emits ABS32 relocation for dw with unresolved reference" in {
    val tof = assemble("dw handler\nhalt\n", relocatable = true)
    val seg = tof.segments.head
    seg.externs should contain("handler")
    seg.relocs.length shouldBe 1
    seg.relocs.head.typ shouldBe RelocType.ABS32
    seg.relocs.head.symbol shouldBe "handler"
  }

  "relocatable mode resolves local references normally" in {
    val tof = assemble("main\n  movi r1, main\n  halt\n", relocatable = true)
    val seg = tof.segments.head
    // main is defined locally, so no extern/reloc needed
    seg.externs shouldBe empty
    seg.relocs shouldBe empty
  }

  "relocatable TOF round-trips through serialize/deserialize" in {
    val tof = assemble(
      """dw handler
        |main
        |  movi r1, printf
        |  halt
        |""".stripMargin, relocatable = true)
    val s = tof.serialize
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.externs should contain("printf")
    tof2.segments.head.externs should contain("handler")
    tof2.segments.head.relocs.length shouldBe 2
    tof2.segments.head.symbols.map(_.name) should contain("main")
    tof2.serialize shouldBe s
  }

  "relocatable mode is not fully resolved" in {
    val tof = assemble("movi r1, ext_func\nhalt\n", relocatable = true)
    tof.isFullyResolved shouldBe false
  }

  "non-relocatable mode rejects unresolved references" in {
    an[Exception] should be thrownBy {
      assemble("movi r1, nowhere\nhalt\n", relocatable = false)
    }
  }

  "relocatable mode movi placeholder is correct size" in {
    // movi with addresses=2 should emit 4 bytes (2 instructions)
    val tof = assemble("movi r1, ext\nhalt\n", relocatable = true, addresses = 2)
    val seg = tof.segments.head
    seg.relocs.head.offset shouldBe 0
    // The data should have 4 bytes for movi (2 instructions) + 2 bytes for halt = 6 bytes total
    val dataSize = seg.chunks.collect { case TOF.DataChunk(d) => d.length }.sum
    dataSize shouldBe 6
  }

  "relocatable mode dw placeholder is correct size" in {
    val tof = assemble("dw ext\nhalt\n", relocatable = true)
    val seg = tof.segments.head
    seg.relocs.head.offset shouldBe 0
    // 4 bytes for dw placeholder + 2 bytes for halt = 6 bytes
    val dataSize = seg.chunks.collect { case TOF.DataChunk(d) => d.length }.sum
    dataSize shouldBe 6
  }
}
