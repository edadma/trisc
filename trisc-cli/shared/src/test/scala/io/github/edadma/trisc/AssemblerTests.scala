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
      """dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
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

  "illegal instruction sets UnimplementedOpcode state" in {
    val cpu = new CPU(new RAM(0, 64)) { quiet = true }
    IllegalInstruction(cpu)
    cpu.state shouldBe State.UnimplementedOpcode
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
    val tof = assemble("main\n  movi r1, printf\n  halt\n", relocatable = true, addresses = 2)
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

  "relocatable mode defers local absolute references to linker" in {
    val tof = assemble("main\n  movi r1, main\n  halt\n", relocatable = true)
    val seg = tof.segments.head
    // Local movi generates a relocation so the linker can adjust the address
    seg.relocs should not be empty
    seg.relocs.head.symbol shouldBe "main"
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
    val tof = assemble("movi r1, ext\nhalt\n", relocatable = true, addresses = 2)
    val seg = tof.segments.head
    seg.relocs.head.offset shouldBe 0
    val dataSize = seg.chunks.collect { case TOF.DataChunk(d) => d.length }.sum
    dataSize shouldBe 6
  }

  "relocatable mode dw placeholder is correct size" in {
    val tof = assemble("dw ext\nhalt\n", relocatable = true)
    val seg = tof.segments.head
    seg.relocs.head.offset shouldBe 0
    val dataSize = seg.chunks.collect { case TOF.DataChunk(d) => d.length }.sum
    dataSize shouldBe 6
  }

  // ===== extern directive =====

  "extern declares external symbol" in {
    val tof = assemble(
      """extern printf
        |main
        |  movi r1, printf
        |  halt
        |""".stripMargin, relocatable = true)
    val seg = tof.segments.head
    seg.externs should contain("printf")
    seg.relocs.length shouldBe 1
    seg.relocs.head.symbol shouldBe "printf"
  }

  "extern works without relocatable mode for movi" in {
    val tof = assemble(
      """extern printf
        |global main, func
        |main
        |  movi r1, printf
        |  halt
        |""".stripMargin)
    val seg = tof.segments.head
    seg.externs should contain("printf")
    seg.relocs.length shouldBe 1
    seg.relocs.head.symbol shouldBe "printf"
  }

  "extern works without relocatable mode for dw" in {
    val tof = assemble(
      """extern handler
        |dw handler
        |halt
        |""".stripMargin)
    val seg = tof.segments.head
    seg.externs should contain("handler")
    seg.relocs.length shouldBe 1
    seg.relocs.head.typ shouldBe RelocType.ABS32
  }

  "extern rejects duplicate with label" in {
    an[Exception] should be thrownBy {
      assemble("foo\n  halt\nextern foo\n")
    }
  }

  "extern rejects duplicate with another extern" in {
    an[Exception] should be thrownBy {
      assemble("extern foo\nextern foo\nhalt\n")
    }
  }

  "undeclared symbol in non-relocatable mode still errors" in {
    an[Exception] should be thrownBy {
      assemble("movi r1, unknown\nhalt\n")
    }
  }

  // ===== global directive =====

  "global exports label with func type" in {
    val tof = assemble(
      """global main, func
        |main
        |  halt
        |""".stripMargin, relocatable = true)
    val sym = tof.symbolByName("main").get
    sym.typ shouldBe SymbolType.Func
  }

  "global exports label with data type" in {
    val tof = assemble(
      """global buf, data
        |buf rb 16
        |""".stripMargin, relocatable = true)
    val sym = tof.symbolByName("buf").get
    sym.typ shouldBe SymbolType.Data
  }

  "global exports label with data type and size" in {
    val tof = assemble(
      """global buf, data, 256
        |buf rb 256
        |""".stripMargin, relocatable = true)
    val sym = tof.symbolByName("buf").get
    sym.typ shouldBe SymbolType.Data
    sym.size shouldBe Some(256)
  }

  "global defaults to func type when no type specified" in {
    val tof = assemble(
      """global main
        |main
        |  halt
        |""".stripMargin, relocatable = true)
    val sym = tof.symbolByName("main").get
    sym.typ shouldBe SymbolType.Func
  }

  "global with hex size" in {
    val tof = assemble(
      """global buf, data, 0x100
        |buf rb 256
        |""".stripMargin, relocatable = true)
    val sym = tof.symbolByName("buf").get
    sym.size shouldBe Some(256)
  }

  "global overrides default export — only globals are exported" in {
    val tof = assemble(
      """global main, func
        |main
        |  halt
        |helper
        |  halt
        |""".stripMargin, relocatable = true)
    val syms = tof.segments.head.symbols
    syms.map(_.name) should contain("main")
    syms.map(_.name) should not contain "helper"
  }

  "multiple globals" in {
    val tof = assemble(
      """global foo, func
        |global bar, func
        |foo
        |  halt
        |bar
        |  halt
        |""".stripMargin, relocatable = true)
    val syms = tof.segments.head.symbols
    syms.map(_.name) should contain("foo")
    syms.map(_.name) should contain("bar")
    syms.length shouldBe 2
  }

  "global referencing nonexistent label errors" in {
    an[Exception] should be thrownBy {
      assemble("global missing, func\nhalt\n", relocatable = true)
    }
  }

  // ===== extern + global + linker integration =====

  "extern + global end-to-end with linker" in {
    val main = assemble(
      """extern helper
        |global main, func
        |dd main
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |main
        |  movi r1, helper
        |  jalr r7, r1
        |  halt
        |""".stripMargin)
    val lib = assemble(
      """global helper, func
        |helper
        |  ldi r2, 42
        |  jalr r0, r7
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, lib))
    linked.isFullyResolved shouldBe true

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 10000; quiet = true }
    cpu.reset()
    cpu.run()
    cpu.r(2).read shouldBe 42
  }

  // ===== Synthesized branch relaxation boundary tests =====
  //
  // bne/bge/etc. emit two instructions in short form:
  //   beq rA, rB, +2    (skip bra if condition FALSE)
  //   bra target         (take branch if condition TRUE)
  //
  // The relaxation check must account for the bra being 2 bytes
  // past the start, so its displacement is (fold_offset - 2).

  "bne backward branch at relaxation boundary should relax" in {
    // Execution flow: skip past target, run through nops, bne backward to target,
    // target sets r2=99 and halts. If bne goes to the wrong place, r2 stays 77.
    //
    // Layout (code starts at 160 after VECTORS):
    //   160: bra skip           (2 bytes)
    //   162: target: ldi r2,99  (2 bytes)
    //   164:         halt        (2 bytes)
    //   166: skip:   ldi r1, 1  (2 bytes)
    //   168: <60 nops>          (120 bytes)
    //   288: bne r1, r0, target (short form = 4 bytes)
    //   292:         ldi r2,77  (2 bytes, reached if bne is buggy backward)
    //   294:         halt
    //
    // fold offset at bne (addr 288): n = 162 - (288 + 2) = -128 (at boundary).
    // bra's actual displacement would be -130 if not relaxed — overflows 7-bit.
    // Without the fix: bra wraps to +63, jumps +126 bytes from bra (at 290 + 2),
    //                  lands at 418 (garbage zeros, eventually executes halt-like 0s).
    // With the fix: bne is relaxed to long branch, correctly reaches target → r2=99.
    val nops = "  nop\n" * 60
    val program = VECTORS +
      s"""  bra skip
         |target
         |  ldi r2, 99
         |  halt
         |skip
         |  ldi r1, 1
         |$nops  bne r1, r0, target
         |  ldi r2, 77
         |  halt
         |""".stripMargin
    val cpu = runCPU(program)
    cpu.r(2).read shouldBe 99
  }

  "bne backward branch just within short-form range works correctly" in {
    // 59 nops instead of 60. fold offset = -126, actual bra disp = -128. Fits.
    val nops = "  nop\n" * 59
    val program = VECTORS +
      s"""  bra skip
         |target
         |  ldi r2, 99
         |  halt
         |skip
         |  ldi r1, 1
         |$nops  bne r1, r0, target
         |  ldi r2, 77
         |  halt
         |""".stripMargin
    val cpu = runCPU(program)
    cpu.r(2).read shouldBe 99
  }

  "bne forward branch at relaxation boundary should work" in {
    // 63 nops between bne and target. fold offset = +130. Needs relaxation.
    val nops = "  nop\n" * 63
    val program = VECTORS +
      s"""ldi r1, 0
         |  bne r1, r0, target
         |$nops  halt
         |target
         |  ldi r2, 42
         |  halt
         |""".stripMargin
    val cpu = runCPU(program)
    // bne r1, r0 is false (r1=0), so should NOT branch — fall through to nops then halt
    cpu.r(2).read shouldBe 0
  }
}
