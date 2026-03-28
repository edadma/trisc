package io.github.edadma.trisc

class LinkerTests extends TestHelpers {

  // ===== Basic linking =====

  "link single fully-resolved TOF unchanged" in {
    val tof = assemble(VECTORS + "ldi r1, 42\nhalt\n")
    val linked = Linker.link(Seq(tof))
    linked.isFullyResolved shouldBe true
    linked.segments.length shouldBe 1
  }

  "link two TOFs with cross-references" in {
    val main = assemble(
      """dd 0xFF0
        |dd start
        |resb 144
        |start
        |  movi r1, helper
        |  jalr r7, r1
        |  halt
        |""".stripMargin, relocatable = true)
    val lib = assemble(
      """helper
        |  ldi r2, 42
        |  jalr r0, r7
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, lib))
    linked.isFullyResolved shouldBe true

    // Execute it
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(2).read shouldBe 42
  }

  // ===== MOVI relocation patching =====

  "MOVI2 relocation patches correctly" in {
    val caller = assemble("movi r1, target\nhalt\n", relocatable = true, addresses = 2)
    val callee = assemble("target\n  ldi r2, 99\n  halt\n", relocatable = true, addresses = 2)

    val linked = Linker.link(Seq(caller, callee), addresses = 2)
    linked.isFullyResolved shouldBe true

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.pc = 0
    cpu.state = State.Run
    cpu.run()
    // r1 should contain the address of 'target'
    // caller is 6 bytes (movi=4 + halt=2), aligned to 8, target at offset 8
    cpu.r(1).read shouldBe 8
  }

  "MOVI3 relocation patches correctly" in {
    val caller = assemble("movi r1, target\nhalt\n", relocatable = true, addresses = 3)
    val callee = assemble("target\n  ldi r2, 77\n  halt\n", relocatable = true, addresses = 3)

    val linked = Linker.link(Seq(caller, callee), addresses = 3)

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.pc = 0
    cpu.state = State.Run
    cpu.run()
    // caller is 8 bytes (movi=6 + halt=2), already aligned, target at offset 8
    cpu.r(1).read shouldBe 8
  }

  "MOVI4 relocation patches correctly" in {
    val caller = assemble("movi r1, target\nhalt\n", relocatable = true, addresses = 4)
    val callee = assemble("target\n  ldi r2, 55\n  halt\n", relocatable = true, addresses = 4)

    val linked = Linker.link(Seq(caller, callee), addresses = 4)

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.pc = 0
    cpu.state = State.Run
    cpu.run()
    // caller is 10 bytes (movi=8 + halt=2), aligned to 16, target at offset 16
    cpu.r(1).read shouldBe 16
  }

  // ===== ABS32 relocation patching =====

  "ABS32 relocation patches dw correctly" in {
    val main = assemble(
      """dw handler
        |halt
        |""".stripMargin, relocatable = true)
    val handlers = assemble(
      """handler
        |  ldi r1, 42
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, handlers))

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    // The dw at address 0 should contain the address of handler
    // main segment: 4 bytes dw + 2 bytes halt = 6 bytes, aligned to 8
    // handler starts at offset 8
    mem.readInt(0) shouldBe 8
  }

  "ABS64 relocation in vector table enables CPU reset dispatch" in {
    val main = assemble(
      """dd 0xFF0
        |dl start
        |resb 144
        |nop
        |""".stripMargin, relocatable = true)
    val code = assemble(
      """start
        |  ldi r1, 99
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, code))

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 99
  }

  // ===== Error handling =====

  "link rejects undefined symbol" in {
    val tof = assemble("movi r1, nowhere\nhalt\n", relocatable = true)
    val ex = the[Linker.LinkerError] thrownBy Linker.link(Seq(tof))
    ex.msg should include("undefined symbol")
    ex.msg should include("nowhere")
  }

  "link rejects duplicate symbol" in {
    val a = assemble("foo\n  halt\n", relocatable = true)
    val b = assemble("foo\n  halt\n", relocatable = true)
    val ex = the[Linker.LinkerError] thrownBy Linker.link(Seq(a, b))
    ex.msg should include("duplicate symbol")
    ex.msg should include("foo")
  }

  // ===== Segment placement =====

  "segments are placed sequentially from base address" in {
    val a = assemble("ldi r1, 1\nhalt\n", relocatable = true)
    val b = assemble("ldi r2, 2\nhalt\n", relocatable = true)

    val linked = Linker.link(Seq(a, b), baseAddress = 0x100)
    // Same-named segments get merged with 8-byte alignment padding between units
    linked.segments(0).org shouldBe 0x100
    val data = linked.segments(0).chunks.head.asInstanceOf[TOF.DataChunk].data
    data.length shouldBe 12 // 4 bytes from a + 4 padding (align 8) + 4 bytes from b
  }

  "segment with explicit org keeps its origin" in {
    val tof = assemble(
      """segment code
        |ldi r1, 1
        |halt
        |""".stripMargin, relocatable = true, orgs = Map("code" -> 0x200))
    val linked = Linker.link(Seq(tof))
    linked.segments.head.org shouldBe 0x200
  }

  // ===== Symbols preserved in output =====

  "linked output preserves symbols for debugging" in {
    val tof = assemble("main\n  ldi r1, 42\n  halt\n", relocatable = true)
    val linked = Linker.link(Seq(tof))
    linked.segments.head.symbols.map(_.name) should contain("main")
  }

  // ===== Full integration: call and return across modules =====

  "full cross-module call and return" in {
    val output = new StringBuilder
    val stdout = new Device with WriteOnlyAddressable {
      val name = "stdout"
      val base: Long = 0xFF8
      val size: Long = 1
      def writeByte(addr: Long, data: Long): Unit = output += data.toChar
      override def loadByte(addr: Long, data: Long): Unit = ()
    }

    val main = assemble(
      """STDOUT = 0xFF8
        |dd 0xFF0
        |dd start
        |resb 144
        |start
        |  movi r7, 0xF00
        |  movi r1, printA
        |  jalr r6, r1
        |  movi r1, printB
        |  jalr r6, r1
        |  halt
        |""".stripMargin, relocatable = true)
    val libA = assemble(
      """STDOUT = 0xFF8
        |printA
        |  movi r3, STDOUT
        |  sti r3, 'A'
        |  jalr r0, r6
        |""".stripMargin, relocatable = true)
    val libB = assemble(
      """STDOUT = 0xFF8
        |printB
        |  movi r3, STDOUT
        |  sti r3, 'B'
        |  jalr r0, r6
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, libA, libB))

    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    output.toString shouldBe "AB"
  }

  // ===== Serialization round-trip =====

  "linked TOF serializes as v1" in {
    val tof = assemble("ldi r1, 42\nhalt\n", relocatable = true)
    val linked = Linker.link(Seq(tof))
    linked.serialize should startWith("TOF v1")
  }

  "linked TOF round-trips through serialize/deserialize" in {
    val tof = assemble("main\n  ldi r1, 42\n  halt\n", relocatable = true)
    val linked = Linker.link(Seq(tof))
    val s = linked.serialize
    val reloaded = TOF.deserialize(s)
    reloaded.serialize shouldBe s
  }

  // ===== Multi-segment round-trip (boot + code) =====

  "linked multi-segment TOF round-trips correctly" in {
    val boot = assemble(
      s"""extern main
         |  dl ${Runtime.initialSSP}
         |  dl main
         |""".stripMargin, relocatable = true)
    val code = assemble(
      """entry main
        |main
        |  ldi r1, 42
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(boot, code))
    val s = linked.serialize
    val reloaded = TOF.deserialize(s)

    // Load and run the reloaded TOF
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    reloaded.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
  }

  "movi reloc is applied before merge" in {
    val boot = assemble(
      s"""extern main
         |  dl ${Runtime.initialSSP}
         |  dl main
         |""".stripMargin, relocatable = true)
    val code = assemble(
      """entry main
        |target
        |  ldi r1, 42
        |  halt
        |main
        |  movi r1, target
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(boot, code))

    // Load into memory and check that movi was patched
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)

    // target is at offset 0 in user segment, which is placed at 0x10
    // So target absolute address = 0x10
    // main is at offset 4 in user segment = 0x14
    // movi r1, target starts at 0x14
    // After patching: first instr byte 1 should be 0x00, second instr byte 1 should be 0x10
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 0x10 // r1 = address of target
  }

  "linked multi-segment TOF with function call round-trips correctly" in {
    // Use the real sysl pipeline: parse, analyze, codegen, assemble, link with runtime
    val parser = new SyslParser
    val Right(ast) = parser.parseProgram(
      """twice(x: int) -> int = x * 2
        |main() -> int = twice(21)
        |""".stripMargin): @unchecked
    val analyzer = new SyslAnalyzer
    val typed = analyzer.analyze(ast)
    val codegen = new SyslTriscCodegen
    val asm = codegen.generate(typed)
    val userTof = assemble(asm, relocatable = true)
    val linked = Linker.link(Seq(Runtime.bootTof, userTof, Runtime.ioTof))

    // Verify it works before round-trip
    val stdout1 = new Stdout(Runtime.stdoutAddress)
    val ram1 = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem1 = new Memory("Memory", ram1, stdout1)
    linked.load(mem1)
    val cpu1 = new CPU(mem1, Nil) { limit = 100000 }
    cpu1.reset()
    cpu1.run()
    cpu1.r(1).read shouldBe 42

    // Now round-trip through serialize/deserialize
    val s = linked.serialize
    val reloaded = TOF.deserialize(s)
    val stdout2 = new Stdout(Runtime.stdoutAddress)
    val ram2 = new RAM(0, Runtime.stdoutAddress.toInt)
    val mem2 = new Memory("Memory", ram2, stdout2)
    reloaded.load(mem2)
    val cpu2 = new CPU(mem2, Nil) { limit = 100000 }
    cpu2.reset()
    cpu2.run()
    cpu2.r(1).read shouldBe 42
  }

  // ===== Relocatable executables =====

  "relocatable link produces Relocatable type" in {
    val main = assemble(
      """extern helper
        |global main, func
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

    val linked = Linker.link(Seq(main, lib), relocatable = true)
    linked.tofType shouldBe TOFType.Relocatable
  }

  "relocatable link preserves relocs with empty symbol" in {
    val tof = assemble(
      """global main, func
        |main
        |  movi r1, main
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(tof), relocatable = true)
    linked.allRelocs should not be empty
    linked.allRelocs.foreach { case (_, reloc) =>
      reloc.symbol shouldBe ""
    }
    linked.allExterns shouldBe empty
  }

  "relocatable link round-trips through serialize/deserialize" in {
    val tof = assemble(
      """global main, func
        |main
        |  movi r1, main
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(tof), relocatable = true)
    val serialized = linked.serialize
    val deserialized = TOF.deserialize(serialized)

    deserialized.tofType shouldBe TOFType.Relocatable
    deserialized.allRelocs.length shouldBe linked.allRelocs.length
  }

  "relocatable executable loads and runs at link-time base" in {
    val boot = assemble(
      """dd 0xFF0
        |dd main
        |resb 144
        |""".stripMargin, relocatable = true)
    val prog = assemble(
      """global main, func
        |main
        |  ldi r1, 42
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(boot, prog), relocatable = true)
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem) // load at link-time base (0)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 42
    cpu.state shouldBe State.Halt
  }

  "relocatable executable loads and runs at different base" in {
    val boot = assemble(
      """extern target
        |global _start, func
        |entry _start
        |_start
        |  movi r4, target
        |  jalr r6, r4
        |  halt
        |""".stripMargin, relocatable = true)
    val prog = assemble(
      """global target, func
        |target
        |  ldi r1, 99
        |  jalr r0, r6
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(boot, prog), relocatable = true)

    // Load at offset 0x200 instead of 0
    val base = 0x200L
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem, base)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.pc = linked.entryAddress(base).get
    cpu.psr = 0x02 // supervisor mode
    cpu.state = State.Run
    cpu.r(7).write(0xF00)
    cpu.run()
    cpu.r(1).read shouldBe 99
    cpu.state shouldBe State.Halt
  }

  // ===== Relocatable linking with unresolved externs =====

  "relocatable link preserves unresolved externs" in {
    val tof = assemble(
      """extern putchar
        |global main, func
        |main
        |  movi r1, putchar
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(tof), relocatable = true)
    linked.tofType shouldBe TOFType.Relocatable
    linked.allExterns.map(_._2) should contain("putchar")
  }

  "relocatable link preserves unresolved relocs" in {
    val tof = assemble(
      """extern putchar
        |global main, func
        |main
        |  movi r1, putchar
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(tof), relocatable = true)
    // Should have a reloc for putchar that's still named (not empty)
    val namedRelocs = linked.allRelocs.filter(_._2.symbol == "putchar")
    namedRelocs should not be empty
  }

  "relocatable link resolves cross-module refs but preserves runtime externs" in {
    val main = assemble(
      """extern helper
        |extern putchar
        |global main, func
        |main
        |  movi r1, helper
        |  movi r2, putchar
        |  halt
        |""".stripMargin, relocatable = true)
    val lib = assemble(
      """global helper, func
        |helper
        |  ldi r2, 42
        |  jalr r0, r7
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, lib), relocatable = true)
    linked.tofType shouldBe TOFType.Relocatable
    // helper should be resolved (not in externs)
    linked.allExterns.map(_._2) should not contain "helper"
    // putchar should still be unresolved
    linked.allExterns.map(_._2) should contain("putchar")
  }

  "relocatable link then final link resolves all symbols" in {
    val main = assemble(
      """extern helper
        |extern putchar
        |global main, func
        |main
        |  movi r1, helper
        |  movi r2, putchar
        |  halt
        |""".stripMargin, relocatable = true)
    val lib = assemble(
      """global helper, func
        |helper
        |  ldi r2, 42
        |  jalr r0, r7
        |""".stripMargin, relocatable = true)
    val runtime = assemble(
      """global putchar, func
        |putchar
        |  jalr r0, r7
        |""".stripMargin, relocatable = true)

    // First: relocatable link of user modules
    val partial = Linker.link(Seq(main, lib), relocatable = true)
    partial.allExterns.map(_._2) should contain("putchar")

    // Then: final link with runtime
    val linked = Linker.link(Seq(partial, runtime))
    linked.tofType shouldBe TOFType.Executable
    linked.allExterns shouldBe empty
  }

  "relocatable link round-trips with unresolved externs" in {
    val tof = assemble(
      """extern putchar
        |global main, func
        |main
        |  movi r1, putchar
        |  halt
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(tof), relocatable = true)
    val serialized = linked.serialize
    val deserialized = TOF.deserialize(serialized)

    deserialized.tofType shouldBe TOFType.Relocatable
    deserialized.allExterns.map(_._2) should contain("putchar")
    deserialized.allRelocs.length shouldBe linked.allRelocs.length
  }

  "non-relocatable link still rejects unresolved symbols" in {
    val tof = assemble(
      """extern nowhere
        |global main, func
        |main
        |  movi r1, nowhere
        |  halt
        |""".stripMargin, relocatable = true)

    val ex = the[Linker.LinkerError] thrownBy Linker.link(Seq(tof))
    ex.msg should include("undefined symbol")
    ex.msg should include("nowhere")
  }

  "relocatable executable with vector table loads at different base" in {
    val boot = assemble(
      """extern main
        |dd 0x1F00
        |dd _start
        |resb 144
        |global _start, func
        |entry _start
        |_start
        |  movi r4, main
        |  jalr r6, r4
        |  halt
        |""".stripMargin, relocatable = true)
    val prog = assemble(
      """global main, func
        |main
        |  ldi r1, 77
        |  jalr r0, r6
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(boot, prog), relocatable = true)

    // Verify ABS64 relocs exist (from dd _start reference in vector table)
    val abs64Relocs = linked.allRelocs.filter(_._2.typ == RelocType.ABS64)
    abs64Relocs should not be empty

    // Verify relocs: should have ABS64 (dd _start) and MOVI4 (movi r4, main)
    val moviRelocs = linked.allRelocs.filter(_._2.typ == RelocType.MOVI4)
    moviRelocs should not be empty

    // Now load at offset 0x400
    val base = 0x400L
    val mem = new Memory("Memory", new RAM(0, 0x2000))
    linked.load(mem, base)

    // Read relocated vector table
    val pc = mem.readLong(base + 8)
    pc should be >= base

    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.pc = pc
    cpu.psr = 0x02 // supervisor mode, interrupts enabled
    cpu.state = State.Run
    cpu.r(7).write(0x1F00)
    cpu.run()
    cpu.r(1).read shouldBe 77
    cpu.state shouldBe State.Halt
  }
}
