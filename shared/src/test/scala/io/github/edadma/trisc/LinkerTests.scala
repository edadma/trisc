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
      """dd start
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
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
    // caller is 6 bytes (movi=4 + halt=2), target is at offset 6
    cpu.r(1).read shouldBe 6
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
    // caller is 8 bytes (movi=6 + halt=2), target at offset 8
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
    // caller is 10 bytes (movi=8 + halt=2), target at offset 10
    cpu.r(1).read shouldBe 10
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
    // main segment: 4 bytes dw + 2 bytes halt = 6 bytes
    // handler starts at offset 6
    mem.readInt(0) shouldBe 6
  }

  "ABS64 relocation in vector table enables CPU reset dispatch" in {
    val main = assemble(
      """dl start
        |dl 0
        |dl 0
        |dl 0
        |dl 0
        |dl 0
        |dl 0
        |dl 0
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
    linked.segments(0).org shouldBe 0x100
    // first segment is 4 bytes (ldi=2 + halt=2)
    linked.segments(1).org shouldBe 0x104
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
        |dd start
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
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

  "linked TOF serializes as v2" in {
    val tof = assemble("ldi r1, 42\nhalt\n", relocatable = true)
    val linked = Linker.link(Seq(tof))
    linked.serialize should startWith("TOF v2")
  }

  "linked TOF round-trips through serialize/deserialize" in {
    val tof = assemble("main\n  ldi r1, 42\n  halt\n", relocatable = true)
    val linked = Linker.link(Seq(tof))
    val s = linked.serialize
    val reloaded = TOF.deserialize(s)
    reloaded.serialize shouldBe s
  }
}
