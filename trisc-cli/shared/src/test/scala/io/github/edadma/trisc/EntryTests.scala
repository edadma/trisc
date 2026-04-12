package io.github.edadma.trisc

class EntryTests extends TestHelpers {

  // ===== Assembler entry directive =====

  "entry directive sets entry point" in {
    val tof = assemble(
      """entry main
        |main
        |  halt
        |""".stripMargin)
    tof.entry shouldBe Some("main")
  }

  "entry directive absent means no entry" in {
    val tof = assemble("global main, func\nmain\n  halt\n")
    tof.entry shouldBe None
  }

  "duplicate entry directive errors" in {
    an[Exception] should be thrownBy {
      assemble("entry foo\nentry bar\nfoo\n  halt\nbar\n  halt\n")
    }
  }

  // ===== TOF serialization =====

  "entry serializes as ENTRY line" in {
    val tof = assemble("entry main\nmain\n  halt\n")
    val s = tof.serialize
    s should include("ENTRY:main")
  }

  "no entry means no ENTRY line" in {
    val tof = assemble("global main, func\nmain\n  halt\n")
    val s = tof.serialize
    s should not include "ENTRY:"
  }

  "entry appears before segments in serialized output" in {
    val tof = assemble("entry main\nmain\n  halt\n")
    val s = tof.serialize
    val entryIdx = s.indexOf("ENTRY:")
    val segIdx = s.indexOf("SEGMENT:")
    entryIdx should be < segIdx
  }

  // ===== TOF.fromString =====

  "fromString reads ENTRY line" in {
    val tof = TOF.fromString("TOF v1\nENTRY:main\nSEGMENT:code,0\nSYMBOL:main,0,func\nDATA:00\n")
    tof.entry shouldBe Some("main")
  }

  "fromString without ENTRY has no entry" in {
    val tof = TOF.fromString("TOF v1\nSEGMENT:code,0\nDATA:00\n")
    tof.entry shouldBe None
  }

  // ===== Round-trip =====

  "entry round-trips through serialize/fromString" in {
    val tof = assemble("entry main\nmain\n  halt\n")
    val s = tof.serialize
    val tof2 = TOF.fromString(s)
    tof2.entry shouldBe Some("main")
    tof2.serialize shouldBe s
  }

  // ===== entryAddress =====

  "entryAddress resolves to absolute address" in {
    val tof = assemble(
      """entry main
        |dw 0
        |dw 0
        |main
        |  halt
        |""".stripMargin)
    // 2 dw = 8 bytes, main is at offset 8
    tof.entryAddress shouldBe Some(8)
  }

  "entryAddress returns None when no entry" in {
    val tof = assemble("global main, func\nmain\n  halt\n")
    tof.entryAddress shouldBe None
  }

  "entryAddress returns None when entry symbol not found" in {
    val b = TOF.builder
    b.setEntry("nonexistent")
    b.segment("code", 0)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    tof.entryAddress shouldBe None
  }

  "entryAddress with segment origin" in {
    val tof = assemble(
      """entry start
        |segment code
        |start
        |  halt
        |""".stripMargin, orgs = Map("code" -> 0x100L))
    tof.entryAddress shouldBe Some(0x100)
  }

  // ===== Linker preserves entry =====

  "linker preserves entry point from input" in {
    val main = assemble(
      """entry main
        |global main, func
        |main
        |  halt
        |""".stripMargin, relocatable = true)
    val linked = Linker.link(Seq(main))
    linked.entry shouldBe Some("main")
  }

  "linker validates entry symbol exists" in {
    val b = TOF.builder
    b.setEntry("nonexistent")
    b.segment("code", 0)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val ex = the[Linker.LinkerError] thrownBy Linker.link(Seq(tof))
    ex.msg should include("entry point")
  }

  "linker resolves entry address across modules" in {
    val main = assemble(
      """entry start
        |extern helper
        |global start, func
        |start
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
    linked.entry shouldBe Some("start")
    linked.entryAddress shouldBe Some(0) // start is first symbol at offset 0
  }

  "linked TOF with entry can be loaded and run from entry address" in {
    val main = assemble(
      """entry main
        |global main, func
        |nop
        |nop
        |main
        |  ldi r1, 99
        |  halt
        |""".stripMargin, relocatable = true)
    val linked = Linker.link(Seq(main))

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 10000; quiet = true }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    cpu.run()
    cpu.r(1).read shouldBe 99
  }

  // ===== Entry with relocatable + linker integration =====

  "full pipeline: entry + extern + global + linker" in {
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
        |entry main
        |extern greet
        |global main, func
        |main
        |  movi r7, 0xF00
        |  movi r1, greet
        |  jalr r6, r1
        |  halt
        |""".stripMargin)
    val lib = assemble(
      """STDOUT = 0xFF8
        |global greet, func
        |greet
        |  movi r3, STDOUT
        |  sti r3, 'H'
        |  sti r3, 'i'
        |  jalr r0, r6
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, lib))
    val mem = new Memory("Memory", new RAM(0, 0xFF8), stdout)
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 10000; quiet = true }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    cpu.run()
    output.toString shouldBe "Hi"
  }
}
