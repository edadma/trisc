package io.github.edadma.trisc

class LinkerScriptTests extends TestHelpers {

  // ===== Parser: MEMORY block =====

  "parses empty script" in {
    val Right(script) = LinkerScriptParser.parse(""): @unchecked
    script.memory shouldBe empty
    script.sections shouldBe empty
    script.entry shouldBe None
  }

  "parses MEMORY block with one region" in {
    val Right(script) = LinkerScriptParser.parse(
      """MEMORY
        |  RAM: 0x0000, 0x10000
        |""".stripMargin): @unchecked
    script.memory.length shouldBe 1
    script.memory.head shouldBe MemoryRegion("RAM", 0, 0x10000)
  }

  "parses MEMORY block with multiple regions" in {
    val Right(script) = LinkerScriptParser.parse(
      """MEMORY
        |  RAM: 0x0000, 0x10000
        |  ROM: 0x10000, 0x8000
        |""".stripMargin): @unchecked
    script.memory.length shouldBe 2
    script.memory(0) shouldBe MemoryRegion("RAM", 0, 0x10000)
    script.memory(1) shouldBe MemoryRegion("ROM", 0x10000, 0x8000)
  }

  // ===== Parser: SECTIONS block =====

  "parses SECTIONS with absolute addresses" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  .text: 0x0000
        |  .data: 0x1000
        |""".stripMargin): @unchecked
    script.sections.length shouldBe 2
    script.sections(0) shouldBe SectionDef(".text", SectionPlacement.At(0))
    script.sections(1) shouldBe SectionDef(".data", SectionPlacement.At(0x1000))
  }

  "parses SECTIONS with AFTER placement" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  .text: 0x0000
        |  .data: AFTER .text
        |  .bss: AFTER .data
        |""".stripMargin): @unchecked
    script.sections(0) shouldBe SectionDef(".text", SectionPlacement.At(0))
    script.sections(1) shouldBe SectionDef(".data", SectionPlacement.After(".text"))
    script.sections(2) shouldBe SectionDef(".bss", SectionPlacement.After(".data"))
  }

  "parses SECTIONS with mixed placement" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  .text: 0x100
        |  .data: AFTER .text
        |""".stripMargin): @unchecked
    script.sections(0).placement shouldBe SectionPlacement.At(0x100)
    script.sections(1).placement shouldBe SectionPlacement.After(".text")
  }

  // ===== Parser: ENTRY =====

  "parses ENTRY" in {
    val Right(script) = LinkerScriptParser.parse("ENTRY main\n"): @unchecked
    script.entry shouldBe Some("main")
  }

  "parses ENTRY with underscored name" in {
    val Right(script) = LinkerScriptParser.parse("ENTRY _start\n"): @unchecked
    script.entry shouldBe Some("_start")
  }

  // ===== Parser: comments and blank lines =====

  "ignores comments" in {
    val Right(script) = LinkerScriptParser.parse(
      """# This is a comment
        |MEMORY
        |  RAM: 0x0000, 0x10000  # inline comment
        |""".stripMargin): @unchecked
    script.memory.length shouldBe 1
  }

  "ignores blank lines" in {
    val Right(script) = LinkerScriptParser.parse(
      """
        |MEMORY
        |  RAM: 0x0000, 0x10000
        |
        |SECTIONS
        |  .text: 0x0000
        |
        |""".stripMargin): @unchecked
    script.memory.length shouldBe 1
    script.sections.length shouldBe 1
  }

  // ===== Parser: full script =====

  "parses complete script" in {
    val Right(script) = LinkerScriptParser.parse(
      """# TRISC linker script
        |MEMORY
        |  RAM: 0x0000, 0x10000
        |
        |SECTIONS
        |  .text: 0x0000
        |  .data: AFTER .text
        |  .bss: AFTER .data
        |
        |ENTRY main
        |""".stripMargin): @unchecked
    script.memory.length shouldBe 1
    script.sections.length shouldBe 3
    script.entry shouldBe Some("main")
  }

  // ===== Parser: decimal numbers =====

  "parses decimal numbers" in {
    val Right(script) = LinkerScriptParser.parse(
      """MEMORY
        |  RAM: 0, 65536
        |""".stripMargin): @unchecked
    script.memory.head shouldBe MemoryRegion("RAM", 0, 65536)
  }

  // ===== Parser: error handling =====

  "rejects invalid input" in {
    val result = LinkerScriptParser.parse("GARBAGE stuff\n")
    result.isLeft shouldBe true
  }

  // ===== Linker integration with scripts =====

  "script places .text at explicit address" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  _default_: 0x100
        |""".stripMargin): @unchecked
    val tof = assemble("ldi r1, 42\nhalt\n", relocatable = true)
    val linked = Linker.link(Seq(tof), script, 0, 2)
    linked.segments.head.org shouldBe 0x100
  }

  "script places sections with AFTER" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  code: 0x0
        |  data: AFTER code
        |""".stripMargin): @unchecked
    val tof = assemble(
      """segment code
        |ldi r1, 42
        |halt
        |segment data
        |db 0x01, 0x02
        |""".stripMargin, orgs = Map("code" -> 0L, "data" -> 0L), relocatable = true)
    val linked = Linker.link(Seq(tof), script, 0, 2)
    val codeSeg = linked.segments.find(_.name == "code").get
    val dataSeg = linked.segments.find(_.name == "data").get
    codeSeg.org shouldBe 0
    dataSeg.org shouldBe codeSeg.org + codeSeg.chunks.head.asInstanceOf[TOF.DataChunk].data.length
  }

  "script ENTRY overrides TOF entry" in {
    val Right(script) = LinkerScriptParser.parse(
      """ENTRY alt_start
        |""".stripMargin): @unchecked
    val tof = assemble(
      """entry main
        |global main, func
        |global alt_start, func
        |main
        |  halt
        |alt_start
        |  ldi r1, 99
        |  halt
        |""".stripMargin, relocatable = true)
    val linked = Linker.link(Seq(tof), script, 0, 2)
    linked.entry shouldBe Some("alt_start")
  }

  "script AFTER errors on unknown reference" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  data: AFTER text
        |""".stripMargin): @unchecked
    val tof = assemble(
      """segment data
        |db 0x01
        |""".stripMargin, orgs = Map("data" -> 0L), relocatable = true)
    a[Linker.LinkerError] should be thrownBy
      Linker.link(Seq(tof), script, 0, 2)
  }

  // ===== Full end-to-end with script =====

  "full pipeline: script + assembler + linker + execution" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  _default_: 0x100
        |
        |ENTRY main
        |""".stripMargin): @unchecked

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

    val linked = Linker.link(Seq(main, lib), script, 0, 2)

    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    cpu.run()
    cpu.r(2).read shouldBe 42
    linked.entryAddress.get should be >= 0x100L
  }
}
