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
    script.sections(0) shouldBe SectionDef(".text", Some(0))
    script.sections(1) shouldBe SectionDef(".data", Some(0x1000))
  }

  "parses SECTIONS with sequential placement" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  .text: 0x0000
        |  .data
        |  .bss
        |""".stripMargin): @unchecked
    script.sections.length shouldBe 3
    script.sections(0) shouldBe SectionDef(".text", Some(0))
    script.sections(1) shouldBe SectionDef(".data", None)
    script.sections(2) shouldBe SectionDef(".bss", None)
  }

  "parses SECTIONS with mixed placement" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  .text: 0x100
        |  .data
        |""".stripMargin): @unchecked
    script.sections(0).address shouldBe Some(0x100)
    script.sections(1).address shouldBe None
  }

  // ===== Parser: ENTRY =====

  "parses ENTRY directive" in {
    val Right(script) = LinkerScriptParser.parse(
      """ENTRY main
        |""".stripMargin): @unchecked
    script.entry shouldBe Some("main")
  }

  // ===== Parser: SYMBOL =====

  "parses SYMBOL with absolute address" in {
    val Right(script) = LinkerScriptParser.parse(
      """SYMBOL _heap_end = 0x100000
        |""".stripMargin): @unchecked
    script.symbols.length shouldBe 1
    script.symbols.head shouldBe SymbolDef("_heap_end", SymbolValue.Absolute(0x100000))
  }

  "parses SYMBOL with AFTER" in {
    val Right(script) = LinkerScriptParser.parse(
      """SYMBOL _heap_start = AFTER bss
        |""".stripMargin): @unchecked
    script.symbols.length shouldBe 1
    script.symbols.head shouldBe SymbolDef("_heap_start", SymbolValue.AfterSection("bss"))
  }

  // ===== Parser: combined blocks =====

  "parses MEMORY + SECTIONS" in {
    val Right(script) = LinkerScriptParser.parse(
      """MEMORY
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
        |  .data
        |  .bss
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

  "script places section at explicit address" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  _default_: 0x100
        |""".stripMargin): @unchecked
    val tof = assemble("ldi r1, 42\nhalt\n", relocatable = true)
    val linked = Linker.link(Seq(tof), script, 0, 2)
    linked.segments.head.org shouldBe 0x100
  }

  "script places sections sequentially" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  code: 0x0
        |  data
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
    dataSeg.org should be > codeSeg.org
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

  "empty sections are handled gracefully" in {
    val Right(script) = LinkerScriptParser.parse(
      """SECTIONS
        |  code: 0x0
        |  rodata
        |  data
        |  bss
        |""".stripMargin): @unchecked
    // Only code segment has content — others are empty
    val tof = assemble(
      """segment code
        |ldi r1, 42
        |halt
        |""".stripMargin, orgs = Map("code" -> 0L), relocatable = true)
    val linked = Linker.link(Seq(tof), script, 0, 2)
    linked.segments.find(_.name == "code").get.org shouldBe 0
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
        |""".stripMargin, relocatable = true)

    val helper = assemble(
      """global helper, func
        |helper
        |  ldi r1, 99
        |  jalr r0, r7
        |""".stripMargin, relocatable = true)

    val linked = Linker.link(Seq(main, helper), script, 0, 2)
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe 99
    cpu.state shouldBe State.Halt
  }
}
