package io.github.edadma.trisc

class SymbolOffsetTests extends TestHelpers {

  // These tests expose the bug: symbol offsets should be segment-relative,
  // not absolute addresses.

  "global symbol offset is segment-relative with nonzero org" in {
    val tof = assemble(
      """global main, func
        |segment code
        |nop
        |main
        |  halt
        |""".stripMargin, orgs = Map("code" -> 0x100L), relocatable = true)
    val sym = tof.symbolByName("main").get
    // main is 2 bytes into the code segment (after nop)
    // offset should be 2, NOT 0x102
    sym.offset shouldBe 2
  }

  "global symbol at start of segment with nonzero org has offset 0" in {
    val tof = assemble(
      """global start, func
        |segment code
        |start
        |  halt
        |""".stripMargin, orgs = Map("code" -> 0x200L), relocatable = true)
    val sym = tof.symbolByName("start").get
    sym.offset shouldBe 0
  }

  "multiple global symbols in segment with nonzero org" in {
    val tof = assemble(
      """global foo, func
        |global bar, func
        |segment code
        |foo
        |  nop
        |  nop
        |bar
        |  halt
        |""".stripMargin, orgs = Map("code" -> 0x100L), relocatable = true)
    val foo = tof.symbolByName("foo").get
    val bar = tof.symbolByName("bar").get
    foo.offset shouldBe 0
    bar.offset shouldBe 4 // 2 nops = 4 bytes
  }

  "relocatable mode auto-export offsets are segment-relative" in {
    val tof = assemble(
      """segment code
        |nop
        |helper
        |  halt
        |""".stripMargin, orgs = Map("code" -> 0x100L), relocatable = true)
    val sym = tof.symbolByName("helper").get
    sym.offset shouldBe 2
  }

  "linker resolves correct address with segment-relative symbols" in {
    val main = assemble(
      """extern helper
        |global start, func
        |entry start
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
    val mem = new Memory("Memory", new RAM(0, 0x1000))
    linked.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.pc = linked.entryAddress.get
    cpu.state = State.Run
    cpu.run()
    cpu.r(2).read shouldBe 42
  }

  "symbol offset round-trips correctly through serialize/fromString" in {
    val tof = assemble(
      """global main, func
        |segment code
        |nop
        |main
        |  halt
        |""".stripMargin, orgs = Map("code" -> 0x100L), relocatable = true)
    val s = tof.serialize
    val tof2 = TOF.fromString(s)
    val sym = tof2.symbolByName("main").get
    sym.offset shouldBe 2
    // And entryAddress should work on the reloaded TOF too
  }
}
