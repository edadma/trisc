package io.github.edadma.trisc

class DumpLayoutTests extends TestHelpers {

  "dumpLayout includes entry point" in {
    val tof = assemble("entry main\nmain\n  halt\n")
    val dump = tof.dumpLayout
    dump should include("Entry: main")
  }

  "dumpLayout lists all segments with addresses" in {
    val tof = assemble(
      """segment code
        |ldi r1, 42
        |halt
        |segment data
        |dd 0x1234
        |""".stripMargin,
      orgs = Map("code" -> 0L, "data" -> 0x100L))
    val dump = tof.dumpLayout
    dump should include("code")
    dump should include("data")
    dump should include("0x000100")
  }

  "dumpLayout detects overlapping segments" in {
    // Build a TOF with overlapping segments manually
    val tof = new TOF(
      None,
      Seq(
        TOF.Segment("a", 0, Seq(TOF.DataChunk(Seq.fill(16)(0.toByte)))),
        TOF.Segment("b", 8, Seq(TOF.DataChunk(Seq.fill(16)(0.toByte)))),
      ),
    )
    val dump = tof.dumpLayout
    dump should include("OVERLAP")
  }

  "dumpLayout reports no overlaps for clean layout" in {
    val tof = assemble(
      """segment code
        |ldi r1, 42
        |halt
        |segment data
        |dd 0x1234
        |""".stripMargin,
      orgs = Map("code" -> 0L, "data" -> 0x100L))
    val dump = tof.dumpLayout
    dump should include("No overlaps")
  }

  "dumpLayout lists symbols with absolute addresses" in {
    val tof = assemble(
      """segment code
        |global foo, func
        |foo
        |  ldi r1, 42
        |  halt
        |""".stripMargin,
      orgs = Map("code" -> 0x200L), relocatable = true)
    val dump = tof.dumpLayout
    dump should include("foo")
    dump should include("0x000200")
  }

  "dumpLayout lists unresolved externs" in {
    val tof = assemble("extern bar\nmovi r1, bar\nhalt\n", relocatable = true)
    val dump = tof.dumpLayout
    dump should include("Unresolved Externs")
    dump should include("bar")
  }

  "dumpLayout lists relocations" in {
    val tof = assemble("movi r1, target\nhalt\n", relocatable = true)
    val dump = tof.dumpLayout
    dump should include("Relocations")
    dump should include("MOVI4")
  }

  "dumpLayout on linked multi-segment TOF shows complete layout" in {
    val main = assemble(
      """segment code
        |global main, func
        |entry main
        |main
        |  ldi r1, 42
        |  halt
        |segment data
        |global val1, data, 4
        |val1
        |  dd 0x1234
        |""".stripMargin, relocatable = true)
    val linked = Linker.link(Seq(main))
    val dump = linked.dumpLayout
    dump should include("Entry: main")
    dump should include("code")
    dump should include("data")
    dump should include("main")
    dump should include("val1")
    dump should include("No overlaps")
  }
}
