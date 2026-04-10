package io.github.edadma.trisc

class TriscBinaryTests extends TestHelpers:

  private val tinyScript: LinkerScript = LinkerScriptParser.parse(
    """SECTIONS
      |    code: 0x1000
      |    data
      |    bss
      |ENTRY start
      |""".stripMargin,
  ) match
    case Right(s) => s
    case Left(e)  => throw new RuntimeException(e)

  "TriscBinary.loadIntoMemory matches TOF.load for a linked executable" in {
    val code = assemble(
      VECTORS + """start
        |  ldi r1, 42
        |  halt
        |""".stripMargin,
      relocatable = true,
    )
    val linked = Linker.link(Seq(code), tinyScript, 0)
    linked.tofType shouldBe TOFType.Executable
    linked.isFullyResolved shouldBe true

    val img = TriscBinary.serialize(linked)
    img(0) shouldBe 'T'.toByte
    img(1) shouldBe 'R'.toByte
    img(2) shouldBe 'B'.toByte
    img(3) shouldBe 0x01.toByte

    val ramTof = new RAM(0, 0x10000)
    val ramTrb = new RAM(0, 0x10000)
    linked.load(ramTof)
    val entry = TriscBinary.loadIntoMemory(img, ramTrb)
    entry shouldBe linked.entryAddress.get
    ramTof.bytes shouldEqual ramTrb.bytes
  }

end TriscBinaryTests
