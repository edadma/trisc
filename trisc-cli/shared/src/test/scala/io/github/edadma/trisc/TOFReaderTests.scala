package io.github.edadma.trisc

class TOFReaderTests extends TestHelpers {

  // ===== Version handling =====

  "reads v1 format" in {
    val tof = TOF.fromString("TOF v1\nSEGMENT:code,0\nDATA:002a\n")
    tof.segments.length shouldBe 1
    tof.segmentNames shouldBe Seq("code")
  }

  "reads v2 format" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:002a\n")
    tof.segments.length shouldBe 1
  }

  "rejects unsupported version" in {
    val ex = the[TOFReadError] thrownBy TOF.fromString("TOF v99\n")
    ex.msg should include("unsupported")
    ex.line shouldBe 1
  }

  "rejects missing header" in {
    val ex = the[TOFReadError] thrownBy TOF.fromString("SEGMENT:code,0\n")
    ex.msg should include("expected TOF version header")
    ex.line shouldBe 1
  }

  "rejects empty input" in {
    val ex = the[TOFReadError] thrownBy TOF.fromString("")
    ex.msg should include("empty or missing")
  }

  "rejects whitespace-only input" in {
    val ex = the[TOFReadError] thrownBy TOF.fromString("   \n  \n  ")
    ex.msg should include("empty or missing")
  }

  // ===== Segments =====

  "reads segment with origin" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,100\nDATA:00\n")
    tof.segments.head.org shouldBe 0x100
    tof.segments.head.name shouldBe "code"
  }

  "reads multiple segments" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:01\nSEGMENT:data,100\nDATA:02\n")
    tof.segmentNames shouldBe Seq("code", "data")
    tof.segment("code").get.org shouldBe 0
    tof.segment("data").get.org shouldBe 0x100
  }

  "duplicate segment name merges data" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:00\nSEGMENT:code,100\nDATA:01\n")
    tof.segments should have size 1
    // Both DATA lines merged into the same segment's chunk list
    val data = tof.segments.head.chunks.head.asInstanceOf[TOF.DataChunk].data
    data shouldBe Seq(0, 1)
  }

  // ===== DATA =====

  "reads data bytes" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:0102030405\n")
    val data = tof.segments.head.chunks.head.asInstanceOf[TOF.DataChunk].data
    data shouldBe Seq(1, 2, 3, 4, 5)
  }

  "reads empty data" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:\nRES:1\n")
    tof.segments.length shouldBe 1
  }

  "reads multiple data chunks" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:0102\nDATA:0304\n")
    tof.totalDataSize shouldBe 4
  }

  // ===== RES =====

  "reads reserve chunk" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:bss,0\nRES:100\n")
    val res = tof.segments.head.chunks.head.asInstanceOf[TOF.ResChunk]
    res.size shouldBe 0x100
  }

  // ===== SYMBOL =====

  "reads func symbol" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nSYMBOL:main,10,func\nDATA:00\n")
    val sym = tof.symbolByName("main").get
    sym.offset shouldBe 0x10
    sym.typ shouldBe SymbolType.Func
    sym.size shouldBe None
  }

  "reads data symbol without size" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:data,0\nSYMBOL:buf,0,data\nRES:10\n")
    val sym = tof.symbolByName("buf").get
    sym.typ shouldBe SymbolType.Data
    sym.size shouldBe None
  }

  "reads data symbol with size" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:data,0\nSYMBOL:buf,0,data,100\nRES:100\n")
    val sym = tof.symbolByName("buf").get
    sym.typ shouldBe SymbolType.Data
    sym.size shouldBe Some(0x100)
  }

  "reads const symbol" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nSYMBOL:MAX,ff,const\nDATA:00\n")
    val sym = tof.symbolByName("MAX").get
    sym.typ shouldBe SymbolType.Const
    sym.offset shouldBe 0xff
  }

  "reads multiple symbols" in {
    val tof = TOF.fromString(
      "TOF v2\nSEGMENT:code,0\nSYMBOL:a,0,func\nSYMBOL:b,10,func\nSYMBOL:c,20,data\nDATA:00\n")
    tof.allSymbols.length shouldBe 3
    tof.symbolByName("a").get.typ shouldBe SymbolType.Func
    tof.symbolByName("c").get.typ shouldBe SymbolType.Data
  }

  "rejects bad symbol line" in {
    val ex = the[TOFReadError] thrownBy
      TOF.fromString("TOF v2\nSEGMENT:code,0\nSYMBOL:bad\nDATA:00\n")
    ex.msg should include("bad SYMBOL line")
  }

  // ===== EXTERN =====

  "reads extern" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nEXTERN:printf\nDATA:00\n")
    tof.segments.head.externs shouldBe Seq("printf")
  }

  "reads multiple externs" in {
    val tof = TOF.fromString(
      "TOF v2\nSEGMENT:code,0\nEXTERN:malloc\nEXTERN:free\nDATA:00\n")
    tof.allExterns.map(_._2) shouldBe Seq("malloc", "free")
  }

  // ===== RELOC =====

  "reads MOVI2 reloc" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nRELOC:MOVI2,10,target\nDATA:00\n")
    val reloc = tof.segments.head.relocs.head
    reloc.typ shouldBe RelocType.MOVI2
    reloc.offset shouldBe 0x10
    reloc.symbol shouldBe "target"
  }

  "reads MOVI3 reloc" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nRELOC:MOVI3,8,func\nDATA:00\n")
    tof.segments.head.relocs.head.typ shouldBe RelocType.MOVI3
  }

  "reads MOVI4 reloc" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nRELOC:MOVI4,0,far\nDATA:00\n")
    tof.segments.head.relocs.head.typ shouldBe RelocType.MOVI4
  }

  "reads ABS32 reloc" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nRELOC:ABS32,0,handler\nDATA:00\n")
    tof.segments.head.relocs.head.typ shouldBe RelocType.ABS32
  }

  "reads multiple relocs" in {
    val tof = TOF.fromString(
      "TOF v2\nSEGMENT:code,0\nRELOC:MOVI2,0,a\nRELOC:ABS32,4,b\nDATA:00\n")
    tof.allRelocs.length shouldBe 2
  }

  "rejects unknown reloc type" in {
    val ex = the[TOFReadError] thrownBy
      TOF.fromString("TOF v2\nSEGMENT:code,0\nRELOC:BOGUS,0,x\nDATA:00\n")
    ex.msg should include("unknown relocation type")
  }

  "rejects bad reloc line" in {
    val ex = the[TOFReadError] thrownBy
      TOF.fromString("TOF v2\nSEGMENT:code,0\nRELOC:MOVI2\nDATA:00\n")
    ex.msg should include("bad RELOC line")
  }

  // ===== Unrecognized lines =====

  "rejects unrecognized line" in {
    val ex = the[TOFReadError] thrownBy
      TOF.fromString("TOF v2\nSEGMENT:code,0\nGARBAGE\n")
    ex.msg should include("unrecognized line")
  }

  // ===== Blank lines =====

  "ignores blank lines" in {
    val tof = TOF.fromString("TOF v2\n\nSEGMENT:code,0\n\nDATA:0102\n\n")
    tof.segments.length shouldBe 1
  }

  "ignores whitespace-only lines" in {
    val tof = TOF.fromString("TOF v2\n   \nSEGMENT:code,0\n  \nDATA:0102\n")
    tof.segments.length shouldBe 1
  }

  // ===== Access methods =====

  "allSymbols returns symbols across segments" in {
    val tof = TOF.fromString(
      "TOF v2\nSEGMENT:code,0\nSYMBOL:a,0,func\nDATA:00\nSEGMENT:data,100\nSYMBOL:b,0,data\nRES:10\n")
    val syms = tof.allSymbols
    syms.length shouldBe 2
    syms.map(_._1) shouldBe Seq("code", "data")
    syms.map(_._2.name) shouldBe Seq("a", "b")
  }

  "allExterns returns externs across segments" in {
    val tof = TOF.fromString(
      "TOF v2\nSEGMENT:a,0\nEXTERN:x\nDATA:00\nSEGMENT:b,100\nEXTERN:y\nDATA:00\n")
    tof.allExterns.map(_._2) shouldBe Seq("x", "y")
  }

  "allRelocs returns relocs across segments" in {
    val tof = TOF.fromString(
      "TOF v2\nSEGMENT:a,0\nRELOC:MOVI2,0,x\nDATA:00\nSEGMENT:b,100\nRELOC:ABS32,0,y\nDATA:00\n")
    tof.allRelocs.length shouldBe 2
  }

  "symbolByName returns None for missing symbol" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:00\n")
    tof.symbolByName("nonexistent") shouldBe None
  }

  "segment returns None for missing segment" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:00\n")
    tof.segment("nonexistent") shouldBe None
  }

  "totalDataSize sums all chunks" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:01020304\nRES:10\n")
    tof.totalDataSize shouldBe 4 + 16
  }

  "isFullyResolved true when no externs/relocs" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nSYMBOL:main,0,func\nDATA:0102\n")
    tof.isFullyResolved shouldBe true
  }

  "isFullyResolved false with externs" in {
    val tof = TOF.fromString("TOF v2\nSEGMENT:code,0\nEXTERN:foo\nDATA:00\n")
    tof.isFullyResolved shouldBe false
  }

  // ===== Round-trip: serialize then read =====

  "round-trip: assemble → serialize → read → serialize" in {
    val tof1 = assemble("main\n  ldi r1, 42\n  halt\n", relocatable = true)
    val s1 = tof1.serialize
    val tof2 = TOF.fromString(s1)
    tof2.serialize shouldBe s1
  }

  "round-trip: builder with all features → serialize → read" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("main", 0, SymbolType.Func)
    b.addSymbol("buf", 0x20, SymbolType.Data, Some(64))
    b.addExtern("printf")
    b.addReloc(RelocType.MOVI2, 4, "printf")
    for i <- 0 until 48 do b += i.toByte
    b.segment("bss", 0x1000)
    b.addSymbol("heap", 0, SymbolType.Data, Some(1024))
    b.addRes(1024)
    val tof = b.tof
    val s = tof.serialize
    val tof2 = TOF.fromString(s)

    tof2.segmentNames shouldBe Seq("code", "bss")
    tof2.symbolByName("main").get.typ shouldBe SymbolType.Func
    tof2.symbolByName("buf").get.size shouldBe Some(64)
    tof2.symbolByName("heap").get.size shouldBe Some(1024)
    tof2.allExterns.map(_._2) shouldBe Seq("printf")
    tof2.allRelocs.length shouldBe 1
    tof2.serialize shouldBe s
  }

  // ===== Error line numbers =====

  "error reports correct line number" in {
    val ex = the[TOFReadError] thrownBy
      TOF.fromString("TOF v2\nSEGMENT:code,0\nDATA:01\nBADLINE\n")
    ex.line shouldBe 4
  }

  "error reports line number with blank lines" in {
    val ex = the[TOFReadError] thrownBy
      TOF.fromString("TOF v2\n\n\nSEGMENT:code,0\n\nBADLINE\n")
    ex.line shouldBe 6
  }
}
