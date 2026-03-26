package io.github.edadma.trisc

class TOFTests extends TestHelpers {

  // ===== Existing functionality =====

  "round-trips through serialize/deserialize" in {
    val tof = assemble(VECTORS + "ldi r1, 42\nhalt\n")
    val serialized = tof.serialize
    val deserialized = TOF.deserialize(serialized)
    deserialized.serialize shouldBe serialized
  }

  "loads data into memory correctly" in {
    val ram = new RAM(0, 256)
    val tof = assemble("db 0x12, 0x34\n")
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x12
    ram.readByteUnsigned(1) shouldBe 0x34
  }

  "handles multiple segments" in {
    val tof = assemble(
      """segment code
        |db 0x01
        |segment data
        |db 0x02
        |""".stripMargin,
      orgs = Map("code" -> 0L, "data" -> 0x100L))
    val ram = new RAM(0, 0x200)
    tof.load(ram)
    ram.readByteUnsigned(0) shouldBe 0x01
    ram.readByteUnsigned(0x100) shouldBe 0x02
  }

  // ===== v1 backward compatibility =====

  "deserializes v1 format" in {
    val v1 = "TOF v1\nSEGMENT:_default_,0\nDATA:002a\n"
    val tof = TOF.deserialize(v1)
    tof.segments.length shouldBe 1
    tof.segments.head.symbols shouldBe empty
    tof.segments.head.externs shouldBe empty
    tof.segments.head.relocs shouldBe empty
  }

  // ===== Symbols =====

  "serialize/deserialize func symbol" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("main", 0x10, SymbolType.Func)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:main,10,func")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.symbols.length shouldBe 1
    tof2.segments.head.symbols.head shouldBe TOFSymbol("main", 0x10, SymbolType.Func)
  }

  "serialize/deserialize data symbol with size" in {
    val b = TOF.builder
    b.segment("data", 0x100)
    b.addSymbol("buffer", 0, SymbolType.Data, Some(64))
    b.addRes(64)
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:buffer,0,data,40")
    val tof2 = TOF.deserialize(s)
    val sym = tof2.segments.head.symbols.head
    sym shouldBe TOFSymbol("buffer", 0, SymbolType.Data, Some(64))
  }

  "serialize/deserialize data symbol without size" in {
    val b = TOF.builder
    b.segment("data", 0)
    b.addSymbol("x", 4, SymbolType.Data)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:x,4,data")
    s should not include "SYMBOL:x,4,data,"
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.symbols.head shouldBe TOFSymbol("x", 4, SymbolType.Data, None)
  }

  "serialize/deserialize const symbol" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("MAX_SIZE", 0xff, SymbolType.Const)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:MAX_SIZE,ff,const")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.symbols.head shouldBe TOFSymbol("MAX_SIZE", 0xff, SymbolType.Const)
  }

  "multiple symbols in one segment" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("foo", 0, SymbolType.Func)
    b.addSymbol("bar", 0x10, SymbolType.Func)
    b.addSymbol("baz", 0x20, SymbolType.Func)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.symbols.length shouldBe 3
    tof2.segments.head.symbols.map(_.name) shouldBe Seq("foo", "bar", "baz")
  }

  // ===== Type info on symbols =====

  "func symbol with type info round-trips" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("add", 0, SymbolType.Func, typeInfo = Some("2 int int int"))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:add,0,func,2 int int int")
    val tof2 = TOF.deserialize(s)
    val sym = tof2.segments.head.symbols.head
    sym shouldBe TOFSymbol("add", 0, SymbolType.Func, typeInfo = Some("2 int int int"))
  }

  "func symbol with no params round-trips" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("getval", 0, SymbolType.Func, typeInfo = Some("0 int"))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.symbols.head.typeInfo shouldBe Some("0 int")
  }

  "func symbol with pointer types round-trips" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("swap", 0, SymbolType.Func, typeInfo = Some("2 ptr int ptr int void"))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.symbols.head.typeInfo shouldBe Some("2 ptr int ptr int void")
  }

  "func symbol without type info still works" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("legacy", 0x10, SymbolType.Func)
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.symbols.head shouldBe TOFSymbol("legacy", 0x10, SymbolType.Func)
  }

  "data symbol with type info round-trips" in {
    val b = TOF.builder
    b.segment("data", 0)
    b.addSymbol("counter", 0, SymbolType.Data, typeInfo = Some("int"))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:counter,0,data,int")
    val tof2 = TOF.deserialize(s)
    val sym = tof2.segments.head.symbols.head
    sym shouldBe TOFSymbol("counter", 0, SymbolType.Data, typeInfo = Some("int"))
  }

  "data symbol with size and type info round-trips" in {
    val b = TOF.builder
    b.segment("data", 0)
    b.addSymbol("buf", 0, SymbolType.Data, Some(40), Some("arr 5 int"))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:buf,0,data,28,arr 5 int")
    val tof2 = TOF.deserialize(s)
    val sym = tof2.segments.head.symbols.head
    sym shouldBe TOFSymbol("buf", 0, SymbolType.Data, Some(40), Some("arr 5 int"))
  }

  "data symbol with size only (no type info) still works" in {
    val b = TOF.builder
    b.segment("data", 0)
    b.addSymbol("buffer", 0, SymbolType.Data, Some(64))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.symbols.head shouldBe TOFSymbol("buffer", 0, SymbolType.Data, Some(64))
  }

  "const symbol with type info round-trips" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("MAX", 0xff, SymbolType.Const, typeInfo = Some("int"))
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("SYMBOL:MAX,ff,const,int")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.symbols.head shouldBe TOFSymbol("MAX", 0xff, SymbolType.Const, typeInfo = Some("int"))
  }

  // ===== Externs =====

  "serialize/deserialize extern" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addExtern("printf")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("EXTERN:printf")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.externs shouldBe Seq("printf")
  }

  "multiple externs" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addExtern("malloc")
    b.addExtern("free")
    b.addExtern("exit")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.externs shouldBe Seq("malloc", "free", "exit")
  }

  "duplicate extern is deduplicated" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addExtern("foo")
    b.addExtern("foo")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    tof.segments.head.externs.length shouldBe 1
  }

  // ===== Relocations =====

  "serialize/deserialize MOVI2 relocation" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addExtern("target")
    b.addReloc(RelocType.MOVI2, 0x10, "target")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("RELOC:MOVI2,10,target")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.relocs.head shouldBe TOFReloc(RelocType.MOVI2, 0x10, "target")
  }

  "serialize/deserialize MOVI3 relocation" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addReloc(RelocType.MOVI3, 8, "func")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("RELOC:MOVI3,8,func")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.relocs.head shouldBe TOFReloc(RelocType.MOVI3, 8, "func")
  }

  "serialize/deserialize MOVI4 relocation" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addReloc(RelocType.MOVI4, 0, "far_func")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.relocs.head shouldBe TOFReloc(RelocType.MOVI4, 0, "far_func")
  }

  "serialize/deserialize ABS32 relocation" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addReloc(RelocType.ABS32, 0, "reset_handler")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val s = tof.serialize
    s should include("RELOC:ABS32,0,reset_handler")
    val tof2 = TOF.deserialize(s)
    tof2.segments.head.relocs.head shouldBe TOFReloc(RelocType.ABS32, 0, "reset_handler")
  }

  "multiple relocations" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addReloc(RelocType.MOVI2, 0, "foo")
    b.addReloc(RelocType.MOVI2, 4, "bar")
    b.addReloc(RelocType.ABS32, 8, "baz")
    b += 0.toByte
    b += 0.toByte
    val tof = b.tof
    val tof2 = TOF.deserialize(tof.serialize)
    tof2.segments.head.relocs.length shouldBe 3
  }

  // ===== isFullyResolved =====

  "isFullyResolved true when no externs or relocs" in {
    val tof = assemble(VECTORS + "ldi r1, 42\nhalt\n")
    tof.isFullyResolved shouldBe true
  }

  "isFullyResolved false when externs exist" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addExtern("foo")
    b += 0.toByte
    b += 0.toByte
    b.tof.isFullyResolved shouldBe false
  }

  "isFullyResolved false when relocs exist" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addReloc(RelocType.MOVI2, 0, "foo")
    b += 0.toByte
    b += 0.toByte
    b.tof.isFullyResolved shouldBe false
  }

  // ===== Full round-trip with all features =====

  "full round-trip with symbols, externs, relocs, data, and res" in {
    val b = TOF.builder
    b.segment("code", 0)
    b.addSymbol("main", 0x10, SymbolType.Func)
    b.addSymbol("helper", 0x20, SymbolType.Func)
    b.addExtern("printf")
    b.addReloc(RelocType.MOVI2, 0x12, "printf")
    b.addReloc(RelocType.ABS32, 0, "main")
    // vector table placeholder
    for _ <- 0 until 16 do b += 0.toByte
    // some code
    for _ <- 0 until 32 do b += 0xCC.toByte

    b.segment("bss", 0x1000)
    b.addSymbol("buffer", 0, SymbolType.Data, Some(256))
    b.addRes(256)

    val tof = b.tof
    val serialized = tof.serialize
    val tof2 = TOF.deserialize(serialized)

    tof2.segments.length shouldBe 2

    val code = tof2.segments.head
    code.name shouldBe "code"
    code.symbols.length shouldBe 2
    code.externs shouldBe Seq("printf")
    code.relocs.length shouldBe 2

    val bss = tof2.segments(1)
    bss.name shouldBe "bss"
    bss.symbols.length shouldBe 1
    bss.symbols.head shouldBe TOFSymbol("buffer", 0, SymbolType.Data, Some(256))

    // round-trip stability
    tof2.serialize shouldBe serialized
  }

  // ===== Version handling =====

  "serializes as v2" in {
    val tof = assemble(VECTORS + "halt\n")
    tof.serialize should startWith("TOF v2")
  }

  "rejects unknown version" in {
    an[RuntimeException] should be thrownBy TOF.deserialize("TOF v99\n")
  }

  "rejects missing magic" in {
    an[RuntimeException] should be thrownBy TOF.deserialize("SEGMENT:code,0\n")
  }
}
