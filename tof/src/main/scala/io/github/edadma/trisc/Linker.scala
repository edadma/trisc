package io.github.edadma.trisc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Linker:
  case class LinkerError(msg: String) extends RuntimeException(msg)

  def link(tofs: Seq[TOF], baseAddress: Long = 0, addresses: Int = 2): TOF =
    link(tofs, LinkerScript(), baseAddress, addresses)

  def link(tofs: Seq[TOF], script: LinkerScript, baseAddress: Long, addresses: Int): TOF =
    case class PlacedSegment(
        name: String,
        org: Long,
        data: ArrayBuffer[Byte],
        symbols: Seq[TOFSymbol],
        externs: Seq[String],
        relocs: Seq[TOFReloc],
    )

    // Phase 1: flatten each segment's chunks into a contiguous byte array
    def flattenChunks(chunks: Seq[TOF.Chunk]): ArrayBuffer[Byte] =
      val buf = new ArrayBuffer[Byte]
      for chunk <- chunks do
        chunk match
          case TOF.DataChunk(data)   => buf ++= data
          case TOF.ResChunk(size)    => for _ <- 0L until size do buf += 0.toByte
          case TOF.CommentChunk(_)   =>
      buf

    // Collect all input segments with their flattened data
    case class InputSegment(
        name: String,
        originalOrg: Long,
        data: ArrayBuffer[Byte],
        symbols: Seq[TOFSymbol],
        externs: Seq[String],
        relocs: Seq[TOFReloc],
    )

    val inputSegments = new ArrayBuffer[InputSegment]
    for tof <- tofs do
      for seg <- tof.segments do
        inputSegments += InputSegment(seg.name, seg.org, flattenChunks(seg.chunks), seg.symbols, seg.externs, seg.relocs)

    // Phase 2: place segments using linker script or sequential placement
    val sectionDefs = script.sections.map(s => s.name -> s).toMap
    val placed = new ArrayBuffer[PlacedSegment]
    val placedByName = new mutable.LinkedHashMap[String, PlacedSegment]
    var nextAddr = baseAddress

    // If the script defines sections, place them in script order first,
    // then place any remaining segments sequentially
    val scriptSectionNames = script.sections.map(_.name).toSet

    // Resolve placement for a section
    def resolveOrg(seg: InputSegment): Long =
      sectionDefs.get(seg.name) match
        case Some(SectionDef(_, SectionPlacement.At(addr))) => addr
        case Some(SectionDef(_, SectionPlacement.After(ref))) =>
          placedByName.get(ref) match
            case Some(prev) => prev.org + prev.data.length
            case None => throw LinkerError(s"section '${seg.name}' placed AFTER '$ref', but '$ref' has not been placed yet")
        case None =>
          if seg.originalOrg != 0 then seg.originalOrg
          else nextAddr

    // Place segments that have script definitions first (in script order)
    for secDef <- script.sections do
      val matching = inputSegments.filter(_.name == secDef.name)
      for seg <- matching do
        val org = resolveOrg(seg)
        val ps = PlacedSegment(seg.name, org, seg.data, seg.symbols, seg.externs, seg.relocs)
        placed += ps
        placedByName(seg.name) = ps
        nextAddr = org + seg.data.length

    // Place remaining segments not in the script
    for seg <- inputSegments if !scriptSectionNames.contains(seg.name) do
      val org = resolveOrg(seg)
      val ps = PlacedSegment(seg.name, org, seg.data, seg.symbols, seg.externs, seg.relocs)
      placed += ps
      placedByName(seg.name) = ps
      nextAddr = org + seg.data.length

    // Phase 3: build global symbol table
    val globalSymbols = new mutable.LinkedHashMap[String, (Long, TOFSymbol)]

    for seg <- placed do
      for sym <- seg.symbols do
        val absAddr = seg.org + sym.offset
        if globalSymbols.contains(sym.name) then
          throw LinkerError(s"duplicate symbol: '${sym.name}'")
        globalSymbols(sym.name) = (absAddr, sym)

    // Phase 4: resolve relocations
    for seg <- placed do
      for ext <- seg.externs do
        if !globalSymbols.contains(ext) then
          throw LinkerError(s"undefined symbol: '$ext'")

      for reloc <- seg.relocs do
        val (addr, _) = globalSymbols.getOrElse(
          reloc.symbol,
          throw LinkerError(s"undefined symbol: '${reloc.symbol}'"),
        )

        reloc.typ match
          case RelocType.ABS32 =>
            val off = reloc.offset.toInt
            seg.data(off) = ((addr >> 24) & 0xff).toByte
            seg.data(off + 1) = ((addr >> 16) & 0xff).toByte
            seg.data(off + 2) = ((addr >> 8) & 0xff).toByte
            seg.data(off + 3) = (addr & 0xff).toByte

          case RelocType.ABS64 =>
            val off = reloc.offset.toInt
            seg.data(off) = ((addr >> 56) & 0xff).toByte
            seg.data(off + 1) = ((addr >> 48) & 0xff).toByte
            seg.data(off + 2) = ((addr >> 40) & 0xff).toByte
            seg.data(off + 3) = ((addr >> 32) & 0xff).toByte
            seg.data(off + 4) = ((addr >> 24) & 0xff).toByte
            seg.data(off + 5) = ((addr >> 16) & 0xff).toByte
            seg.data(off + 6) = ((addr >> 8) & 0xff).toByte
            seg.data(off + 7) = (addr & 0xff).toByte

          case RelocType.MOVI2 =>
            patchMovi(seg.data, reloc.offset.toInt, addr, 2)

          case RelocType.MOVI3 =>
            patchMovi(seg.data, reloc.offset.toInt, addr, 3)

          case RelocType.MOVI4 =>
            patchMovi(seg.data, reloc.offset.toInt, addr, 4)

    // Phase 5: resolve entry point (script entry overrides TOF entry)
    val entry = script.entry.orElse(tofs.flatMap(_.entry).lastOption)
    for name <- entry do
      if !globalSymbols.contains(name) then
        throw LinkerError(s"entry point '$name' is not a defined symbol")

    // Phase 6: produce output TOF
    val outSegments = placed.map { seg =>
      TOF.Segment(
        seg.name,
        seg.org,
        Seq(TOF.DataChunk(seg.data.toSeq)),
        seg.symbols.map(s => s.copy(offset = s.offset)),
      )
    }.toSeq

    TOF(entry, outSegments)

  private def patchMovi(data: ArrayBuffer[Byte], offset: Int, addr: Long, n: Int): Unit =
    val a = addr.toInt
    for i <- 0 until n do
      val byteIdx = offset + i * 2 + 1
      val shift = (n - 1 - i) * 8
      data(byteIdx) = ((a >> shift) & 0xff).toByte
