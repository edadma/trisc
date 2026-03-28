package io.github.edadma.trisc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Linker:
  case class LinkerError(msg: String) extends RuntimeException(msg)

  def link(tofs: Seq[TOF], baseAddress: Long = 0, addresses: Int = 2, relocatable: Boolean = false): TOF =
    link(tofs, LinkerScript(), baseAddress, addresses, relocatable)

  def link(tofs: Seq[TOF], script: LinkerScript, baseAddress: Long, addresses: Int): TOF =
    link(tofs, script, baseAddress, addresses, false)

  def link(tofs: Seq[TOF], script: LinkerScript, baseAddress: Long, addresses: Int, relocatable: Boolean): TOF =
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

    // Track which script sections have had their first segment placed
    val sectionPlaced = new mutable.HashSet[String]

    // Resolve placement for a section
    def resolveOrg(seg: InputSegment): Long =
      sectionDefs.get(seg.name) match
        case Some(SectionDef(_, SectionPlacement.At(addr))) if !sectionPlaced.contains(seg.name) => addr
        case Some(SectionDef(_, SectionPlacement.At(_))) => nextAddr
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
        sectionPlaced += seg.name
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
    // When producing a relocatable executable, collect base-relative relocs per placed segment
    val outputRelocs = new ArrayBuffer[ArrayBuffer[TOFReloc]]

    for seg <- placed do
      for ext <- seg.externs do
        if !globalSymbols.contains(ext) then
          throw LinkerError(s"undefined symbol: '$ext'")

      val segRelocs = if relocatable then
        val buf = new ArrayBuffer[TOFReloc]
        outputRelocs += buf
        buf
      else
        outputRelocs += ArrayBuffer.empty
        null

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

        // Preserve reloc as base-relative (empty symbol name)
        if relocatable then
          segRelocs += TOFReloc(reloc.typ, reloc.offset, "")

    // Phase 5: resolve entry point (script entry overrides TOF entry)
    val entry = script.entry.orElse(tofs.flatMap(_.entry).lastOption)
    for name <- entry do
      if !globalSymbols.contains(name) then
        throw LinkerError(s"entry point '$name' is not a defined symbol")

    // Phase 6: merge same-named segments into single contiguous segments.
    // This is required because TOF serialize/deserialize uses segment name as key,
    // so multiple segments with the same name would lose their distinct origins.
    val mergedSegments = new mutable.LinkedHashMap[String, (Long, ArrayBuffer[Byte], ArrayBuffer[TOFSymbol], ArrayBuffer[TOFReloc])]
    for (seg, idx) <- placed.zipWithIndex do
      val segRelocs = outputRelocs(idx)
      mergedSegments.get(seg.name) match
        case None =>
          mergedSegments(seg.name) = (seg.org, ArrayBuffer.from(seg.data), ArrayBuffer.from(seg.symbols), ArrayBuffer.from(segRelocs))
        case Some((baseOrg, mergedData, mergedSyms, mergedRelocs)) =>
          val dataOffset = (seg.org - baseOrg).toInt
          // Pad if there's a gap between segments
          while mergedData.length < dataOffset do mergedData += 0.toByte
          mergedData ++= seg.data
          // Adjust symbol offsets relative to merged segment start
          for sym <- seg.symbols do
            mergedSyms += sym.copy(offset = sym.offset + dataOffset)
          // Adjust reloc offsets relative to merged segment start
          for reloc <- segRelocs do
            mergedRelocs += reloc.copy(offset = reloc.offset + dataOffset)

    val outType = if relocatable then TOFType.Relocatable else TOFType.Executable

    val outSegments = mergedSegments.map { case (name, (segOrg, data, syms, relocs)) =>
      TOF.Segment(name, segOrg, Seq(TOF.DataChunk(data.toSeq)), syms.toSeq, relocs = relocs.toSeq)
    }.toSeq

    TOF(entry, outSegments, outType)

  private def patchMovi(data: ArrayBuffer[Byte], offset: Int, addr: Long, n: Int): Unit =
    val a = addr.toInt
    for i <- 0 until n do
      val byteIdx = offset + i * 2 + 1
      val shift = (n - 1 - i) * 8
      data(byteIdx) = ((a >> shift) & 0xff).toByte
