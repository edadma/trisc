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
        explicitOrg: Boolean,
    )

    val inputSegments = new ArrayBuffer[InputSegment]
    for tof <- tofs do
      for seg <- tof.segments do
        inputSegments += InputSegment(seg.name, seg.org, flattenChunks(seg.chunks), seg.symbols, seg.externs, seg.relocs, seg.explicitOrg)

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
          if seg.explicitOrg then seg.originalOrg
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
        nextAddr = (org + seg.data.length + 7) & ~7L // 8-byte align for next segment

    // Place remaining segments not in the script
    for seg <- inputSegments if !scriptSectionNames.contains(seg.name) do
      val org = resolveOrg(seg)
      val ps = PlacedSegment(seg.name, org, seg.data, seg.symbols, seg.externs, seg.relocs)
      placed += ps
      placedByName(seg.name) = ps
      nextAddr = (org + seg.data.length + 7) & ~7L // 8-byte align for next segment

    // Phase 3: build global symbol table
    val globalSymbols = new mutable.LinkedHashMap[String, (Long, TOFSymbol)]

    for seg <- placed do
      for sym <- seg.symbols do
        val absAddr = seg.org + sym.offset
        if globalSymbols.contains(sym.name) then
          throw LinkerError(s"duplicate symbol: '${sym.name}'")
        globalSymbols(sym.name) = (absAddr, sym)

    // Phase 3b: resolve linker script SYMBOL definitions
    for symDef <- script.symbols do
      val addr = symDef.value match
        case SymbolValue.Absolute(a) => a
        case SymbolValue.AfterSection(ref) =>
          placedByName.get(ref) match
            case Some(prev) => (prev.org + prev.data.length + 7) & ~7L // 8-byte aligned
            case None => nextAddr // section not found, use next available
      if globalSymbols.contains(symDef.name) then
        throw LinkerError(s"linker symbol '${symDef.name}' conflicts with an existing symbol")
      globalSymbols(symDef.name) = (addr, TOFSymbol(symDef.name, 0, SymbolType.Data, Some(8)))

    // Phase 4: resolve relocations
    // When producing a relocatable output, unresolved externs are preserved
    val outputRelocs = new ArrayBuffer[ArrayBuffer[TOFReloc]]
    val outputExterns = new ArrayBuffer[ArrayBuffer[String]]

    for seg <- placed do
      if !relocatable then
        for ext <- seg.externs do
          if !globalSymbols.contains(ext) then
            throw LinkerError(s"undefined symbol: '$ext'")

      val segRelocs = new ArrayBuffer[TOFReloc]
      outputRelocs += segRelocs
      val segExterns = new ArrayBuffer[String]
      outputExterns += segExterns

      // Collect unresolved externs for relocatable output
      if relocatable then
        for ext <- seg.externs do
          if !globalSymbols.contains(ext) && !segExterns.contains(ext) then
            segExterns += ext

      for reloc <- seg.relocs do
        if reloc.symbol.isEmpty then
          // Already resolved (base-relative) — adjust for new segment placement
          val off = reloc.offset.toInt
          val delta = seg.org
          if delta != 0 then
            reloc.typ match
              case RelocType.ABS32 =>
                val old = ((seg.data(off) & 0xff) << 24) | ((seg.data(off + 1) & 0xff) << 16) |
                  ((seg.data(off + 2) & 0xff) << 8) | (seg.data(off + 3) & 0xff)
                val addr = old.toLong + delta
                seg.data(off) = ((addr >> 24) & 0xff).toByte
                seg.data(off + 1) = ((addr >> 16) & 0xff).toByte
                seg.data(off + 2) = ((addr >> 8) & 0xff).toByte
                seg.data(off + 3) = (addr & 0xff).toByte
              case RelocType.ABS64 =>
                var old = 0L
                for i <- 0 until 8 do old = (old << 8) | (seg.data(off + i) & 0xff)
                val addr = old + delta
                for i <- 0 until 8 do seg.data(off + i) = ((addr >> ((7 - i) * 8)) & 0xff).toByte
              case RelocType.MOVI2 | RelocType.MOVI3 | RelocType.MOVI4 =>
                val n = reloc.typ match
                  case RelocType.MOVI2 => 2
                  case RelocType.MOVI3 => 3
                  case RelocType.MOVI4 => 4
                  case _               => throw LinkerError(s"unexpected reloc type in MOVI branch: ${reloc.typ}")
                // Read current address from movi instruction bytes
                var old = 0L
                for i <- 0 until n do old = (old << 8) | (seg.data(off + i * 2 + 1) & 0xff)
                patchMovi(seg.data, off, old + delta, n)
          if relocatable then segRelocs += reloc
        else globalSymbols.get(reloc.symbol) match
          case Some((addr, _)) =>
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

            // Preserve reloc as base-relative (resolved symbol)
            if relocatable then
              segRelocs += TOFReloc(reloc.typ, reloc.offset, "")

          case None if relocatable =>
            // Unresolved — preserve reloc with original symbol name
            segRelocs += reloc

          case None =>
            throw LinkerError(s"undefined symbol: '${reloc.symbol}'")

    // Phase 5: resolve entry point (script entry overrides TOF entry)
    val entry = script.entry.orElse(tofs.flatMap(_.entry).lastOption)
    for name <- entry do
      if !globalSymbols.contains(name) then
        throw LinkerError(s"entry point '$name' is not a defined symbol")

    // Phase 6: merge same-named segments into single contiguous segments.
    // This is required because TOF serialize/deserialize uses segment name as key,
    // so multiple segments with the same name would lose their distinct origins.
    // Gaps between same-named segments use ResChunk so interleaved segments aren't overwritten on load.
    case class MergedSegment(
        org: Long,
        chunks: ArrayBuffer[TOF.Chunk],
        totalLength: Long,
        symbols: ArrayBuffer[TOFSymbol],
        relocs: ArrayBuffer[TOFReloc],
        externs: ArrayBuffer[String],
    )
    val mergedSegments = new mutable.LinkedHashMap[String, MergedSegment]
    for (seg, idx) <- placed.zipWithIndex do
      val segRelocs = outputRelocs(idx)
      val segExterns = outputExterns(idx)
      mergedSegments.get(seg.name) match
        case None =>
          mergedSegments(seg.name) = MergedSegment(
            seg.org,
            ArrayBuffer(TOF.DataChunk(seg.data.toSeq)),
            seg.data.length.toLong,
            ArrayBuffer.from(seg.symbols),
            ArrayBuffer.from(segRelocs),
            ArrayBuffer.from(segExterns),
          )
        case Some(ms) =>
          val dataOffset = (seg.org - ms.org).toInt
          // Insert a ResChunk for the gap (preserves interleaved segments' data on load)
          val gap = dataOffset - ms.totalLength.toInt
          if gap > 0 then ms.chunks += TOF.ResChunk(gap)
          ms.chunks += TOF.DataChunk(seg.data.toSeq)
          val newTotalLength = dataOffset.toLong + seg.data.length
          // Adjust symbol offsets relative to merged segment start
          for sym <- seg.symbols do
            ms.symbols += sym.copy(offset = sym.offset + dataOffset)
          // Adjust reloc offsets relative to merged segment start
          for reloc <- segRelocs do
            ms.relocs += reloc.copy(offset = reloc.offset + dataOffset)
          // Merge unresolved externs (deduplicate)
          for ext <- segExterns do
            if !ms.externs.contains(ext) then ms.externs += ext
          mergedSegments(seg.name) = ms.copy(totalLength = newTotalLength)

    val outType = if relocatable then TOFType.Relocatable else TOFType.Executable

    val outSegments = mergedSegments.map { case (name, ms) =>
      TOF.Segment(name, ms.org, ms.chunks.toSeq, ms.symbols.toSeq, externs = ms.externs.toSeq, relocs = ms.relocs.toSeq)
    }.toSeq

    TOF(entry, outSegments, outType)

  private def patchMovi(data: ArrayBuffer[Byte], offset: Int, addr: Long, n: Int): Unit =
    val a = addr.toInt
    for i <- 0 until n do
      val byteIdx = offset + i * 2 + 1
      val shift = (n - 1 - i) * 8
      data(byteIdx) = ((a >> shift) & 0xff).toByte
