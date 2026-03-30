package io.github.edadma.trisc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Linker:
  case class LinkerError(msg: String) extends RuntimeException(msg)

  def link(tofs: Seq[TOF], baseAddress: Long = 0, relocatable: Boolean = false): TOF =
    link(tofs, LinkerScript(), baseAddress, relocatable)

  def link(tofs: Seq[TOF], script: LinkerScript, baseAddress: Long): TOF =
    link(tofs, script, baseAddress, false)

  def link(tofs: Seq[TOF], script: LinkerScript, baseAddress: Long, relocatable: Boolean): TOF =
    case class PlacedSegment(
        name: String,
        org: Long,
        data: ArrayBuffer[Byte],
        symbols: Seq[TOFSymbol],
        externs: Seq[String],
        relocs: Seq[TOFReloc],
        groupIdx: Int = -1, // index into tofGroups, -1 if none
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

    // Group input segments by source TOF, tracking which TOFs need unit placement.
    // A relocatable TOF with base-relative relocs must have all its segments placed
    // as a unit (same delta) so cross-segment references remain valid.
    case class TofGroup(segments: Seq[InputSegment], placeAsUnit: Boolean)

    val tofGroups = new ArrayBuffer[TofGroup]
    for tof <- tofs do
      val segs = for seg <- tof.segments yield
        InputSegment(seg.name, seg.org, flattenChunks(seg.chunks), seg.symbols, seg.externs, seg.relocs, seg.explicitOrg)
      val hasBaseRelRelocs = segs.exists(_.relocs.exists(_.symbol.isEmpty))
      tofGroups += TofGroup(segs, hasBaseRelRelocs)

    // Index all input segments with a global index for identity tracking.
    // (InputSegment is a case class, so structural equality can't distinguish
    // two segments with identical content from different TOFs.)
    case class IndexedSegment(idx: Int, seg: InputSegment, groupIdx: Int)

    val indexedSegments = new ArrayBuffer[IndexedSegment]
    for (group, gi) <- tofGroups.zipWithIndex do
      for seg <- group.segments do
        indexedSegments += IndexedSegment(indexedSegments.length, seg, gi)

    // Phase 2: place segments using linker script or sequential placement
    val placed = new ArrayBuffer[PlacedSegment]
    val placedByName = new mutable.LinkedHashMap[String, PlacedSegment]
    var nextAddr = baseAddress

    val scriptSectionNames = script.sections.map(_.name).toSet

    // Track which indexed segments have been placed
    val placedIndices = new mutable.HashSet[Int]

    def placeSeg(is: IndexedSegment, org: Long): Unit =
      val ps = PlacedSegment(is.seg.name, org, is.seg.data, is.seg.symbols, is.seg.externs, is.seg.relocs, is.groupIdx)
      placed += ps
      placedByName(is.seg.name) = ps
      placedIndices += is.idx

    // Place segments in script order. All script sections exist (even if empty).
    for secDef <- script.sections do
      val sectionBase = secDef.address.getOrElse(nextAddr)
      nextAddr = sectionBase

      val matching = indexedSegments.filter(is => is.seg.name == secDef.name && !placedIndices.contains(is.idx))
      for is <- matching do
        placeSeg(is, nextAddr)
        nextAddr = (nextAddr + is.seg.data.length + 7) & ~7L

      if !placedByName.contains(secDef.name) then
        placedByName(secDef.name) = PlacedSegment(secDef.name, sectionBase, ArrayBuffer.empty, Nil, Nil, Nil)

    // Place remaining segments not in the script, grouped by name.
    // All segments with the same name are placed contiguously so the Phase 6
    // merge doesn't produce overlapping ranges with other segment types.
    // For relocatable TOFs with base-relative relocs, all segments from
    // that TOF are placed as a unit (same delta) so cross-segment references stay valid.
    val remaining = indexedSegments.filter(is => !scriptSectionNames.contains(is.seg.name) && !placedIndices.contains(is.idx))

    // Check if any TOF group needs unit placement
    val unitGroupIndices = tofGroups.zipWithIndex.collect {
      case (g, gi) if g.placeAsUnit && remaining.exists(_.groupIdx == gi) => gi
    }.toSet

    if unitGroupIndices.nonEmpty then
      // Unit placement: place all segments from each group together,
      // preserving relative positions within unit groups
      for (group, gi) <- tofGroups.zipWithIndex do
        val groupRemaining = remaining.filter(is => is.groupIdx == gi && !placedIndices.contains(is.idx))
        if groupRemaining.isEmpty then ()
        else if unitGroupIndices.contains(gi) && groupRemaining.length > 1 then
          val firstSeg = groupRemaining.head.seg
          val baseDelta = nextAddr - firstSeg.originalOrg
          var maxEnd = 0L
          for is <- groupRemaining do
            val org = is.seg.originalOrg + baseDelta
            placeSeg(is, org)
            val segEnd = org + is.seg.data.length
            if segEnd > maxEnd then maxEnd = segEnd
          nextAddr = (maxEnd + 7) & ~7L
        else
          for is <- groupRemaining do
            val org = if is.seg.explicitOrg then is.seg.originalOrg else nextAddr
            placeSeg(is, org)
            nextAddr = (org + is.seg.data.length + 7) & ~7L
    else
      // No unit groups: group segments by name so same-named segments are contiguous.
      // This prevents merged segments from overlapping other segment types.
      val segmentNameOrder = remaining.map(_.seg.name).distinct
      for name <- segmentNameOrder do
        for is <- remaining.filter(_.seg.name == name) if !placedIndices.contains(is.idx) do
          val org = if is.seg.explicitOrg then is.seg.originalOrg else nextAddr
          placeSeg(is, org)
          nextAddr = (org + is.seg.data.length + 7) & ~7L

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

    // Phase 3c: build address remap for unit groups.
    // For relocatable TOFs with base-relative relocs, the first-stage link encoded
    // absolute addresses using the original segment layout. If the second-stage link
    // (e.g. via linker script) reorders segments, we need a per-address remap rather
    // than a single delta. For each unit group, map original address ranges to new orgs.
    case class SegmentRange(originalOrg: Long, size: Long, newOrg: Long)
    val groupRemaps = new mutable.HashMap[Int, Seq[SegmentRange]]

    for (group, gi) <- tofGroups.zipWithIndex if group.placeAsUnit do
      val ranges = for seg <- group.segments yield
        val newOrg = placed.find(ps => ps.groupIdx == gi && ps.name == seg.name).map(_.org).getOrElse(seg.originalOrg)
        SegmentRange(seg.originalOrg, seg.data.length.toLong, newOrg)
      groupRemaps(gi) = ranges.sortBy(_.originalOrg)

    def remapAddress(groupIdx: Int, oldAddr: Long): Long =
      groupRemaps.get(groupIdx) match
        case Some(ranges) =>
          // Find which original segment this address falls in
          ranges.findLast(r => oldAddr >= r.originalOrg) match
            case Some(r) => r.newOrg + (oldAddr - r.originalOrg)
            case None => oldAddr // before any segment, shouldn't happen
        case None => oldAddr // no remap for this group

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
          // Already resolved (base-relative) — adjust for new segment placement.
          // For unit groups (relocatable TOFs with base-relative relocs), use per-address
          // remapping to handle segment reordering (e.g. by linker script).
          // For non-unit groups, use simple delta (seg.org).
          val off = reloc.offset.toInt
          val useRemap = seg.groupIdx >= 0 && groupRemaps.contains(seg.groupIdx)

          def adjustAddr(old: Long): Long =
            if useRemap then remapAddress(seg.groupIdx, old)
            else old + seg.org

          reloc.typ match
            case RelocType.ABS32 =>
              val old = ((seg.data(off) & 0xff) << 24) | ((seg.data(off + 1) & 0xff) << 16) |
                ((seg.data(off + 2) & 0xff) << 8) | (seg.data(off + 3) & 0xff)
              val addr = adjustAddr(old.toLong)
              seg.data(off) = ((addr >> 24) & 0xff).toByte
              seg.data(off + 1) = ((addr >> 16) & 0xff).toByte
              seg.data(off + 2) = ((addr >> 8) & 0xff).toByte
              seg.data(off + 3) = (addr & 0xff).toByte
            case RelocType.ABS64 =>
              var old = 0L
              for i <- 0 until 8 do old = (old << 8) | (seg.data(off + i) & 0xff)
              val addr = adjustAddr(old)
              for i <- 0 until 8 do seg.data(off + i) = ((addr >> ((7 - i) * 8)) & 0xff).toByte
            case RelocType.MOVI2 | RelocType.MOVI3 | RelocType.MOVI4 =>
              val n = reloc.typ match
                case RelocType.MOVI2 => 2
                case RelocType.MOVI3 => 3
                case RelocType.MOVI4 => 4
                case _               => throw LinkerError(s"unexpected reloc type in MOVI branch: ${reloc.typ}")
              var old = 0L
              for i <- 0 until n do old = (old << 8) | (seg.data(off + i * 2 + 1) & 0xff)
              patchMovi(seg.data, off, adjustAddr(old), n)
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
                if reloc.symbol == "kernel_main" then
                  System.err.println(f"[LINKER] Patching kernel_main: seg=${seg.name}@0x${seg.org}%x offset=0x${reloc.offset}%x addr=0x$addr%x")
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
