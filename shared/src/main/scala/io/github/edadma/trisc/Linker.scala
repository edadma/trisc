package io.github.edadma.trisc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Linker:
  case class LinkerError(msg: String) extends RuntimeException(msg)

  def link(tofs: Seq[TOF], baseAddress: Long = 0, addresses: Int = 2): TOF =
    // Collect all segments, flattening data into mutable byte arrays for patching
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

    // Phase 2: place segments sequentially from baseAddress
    var nextAddr = baseAddress
    val placed = new ArrayBuffer[PlacedSegment]

    for tof <- tofs do
      for seg <- tof.segments do
        val data = flattenChunks(seg.chunks)
        val org = if seg.org != 0 then seg.org else nextAddr
        placed += PlacedSegment(seg.name, org, data, seg.symbols, seg.externs, seg.relocs)
        nextAddr = org + data.length

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
      // Check all externs are resolvable
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

          case RelocType.MOVI2 =>
            patchMovi(seg.data, reloc.offset.toInt, addr, 2)

          case RelocType.MOVI3 =>
            patchMovi(seg.data, reloc.offset.toInt, addr, 3)

          case RelocType.MOVI4 =>
            patchMovi(seg.data, reloc.offset.toInt, addr, 4)

    // Phase 5: produce output TOF (fully resolved, no externs/relocs)
    val outSegments = placed.map { seg =>
      TOF.Segment(
        seg.name,
        seg.org,
        Seq(TOF.DataChunk(seg.data.toSeq)),
        seg.symbols.map(s => s.copy(offset = s.offset)), // keep symbols for debugging
      )
    }.toSeq

    TOF(outSegments)

  /** Patch a MOVI instruction sequence (ldi + N-1 sli instructions).
    * Each instruction is 16 bits: 111 rrr oo iiiiiiii
    * The immediate byte field is bits 7-0 of each instruction word.
    */
  private def patchMovi(data: ArrayBuffer[Byte], offset: Int, addr: Long, n: Int): Unit =
    val a = addr.toInt
    // Bytes are stored big-endian. Each instruction is 2 bytes.
    // The immediate is the low 8 bits of the instruction word (second byte, bits 7-0).
    for i <- 0 until n do
      val byteIdx = offset + i * 2 + 1 // second byte of each instruction
      val shift = (n - 1 - i) * 8
      data(byteIdx) = ((a >> shift) & 0xff).toByte
