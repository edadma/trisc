package io.github.edadma.trisc

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

enum SymbolType:
  case Func, Data, Const

enum RelocType:
  case MOVI2, MOVI3, MOVI4, ABS32, ABS64

enum TOFType:
  case Object, Executable, Relocatable

case class TOFSymbol(name: String, offset: Long, typ: SymbolType, size: Option[Long] = None, typeInfo: Option[String] = None)
case class TOFReloc(typ: RelocType, offset: Long, symbol: String)

object TOF:
  trait Chunk

  case class DataChunk(data: Seq[Byte]) extends Chunk
  case class ResChunk(size: Long) extends Chunk
  case class CommentChunk(text: String) extends Chunk

  case class Segment(
      name: String,
      org: Long,
      chunks: Seq[Chunk],
      symbols: Seq[TOFSymbol] = Nil,
      externs: Seq[String] = Nil,
      relocs: Seq[TOFReloc] = Nil,
      explicitOrg: Boolean = false,
  )

  class TOFBuilder:
    private case class TOFBuilderChunk(typ: String, data: Int | ArrayBuffer[Byte] | String)
    private class TOFBuilderSegment(
        val org: Long,
        val chunks: ListBuffer[TOFBuilderChunk] = new ListBuffer,
        var length: Long = 0,
        val symbols: ArrayBuffer[TOFSymbol] = new ArrayBuffer,
        val externs: mutable.LinkedHashSet[String] = new mutable.LinkedHashSet,
        val relocs: ArrayBuffer[TOFReloc] = new ArrayBuffer,
        var explicitOrg: Boolean = false,
    )

    private val segments = new mutable.LinkedHashMap[String, TOFBuilderSegment]
    private var current: TOFBuilderSegment = current
    private var _entry: Option[String] = None
    private var _tofType: TOFType = TOFType.Object

    def org: Long = current.org

    def setEntry(name: String): Unit = _entry = Some(name)

    def setType(typ: TOFType): Unit = _tofType = typ

    def length: Long = current.length

    def addSymbol(name: String, offset: Long, typ: SymbolType, size: Option[Long] = None, typeInfo: Option[String] = None): Unit =
      current.symbols += TOFSymbol(name, offset, typ, size, typeInfo)

    def addExtern(name: String): Unit =
      current.externs += name

    def addReloc(typ: RelocType, offset: Long, symbol: String): Unit =
      current.relocs += TOFReloc(typ, offset, symbol)

    def addComment(text: String): Unit =
      current.chunks += TOFBuilderChunk("comment", text)

    def tof: TOF =
      TOF(
        _entry,
        (for (name, seg) <- segments if seg.length > 0 || seg.chunks.nonEmpty
        yield Segment(
          name,
          seg.org,
          seg.chunks.toSeq.map {
            case TOFBuilderChunk("data", data: ArrayBuffer[Byte]) => DataChunk(data.toSeq)
            case TOFBuilderChunk("res", size: Int)                => ResChunk(size)
            case TOFBuilderChunk("comment", text: String)         => CommentChunk(text)
            case chunk => sys.error(s"unexpected chunk: $chunk")
          },
          seg.symbols.toSeq,
          seg.externs.toSeq,
          seg.relocs.toSeq,
          seg.explicitOrg,
        )).toSeq,
        _tofType,
      )

    def segmentDefined(name: String): Boolean = segments contains name

    def segment(name: String, org: Long, explicitOrg: Boolean = false): Unit =
      segments get name match
        case None =>
          current = new TOFBuilderSegment(org, explicitOrg = explicitOrg)
          segments(name) = current
        case Some(s) => current = s

    def +=(b: Byte): Unit =
      if current.chunks.isEmpty || current.chunks.last.typ != "data" then
        current.chunks += TOFBuilderChunk("data", new ArrayBuffer[Byte])

      current.chunks.last.data.asInstanceOf[ArrayBuffer[Byte]] += b
      current.length += 1

    def ++=(bs: IterableOnce[Byte]): Unit = bs.iterator foreach (b => +=(b))

    def addRes(size: Int): Unit =
      current.chunks += TOFBuilderChunk("res", size)
      current.length += size

  def builder: TOFBuilder = new TOFBuilder

  def deserialize(tof: String): TOF = fromString(tof)

  private val SupportedVersions = immutable.TreeSet("1", "2")

  def fromString(source: String): TOF =
    val b = builder
    var version = 0
    var lineNum = 0

    def err(msg: String): Nothing = throw TOFReadError(lineNum, msg)

    def parseLong(s: String): Long =
      try java.lang.Long.parseLong(s, 16)
      catch case _: NumberFormatException => err(s"invalid hex number '$s'")

    def parseSegment(rest: String): Unit =
      rest.split(",", 2).toSeq match
        case Seq(name, org) =>
          b.segment(name, parseLong(org))
        case _ => err(s"bad SEGMENT line, expected SEGMENT:name,org")

    def isHexNumber(s: String): Boolean =
      s.nonEmpty && s.forall(c => c.isDigit || 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')

    def parseSymbol(rest: String): Unit =
      rest.split(",").toSeq match
        case Seq(name, offset, "func", ti) =>
          b.addSymbol(name, parseLong(offset), SymbolType.Func, typeInfo = Some(ti))
        case Seq(name, offset, "func") =>
          b.addSymbol(name, parseLong(offset), SymbolType.Func)
        case Seq(name, offset, "data", size, ti) =>
          b.addSymbol(name, parseLong(offset), SymbolType.Data, Some(parseLong(size)), Some(ti))
        case Seq(name, offset, "data", sizeOrTi) =>
          if isHexNumber(sizeOrTi) then b.addSymbol(name, parseLong(offset), SymbolType.Data, Some(parseLong(sizeOrTi)))
          else b.addSymbol(name, parseLong(offset), SymbolType.Data, typeInfo = Some(sizeOrTi))
        case Seq(name, offset, "data") =>
          b.addSymbol(name, parseLong(offset), SymbolType.Data)
        case Seq(name, offset, "const", ti) =>
          b.addSymbol(name, parseLong(offset), SymbolType.Const, typeInfo = Some(ti))
        case Seq(name, offset, "const") =>
          b.addSymbol(name, parseLong(offset), SymbolType.Const)
        case _ => err(s"bad SYMBOL line, expected SYMBOL:name,offset,type[,size][,typeinfo]")

    def parseReloc(rest: String): Unit =
      rest.split(",", 3).toSeq match
        case Seq(typStr, offset, name) =>
          val typ = typStr match
            case "MOVI2" => RelocType.MOVI2
            case "MOVI3" => RelocType.MOVI3
            case "MOVI4" => RelocType.MOVI4
            case "ABS32" => RelocType.ABS32
            case "ABS64" => RelocType.ABS64
            case _       => err(s"unknown relocation type '$typStr'")
          b.addReloc(typ, parseLong(offset), name)
        case _ => err(s"bad RELOC line, expected RELOC:type,offset,symbol")

    def parseData(hex: String): Unit =
      if hex.length % 2 != 0 then err("DATA hex string has odd length")
      b ++= hex.grouped(2).map(s => Integer.parseInt(s, 16).toByte)

    def parseRes(size: String): Unit =
      b.addRes(Integer.parseInt(size, 16))

    for rawLine <- source.linesIterator do
      lineNum += 1
      val line = rawLine.trim

      if line.nonEmpty then
        if version == 0 then
          line match
            case s"TOF v$n" if SupportedVersions(n) => version = n.toInt
            case s"TOF v$n"                          => err(s"unsupported TOF version '$n' (supported: ${SupportedVersions.mkString(", ")})")
            case _                                   => err("expected TOF version header")
        else
          line match
            case s"# $text"       => b.addComment(text)
            case s"#$text"        => b.addComment(text)
            case s"TYPE:$typStr"  =>
              typStr match
                case "object"      => b.setType(TOFType.Object)
                case "executable"  => b.setType(TOFType.Executable)
                case "relocatable" => b.setType(TOFType.Relocatable)
                case _             => err(s"unknown TOF type '$typStr'")
            case s"ENTRY:$name"   => b.setEntry(name)
            case s"SEGMENT:$rest" => parseSegment(rest)
            case s"SYMBOL:$rest"  => parseSymbol(rest)
            case s"EXTERN:$name"  => b.addExtern(name)
            case s"RELOC:$rest"   => parseReloc(rest)
            case s"DATA:$rest"    => parseData(rest)
            case s"RES:$rest"     => parseRes(rest)
            case _                => err(s"unrecognized line: $line")

    if version == 0 then throw TOFReadError(lineNum, "empty or missing TOF header")

    b.tof

class TOF(val entry: Option[String], val segments: Seq[TOF.Segment], val tofType: TOFType = TOFType.Object):

  def this(segments: Seq[TOF.Segment]) = this(None, segments)

  // --- Loading ---

  def load(mem: Addressable): Unit =
    for TOF.Segment(name, org, chunks, _, _, _, _) <- segments do
      var addr = org

      chunks foreach {
        case TOF.DataChunk(data) =>
          mem.load(addr, data)
          addr += data.length
        case TOF.ResChunk(size)   => addr += size
        case TOF.CommentChunk(_)  =>
      }

  def load(mem: Addressable, baseAddress: Long): Unit =
    val linkTimeBase = segments.headOption.map(_.org).getOrElse(0L)
    val delta = baseAddress - linkTimeBase

    // Load segment data at adjusted addresses
    for seg <- segments do
      var addr = seg.org + delta
      seg.chunks foreach {
        case TOF.DataChunk(data) =>
          mem.load(addr, data)
          addr += data.length
        case TOF.ResChunk(size) => addr += size
        case TOF.CommentChunk(_) =>
      }

    // Patch relocations in memory
    for seg <- segments do
      val segBase = seg.org + delta
      for reloc <- seg.relocs do
        val patchAddr = segBase + reloc.offset
        reloc.typ match
          case RelocType.ABS32 =>
            val oldVal = mem.readInt(patchAddr)
            mem.writeInt(patchAddr, (oldVal + delta).toInt)
          case RelocType.ABS64 =>
            val oldVal = mem.readLong(patchAddr)
            mem.writeLong(patchAddr, oldVal + delta)
          case rt @ (RelocType.MOVI2 | RelocType.MOVI3 | RelocType.MOVI4) =>
            val n = rt match
              case RelocType.MOVI2 => 2
              case RelocType.MOVI3 => 3
              case RelocType.MOVI4 => 4
            patchMoviInMemory(mem, patchAddr, delta, n)

  private def patchMoviInMemory(mem: Addressable, addr: Long, delta: Long, n: Int): Unit =
    // Read current encoded address from instruction immediate bytes
    var current = 0L
    for i <- 0 until n do
      current = (current << 8) | mem.readByteUnsigned(addr + i * 2 + 1)
    // Apply delta and write back
    val newVal = current + delta
    for i <- 0 until n do
      val shift = (n - 1 - i) * 8
      mem.writeByte(addr + i * 2 + 1, (newVal >> shift) & 0xff)

  // --- Query ---

  def isFullyResolved: Boolean = segments.forall(s => s.externs.isEmpty && s.relocs.isEmpty)

  def entryAddress: Option[Long] =
    entry.flatMap { name =>
      for
        seg <- segments.find(_.symbols.exists(_.name == name))
        sym <- seg.symbols.find(_.name == name)
      yield seg.org + sym.offset
    }

  def entryAddress(baseAddress: Long): Option[Long] =
    val linkTimeBase = segments.headOption.map(_.org).getOrElse(0L)
    entryAddress.map(_ + baseAddress - linkTimeBase)

  def allSymbols: Seq[(String, TOFSymbol)] =
    for seg <- segments; sym <- seg.symbols yield (seg.name, sym)

  def allExterns: Seq[(String, String)] =
    for seg <- segments; ext <- seg.externs yield (seg.name, ext)

  def allRelocs: Seq[(String, TOFReloc)] =
    for seg <- segments; reloc <- seg.relocs yield (seg.name, reloc)

  def symbolByName(name: String): Option[TOFSymbol] =
    segments.flatMap(_.symbols).find(_.name == name)

  def segment(name: String): Option[TOF.Segment] =
    segments.find(_.name == name)

  def segmentNames: Seq[String] = segments.map(_.name)

  def totalDataSize: Long =
    segments.map { seg =>
      seg.chunks.map {
        case TOF.DataChunk(data) => data.length.toLong
        case TOF.ResChunk(size)  => size
        case _                   => 0L
      }.sum
    }.sum

  /** Produce a detailed report of all segment addresses, symbols, relocations, and overlap checks. */
  def dumpLayout: String =
    val sb = new StringBuilder

    // Entry point
    sb ++= s"Entry: ${entry.map(n => f"$n (0x${entryAddress.getOrElse(0L)}%x)").getOrElse("none")}\n"
    sb ++= s"Type: $tofType\n\n"

    // Segment layout
    sb ++= "=== Segments ===\n"
    case class SegRange(name: String, org: Long, end: Long)
    val ranges = new ArrayBuffer[SegRange]

    for seg <- segments do
      val size = seg.chunks.map {
        case TOF.DataChunk(d) => d.length.toLong
        case TOF.ResChunk(s)  => s
        case _                => 0L
      }.sum
      val end = seg.org + size
      ranges += SegRange(seg.name, seg.org, end)
      sb ++= f"  ${seg.name}%-12s org=0x${seg.org}%06x  end=0x$end%06x  size=$size%d"
      if seg.explicitOrg then sb ++= "  [explicit]"
      sb ++= s"  (${seg.symbols.length} syms, ${seg.relocs.length} relocs, ${seg.externs.length} externs)\n"

    // Overlap check
    sb ++= "\n=== Overlap Check ===\n"
    var hasOverlap = false
    for i <- ranges.indices; j <- (i + 1) until ranges.length do
      val a = ranges(i); val b = ranges(j)
      if a.org < b.end && b.org < a.end then
        sb ++= f"  OVERLAP: ${a.name} [0x${a.org}%06x-0x${a.end}%06x) vs ${b.name} [0x${b.org}%06x-0x${b.end}%06x)\n"
        hasOverlap = true
    if !hasOverlap then sb ++= "  No overlaps\n"

    // Symbols by segment
    sb ++= "\n=== Symbols ===\n"
    for seg <- segments if seg.symbols.nonEmpty do
      sb ++= s"  ${seg.name}:\n"
      for sym <- seg.symbols.sortBy(_.offset) do
        val abs = seg.org + sym.offset
        sb ++= f"    ${sym.name}%-40s offset=0x${sym.offset}%04x  abs=0x$abs%06x  ${sym.typ}${sym.size.map(s => s"  size=$s").getOrElse("")}\n"

    // Unresolved externs
    val allExts = segments.flatMap(_.externs).distinct
    if allExts.nonEmpty then
      sb ++= "\n=== Unresolved Externs ===\n"
      for seg <- segments if seg.externs.nonEmpty do
        sb ++= s"  ${seg.name}: ${seg.externs.mkString(", ")}\n"

    // Relocations
    val totalRelocs = segments.map(_.relocs.length).sum
    if totalRelocs > 0 then
      sb ++= s"\n=== Relocations ($totalRelocs total) ===\n"
      for seg <- segments if seg.relocs.nonEmpty do
        val baseRel = seg.relocs.count(_.symbol.isEmpty)
        val named = seg.relocs.count(_.symbol.nonEmpty)
        sb ++= s"  ${seg.name}: $baseRel base-relative, $named named\n"
        for reloc <- seg.relocs.take(20) do
          val sym = if reloc.symbol.isEmpty then "(base-rel)" else reloc.symbol
          sb ++= f"    ${reloc.typ}%-8s offset=0x${reloc.offset}%04x  $sym\n"
        if seg.relocs.length > 20 then
          sb ++= s"    ... and ${seg.relocs.length - 20} more\n"

    sb.toString

  // --- Serialization ---

  def serialize: String =
    val buf = new StringBuilder

    buf ++= "TOF v1\n"

    tofType match
      case TOFType.Executable  => buf ++= "TYPE:executable\n"
      case TOFType.Relocatable => buf ++= "TYPE:relocatable\n"
      case TOFType.Object      => // omit for backward compatibility

    for e <- entry do
      buf ++= s"ENTRY:$e\n"

    for s <- segments do
      buf ++= s"SEGMENT:${s.name},${s.org.toHexString}\n"

      for sym <- s.symbols do
        val typStr = sym.typ match
          case SymbolType.Func  => "func"
          case SymbolType.Data  => "data"
          case SymbolType.Const => "const"
        val sizeStr = sym.size.map(sz => s",${sz.toHexString}").getOrElse("")
        val tiStr = sym.typeInfo.map(ti => s",$ti").getOrElse("")
        buf ++= s"SYMBOL:${sym.name},${sym.offset.toHexString},$typStr$sizeStr$tiStr\n"

      for ext <- s.externs do
        buf ++= s"EXTERN:$ext\n"

      for reloc <- s.relocs do
        buf ++= s"RELOC:${reloc.typ},${reloc.offset.toHexString},${reloc.symbol}\n"

      s.chunks foreach {
        case TOF.CommentChunk(text) => buf ++= s"# $text\n"
        case TOF.DataChunk(data)    =>
          // Limit DATA lines to 2048 hex chars (1024 bytes) for readability
          val hex = data.map(b => f"${b & 0xff}%02x").mkString
          for chunk <- hex.grouped(2048) do
            buf ++= s"DATA:$chunk\n"
        case TOF.ResChunk(size)     => buf ++= s"RES:${size.toHexString}\n"
        case c                      => sys.error(s"can't serialize $c")
      }

    buf.toString
