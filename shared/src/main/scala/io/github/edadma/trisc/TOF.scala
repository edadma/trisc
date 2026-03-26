package io.github.edadma.trisc

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

enum SymbolType:
  case Func, Data, Const

enum RelocType:
  case MOVI2, MOVI3, MOVI4, ABS32

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
    )

    private val segments = new mutable.LinkedHashMap[String, TOFBuilderSegment]
    private var current: TOFBuilderSegment = current
    private var _entry: Option[String] = None

    def org: Long = current.org

    def setEntry(name: String): Unit = _entry = Some(name)

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
        )).toSeq,
      )

    def segmentDefined(name: String): Boolean = segments contains name

    def segment(name: String, org: Long): Unit =
      segments get name match
        case None =>
          current = new TOFBuilderSegment(org)
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
          if b.segmentDefined(name) then err(s"duplicate segment '$name'")
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

class TOF(val entry: Option[String], val segments: Seq[TOF.Segment]):

  def this(segments: Seq[TOF.Segment]) = this(None, segments)

  // --- Loading ---

  def load(mem: Addressable): Unit =
    for TOF.Segment(name, org, chunks, _, _, _) <- segments do
      var addr = org

      chunks foreach {
        case TOF.DataChunk(data) =>
          mem.load(addr, data)
          addr += data.length
        case TOF.ResChunk(size)   => addr += size
        case TOF.CommentChunk(_)  =>
      }

  // --- Query ---

  def isFullyResolved: Boolean = segments.forall(s => s.externs.isEmpty && s.relocs.isEmpty)

  def entryAddress: Option[Long] =
    entry.flatMap { name =>
      for
        seg <- segments.find(_.symbols.exists(_.name == name))
        sym <- seg.symbols.find(_.name == name)
      yield seg.org + sym.offset
    }

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

  // --- Serialization ---

  def serialize: String =
    val buf = new StringBuilder

    buf ++= "TOF v2\n"

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
        case TOF.DataChunk(data)    => buf ++= s"DATA:${data.map(b => f"${b & 0xff}%02x").mkString}\n"
        case TOF.ResChunk(size)     => buf ++= s"RES:${size.toHexString}\n"
        case c                      => sys.error(s"can't serialize $c")
      }

    buf.toString
