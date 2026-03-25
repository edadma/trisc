package io.github.edadma.trisc

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

enum SymbolType:
  case Func, Data, Const

enum RelocType:
  case MOVI2, MOVI3, MOVI4, ABS32

case class TOFSymbol(name: String, offset: Long, typ: SymbolType, size: Option[Long] = None)
case class TOFReloc(typ: RelocType, offset: Long, symbol: String)

object TOF:
  trait Chunk

  case class DataChunk(data: Seq[Byte]) extends Chunk
  case class ResChunk(size: Long) extends Chunk

  case class Segment(
      name: String,
      org: Long,
      chunks: Seq[Chunk],
      symbols: Seq[TOFSymbol] = Nil,
      externs: Seq[String] = Nil,
      relocs: Seq[TOFReloc] = Nil,
  )

  class TOFBuilder:
    private case class TOFBuilderChunk(typ: String, data: Int | ArrayBuffer[Byte])
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

    def org: Long = current.org

    def length: Long = current.length

    def addSymbol(name: String, offset: Long, typ: SymbolType, size: Option[Long] = None): Unit =
      current.symbols += TOFSymbol(name, offset, typ, size)

    def addExtern(name: String): Unit =
      current.externs += name

    def addReloc(typ: RelocType, offset: Long, symbol: String): Unit =
      current.relocs += TOFReloc(typ, offset, symbol)

    def tof: TOF =
      TOF(
        (for (name, seg) <- segments if seg.length > 0
        yield Segment(
          name,
          seg.org,
          seg.chunks.toSeq.map {
            case TOFBuilderChunk("data", data: ArrayBuffer[Byte]) => DataChunk(data.toSeq)
            case TOFBuilderChunk("res", size: Int)                => ResChunk(size)
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

  def deserialize(tof: String): TOF = readTOF(tof)

class TOF(val segments: Seq[TOF.Segment]):

  // --- Loading ---

  def load(mem: Addressable): Unit =
    for TOF.Segment(name, org, chunks, _, _, _) <- segments do
      var addr = org

      chunks foreach {
        case TOF.DataChunk(data) =>
          mem.load(addr, data)
          addr += data.length
        case TOF.ResChunk(size) => addr += size
      }

  // --- Query ---

  def isFullyResolved: Boolean = segments.forall(s => s.externs.isEmpty && s.relocs.isEmpty)

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
      }.sum
    }.sum

  // --- Serialization ---

  def serialize: String =
    val buf = new StringBuilder

    buf ++= "TOF v2\n"

    for s <- segments do
      buf ++= s"SEGMENT:${s.name},${s.org.toHexString}\n"

      for sym <- s.symbols do
        val typStr = sym.typ match
          case SymbolType.Func  => "func"
          case SymbolType.Data  => "data"
          case SymbolType.Const => "const"
        val sizeStr = sym.size.map(sz => s",${sz.toHexString}").getOrElse("")
        buf ++= s"SYMBOL:${sym.name},${sym.offset.toHexString},$typStr$sizeStr\n"

      for ext <- s.externs do
        buf ++= s"EXTERN:$ext\n"

      for reloc <- s.relocs do
        buf ++= s"RELOC:${reloc.typ},${reloc.offset.toHexString},${reloc.symbol}\n"

      s.chunks foreach {
        case TOF.DataChunk(data) => buf ++= s"DATA:${data.map(b => f"${b & 0xff}%02x").mkString}\n"
        case TOF.ResChunk(size)  => buf ++= s"RES:${size.toHexString}\n"
        case c                   => sys.error(s"can't serialize $c")
      }

    buf.toString
