package io.github.edadma.trisc

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object TOF:
  trait Chunk

  case class DataChunk(data: Seq[Byte]) extends Chunk
  case class ResChunk(size: Long) extends Chunk

  case class Segment(name: String, org: Long, chunks: Seq[Chunk])

  class TOFBuilder:
    private case class TOFBuilderChunk(typ: String, data: Int | ArrayBuffer[Byte])
    private class TOFBuilderSegment(
        val org: Long,
        val chunks: ListBuffer[TOFBuilderChunk] = new ListBuffer,
        var length: Long = 0,
    )

    private val segments = new mutable.LinkedHashMap[String, TOFBuilderSegment]
    private var current: TOFBuilderSegment = current

    def org: Long = current.org

    def length: Long = current.length

    def tof: TOF = null

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

  def deserialize(tof: String): TOF =
    val lines = scala.io.Source.fromString(tof).getLines
    val versions = immutable.TreeSet("1")
    var v = 0
    val b = builder

    lines.zipWithIndex map ((s, idx) => (s.trim, idx + 1)) foreach {
      case (s, _) if s.isEmpty             =>
      case (s"TOF v$n", l) if !versions(n) => sys.error(s"TOF version must be one of [$versions] on line $l")
      case (s"TOF v$n", _) if v == 0       => v = n.toInt
      case (_, l) if v == 0                => sys.error(s"missing magic on line $l")
      case (s"SEGMENT:$name,$org", l) =>
        if b.segmentDefined(name) then sys.error(s"duplicate segment on line $l")
        b.segment(name, java.lang.Long.parseUnsignedLong(org, 16))
      case (s"DATA:$data", l) =>
        b ++= data grouped 2 map (b => java.lang.Byte.parseByte(b, 16))
      case (s"RES:$size", l) =>
        b addRes java.lang.Integer.parseUnsignedInt(size, 16)
      case (_, l) => sys.error(s"error on line $l")
    }

    b.tof

class TOF(val segments: Seq[TOF.Segment]):
  def serialize: String =
    val buf = new StringBuilder

    buf ++= "TOF v1\n"

    for s <- segments do
      buf ++= s"SEGMENT:${s.name},${s.org.toHexString}\n"

      s.chunks foreach {
        case TOF.DataChunk(data) => buf ++= s"DATA:${data map (b => f"$b%02x") mkString}\n"
        case TOF.ResChunk(size)  => buf ++= s"RES:${size.toHexString}\n"
        case c                   => sys.error(s"can't serialize $c")
      }

    buf.toString
