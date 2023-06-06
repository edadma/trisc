package io.github.edadma.trisc

import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object TOF:
  trait Chunk

  case class DataChunk(data: Seq[Byte]) extends Chunk
  case class ResChunk(size: Long) extends Chunk

  case class Segment(name: String, org: Long, chunks: Seq[Chunk])

  class TOFBuilder:
    val segments = new mutable.LinkedHashMap[String, (Long, ListBuffer[(String, Int | ArrayBuffer[Byte])])]
    def tof: TOF = null

  def builder: TOFBuilder = new TOFBuilder

  def deserialize(tof: String): Seq[Segment] =
    val lines = scala.io.Source.fromString(tof).getLines
    val versions = immutable.TreeSet("1")
    var v = 0
    val segments = new ListBuffer[Segment]
    var segment: Segment = null

    lines.zipWithIndex map ((s, idx) => (s.trim, idx + 1)) foreach {
      case (s, _) if s.isEmpty             =>
      case (s"TOF v$n", l) if !versions(n) => sys.error(s"TOF version must be one of [$versions] on line $l")
      case (s"TOF v$n", _) if v == 0       => v = n.toInt
      case (_, l) if v == 0                => sys.error(s"missing magic on line $l")
      case (s"SEGMENT:$name,$org", l) =>
        segment = Segment(name)
        segment.org = java.lang.Long.parseUnsignedLong(org, 16)
        segments += segment
      case (s"DATA:$data", l) =>
        segment ++= data grouped 2 map (b => java.lang.Byte.parseByte(b, 16))
      case (s"RES:$size", l) =>
        segment.chunks += ResChunk(java.lang.Long.parseUnsignedLong(size, 16))
      case (_, l) => sys.error(s"error on line $l")
    }

    segments.toSeq

class TOF(val segments: Seq[Segment]):
  def serialize: String =
    val buf = new StringBuilder

    buf ++= "TOF v1\n"

    for s <- segments do
      buf ++= s"SEGMENT:${s.name},${s.org.toHexString}\n"

      s.chunks foreach {
        case DataChunk(data) => buf ++= s"DATA:${data map (b => f"$b%02x") mkString}\n"
        case ResChunk(size)  => buf ++= s"RES:${size.toHexString}\n"
        case c               => sys.error(s"can't serialize $c")
      }

    buf.toString
