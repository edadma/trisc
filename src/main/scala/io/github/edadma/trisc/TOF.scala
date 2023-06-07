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
    var current: ListBuffer[(String, Int | ArrayBuffer[Byte])]
    var length = 0L

    def tof: TOF = null

    def addSegment(name: String, org: Long): Boolean =
      if segments contains name then false
      else
        current = new ListBuffer
        segments(name) = (org, current)
        true

    def +=(b: Byte): Unit =
      if current.isEmpty || current.last._1 != "data" then current += ("data" -> new ArrayBuffer[Byte])
      current.last._2.asInstanceOf[ArrayBuffer[Byte]] += b
      length += 1

    def ++=(bs: IterableOnce[Byte]): Unit = bs.iterator foreach (b => +=(b))

    def addRes(size: Int): Unit = current += ("res" -> size)

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
      case (s"SEGMENT:$name,$org", l)      => b.addSegment(name, java.lang.Long.parseUnsignedLong(org, 16))
      case (s"DATA:$data", l) =>
        b ++= data grouped 2 map (b => java.lang.Byte.parseByte(b, 16))
      case (s"RES:$size", l) =>
        b addRes java.lang.Integer.parseUnsignedInt(size, 16)
      case (_, l) => sys.error(s"error on line $l")
    }

    b.tof

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
