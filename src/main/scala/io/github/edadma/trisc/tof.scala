package io.github.edadma.trisc

import scala.collection.immutable

def serialize(segs: Seq[Segment]): String =
  val buf = new StringBuilder

  buf ++= "TOF v1\n"

  for s <- segs do
    buf ++= s"SEGMENT:${s.name},${s.org.toHexString}\n"
    s.chunks foreach {
      case DataChunk(data) => buf ++= s"DATA:${data map (b => f"$b%02x") mkString}\n"
      case ResChunk(size)  => buf ++= s"RES:${size.toHexString}\n"
      case c               => sys.error(s"can't serialize $c")
    }

  buf.toString

def deserialize(tof: String): Seq[Segment] =
  val lines = scala.io.Source.fromString(tof).getLines
  val versions = immutable.TreeSet("1")
  var v = 0
  var segment: Segment = null

  lines.zipWithIndex map ((s, idx) => (s.trim, idx + 1)) foreach {
    case (s, _) if s.isEmpty             =>
    case (s"TOF v$n", l) if !versions(n) => sys.error(s"TOF version must be one of [$versions] on line $l")
    case (s"TOF v$n", _) if v == 0       => v = n.toInt
    case (_, l) if v == 0                => sys.error(s"missing magic on line $l")
    case (s"SEGMENT:$name,$org", l) =>
      segment = Segment(name)
      segment.org = java.lang.Long.parseUnsignedLong(org, 16)
    case (s"DATA:$data", l) =>
      segment ++= data grouped 2 map (b => java.lang.Byte.parseByte(b, 16))
    case (s"RES:$size", l) =>
      segment.chunks += ResChunk(java.lang.Long.parseUnsignedLong(size, 16))
    case (_, l) => sys.error(s"error on line $l")
  }

  null
