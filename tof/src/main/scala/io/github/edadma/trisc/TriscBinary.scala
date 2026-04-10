package io.github.edadma.trisc

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer

/** Compact loadable executable image for the guest loader (and fast ramdisk prefill).
  *
  * Layout (little-endian, v1):
  *   0..3   magic `TRB` + format byte 0x01
  *   4..7   u32 entry point (absolute)
  *   8..11  u32 record count
  *   then for each record: u32 org, u32 kind, u32 size, [size bytes if kind==PROGBITS]
  *   kind 0 = PROGBITS (payload follows), kind 1 = NOBITS (zero-fill, no payload)
  *
  * Produced from a fully linked executable [[TOF]] (same information as text TOF, without hex
  * expansion). The guest [[oskit.loader]] detects magic and loads; otherwise it parses text TOF.
  */
object TriscBinary:

  val Magic: Array[Byte] = Array('T'.toByte, 'R'.toByte, 'B'.toByte, 0x01)

  val KindProg: Int   = 0
  val KindNoBits: Int = 1

  private def putU32(out: ByteArrayOutputStream, v: Int): Unit =
    out.write(v & 0xff)
    out.write((v >> 8) & 0xff)
    out.write((v >> 16) & 0xff)
    out.write((v >> 24) & 0xff)

  /** Serialize a linked executable TOF. */
  def serialize(tof: TOF): Array[Byte] =
    if tof.tofType != TOFType.Executable then
      throw new IllegalArgumentException("TriscBinary.serialize requires TYPE:executable")
    if !tof.isFullyResolved then
      throw new IllegalArgumentException("TriscBinary.serialize requires fully resolved TOF (no relocs/externs)")
    val entry = tof.entryAddress.getOrElse:
      throw new IllegalArgumentException("TriscBinary.serialize requires ENTRY")

    enum Rec:
      case Prog(org: Long, data: Array[Byte])
      case Nobits(org: Long, size: Int)

    val recs = ArrayBuffer.empty[Rec]

    for seg <- tof.segments do
      var addr = seg.org
      for ch <- seg.chunks do
        ch match
          case TOF.DataChunk(data) =>
            val arr = data.toArray
            recs += Rec.Prog(addr, arr)
            addr += arr.length
          case TOF.ResChunk(size) =>
            val n = size.toInt
            if n.toLong != size then throw new IllegalArgumentException("TriscBinary: RES too large")
            recs += Rec.Nobits(addr, n)
            addr += size
          case _: TOF.CommentChunk =>

    val out = new ByteArrayOutputStream()
    out.write(Magic)
    putU32(out, (entry & 0xffffffffL).toInt)
    putU32(out, recs.length)
    for r <- recs do
      r match
        case Rec.Prog(org, data) =>
          putU32(out, (org & 0xffffffffL).toInt)
          putU32(out, KindProg)
          putU32(out, data.length)
          out.write(data)
        case Rec.Nobits(org, size) =>
          putU32(out, (org & 0xffffffffL).toInt)
          putU32(out, KindNoBits)
          putU32(out, size)

    out.toByteArray

  /** JVM-side load (tests / tools); mirrors guest `load_trb1_from_buf`. Returns entry PC. */
  def loadIntoMemory(image: Array[Byte], mem: Addressable): Long =
    if image.length < 12 then throw new IllegalArgumentException("TriscBinary: image too small")
    if image(0) != Magic(0) || image(1) != Magic(1) || image(2) != Magic(2) || image(3) != Magic(3) then
      throw new IllegalArgumentException("TriscBinary: bad magic")

    def u32(off: Int): Int =
      (image(off) & 0xff) | ((image(off + 1) & 0xff) << 8) | ((image(off + 2) & 0xff) << 16) | ((image(
        off + 3,
      ) & 0xff) << 24)

    val entry = u32(4).toLong & 0xffffffffL
    val nrec  = u32(8)
    var pos   = 12
    var i     = 0
    while i < nrec do
      if pos + 12 > image.length then throw new IllegalArgumentException("TriscBinary: truncated header")
      val org  = u32(pos).toLong & 0xffffffffL
      pos += 4
      val kind = u32(pos)
      pos += 4
      val sz   = u32(pos)
      pos += 4
      kind match
        case KindProg =>
          if pos + sz > image.length then throw new IllegalArgumentException("TriscBinary: truncated payload")
          var j = 0
          while j < sz do
            mem.writeByte(org + j, image(pos + j) & 0xffL)
            j += 1
          pos += sz
        case KindNoBits =>
          var j = 0L
          while j < sz do
            mem.writeByte(org + j, 0L)
            j += 1
        case _ => throw new IllegalArgumentException(s"TriscBinary: unknown kind $kind")
      i += 1
    if pos != image.length then throw new IllegalArgumentException("TriscBinary: trailing garbage")
    entry

end TriscBinary
