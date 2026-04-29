package io.github.edadma.sfs

/** Little-endian byte read/write helpers over `Array[Byte]`.
  *
  * Every multi-byte field on disk in SFS is little-endian. Centralizing the
  * byte fiddling here means encoders and decoders stay declarative, and keeps
  * this module the only place where signedness and shifts can go wrong.
  *
  * Naming convention: `u8`/`u16`/`u32`/`u64` read unsigned values (returned in
  * the smallest signed Scala type that can hold them — `Int` for u8/u16/u32,
  * `Long` for u64). Writers accept whatever fits and ignore the upper bits.
  */
object Le:

  // ---- reads ------------------------------------------------------------

  def u8(buf: Array[Byte], off: Int): Int =
    buf(off) & 0xff

  def u16(buf: Array[Byte], off: Int): Int =
    (buf(off) & 0xff) | ((buf(off + 1) & 0xff) << 8)

  def u32(buf: Array[Byte], off: Int): Int =
    (buf(off) & 0xff) |
      ((buf(off + 1) & 0xff) << 8) |
      ((buf(off + 2) & 0xff) << 16) |
      ((buf(off + 3) & 0xff) << 24)

  /** u32 widened to a Long with no sign extension. Use this when the value
    * is logically unsigned and may be compared against block counts etc. */
  def u32AsLong(buf: Array[Byte], off: Int): Long =
    u32(buf, off).toLong & 0xffffffffL

  def u64(buf: Array[Byte], off: Int): Long =
    u32AsLong(buf, off) | (u32AsLong(buf, off + 4) << 32)

  /** Read `len` bytes into a freshly-allocated `Array[Byte]`. */
  def bytes(buf: Array[Byte], off: Int, len: Int): Array[Byte] =
    val out = new Array[Byte](len)
    System.arraycopy(buf, off, out, 0, len)
    out

  // ---- writes -----------------------------------------------------------

  def putU8(buf: Array[Byte], off: Int, v: Int): Unit =
    buf(off) = v.toByte

  def putU16(buf: Array[Byte], off: Int, v: Int): Unit =
    buf(off) = v.toByte
    buf(off + 1) = (v >>> 8).toByte

  def putU32(buf: Array[Byte], off: Int, v: Int): Unit =
    buf(off) = v.toByte
    buf(off + 1) = (v >>> 8).toByte
    buf(off + 2) = (v >>> 16).toByte
    buf(off + 3) = (v >>> 24).toByte

  def putU64(buf: Array[Byte], off: Int, v: Long): Unit =
    putU32(buf, off, v.toInt)
    putU32(buf, off + 4, (v >>> 32).toInt)

  /** Copy `src` into `buf` at `off`. Fails fast if there isn't room. */
  def putBytes(buf: Array[Byte], off: Int, src: Array[Byte]): Unit =
    System.arraycopy(src, 0, buf, off, src.length)

  /** Zero `len` bytes starting at `off`. */
  def zero(buf: Array[Byte], off: Int, len: Int): Unit =
    java.util.Arrays.fill(buf, off, off + len, 0.toByte)
