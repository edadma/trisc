package io.github.edadma.sfs

import Constants.*
import java.nio.charset.StandardCharsets.UTF_8

/** A single extended-attribute on disk: a UTF-8 name (1..255 bytes) and
  * an opaque value (0..[[XattrBlock.MaxValueLen]] bytes).
  *
  * Names use `namespace.suffix` form by convention (`user.foo`,
  * `security.selinux`, …). The format here doesn't enforce that — it
  * just stores arbitrary bytes — but it's useful to know that real
  * callers will partition the namespace this way.
  */
final case class XattrEntry(name: String, value: Array[Byte]):
  require(
    name.getBytes(UTF_8).length >= 1 && name.getBytes(UTF_8).length <= XattrBlock.MaxNameLen,
    s"xattr name length ${name.getBytes(UTF_8).length} out of range [1, ${XattrBlock.MaxNameLen}]",
  )
  require(
    value.length <= XattrBlock.MaxValueLen,
    s"xattr value length ${value.length} > ${XattrBlock.MaxValueLen}",
  )

  /** Bytes consumed on disk: 4-byte entry header + name + value. */
  def recLen: Int = 4 + name.getBytes(UTF_8).length + value.length

object XattrEntry:

  /** Per-entry header bytes (name_len u8 + reserved u8 + value_len u16). */
  val HeaderSize: Int = 4

/** A 4 KiB block holding a list of [[XattrEntry]]s.
  *
  * Layout:
  * {{{
  *   0      4   magic       0x53465358 ("SFSX")
  *   4      4   inode_num   owning inode (block-swap protection)
  *   8      2   entry_count
  *   10     2   reserved    (zeroed)
  *   12   ...   entries     end-to-end, each (name_len, _, value_len, name, value)
  *   -4     4   crc32       over bytes 0..(BlockSize - 4)
  * }}}
  *
  * Bytes between the last entry and the CRC are zeroed. The block-wide
  * CRC + owner-inode binding mirrors [[DirTail]] — a structurally valid
  * xattr block from a *different* inode will fail verification.
  */
object XattrBlock:

  /** Magic at offset 0: ASCII "SFSX". */
  val Magic: Int = 0x53465358

  val MagicOff: Int = 0
  val OwnerOff: Int = 4
  val CountOff: Int = 8
  val ReservedOff: Int = 10

  /** Bytes for the fixed header at the start of the block. */
  val HeaderSize: Int = 12

  /** Bytes available for entries (between header and trailing CRC). */
  val UsableSize: Int = BlockSize - HeaderSize - 4

  val CrcOff: Int = BlockSize - 4

  /** Maximum length of a single attribute name, in UTF-8 bytes. */
  val MaxNameLen: Int = 255

  /** Maximum length of a single attribute value, in bytes. Capped by
    * `UsableSize - HeaderSize - per-entry header (4) - 1-byte name`. */
  val MaxValueLen: Int = UsableSize - XattrEntry.HeaderSize - 1

  // ---- pack -----------------------------------------------------------

  /** Pack `entries` into `buf`. The entries' total `recLen` must fit in
    * [[UsableSize]] — caller (`XattrOps`) is responsible for refusing
    * larger sets up front. */
  def pack(entries: Seq[XattrEntry], ownerInode: Int, buf: Array[Byte]): Unit =
    require(buf.length == BlockSize, s"xattr block must be $BlockSize bytes")
    val total = entries.foldLeft(0)(_ + _.recLen)
    require(
      total <= UsableSize,
      s"xattr entries occupy $total bytes, exceeds usable $UsableSize",
    )
    require(
      entries.length <= 0xffff,
      s"xattr entry count ${entries.length} exceeds u16 range",
    )
    Le.zero(buf, 0, BlockSize)
    Le.putU32(buf, MagicOff, Magic)
    Le.putU32(buf, OwnerOff, ownerInode)
    Le.putU16(buf, CountOff, entries.length)
    Le.putU16(buf, ReservedOff, 0)
    var off = HeaderSize
    var i = 0
    while i < entries.length do
      val e = entries(i)
      val nameBytes = e.name.getBytes(UTF_8)
      Le.putU8(buf, off, nameBytes.length)
      Le.putU8(buf, off + 1, 0)
      Le.putU16(buf, off + 2, e.value.length)
      Le.putBytes(buf, off + 4, nameBytes)
      Le.putBytes(buf, off + 4 + nameBytes.length, e.value)
      off += e.recLen
      i += 1
    Le.putU32(buf, CrcOff, 0)
    val crc = Crc32.compute(buf, 0, BlockSize - 4)
    Le.putU32(buf, CrcOff, crc)

  // ---- unpack ---------------------------------------------------------

  /** Verify magic, owner-inode, CRC, then walk entries. Throws
    * [[SfsCorruptError]] on any inconsistency. */
  def unpack(buf: Array[Byte], expectedInode: Int): IndexedSeq[XattrEntry] =
    require(buf.length == BlockSize, s"xattr block must be $BlockSize bytes")
    val gotMagic = Le.u32(buf, MagicOff)
    if gotMagic != Magic then
      throw new SfsCorruptError(
        f"xattr block magic mismatch: expected 0x$Magic%08x, got 0x$gotMagic%08x",
      )
    val gotOwner = Le.u32(buf, OwnerOff)
    if gotOwner != expectedInode then
      throw new SfsCorruptError(
        s"xattr block owner mismatch: expected $expectedInode, got $gotOwner",
      )
    val storedCrc = Le.u32(buf, CrcOff)
    Le.putU32(buf, CrcOff, 0)
    val computedCrc = Crc32.compute(buf, 0, BlockSize - 4)
    Le.putU32(buf, CrcOff, storedCrc)
    if storedCrc != computedCrc then
      throw new SfsCorruptError(
        f"xattr block CRC mismatch: stored 0x$storedCrc%08x, computed 0x$computedCrc%08x",
      )

    val count = Le.u16(buf, CountOff)
    val out = IndexedSeq.newBuilder[XattrEntry]
    var off = HeaderSize
    var i = 0
    while i < count do
      if off + XattrEntry.HeaderSize > BlockSize - 4 then
        throw new SfsCorruptError(s"xattr entry $i header overruns usable region at $off")
      val nameLen = Le.u8(buf, off)
      val valueLen = Le.u16(buf, off + 2)
      if nameLen < 1 || nameLen > MaxNameLen then
        throw new SfsCorruptError(s"xattr entry $i name_len $nameLen out of range")
      val end = off + 4 + nameLen + valueLen
      if end > BlockSize - 4 then
        throw new SfsCorruptError(
          s"xattr entry $i ends at $end, beyond usable region (${BlockSize - 4})",
        )
      val nameBytes = Le.bytes(buf, off + 4, nameLen)
      val value = Le.bytes(buf, off + 4 + nameLen, valueLen)
      out += XattrEntry(new String(nameBytes, UTF_8), value)
      off = end
      i += 1
    out.result()
