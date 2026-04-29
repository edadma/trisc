package io.github.edadma.sfs

import Constants.*
import java.nio.charset.StandardCharsets.UTF_8

/** A single variable-length directory leaf entry.
  *
  * On-disk layout:
  * {{{
  *   0       4     inode
  *   4       2     rec_len     (this record's total bytes including padding)
  *   6       1     name_len    (actual name bytes, max 255)
  *   7       1     file_type   (0=unknown, 1=regular, 2=dir, 3=symlink, 4=other)
  *   8       n     name        (NOT null-terminated)
  *   8+n     p     padding     (to next 4-byte boundary; may be larger if
  *                              this entry is acting as a tombstone)
  * }}}
  *
  * `recLen` may exceed `8 + nameLen + minimal padding`: that excess is
  * tombstone space from a deleted neighboring entry. fsck and directory
  * iteration rely on `recLen` (not `nameLen`) to advance.
  */
final case class DirEntry(
    inode: Int,
    fileType: Int,
    name: String,
    recLen: Int,
):
  val utf8: Array[Byte] = name.getBytes(UTF_8)
  val nameLen: Int = utf8.length

  require(inode >= 0, s"inode must be non-negative, got $inode")
  require(nameLen <= NameMax, s"name too long: $nameLen > $NameMax")
  require(
    recLen >= DirEntry.minRecLen(nameLen),
    s"recLen $recLen < minimum ${DirEntry.minRecLen(nameLen)} for name length $nameLen",
  )
  require(recLen % 4 == 0, s"recLen $recLen not 4-aligned")
  require(fileType >= 0 && fileType <= 0xff, s"fileType $fileType out of range")

object DirEntry:

  /** Minimum on-disk record length for an entry whose name is `nameLen` bytes,
    * rounded up to the next multiple of 4. */
  def minRecLen(nameLen: Int): Int = (8 + nameLen + 3) & ~3

  /** Convenience constructor that uses the minimum legal `recLen`. */
  def apply(inode: Int, fileType: Int, name: String): DirEntry =
    DirEntry(inode, fileType, name, minRecLen(name.getBytes(UTF_8).length))

  // file_type tag values (mirroring the spec)
  val TypeUnknown: Int = 0
  val TypeRegular: Int = 1
  val TypeDirectory: Int = 2
  val TypeSymlink: Int = 3
  val TypeOther: Int = 4

  /** A 12-byte entry with `inode == 0` is the canonical tombstone — its
    * recLen is whatever was donated to it; pack uses this when filling out
    * an empty leaf. */
  def tombstone(recLen: Int): DirEntry =
    require(recLen >= 8 && recLen % 4 == 0, s"tombstone recLen $recLen invalid")
    DirEntry(0, 0, "", recLen)

  def pack(e: DirEntry, buf: Array[Byte], off: Int): Int =
    Le.putU32(buf, off, e.inode)
    Le.putU16(buf, off + 4, e.recLen)
    Le.putU8(buf, off + 6, e.nameLen)
    Le.putU8(buf, off + 7, e.fileType)
    Le.putBytes(buf, off + 8, e.utf8)
    Le.zero(buf, off + 8 + e.nameLen, e.recLen - 8 - e.nameLen)
    e.recLen

  def unpack(buf: Array[Byte], off: Int): DirEntry =
    val inode = Le.u32(buf, off)
    val recLen = Le.u16(buf, off + 4)
    val nameLen = Le.u8(buf, off + 6)
    val fileType = Le.u8(buf, off + 7)
    if recLen < minRecLen(nameLen) then
      throw new SfsCorruptError(
        s"directory entry at $off: recLen $recLen < minimum ${minRecLen(nameLen)}",
      )
    if recLen % 4 != 0 then
      throw new SfsCorruptError(s"directory entry at $off: recLen $recLen not 4-aligned")
    val name = new String(buf, off + 8, nameLen, UTF_8)
    DirEntry(inode, fileType, name, recLen)
