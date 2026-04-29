package io.github.edadma.sfs

import Constants.*
import java.nio.charset.StandardCharsets.UTF_8

/** The 256-byte on-disk inode.
  *
  * Bytes 0..207 are payload (CRC-protected); bytes 212..255 are reserved.
  *
  * The 128-byte union at offset 64 is reinterpreted by type:
  *  - `INLINE_SYMLINK` flag *clear* → 16 inline [[Extent]] records.
  *  - `INLINE_SYMLINK` flag *set*   → up to 127 bytes of UTF-8 symlink
  *    target plus a NUL terminator.
  *
  * Inodes for regular files, directories, long symlinks, and the bad-blocks
  * file all use the Extents form. Short symlinks (≤127 bytes) get the
  * InlineSymlink form, which avoids allocating any data block at all.
  */
enum InodeBody:
  case Extents(extents: IndexedSeq[Extent])
  case InlineSymlink(target: String)

object InodeBody:
  /** Bytes the union occupies on disk. */
  val UnionSize: Int = 128

  /** Sixteen empty extents — the canonical "no inline data" form. */
  val EmptyExtents: Extents = Extents(IndexedSeq.fill(InlineExtents)(Extent.Empty))

final case class Inode(
    mode: Int, // u16
    linkCount: Int, // u16
    uid: Int,
    gid: Int,
    flags: Int,
    size: Long,
    blockCount: Int, // 512-byte units (st_blocks)
    generation: Int,
    atimeSec: Int,
    atimeNsec: Int,
    mtimeSec: Int,
    mtimeNsec: Int,
    ctimeSec: Int,
    ctimeNsec: Int,
    crtimeSec: Int,
    crtimeNsec: Int,
    body: InodeBody,
    indirect1: Int,
    indirect2: Int,
    indirect3: Int,
    xattrBlock: Int,
):
  require(mode >= 0 && mode <= 0xffff, s"mode $mode out of u16 range")
  require(linkCount >= 0 && linkCount <= 0xffff, s"linkCount $linkCount out of u16 range")
  require(size >= 0, s"size $size must be non-negative")
  require(blockCount >= 0, s"blockCount $blockCount must be non-negative")
  body match
    case InodeBody.Extents(xs) =>
      require(xs.length == InlineExtents, s"inline extents must be exactly $InlineExtents")
      require(
        (flags & InodeFlagInlineSymlink) == 0,
        "INLINE_SYMLINK flag must be clear when body is Extents",
      )
    case InodeBody.InlineSymlink(target) =>
      val bytes = target.getBytes(UTF_8)
      require(
        bytes.length <= InlineSymlinkMax,
        s"inline symlink target too long: ${bytes.length} > $InlineSymlinkMax",
      )
      require(
        (flags & InodeFlagInlineSymlink) != 0,
        "INLINE_SYMLINK flag must be set when body is InlineSymlink",
      )

object Inode:

  /** Total on-disk size. */
  val Size: Int = InodeSize

  /** Bytes covered by the CRC (0..207 inclusive). */
  val CrcCoverage: Int = 208

  /** Offset of the CRC field. */
  val CrcOff: Int = 208

  /** Offset where the 128-byte union starts. */
  val UnionOff: Int = 64

  def pack(i: Inode, buf: Array[Byte], off: Int): Unit =
    Le.putU16(buf, off + 0, i.mode)
    Le.putU16(buf, off + 2, i.linkCount)
    Le.putU32(buf, off + 4, i.uid)
    Le.putU32(buf, off + 8, i.gid)
    Le.putU32(buf, off + 12, i.flags)
    Le.putU64(buf, off + 16, i.size)
    Le.putU32(buf, off + 24, i.blockCount)
    Le.putU32(buf, off + 28, i.generation)
    Le.putU32(buf, off + 32, i.atimeSec)
    Le.putU32(buf, off + 36, i.atimeNsec)
    Le.putU32(buf, off + 40, i.mtimeSec)
    Le.putU32(buf, off + 44, i.mtimeNsec)
    Le.putU32(buf, off + 48, i.ctimeSec)
    Le.putU32(buf, off + 52, i.ctimeNsec)
    Le.putU32(buf, off + 56, i.crtimeSec)
    Le.putU32(buf, off + 60, i.crtimeNsec)
    packBody(i.body, buf, off + UnionOff)
    Le.putU32(buf, off + 192, i.indirect1)
    Le.putU32(buf, off + 196, i.indirect2)
    Le.putU32(buf, off + 200, i.indirect3)
    Le.putU32(buf, off + 204, i.xattrBlock)
    Le.putU32(buf, off + CrcOff, 0)
    val crc = Crc32.compute(buf, off, CrcCoverage)
    Le.putU32(buf, off + CrcOff, crc)
    Le.zero(buf, off + 212, Size - 212) // reserved trailing payload

  def unpack(buf: Array[Byte], off: Int): Inode =
    val storedCrc = Le.u32(buf, off + CrcOff)
    Le.putU32(buf, off + CrcOff, 0)
    val computedCrc = Crc32.compute(buf, off, CrcCoverage)
    Le.putU32(buf, off + CrcOff, storedCrc)
    if storedCrc != computedCrc then
      throw new SfsCorruptError(
        f"inode CRC mismatch: stored 0x$storedCrc%08x, computed 0x$computedCrc%08x",
      )
    val flags = Le.u32(buf, off + 12)
    val isInlineSymlink = (flags & InodeFlagInlineSymlink) != 0
    val body =
      if isInlineSymlink then unpackInlineSymlink(buf, off + UnionOff)
      else unpackExtents(buf, off + UnionOff)
    Inode(
      mode = Le.u16(buf, off + 0),
      linkCount = Le.u16(buf, off + 2),
      uid = Le.u32(buf, off + 4),
      gid = Le.u32(buf, off + 8),
      flags = flags,
      size = Le.u64(buf, off + 16),
      blockCount = Le.u32(buf, off + 24),
      generation = Le.u32(buf, off + 28),
      atimeSec = Le.u32(buf, off + 32),
      atimeNsec = Le.u32(buf, off + 36),
      mtimeSec = Le.u32(buf, off + 40),
      mtimeNsec = Le.u32(buf, off + 44),
      ctimeSec = Le.u32(buf, off + 48),
      ctimeNsec = Le.u32(buf, off + 52),
      crtimeSec = Le.u32(buf, off + 56),
      crtimeNsec = Le.u32(buf, off + 60),
      body = body,
      indirect1 = Le.u32(buf, off + 192),
      indirect2 = Le.u32(buf, off + 196),
      indirect3 = Le.u32(buf, off + 200),
      xattrBlock = Le.u32(buf, off + 204),
    )

  private def packBody(body: InodeBody, buf: Array[Byte], off: Int): Unit =
    body match
      case InodeBody.Extents(xs) =>
        var i = 0
        while i < InlineExtents do
          Extent.pack(xs(i), buf, off + i * ExtentSize)
          i += 1
      case InodeBody.InlineSymlink(target) =>
        val bytes = target.getBytes(UTF_8)
        Le.zero(buf, off, InodeBody.UnionSize)
        Le.putBytes(buf, off, bytes)
        // trailing byte stays 0 → NUL terminator

  private def unpackExtents(buf: Array[Byte], off: Int): InodeBody.Extents =
    val xs = new Array[Extent](InlineExtents)
    var i = 0
    while i < InlineExtents do
      xs(i) = Extent.unpack(buf, off + i * ExtentSize)
      i += 1
    InodeBody.Extents(xs.toIndexedSeq)

  private def unpackInlineSymlink(buf: Array[Byte], off: Int): InodeBody.InlineSymlink =
    var len = 0
    while len < InodeBody.UnionSize && buf(off + len) != 0.toByte do len += 1
    InodeBody.InlineSymlink(new String(buf, off, len, UTF_8))
