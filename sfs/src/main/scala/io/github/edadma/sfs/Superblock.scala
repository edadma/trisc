package io.github.edadma.sfs

import Constants.*
import java.nio.charset.StandardCharsets.UTF_8

/** The 256-byte filesystem superblock — single source of truth for the
  * geometry, region layout, and live state of the filesystem.
  *
  * Two copies live on disk: at block 0 (primary) and block 1 (backup).
  * Both copies have a CRC32 over bytes 0..127. The remaining 128 bytes of
  * the 256-byte payload are reserved for future fields; the rest of the
  * 4 KiB block is zero padding.
  *
  * See SPEC.md → On-Disk Structures → Superblock for the field-by-field
  * layout this codec implements.
  */
final case class Superblock(
    versionMajor: Int,
    versionMinor: Int,
    fsState: Int,
    totalBlocks: Int,
    freeBlocks: Int,
    totalInodes: Int,
    freeInodes: Int,
    blockBitmapStart: Int,
    blockBitmapLen: Int,
    inodeBitmapStart: Int,
    inodeBitmapLen: Int,
    inodeTableStart: Int,
    inodeTableLen: Int,
    journalStart: Int,
    journalLen: Int,
    dataStart: Int,
    rootInode: Int,
    hashAlgorithm: Int,
    formatTime: Long,
    lastMountTime: Long,
    lastWriteTime: Long,
    uuid: IndexedSeq[Byte],
    volumeName: String,
):
  require(uuid.length == Superblock.UuidSize, s"uuid must be ${Superblock.UuidSize} bytes")
  require(
    fsState == FsClean || fsState == FsDirty || fsState == FsError,
    s"fsState $fsState not one of {clean=$FsClean, dirty=$FsDirty, error=$FsError}",
  )
  require(versionMajor >= 0 && versionMajor <= 0xffff, s"versionMajor out of u16 range")
  require(versionMinor >= 0 && versionMinor <= 0xffff, s"versionMinor out of u16 range")
  require(
    volumeName.getBytes(UTF_8).length <= Superblock.VolumeNameMax,
    s"volumeName too long for ${Superblock.VolumeNameMax}-byte usable region",
  )

object Superblock:

  /** Bytes covered by the CRC and considered "meaningful" by this codec. */
  val PayloadSize: Int = 256

  /** Bytes covered by the CRC field (0..127 inclusive). */
  val CrcCoverage: Int = 128

  /** Offset of the CRC32 field within the payload. */
  val CrcOff: Int = 128

  /** UUID field width. */
  val UuidSize: Int = 16

  /** Volume name field width on disk (15 usable + 1 NUL). */
  val VolumeNameField: Int = 16

  /** Maximum encodable volume name length. */
  val VolumeNameMax: Int = VolumeNameField - 1

  /** Pack a [[Superblock]] into 256 bytes at `off`. The CRC32 field is
    * computed over bytes 0..127 of the freshly-written payload. The full
    * disk image is the 256-byte payload zero-padded out to one block; this
    * codec deliberately doesn't pad — the caller chooses the destination
    * buffer size. */
  def pack(s: Superblock, buf: Array[Byte], off: Int): Unit =
    Le.putU32(buf, off + 0, MagicSuperblock)
    Le.putU16(buf, off + 4, s.versionMajor)
    Le.putU16(buf, off + 6, s.versionMinor)
    Le.putU16(buf, off + 8, BlockSize)
    Le.putU16(buf, off + 10, s.fsState)
    Le.putU32(buf, off + 12, s.totalBlocks)
    Le.putU32(buf, off + 16, s.freeBlocks)
    Le.putU32(buf, off + 20, s.totalInodes)
    Le.putU32(buf, off + 24, s.freeInodes)
    Le.putU32(buf, off + 28, s.blockBitmapStart)
    Le.putU32(buf, off + 32, s.blockBitmapLen)
    Le.putU32(buf, off + 36, s.inodeBitmapStart)
    Le.putU32(buf, off + 40, s.inodeBitmapLen)
    Le.putU32(buf, off + 44, s.inodeTableStart)
    Le.putU32(buf, off + 48, s.inodeTableLen)
    Le.putU32(buf, off + 52, s.journalStart)
    Le.putU32(buf, off + 56, s.journalLen)
    Le.putU32(buf, off + 60, s.dataStart)
    Le.putU32(buf, off + 64, s.rootInode)
    Le.putU8(buf, off + 68, s.hashAlgorithm)
    Le.zero(buf, off + 69, 3) // reserved
    Le.putU64(buf, off + 72, s.formatTime)
    Le.putU64(buf, off + 80, s.lastMountTime)
    Le.putU64(buf, off + 88, s.lastWriteTime)
    Le.putBytes(buf, off + 96, s.uuid.toArray)
    writeVolumeName(buf, off + 112, s.volumeName)
    Le.putU32(buf, off + CrcOff, 0) // zero before CRC
    val crc = Crc32.compute(buf, off, CrcCoverage)
    Le.putU32(buf, off + CrcOff, crc)
    Le.zero(buf, off + 132, PayloadSize - 132) // reserved trailing payload

  def unpack(buf: Array[Byte], off: Int): Superblock =
    val gotMagic = Le.u32(buf, off + 0)
    if gotMagic != MagicSuperblock then
      throw new SfsCorruptError(
        f"superblock magic mismatch: expected 0x$MagicSuperblock%08x, got 0x$gotMagic%08x",
      )
    val storedCrc = Le.u32(buf, off + CrcOff)
    Le.putU32(buf, off + CrcOff, 0)
    val computedCrc = Crc32.compute(buf, off, CrcCoverage)
    Le.putU32(buf, off + CrcOff, storedCrc)
    if storedCrc != computedCrc then
      throw new SfsCorruptError(
        f"superblock CRC mismatch: stored 0x$storedCrc%08x, computed 0x$computedCrc%08x",
      )
    val gotBlockSize = Le.u16(buf, off + 8)
    if gotBlockSize != BlockSize then
      throw new SfsCorruptError(
        s"superblock block_size mismatch: expected $BlockSize, got $gotBlockSize",
      )
    Superblock(
      versionMajor = Le.u16(buf, off + 4),
      versionMinor = Le.u16(buf, off + 6),
      fsState = Le.u16(buf, off + 10),
      totalBlocks = Le.u32(buf, off + 12),
      freeBlocks = Le.u32(buf, off + 16),
      totalInodes = Le.u32(buf, off + 20),
      freeInodes = Le.u32(buf, off + 24),
      blockBitmapStart = Le.u32(buf, off + 28),
      blockBitmapLen = Le.u32(buf, off + 32),
      inodeBitmapStart = Le.u32(buf, off + 36),
      inodeBitmapLen = Le.u32(buf, off + 40),
      inodeTableStart = Le.u32(buf, off + 44),
      inodeTableLen = Le.u32(buf, off + 48),
      journalStart = Le.u32(buf, off + 52),
      journalLen = Le.u32(buf, off + 56),
      dataStart = Le.u32(buf, off + 60),
      rootInode = Le.u32(buf, off + 64),
      hashAlgorithm = Le.u8(buf, off + 68),
      formatTime = Le.u64(buf, off + 72),
      lastMountTime = Le.u64(buf, off + 80),
      lastWriteTime = Le.u64(buf, off + 88),
      uuid = Le.bytes(buf, off + 96, UuidSize).toIndexedSeq,
      volumeName = readVolumeName(buf, off + 112),
    )

  private def writeVolumeName(buf: Array[Byte], off: Int, name: String): Unit =
    val nameBytes = name.getBytes(UTF_8)
    Le.zero(buf, off, VolumeNameField)
    Le.putBytes(buf, off, nameBytes)
    // last byte stays zero for NUL terminator unless name is exactly VolumeNameMax bytes,
    // in which case the require(...) above prevents it from extending into the NUL slot.

  private def readVolumeName(buf: Array[Byte], off: Int): String =
    var len = 0
    while len < VolumeNameField && buf(off + len) != 0.toByte do len += 1
    new String(buf, off, len, UTF_8)
