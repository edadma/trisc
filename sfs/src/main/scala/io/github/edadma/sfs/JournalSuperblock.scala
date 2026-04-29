package io.github.edadma.sfs

import Constants.*

/** The journal region's own superblock — describes the circular log that
  * lives in blocks 1..(block_count) of the journal region.
  *
  * Layout:
  * {{{
  *   0       4     magic        (0x5346534A "SFSJ")
  *   4       4     version
  *   8       4     block_count  (journal blocks excluding this SB)
  *   12      4     head         (block offset of first valid transaction)
  *   16      4     tail         (block offset of next write position)
  *   20      4     sequence     (monotonically increasing transaction ID)
  *   24      16    fs_uuid      (must match the filesystem superblock UUID)
  *   40      4     crc32        (over bytes 0..39)
  *   44      212   reserved     (zeroed)
  * }}}
  *
  * `head == tail` means an empty log; the spec uses `sequence` to
  * disambiguate "freshly empty" from "wrapped exactly". `fs_uuid` is what
  * keeps a journal from being mounted against the wrong volume.
  */
final case class JournalSuperblock(
    version: Int,
    blockCount: Int,
    head: Int,
    tail: Int,
    sequence: Int,
    fsUuid: IndexedSeq[Byte],
):
  require(
    fsUuid.length == JournalSuperblock.UuidSize,
    s"fs_uuid must be ${JournalSuperblock.UuidSize} bytes",
  )

object JournalSuperblock:

  val PayloadSize: Int = 256
  val CrcCoverage: Int = 40
  val CrcOff: Int = 40
  val UuidSize: Int = 16

  def pack(s: JournalSuperblock, buf: Array[Byte], off: Int): Unit =
    Le.putU32(buf, off + 0, MagicJournalSuperblock)
    Le.putU32(buf, off + 4, s.version)
    Le.putU32(buf, off + 8, s.blockCount)
    Le.putU32(buf, off + 12, s.head)
    Le.putU32(buf, off + 16, s.tail)
    Le.putU32(buf, off + 20, s.sequence)
    Le.putBytes(buf, off + 24, s.fsUuid.toArray)
    Le.putU32(buf, off + CrcOff, 0)
    val crc = Crc32.compute(buf, off, CrcCoverage)
    Le.putU32(buf, off + CrcOff, crc)
    Le.zero(buf, off + 44, PayloadSize - 44) // reserved

  def unpack(buf: Array[Byte], off: Int): JournalSuperblock =
    val gotMagic = Le.u32(buf, off + 0)
    if gotMagic != MagicJournalSuperblock then
      throw new SfsCorruptError(
        f"journal SB magic mismatch: expected 0x$MagicJournalSuperblock%08x, got 0x$gotMagic%08x",
      )
    val storedCrc = Le.u32(buf, off + CrcOff)
    Le.putU32(buf, off + CrcOff, 0)
    val computedCrc = Crc32.compute(buf, off, CrcCoverage)
    Le.putU32(buf, off + CrcOff, storedCrc)
    if storedCrc != computedCrc then
      throw new SfsCorruptError(
        f"journal SB CRC mismatch: stored 0x$storedCrc%08x, computed 0x$computedCrc%08x",
      )
    JournalSuperblock(
      version = Le.u32(buf, off + 4),
      blockCount = Le.u32(buf, off + 8),
      head = Le.u32(buf, off + 12),
      tail = Le.u32(buf, off + 16),
      sequence = Le.u32(buf, off + 20),
      fsUuid = Le.bytes(buf, off + 24, UuidSize).toIndexedSeq,
    )
