package io.github.edadma.sfs

import Constants.*

/** The 256-byte payload that ends every journal transaction.
  *
  * Layout (first 256 bytes of the commit block; rest of the 4 KiB block
  * is zero):
  * {{{
  *   0       4     magic         (0x53465343 "SFSC")
  *   4       4     sequence
  *   8       8     commit_time   (Unix seconds)
  *   16      4     crc32         (over the entire transaction: descriptor
  *                                 block(s) + metadata blocks + this block,
  *                                 minus the crc32 field itself)
  *   20      236   reserved      (zeroed)
  * }}}
  *
  * The transaction-wide CRC is what makes write-ahead logging atomic: a
  * crash that interrupts the descriptor or metadata writes leaves the
  * commit block's CRC mismatched, so recovery treats the txn as never
  * committed. The CRC is *not* computed by this codec — that's a Phase 13
  * journal-driver concern, since it spans many blocks. This codec just
  * (de)serializes the 256-byte payload, including the stored CRC value.
  */
final case class CommitBlock(
    sequence: Int,
    commitTime: Long,
    crc32: Int,
)

object CommitBlock:

  val PayloadSize: Int = 256

  /** Offset of the CRC field. */
  val CrcOff: Int = 16

  /** Bytes covered by the CRC at the *start* of the commit block payload
    * — i.e. magic + sequence + commit_time. The full transaction CRC also
    * covers everything before this commit block, plus the bytes of this
    * block *after* the CRC field. */
  val PreCrcCoverage: Int = CrcOff

  /** Pack the commit-block payload. The caller is responsible for having
    * already computed the transaction-wide CRC; it's stored verbatim. */
  def pack(c: CommitBlock, buf: Array[Byte], off: Int): Unit =
    Le.putU32(buf, off + 0, MagicCommit)
    Le.putU32(buf, off + 4, c.sequence)
    Le.putU64(buf, off + 8, c.commitTime)
    Le.putU32(buf, off + CrcOff, c.crc32)
    Le.zero(buf, off + 20, PayloadSize - 20)

  /** Decode the commit-block payload. Verifies magic only — CRC must be
    * verified by the caller against an independently computed transaction
    * checksum. */
  def unpack(buf: Array[Byte], off: Int): CommitBlock =
    val gotMagic = Le.u32(buf, off + 0)
    if gotMagic != MagicCommit then
      throw new SfsCorruptError(
        f"commit block magic mismatch: expected 0x$MagicCommit%08x, got 0x$gotMagic%08x",
      )
    CommitBlock(
      sequence = Le.u32(buf, off + 4),
      commitTime = Le.u64(buf, off + 8),
      crc32 = Le.u32(buf, off + CrcOff),
    )
