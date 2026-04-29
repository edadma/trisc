package io.github.edadma.sfs

import Constants.*

/** A single 4 KiB transaction descriptor block — the first block of every
  * journal transaction.
  *
  * Layout (per block):
  * {{{
  *   0       4     magic        (0x53465354 "SFST")
  *   4       4     sequence
  *   8       4     block_count  (metadata blocks in the *whole* transaction)
  *   12      4     flags
  *   16      8×n   entries[]:
  *                   fs_block 4
  *                   flags    4   (ESCAPED=1)
  *   …
  * }}}
  *
  * Up to 510 entries fit in one descriptor block ((4096 − 16) / 8). A
  * transaction with more than 510 entries spans multiple descriptor blocks;
  * each descriptor block carries the same `sequence` and total `block_count`,
  * so a reader can stitch them together.
  *
  * Descriptor blocks have no CRC of their own — authentication is via the
  * commit block's CRC, which covers the entire transaction.
  */
final case class TxnEntry(fsBlock: Int, flags: Int)

object TxnEntry:
  /** Set when the metadata block being shadowed begins with the journal
    * magic, so the journal codec XORs the first word to avoid accidental
    * "fake commit/descriptor" matches during recovery scans. */
  val FlagEscaped: Int = 1

final case class TxnDescriptor(
    sequence: Int,
    blockCount: Int,
    flags: Int,
    entries: IndexedSeq[TxnEntry],
):
  require(
    entries.length <= TxnDescriptor.MaxEntriesPerBlock,
    s"too many entries (${entries.length}) for one descriptor block " +
      s"(capacity ${TxnDescriptor.MaxEntriesPerBlock})",
  )
  require(blockCount >= 0, s"blockCount $blockCount must be non-negative")

object TxnDescriptor:

  val HeaderSize: Int = 16
  val EntrySize: Int = 8

  /** Maximum entries per single descriptor block: (BlockSize - 16) / 8. */
  val MaxEntriesPerBlock: Int = (BlockSize - HeaderSize) / EntrySize // 510

  /** Pack a descriptor block into a full 4 KiB buffer; trailing bytes
    * (after the last entry) are zero-filled. */
  def pack(d: TxnDescriptor, buf: Array[Byte], off: Int): Unit =
    require(
      buf.length - off >= BlockSize,
      s"descriptor block needs $BlockSize bytes from offset $off",
    )
    Le.putU32(buf, off + 0, MagicTxnDescriptor)
    Le.putU32(buf, off + 4, d.sequence)
    Le.putU32(buf, off + 8, d.blockCount)
    Le.putU32(buf, off + 12, d.flags)
    var i = 0
    while i < d.entries.length do
      val e = d.entries(i)
      Le.putU32(buf, off + HeaderSize + i * EntrySize, e.fsBlock)
      Le.putU32(buf, off + HeaderSize + i * EntrySize + 4, e.flags)
      i += 1
    val tailFromEntries = HeaderSize + i * EntrySize
    Le.zero(buf, off + tailFromEntries, BlockSize - tailFromEntries)

  /** Decode a descriptor block. The caller passes `entriesInBlock` to
    * say how many of the up-to-510 entries are valid (the rest are zero
    * padding). For the simple case where every transaction fits in one
    * descriptor block, `entriesInBlock == blockCount`. */
  def unpack(buf: Array[Byte], off: Int, entriesInBlock: Int): TxnDescriptor =
    require(
      entriesInBlock >= 0 && entriesInBlock <= MaxEntriesPerBlock,
      s"entriesInBlock $entriesInBlock outside [0, $MaxEntriesPerBlock]",
    )
    val gotMagic = Le.u32(buf, off + 0)
    if gotMagic != MagicTxnDescriptor then
      throw new SfsCorruptError(
        f"txn descriptor magic mismatch: expected 0x$MagicTxnDescriptor%08x, got 0x$gotMagic%08x",
      )
    val sequence = Le.u32(buf, off + 4)
    val blockCount = Le.u32(buf, off + 8)
    val flags = Le.u32(buf, off + 12)
    val entries = new Array[TxnEntry](entriesInBlock)
    var i = 0
    while i < entriesInBlock do
      val fsBlock = Le.u32(buf, off + HeaderSize + i * EntrySize)
      val eflags = Le.u32(buf, off + HeaderSize + i * EntrySize + 4)
      entries(i) = TxnEntry(fsBlock, eflags)
      i += 1
    TxnDescriptor(sequence, blockCount, flags, entries.toIndexedSeq)
