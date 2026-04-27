package io.github.edadma.sfs

import scala.collection.mutable
import Constants.*

/** A pending journal transaction.
  *
  * Usage:
  * {{{
  *   val tx = sfs.beginTxn()
  *   tx.writeMetadata(blkA, bufA)
  *   tx.writeMetadata(blkB, bufB)
  *   tx.commit() // or tx.abort()
  * }}}
  *
  * `commit` writes a complete write-ahead-logged record to the
  * journal — descriptor block(s), metadata blocks, and a commit
  * block whose CRC covers the entire transaction — flushes, and
  * only then writes the staged metadata to its in-place fs_block
  * locations. A crash before the commit block is durable leaves
  * the on-disk filesystem unchanged; a crash after, but before
  * the in-place writes, leaves enough information for recovery
  * to redo them.
  *
  * `abort` simply discards the staged blocks. No journal write
  * occurs, so the on-disk log is untouched.
  *
  * The class is single-threaded; the [[Sfs]] API serializes calls.
  *
  * data=ordered note: file *data* blocks (user-visible content)
  * are NOT staged here. Higher layers write data blocks directly
  * to disk and call `dev.flush()` *before* a txn that records the
  * matching metadata change, so a crash can never leave a
  * committed inode pointing at unwritten or garbage data. The
  * journal handles metadata only.
  */
final class Transaction private[sfs] (sfs: Sfs):

  // Coalesce by fs block number — last writeMetadata wins for a
  // given block. LinkedHashMap preserves insertion order, which
  // gives a stable replay order.
  private val staged: mutable.LinkedHashMap[Long, Array[Byte]] =
    mutable.LinkedHashMap.empty

  private var _open: Boolean = true

  /** True until [[commit]] or [[abort]] is called. */
  def isOpen: Boolean = _open

  /** Number of distinct metadata blocks staged so far. */
  def size: Int = staged.size

  /** Stage a 4 KiB metadata block to be written at `blockNum` after
    * the journal commit lands. Replaces any previously-staged buffer
    * for the same `blockNum`. */
  def writeMetadata(blockNum: Long, buf: Array[Byte]): Unit =
    require(_open, "Transaction.writeMetadata: transaction is closed")
    require(
      buf.length == BlockSize,
      s"Transaction.writeMetadata: buffer must be $BlockSize bytes",
    )
    if !staged.contains(blockNum) && staged.size >= MaxBlocksPerTransaction then
      throw new SfsNoSpaceError(
        s"transaction full: max $MaxBlocksPerTransaction metadata blocks per txn",
      )
    staged.update(blockNum, buf.clone())

  /** Discard staged blocks. The on-disk log is left untouched
    * (since nothing was written yet). Idempotent for closed txns. */
  def abort(): Unit =
    _open = false
    staged.clear()

  /** Write the staged blocks through the journal and then to their
    * in-place fs_block locations.
    *
    * Steps:
    *   1. Compute log layout: `numDescriptors + N + 1` blocks.
    *   2. Reserve via [[Journal.reserve]] (capacity check; doesn't
    *      mutate journal state).
    *   3. Build journal-side buffers; XOR-escape the leading word of
    *      any metadata block whose first 4 bytes coincidentally
    *      equal a journal magic.
    *   4. Compute the transaction-wide CRC across descriptor(s) +
    *      metadata + commit-block-with-crc-zeroed.
    *   5. Write descriptor(s) and metadata to journal positions, then
    *      `dev.flush()` so they're durable before the commit lands.
    *   6. Write the commit block, then `dev.flush()` again.
    *   7. After the commit is durable, write the metadata to its
    *      in-place fs_block locations. A crash here is safe — recovery
    *      will replay these writes.
    *   8. [[Journal.advance]] + [[Journal.flush]] to record the new
    *      tail and sequence.
    *
    * No-op if nothing is staged. Closes the transaction. */
  def commit(): Unit =
    require(_open, "Transaction.commit: transaction is closed")
    _open = false
    if staged.isEmpty then return

    val journal = sfs.journal
    val device = sfs.device
    val n = staged.size
    val numDescriptors =
      (n + TxnDescriptor.MaxEntriesPerBlock - 1) / TxnDescriptor.MaxEntriesPerBlock
    val totalLogBlocks = numDescriptors + n + 1

    journal.reserve(totalLogBlocks) // capacity check; throws SfsNoSpaceError

    val newSeq = journal.sequence + 1
    val startTail = journal.tail
    val bc = journal.blockCount

    // ---- Stage journal-side metadata buffers (XOR-escape if needed) ----
    val origs = new Array[Array[Byte]](n)
    val journalBufs = new Array[Array[Byte]](n)
    val blkNums = new Array[Long](n)
    val entryFlags = new Array[Int](n)
    var i = 0
    val it = staged.iterator
    while it.hasNext do
      val (blk, orig) = it.next()
      val first = Le.u32(orig, 0)
      val escape = first == MagicJournalSuperblock ||
        first == MagicTxnDescriptor ||
        first == MagicCommit ||
        first == MagicDirTail
      blkNums(i) = blk
      origs(i) = orig
      if escape then
        val cloned = orig.clone()
        Le.putU32(cloned, 0, first ^ Transaction.EscapeSentinel)
        journalBufs(i) = cloned
        entryFlags(i) = TxnEntry.FlagEscaped
      else
        journalBufs(i) = orig
        entryFlags(i) = 0
      i += 1

    // ---- Build descriptor block buffers --------------------------------
    val descBufs = new Array[Array[Byte]](numDescriptors)
    var d = 0
    while d < numDescriptors do
      val from = d * TxnDescriptor.MaxEntriesPerBlock
      val to = math.min(n, (d + 1) * TxnDescriptor.MaxEntriesPerBlock)
      val es = (from until to).map { idx =>
        TxnEntry(fsBlock = blkNums(idx).toInt, flags = entryFlags(idx))
      }.toIndexedSeq
      val desc = TxnDescriptor(
        sequence = newSeq,
        blockCount = n,
        flags = 0,
        entries = es,
      )
      val buf = new Array[Byte](BlockSize)
      TxnDescriptor.pack(desc, buf, 0)
      descBufs(d) = buf
      d += 1

    // ---- Build commit block with crc=0 placeholder --------------------
    val commitBuf = new Array[Byte](BlockSize)
    CommitBlock.pack(
      CommitBlock(sequence = newSeq, commitTime = Sfs.now(), crc32 = 0),
      commitBuf,
      0,
    )
    // CommitBlock.pack only zeroes the 256-byte payload tail; the rest
    // of the 4 KiB block is whatever junk the freshly allocated array
    // started with. On the JVM new arrays are zeroed, but be explicit.
    Le.zero(commitBuf, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)

    // ---- Transaction-wide CRC -----------------------------------------
    var crc = Crc32.start
    var ix = 0
    while ix < numDescriptors do
      crc = Crc32.update(crc, descBufs(ix), 0, BlockSize)
      ix += 1
    var iy = 0
    while iy < n do
      crc = Crc32.update(crc, journalBufs(iy), 0, BlockSize)
      iy += 1
    crc = Crc32.update(crc, commitBuf, 0, BlockSize)
    val finalCrc = Crc32.finish(crc)
    Le.putU32(commitBuf, CommitBlock.CrcOff, finalCrc)

    // ---- Write descriptor(s) and metadata to journal -----------------
    var p = 0
    while p < numDescriptors do
      val pos = (startTail + p) % bc
      device.writeBlock(journal.logPositionToDisk(pos), descBufs(p))
      p += 1
    var q = 0
    while q < n do
      val pos = (startTail + numDescriptors + q) % bc
      device.writeBlock(journal.logPositionToDisk(pos), journalBufs(q))
      q += 1

    device.flush() // descriptor + metadata durable before commit lands

    val commitPos = (startTail + numDescriptors + n) % bc
    device.writeBlock(journal.logPositionToDisk(commitPos), commitBuf)
    device.flush() // commit durable; transaction is now atomic

    // ---- Write metadata to its in-place fs_block locations ----------
    // Safe to interrupt: recovery will redo these writes from the
    // journal copies on next mount.
    var w = 0
    while w < n do
      device.writeBlock(blkNums(w), origs(w))
      w += 1

    // ---- Persist new tail/sequence ---------------------------------
    val newTail = (startTail + totalLogBlocks) % bc
    journal.advance(newTail, newSeq)
    journal.flush()

object Transaction:
  /** XOR sentinel applied to the leading 4 bytes of any metadata
    * block whose first word coincidentally equals one of the
    * journal magics. The same XOR un-does it during recovery. */
  val EscapeSentinel: Int = 0xffffffff
