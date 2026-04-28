package io.github.edadma.sfs

import Constants.*

/** Journal recovery for a dirty mount.
  *
  * Walks the on-disk journal log forward from `journal.head`, parsing
  * each transaction (descriptor block(s) → metadata blocks → commit
  * block), validating the commit-block CRC, and replaying every valid
  * transaction's metadata blocks to their in-place fs_block locations.
  *
  * The XOR escape on a metadata block whose first 4 bytes coincided
  * with a journal magic (see [[Transaction.EscapeSentinel]]) is undone
  * during replay so the in-place block ends up with the original bytes.
  *
  * Recovery stops at the first invalid commit block (wrong magic,
  * wrong sequence, or CRC mismatch). Earlier transactions are still
  * applied — the partial txn just becomes a no-op (its writes are
  * either incomplete or absent on disk).
  *
  * Replay is **idempotent**: applying the same committed txn twice
  * (e.g. because we crashed between in-place writes and the journal-SB
  * flush) writes the same bytes to the same locations.
  */
object Recovery:

  /** Replay every committed transaction in the journal log. Returns
    * the new head position (= old tail, since the log is now empty
    * post-replay).
    *
    * Must be called only on a `dirty` filesystem with an
    * already-loaded [[Journal]] — the caller (Sfs.mount) handles the
    * superblock fsState transition. */
  def replay(dev: BlockDevice, journal: Journal): Int =
    val bc = journal.blockCount
    val startHead = journal.head
    val tail = journal.tail
    var pos = startHead
    var lastReplayedSeq = journal.sequence // sequence of last-known-replayed txn

    // Walk forward until we hit the journal tail (= no more txns to
    // try) or we encounter an invalid txn (CRC or magic failure).
    while pos != tail do
      parseAndReplayOne(dev, journal, pos) match
        case Some((nextPos, seq)) =>
          pos = nextPos
          lastReplayedSeq = seq
        case None =>
          // Stop at first invalid txn — leave head where it is and
          // bail out. Anything between this point and tail is treated
          // as never-committed.
          return pos

    pos // == tail; log is now fully replayed

  /** Try to parse one transaction starting at log position `pos`.
    * Returns `Some((newPos, sequence))` if the txn is valid and was
    * replayed; `None` if the descriptor / commit could not be parsed
    * or its CRC failed.
    *
    * The on-disk layout is:
    * `descriptor[s] || metadata[blockCount] || commit`
    *
    * `blockCount` can exceed [[TxnDescriptor.MaxEntriesPerBlock]] (510)
    * — in that case the txn spans multiple descriptor blocks, all of
    * which carry the same `sequence` and total `blockCount`. */
  private def parseAndReplayOne(
      dev: BlockDevice,
      journal: Journal,
      pos: Int,
  ): Option[(Int, Int)] =
    val bc = journal.blockCount
    val tail = journal.tail
    val firstDescBuf = new Array[Byte](BlockSize)
    dev.readBlock(journal.logPositionToDisk(pos), firstDescBuf)
    val firstMagic = Le.u32(firstDescBuf, 0)
    if firstMagic != MagicTxnDescriptor then return None

    val sequence = Le.u32(firstDescBuf, 4)
    val totalBlocks = Le.u32(firstDescBuf, 8)
    if totalBlocks < 0 || totalBlocks > MaxBlocksPerTransaction then return None

    val numDescriptors =
      (totalBlocks + TxnDescriptor.MaxEntriesPerBlock - 1) /
        TxnDescriptor.MaxEntriesPerBlock
    val totalLogBlocks = numDescriptors + totalBlocks + 1
    if totalLogBlocks > bc then return None

    // Verify we have enough log blocks before tail to cover this txn.
    if !logRangeReachable(pos, totalLogBlocks, tail, bc) then return None

    // ---- Read all descriptor blocks --------------------------------
    val entries = new Array[TxnEntry](totalBlocks)
    var entriesRead = 0
    var d = 0
    while d < numDescriptors do
      val descPos = (pos + d) % bc
      val descBuf =
        if d == 0 then firstDescBuf
        else
          val b = new Array[Byte](BlockSize)
          dev.readBlock(journal.logPositionToDisk(descPos), b)
          b
      val magic = Le.u32(descBuf, 0)
      if magic != MagicTxnDescriptor then return None
      val seq = Le.u32(descBuf, 4)
      if seq != sequence then return None
      val entriesInThisBlock =
        math.min(totalBlocks - entriesRead, TxnDescriptor.MaxEntriesPerBlock)
      val parsed = TxnDescriptor.unpack(descBuf, 0, entriesInThisBlock)
      var i = 0
      while i < parsed.entries.length do
        entries(entriesRead + i) = parsed.entries(i)
        i += 1
      entriesRead += parsed.entries.length
      d += 1

    // ---- Read all metadata blocks (exactly as written to journal) --
    val metadataBufs = new Array[Array[Byte]](totalBlocks)
    var m = 0
    while m < totalBlocks do
      val mpos = (pos + numDescriptors + m) % bc
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(journal.logPositionToDisk(mpos), buf)
      metadataBufs(m) = buf
      m += 1

    // ---- Read commit block & verify magic + sequence ---------------
    val commitPos = (pos + numDescriptors + totalBlocks) % bc
    val commitBuf = new Array[Byte](BlockSize)
    dev.readBlock(journal.logPositionToDisk(commitPos), commitBuf)
    val commitMagic = Le.u32(commitBuf, 0)
    if commitMagic != MagicCommit then return None
    val commit =
      try CommitBlock.unpack(commitBuf, 0)
      catch case _: SfsCorruptError => return None
    if commit.sequence != sequence then return None

    // ---- Recompute CRC over (descriptors || metadata || commit-with-crc-zeroed) ----
    val computedCrc =
      val sentCrc = commit.crc32
      // Zero the CRC field in a copy so the recomputation matches what
      // commit() did.
      val verifyCommit = commitBuf.clone()
      Le.putU32(verifyCommit, CommitBlock.CrcOff, 0)
      var crc = Crc32.start
      var di = 0
      while di < numDescriptors do
        val dpos = (pos + di) % bc
        val buf =
          if di == 0 then firstDescBuf
          else
            val b = new Array[Byte](BlockSize)
            dev.readBlock(journal.logPositionToDisk(dpos), b)
            b
        crc = Crc32.update(crc, buf, 0, BlockSize)
        di += 1
      var mi = 0
      while mi < totalBlocks do
        crc = Crc32.update(crc, metadataBufs(mi), 0, BlockSize)
        mi += 1
      crc = Crc32.update(crc, verifyCommit, 0, BlockSize)
      Crc32.finish(crc)

    if computedCrc != commit.crc32 then return None

    // ---- CRC validated — replay metadata to in-place locations -----
    var w = 0
    while w < totalBlocks do
      val entry = entries(w)
      val src = metadataBufs(w)
      val dst =
        if (entry.flags & TxnEntry.FlagEscaped) != 0 then
          val cloned = src.clone()
          val first = Le.u32(cloned, 0)
          Le.putU32(cloned, 0, first ^ Transaction.EscapeSentinel)
          cloned
        else src
      dev.writeBlock(entry.fsBlock.toLong, dst)
      w += 1
    dev.flush()

    val newPos = (pos + totalLogBlocks) % bc
    Some((newPos, sequence))

  /** True if a contiguous run of `n` log blocks starting at `pos`
    * fits before `tail` in a circular log of `bc` blocks. */
  private def logRangeReachable(pos: Int, n: Int, tail: Int, bc: Int): Boolean =
    val available =
      if pos <= tail then tail - pos
      else bc - pos + tail
    n <= available
