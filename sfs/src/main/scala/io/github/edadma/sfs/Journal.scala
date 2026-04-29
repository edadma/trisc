package io.github.edadma.sfs

import Constants.*

/** In-memory bookkeeping for the on-disk journal log.
  *
  * The journal is a circular log of [[blockCount]] blocks (excluding the
  * journal superblock at [[journalStart]]). Positions are integers in
  * `[0, blockCount)`; the absolute disk block of position `p` is
  * `journalStart + 1 + p`.
  *
  * `head` is the start of the oldest unreplayed transaction. `tail` is
  * where the next transaction will be written. `head == tail` always
  * means the log is empty — one block of slack is left so the
  * empty/full ambiguity never arises in [[freeBlocks]].
  *
  * The class is single-threaded — the higher-level `Sfs` API serializes
  * filesystem ops, and the journal is updated under that same lock.
  *
  * Reservation flow (used by the Transaction API in 13b):
  *
  *   1. Caller computes `n` = log blocks needed for the next txn.
  *   2. Caller calls [[reserve]] to get the starting disk block. This
  *      does NOT mutate state — an aborted txn simply doesn't call
  *      [[advance]], so the on-disk log is left untouched.
  *   3. Caller writes descriptor + metadata + commit blocks, using
  *      [[logPositionToDisk]] modulo [[blockCount]] to handle wrap.
  *   4. After the commit block is durable, caller calls [[advance]]
  *      with the post-commit `tail` and `sequence`.
  *
  * State changes don't hit disk until [[flush]] is called. [[Sfs.unmount]]
  * calls [[flush]]; the Transaction commit path in 13b will too.
  */
final class Journal private[sfs] (
    val device: BlockDevice,
    val journalStart: Long,
    val blockCount: Int,
    val version: Int,
    val fsUuid: IndexedSeq[Byte],
    private var _head: Int,
    private var _tail: Int,
    private var _sequence: Int,
):
  require(blockCount > 1, s"Journal: blockCount must be > 1, got $blockCount")
  require(_head >= 0 && _head < blockCount, s"Journal: head $_head out of range [0, $blockCount)")
  require(_tail >= 0 && _tail < blockCount, s"Journal: tail $_tail out of range [0, $blockCount)")

  def head: Int = _head
  def tail: Int = _tail
  def sequence: Int = _sequence

  /** Number of log blocks available for a new reservation. Always
    * leaves one slot of slack so `head == tail` unambiguously means
    * the log is empty. */
  def freeBlocks: Int =
    val raw = (_head - _tail - 1) % blockCount
    if raw < 0 then raw + blockCount else raw

  /** Convert a log position in `[0, blockCount)` to its absolute disk
    * block. Wrap-around is the caller's responsibility — typical
    * usage is `logPositionToDisk((tail + i) % blockCount)`. */
  def logPositionToDisk(pos: Int): Long =
    require(pos >= 0 && pos < blockCount, s"Journal: log position $pos out of range [0, $blockCount)")
    journalStart + 1L + pos.toLong

  /** Returns the absolute disk block at which a contiguous run of `n`
    * log blocks would start, or throws [[SfsNoSpaceError]] if the
    * journal can't fit it.
    *
    * Does NOT mutate state — the caller updates `tail` via [[advance]]
    * only after the commit block is durable. The reservation may
    * straddle the wrap point; callers compute each block's disk
    * address with [[logPositionToDisk]] modulo [[blockCount]]. */
  def reserve(n: Int): Long =
    require(n > 0, s"Journal.reserve($n): must reserve at least one block")
    val free = freeBlocks
    if n > free then
      throw new SfsNoSpaceError(
        s"journal full: reserve($n) exceeds free $free blocks " +
          s"(blockCount=$blockCount, head=$_head, tail=$_tail)",
      )
    logPositionToDisk(_tail)

  /** Bump tail and sequence after a transaction's commit block is
    * durable. The new tail must lie on the run reserved by the most
    * recent [[reserve]] call. */
  def advance(newTail: Int, newSeq: Int): Unit =
    require(newTail >= 0 && newTail < blockCount, s"Journal.advance: newTail $newTail out of range")
    _tail = newTail
    _sequence = newSeq

  /** Bump head after recovery has replayed all transactions up to
    * (but not including) `newHead`. Calls during normal operation are
    * also legal — checkpoint flushes that confirm metadata reached its
    * final disk locations release the journal blocks. */
  def replayHead(newHead: Int): Unit =
    require(newHead >= 0 && newHead < blockCount, s"Journal.replayHead: newHead $newHead out of range")
    _head = newHead

  /** Persist current head/tail/sequence back to the journal superblock
    * on disk. Called by [[Sfs.unmount]] (and by the Transaction commit
    * path in 13b after every commit, so an unclean shutdown still sees
    * the latest tail/sequence). */
  def flush(): Unit =
    val sb = JournalSuperblock(
      version = version,
      blockCount = blockCount,
      head = _head,
      tail = _tail,
      sequence = _sequence,
      fsUuid = fsUuid,
    )
    val buf = new Array[Byte](BlockSize)
    JournalSuperblock.pack(sb, buf, 0)
    device.writeBlock(journalStart, buf)

object Journal:

  /** Load journal state from disk, validating the on-disk journal
    * superblock against the filesystem's UUID. */
  def load(dev: BlockDevice, journalStart: Long, expectedFsUuid: IndexedSeq[Byte]): Journal =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(journalStart, buf)
    val sb = JournalSuperblock.unpack(buf, 0)
    if sb.fsUuid != expectedFsUuid then
      throw new SfsCorruptError(
        "journal SB fs_uuid does not match filesystem UUID — wrong volume?",
      )
    new Journal(
      device = dev,
      journalStart = journalStart,
      blockCount = sb.blockCount,
      version = sb.version,
      fsUuid = sb.fsUuid,
      _head = sb.head,
      _tail = sb.tail,
      _sequence = sb.sequence,
    )
