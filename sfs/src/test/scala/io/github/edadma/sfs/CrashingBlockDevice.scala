package io.github.edadma.sfs

import Constants.*

/** A [[BlockDevice]] decorator that silently drops writes after a
  * configurable crash barrier — used by Phase 13e crash-injection
  * tests to simulate a power loss at any point during a transaction.
  *
  * Two firing modes:
  *
  *   - **count**: drop every write after the first `crashAfterWrites`
  *     accepted writes. The barrier itself is the (`crashAfterWrites`+1)th
  *     write, which is silently swallowed. Reads always pass through —
  *     a real crash leaves the disk's prior state intact, not garbage.
  *
  *   - **magic match**: drop every write whose first 4 bytes equal a
  *     specific 32-bit value. Useful for "crash right after the
  *     commit-block magic is about to be written" — set the magic to
  *     `MagicCommit` and any commit-block write is skipped (so the
  *     txn is durable up to the metadata but never gets a commit
  *     block on disk).
  *
  * Once `crashed` is `true`, every subsequent write is dropped
  * regardless of the mode — once a power loss has happened, no
  * further writes can succeed.
  *
  * `flush()` becomes a no-op after crashing too: the underlying
  * device may have buffered some of our pre-crash writes, but a real
  * crash means *no* later flush gets to land.
  */
final class CrashingBlockDevice(
    underlying: BlockDevice,
    crashAfterWrites: Int = Int.MaxValue,
    crashOnFirstWord: Option[Int] = None,
) extends BlockDevice:

  require(crashAfterWrites >= 0, s"crashAfterWrites must be ≥ 0, got $crashAfterWrites")

  private var _writes: Int = 0
  private var _crashed: Boolean = false

  /** True once any write has been dropped. */
  def crashed: Boolean = _crashed

  /** Number of writes that have been forwarded to the underlying
    * device (before the crash, if any). */
  def writes: Int = _writes

  val blockCount: Long = underlying.blockCount

  def readBlock(blockNum: Long, buf: Array[Byte]): Unit =
    underlying.readBlock(blockNum, buf)

  def writeBlock(blockNum: Long, buf: Array[Byte]): Unit =
    if _crashed then return // simulated power loss — write is lost
    crashOnFirstWord match
      case Some(magic) if buf.length >= 4 && Le.u32(buf, 0) == magic =>
        _crashed = true
        return
      case _ =>
    if _writes >= crashAfterWrites then
      _crashed = true
      return
    underlying.writeBlock(blockNum, buf)
    _writes += 1

  override def flush(): Unit =
    if !_crashed then underlying.flush()
