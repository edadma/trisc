package io.github.edadma.sfs

import Constants.BlockSize

/** In-memory [[BlockDevice]] backed by an array of independently-allocated
  * 4 KiB blocks.
  *
  * Each block is its own `Array[Byte]`, so reads and writes copy in/out of the
  * caller's buffer — callers cannot accidentally alias the device's storage.
  * All blocks start zeroed, matching freshly-formatted media.
  *
  * Default capacity is 2 MiB (512 blocks), which is enough to exercise the
  * superblock, bitmaps, a small inode table, journal, and a handful of data
  * blocks for unit tests.
  */
final class RamBlockDevice(val blockCount: Long) extends BlockDevice:

  require(blockCount > 0, s"blockCount must be positive (got $blockCount)")
  require(
    blockCount <= Int.MaxValue,
    s"RamBlockDevice cannot hold $blockCount blocks (Int.MaxValue limit)",
  )

  private val blocks: Array[Array[Byte]] =
    Array.fill(blockCount.toInt)(new Array[Byte](BlockSize))

  def readBlock(blockNum: Long, buf: Array[Byte]): Unit =
    checkBlock(blockNum)
    checkBuf(buf)
    System.arraycopy(blocks(blockNum.toInt), 0, buf, 0, BlockSize)

  def writeBlock(blockNum: Long, buf: Array[Byte]): Unit =
    checkBlock(blockNum)
    checkBuf(buf)
    System.arraycopy(buf, 0, blocks(blockNum.toInt), 0, BlockSize)

object RamBlockDevice:

  /** Default in-memory device used by tests and the format scaffolding. */
  val Default2MiBBlocks: Long = (2L * 1024 * 1024) / BlockSize // 512

  /** Create a 2 MiB (512-block) device. */
  def default(): RamBlockDevice = new RamBlockDevice(Default2MiBBlocks)
