package io.github.edadma.sfs

import Constants.BlockSize

/** Abstraction over a block-addressable storage medium.
  *
  * SFS is built on top of fixed-size 4 KiB sectors. This trait is the only seam
  * between the filesystem and whatever backs it — RAM, a host file, a real
  * disk, a network volume — so the rest of SFS can be tested without I/O.
  *
  * Block addresses are 0-based. All buffers passed to read/write must be
  * exactly `BlockSize` bytes; partial-sector access is intentionally not
  * supported (the spec mandates a fixed 4 KiB block size).
  */
trait BlockDevice:

  /** Total addressable blocks on this device. */
  def blockCount: Long

  /** Total capacity in bytes. */
  def sizeInBytes: Long = blockCount * BlockSize

  /** Read one full block into `buf`. `buf.length` must equal [[BlockSize]]. */
  def readBlock(blockNum: Long, buf: Array[Byte]): Unit

  /** Write one full block from `buf`. `buf.length` must equal [[BlockSize]]. */
  def writeBlock(blockNum: Long, buf: Array[Byte]): Unit

  /** Optional durability barrier. No-op for in-memory devices. */
  def flush(): Unit = ()

  protected final def checkBlock(blockNum: Long): Unit =
    if blockNum < 0 || blockNum >= blockCount then
      throw new IndexOutOfBoundsException(
        s"block $blockNum out of range [0, $blockCount)",
      )

  protected final def checkBuf(buf: Array[Byte]): Unit =
    if buf.length != BlockSize then
      throw new IllegalArgumentException(
        s"buffer length ${buf.length} != BlockSize $BlockSize",
      )
