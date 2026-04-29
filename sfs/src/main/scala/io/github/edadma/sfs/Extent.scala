package io.github.edadma.sfs

import Constants.*

/** A single 8-byte on-disk extent record.
  *
  * On-disk layout:
  * {{{
  *   bit  31:    UNINITIALIZED   (preallocated, reads as zeros)
  *   bit  30:    SPARSE          (hole, no blocks allocated, reads as zeros)
  *   bit  29:    reserved
  *   bits 28–0:  start_block     (29-bit block address)
  *   bytes 4–7:  count            (32-bit block count)
  * }}}
  *
  * The 29-bit start width is what limits the volume to 2 TB (2^29 blocks ×
  * 4 KiB). The flag bits are deliberately *outside* the address space so a
  * sparse hole can still record its logical block count without needing a
  * physical block.
  */
final case class Extent(
    start: Int,
    count: Int,
    uninitialized: Boolean = false,
    sparse: Boolean = false,
):
  require(
    start >= 0 && start <= ExtentStartMask,
    s"start_block must fit in 29 bits, got $start",
  )
  require(count >= 0, s"count must be non-negative, got $count")

object Extent:

  /** Serialized size on disk. */
  val Size: Int = ExtentSize

  /** A null extent (start=0, count=0, no flags). Padding for unused inline
    * slots and unused entries in indirect blocks. */
  val Empty: Extent = Extent(0, 0)

  def pack(e: Extent, buf: Array[Byte], off: Int): Unit =
    var word = e.start & ExtentStartMask
    if e.uninitialized then word |= ExtentFlagUninitialized
    if e.sparse then word |= ExtentFlagSparse
    Le.putU32(buf, off, word)
    Le.putU32(buf, off + 4, e.count)

  def unpack(buf: Array[Byte], off: Int): Extent =
    val word = Le.u32(buf, off)
    val cnt = Le.u32(buf, off + 4)
    val start = word & ExtentStartMask
    val uninit = (word & ExtentFlagUninitialized) != 0
    val sparse = (word & ExtentFlagSparse) != 0
    Extent(start, cnt, uninit, sparse)
