package io.github.edadma.sfs

import Constants.*

/** A 4 KiB block holding exactly 512 packed [[Extent]] records.
  *
  * `indirect1` in an inode points to one of these (covers up to 512 extents).
  * Each entry of an `indirect2` pointer block points to one of these (so a
  * full `indirect2` covers 1 024 × 512 = 524 288 extents). Likewise each
  * leaf of an `indirect3` tree.
  *
  * No magic, no CRC: the block's authenticity rests on the inode and journal
  * having pointed to it. This matches the spec's integrity model — only
  * superblocks, inodes, journal, and directory blocks carry their own CRCs.
  */
object IndirectExtentBlock:

  /** Number of extents per block. Equal to BlockSize / ExtentSize = 512. */
  val Capacity: Int = ExtentsPerIndirect

  /** Pack up to [[Capacity]] extents into a 4 KiB block. Extents past the
    * end of `xs` are written as zeroed-out [[Extent.Empty]] slots, so the
    * full block is well-defined. */
  def pack(xs: Seq[Extent], buf: Array[Byte], off: Int): Unit =
    require(
      xs.length <= Capacity,
      s"too many extents (${xs.length}) for indirect block (capacity $Capacity)",
    )
    var i = 0
    while i < xs.length do
      Extent.pack(xs(i), buf, off + i * ExtentSize)
      i += 1
    while i < Capacity do
      Extent.pack(Extent.Empty, buf, off + i * ExtentSize)
      i += 1

  /** Read all [[Capacity]] extents back. Trailing [[Extent.Empty]] entries
    * are returned as-is — interpretation is the caller's job. */
  def unpack(buf: Array[Byte], off: Int): IndexedSeq[Extent] =
    val out = new Array[Extent](Capacity)
    var i = 0
    while i < Capacity do
      out(i) = Extent.unpack(buf, off + i * ExtentSize)
      i += 1
    out.toIndexedSeq
