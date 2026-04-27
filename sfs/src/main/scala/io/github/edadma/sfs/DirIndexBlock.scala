package io.github.edadma.sfs

import Constants.*

/** Interior-node block of an HTree directory (`tree_depth > 0`).
  *
  * Layout:
  * {{{
  *   0       8×n   index_entries[]:
  *                   hash    4   (minimum hash value in child subtree)
  *                   block   4   (block number of child: index or leaf)
  *   ...
  *   4084    12    DirTail
  * }}}
  *
  * Up to 510 entries (4084 / 8) fit before the tail. The spec does not
  * encode a count field; entries with `block == 0` are unused slots, and
  * pack/unpack here treats them as such. The HTree-management code in
  * Phase 9 owns the policy for keeping entries densely packed at the front.
  *
  * No magic; integrity is via [[DirTail]]'s magic+inode+CRC.
  */
object DirIndexBlock:

  val EntrySize: Int = 8

  /** Maximum index entries per block: floor(UsableSize / 8). */
  val Capacity: Int = DirTail.UsableSize / EntrySize

  /** Pack up to [[Capacity]] (hash, block) entries; trailing slots are
    * zero-filled. Then stamp the directory tail. */
  def pack(entries: Seq[(Int, Int)], ownerInode: Int, buf: Array[Byte]): Unit =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    require(
      entries.length <= Capacity,
      s"too many index entries (${entries.length}) for index block (capacity $Capacity)",
    )
    var i = 0
    while i < entries.length do
      val (h, b) = entries(i)
      Le.putU32(buf, i * EntrySize, h)
      Le.putU32(buf, i * EntrySize + 4, b)
      i += 1
    Le.zero(buf, i * EntrySize, DirTail.UsableSize - i * EntrySize)
    DirTail.pack(buf, ownerInode)

  /** Verify the tail and read all [[Capacity]] entries. Trailing entries
    * with `block == 0` are still returned — let the caller decide how to
    * interpret unused slots. */
  def unpack(buf: Array[Byte], expectedInode: Int): IndexedSeq[(Int, Int)] =
    require(buf.length == BlockSize, s"directory block must be $BlockSize bytes")
    DirTail.verify(buf, expectedInode)
    val out = new Array[(Int, Int)](Capacity)
    var i = 0
    while i < Capacity do
      val h = Le.u32(buf, i * EntrySize)
      val b = Le.u32(buf, i * EntrySize + 4)
      out(i) = (h, b)
      i += 1
    out.toIndexedSeq
