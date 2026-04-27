package io.github.edadma.sfs

import Constants.*

/** A 4 KiB block holding exactly 1 024 little-endian u32 block pointers.
  *
  * Used at the second and third tier of the indirect-block tree:
  *  - `indirect2` in the inode points to one of these; each pointer here
  *    addresses an [[IndirectExtentBlock]].
  *  - `indirect3` in the inode points to one of these; each pointer here
  *    addresses another `IndirectPointerBlock`, whose pointers in turn
  *    address [[IndirectExtentBlock]]s.
  *
  * A pointer of `0` means "this slot is unused" — block 0 is the primary
  * superblock, never a metadata block, so 0 cannot collide with a real
  * indirect target.
  */
object IndirectPointerBlock:

  /** Pointers per block. Equal to BlockSize / 4 = 1 024. */
  val Capacity: Int = PtrsPerIndirect2

  /** Pack up to [[Capacity]] block pointers; trailing slots are zeroed. */
  def pack(ptrs: Seq[Int], buf: Array[Byte], off: Int): Unit =
    require(
      ptrs.length <= Capacity,
      s"too many pointers (${ptrs.length}) for pointer block (capacity $Capacity)",
    )
    var i = 0
    while i < ptrs.length do
      Le.putU32(buf, off + i * 4, ptrs(i))
      i += 1
    while i < Capacity do
      Le.putU32(buf, off + i * 4, 0)
      i += 1

  def unpack(buf: Array[Byte], off: Int): IndexedSeq[Int] =
    val out = new Array[Int](Capacity)
    var i = 0
    while i < Capacity do
      out(i) = Le.u32(buf, off + i * 4)
      i += 1
    out.toIndexedSeq
