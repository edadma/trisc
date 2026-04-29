package io.github.edadma.sfs

import Constants.*

/** Result of mapping a file-logical block to its on-disk location. */
enum BlockMapping:
  /** A real physical block — read these bytes from disk to get the data. */
  case Concrete(physicalBlock: Long)

  /** A sparse hole — no physical block exists; reads return zeros. */
  case Sparse

  /** A preallocated extent that has not been written yet — physical
    * blocks exist (and are accounted for in `block_count`) but reads
    * return zeros until the extent is converted to `Concrete`. */
  case Uninitialized

  /** The logical block is past the file's extent map. Out-of-bounds. */
  case OutOfRange

/** Read-path extent walker for one inode.
  *
  * Translates a file-logical block index to the underlying [[BlockMapping]]
  * by walking the inode's extent list:
  *
  *   1. The 16 inline extents in the inode union (when the inode body is
  *      `Extents`, not `InlineSymlink`).
  *   2. The single-indirect block (`indirect1`), holding 512 extents.
  *   3. The double-indirect tree (`indirect2`), holding 1 024 pointers
  *      to single-indirect blocks.
  *   4. The triple-indirect tree (`indirect3`), holding 1 024 pointers
  *      to double-indirect blocks.
  *
  * Each tier is gated by the matching `HAS_INDIRECT*` flag in
  * `inode.flags`. Walking is linear over extents (no extent B-tree per
  * the spec); files that stay inside the 16 inline extents pay only
  * a tight bounded scan.
  *
  * No caching — every triple-indirect lookup re-reads up to 3 indirect
  * blocks from the device. Phase 8 (file I/O) is where read locality
  * justifies adding a cache; this class stays simple for now.
  */
final class ExtentReader(dev: BlockDevice, inode: Inode):

  /** Map file-logical block `logical` to its [[BlockMapping]].
    * `logical` is a *file-logical* block index, not a physical disk
    * address: the first block of the file is `logical = 0`. */
  def physicalBlock(logical: Long): BlockMapping =
    require(logical >= 0L, s"logical block must be ≥ 0, got $logical")
    val r = new ExtentReader.Cursor(logical)
    walkInline(r) match
      case Some(m) => return m
      case None    => ()
    if (inode.flags & InodeFlagHasIndirect1) != 0 then
      walkIndirect1(r, inode.indirect1) match
        case Some(m) => return m
        case None    => ()
    if (inode.flags & InodeFlagHasIndirect2) != 0 then
      walkIndirect2(r, inode.indirect2) match
        case Some(m) => return m
        case None    => ()
    if (inode.flags & InodeFlagHasIndirect3) != 0 then
      walkIndirect3(r, inode.indirect3) match
        case Some(m) => return m
        case None    => ()
    BlockMapping.OutOfRange

  // ---- per-tier walkers ------------------------------------------------

  /** Returns `Some(mapping)` when the cursor is consumed inside this tier;
    * `None` to mean "advance to the next tier". */
  private def walkInline(r: ExtentReader.Cursor): Option[BlockMapping] =
    inode.body match
      case InodeBody.Extents(xs)  => walkExtents(r, xs)
      case InodeBody.InlineSymlink(_) =>
        // Inline-symlink inodes carry their target in the union; they have
        // no data extents at all.
        Some(BlockMapping.OutOfRange)

  private def walkIndirect1(r: ExtentReader.Cursor, blockAddr: Int): Option[BlockMapping] =
    walkExtents(r, readExtentBlock(blockAddr))

  private def walkIndirect2(r: ExtentReader.Cursor, blockAddr: Int): Option[BlockMapping] =
    val ptrs = readPointerBlock(blockAddr)
    var i = 0
    while i < ptrs.length do
      val p = ptrs(i)
      // Zero pointer = end of *this tier*. Return None so the caller can
      // advance to the next outer tier (if the matching `HAS_INDIRECT*`
      // flag is set on the inode) or, if no further tiers, fall through
      // to OutOfRange at the top level.
      if p == 0 then return None
      walkExtents(r, readExtentBlock(p)) match
        case Some(m) => return Some(m)
        case None    => ()
      i += 1
    None

  private def walkIndirect3(r: ExtentReader.Cursor, blockAddr: Int): Option[BlockMapping] =
    val ptrs = readPointerBlock(blockAddr)
    var i = 0
    while i < ptrs.length do
      val p = ptrs(i)
      if p == 0 then return None
      walkIndirect2(r, p) match
        case Some(m) => return Some(m)
        case None    => ()
      i += 1
    None

  /** Walk a sequence of extents, advancing the cursor. Returns
    * `Some(mapping)` when the target is found inside this list, or
    * `None` to mean "advance to the next tier" — including when an
    * empty slot (count=0) marks the end of this tier's contents. */
  private def walkExtents(
      r: ExtentReader.Cursor,
      xs: IndexedSeq[Extent],
  ): Option[BlockMapping] =
    var i = 0
    while i < xs.length do
      val e = xs(i)
      if e.count == 0 then return None
      if r.target < r.cursor + e.count then
        val withinOffset = (r.target - r.cursor).toInt
        return Some(extentMapping(e, withinOffset))
      r.cursor += e.count
      i += 1
    None

  // ---- low-level reads -------------------------------------------------

  private def readExtentBlock(blockAddr: Int): IndexedSeq[Extent] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockAddr.toLong, buf)
    IndirectExtentBlock.unpack(buf, 0)

  private def readPointerBlock(blockAddr: Int): IndexedSeq[Int] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockAddr.toLong, buf)
    IndirectPointerBlock.unpack(buf, 0)

  private def extentMapping(e: Extent, withinOffset: Int): BlockMapping =
    if e.sparse then BlockMapping.Sparse
    else if e.uninitialized then BlockMapping.Uninitialized
    else BlockMapping.Concrete((e.start + withinOffset).toLong)

object ExtentReader:
  /** Mutable cursor used by the walkers — `target` is the file-logical
    * block we're looking for; `cursor` is the running file-logical
    * offset of the current extent slot. */
  private final class Cursor(val target: Long):
    var cursor: Long = 0L
