package io.github.edadma.sfs

import Constants.*

/** Write-path extent allocator.
  *
  * Object-style API: each call returns a new (immutable) [[Inode]].
  * Mutates the inode's extent map (inline body and indirect block contents),
  * the `indirect1/2/3` pointers, the `HAS_INDIRECT*` flags, and the
  * [[Bitmap]] state. Does *not* update `size` / `block_count` / `mtime`
  * / `ctime` — that's Phase 8 (file I/O). Does not journal — Phase 13
  * wraps these calls.
  *
  * All operations are single-threaded; concurrent writers are not supported.
  *
  * Tier-fill invariants enforced (and relied on by [[ExtentReader]]):
  *
  *   - Inline extents are dense from slot 0; first count=0 slot is the end.
  *   - `HAS_INDIRECT1` set ⇒ all 16 inline slots have count > 0.
  *   - `HAS_INDIRECT2` set ⇒ inline + indirect1 fully filled.
  *   - `HAS_INDIRECT3` set ⇒ inline + indirect1 + indirect2 fully filled.
  *   - Indirect-pointer blocks (ind2 / ind3-mid) are dense from slot 0;
  *     first 0 pointer marks the end of that level.
  *   - The very last extent in any tier may be partially filled (count > 0
  *     but the *next* slot is unused).
  *
  * Coalescing rule: when appending a concrete block at physical address `P`,
  * if the last extent `E` in the map is concrete and `P == E.start + E.count`,
  * just bump `E.count`; otherwise write a fresh extent into the next free
  * slot. Sparse appends always extend the last sparse extent's count if one
  * exists; otherwise a new slot is taken. Uninitialized extents are not
  * coalesced (preallocation is deferred to a later phase).
  */
object ExtentAllocator:

  // ---- Public API -----------------------------------------------------

  /** Append `n` concrete data blocks to the file by allocating fresh
    * physical blocks one-at-a-time from `bm`. Returns the updated inode. */
  def append(ino: Inode, dev: BlockDevice, bm: Bitmap, n: Int): Inode =
    require(n >= 0, s"n must be non-negative, got $n")
    var cur = ino
    var i = 0
    while i < n do
      val phys = bm.allocate().getOrElse {
        throw new SfsCorruptError("append: out of free blocks")
      }
      cur = appendOneConcrete(cur, dev, bm, phys)
      i += 1
    cur

  /** Append `n` sparse logical blocks (a hole). No physical blocks are
    * allocated; the extent records the hole's logical length. Returns
    * the updated inode. */
  def appendSparse(ino: Inode, dev: BlockDevice, bm: Bitmap, n: Int): Inode =
    require(n >= 0, s"n must be non-negative, got $n")
    if n == 0 then return ino
    findLastExtent(ino, dev) match
      case Some((loc, e)) if e.sparse && canBumpCount(e.count, n) =>
        writeExtentAt(ino, dev, loc, e.copy(count = e.count + n))
      case _ =>
        appendNewExtent(ino, dev, bm, Extent(start = 0, count = n, sparse = true))

  /** Truncate the file to `target` logical blocks. Frees any physical
    * blocks past the cut point and reclaims indirect blocks whose
    * extents/pointers all become empty. If `target` already equals or
    * exceeds the file's logical block count, the inode is returned
    * unchanged (extending is the caller's job — see [[appendSparse]]). */
  def truncate(ino: Inode, dev: BlockDevice, bm: Bitmap, target: Long): Inode =
    require(target >= 0L, s"target must be non-negative, got $target")
    truncateImpl(ino, dev, bm, target)

  // ---- Internal types -------------------------------------------------

  private sealed trait ExtentLoc
  private final case class InlineLoc(slot: Int) extends ExtentLoc
  private final case class Ind1Loc(slot: Int) extends ExtentLoc
  private final case class Ind2Loc(ptrSlot: Int, slot: Int) extends ExtentLoc
  private final case class Ind3Loc(ptr3Slot: Int, ptr2Slot: Int, slot: Int)
      extends ExtentLoc

  // ---- Append helpers -------------------------------------------------

  private def appendOneConcrete(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      phys: Int,
  ): Inode =
    findLastExtent(ino, dev) match
      case Some((loc, e))
          if !e.sparse && !e.uninitialized
            && phys == e.start + e.count
            && canBumpCount(e.count, 1) =>
        writeExtentAt(ino, dev, loc, e.copy(count = e.count + 1))
      case _ =>
        appendNewExtent(ino, dev, bm, Extent(start = phys, count = 1))

  /** Bumping `cur` by `delta` would not overflow the 32-bit count. */
  private def canBumpCount(cur: Int, delta: Int): Boolean =
    cur >= 0 && delta >= 0 && (Int.MaxValue - cur) >= delta

  /** Place a fresh extent at the next free slot, allocating indirect
    * blocks as required by the lazy tier-promotion rule. */
  private def appendNewExtent(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      e: Extent,
  ): Inode =
    // Inline tier --------------------------------------------------
    val inlineXs = ino.body match
      case InodeBody.Extents(xs) => xs
      case InodeBody.InlineSymlink(_) =>
        throw new SfsCorruptError(
          "appendNewExtent: cannot append to inline-symlink inode",
        )
    val inlineFree = firstEmptySlot(inlineXs)
    if inlineFree >= 0 then
      return writeExtentAt(ino, dev, InlineLoc(inlineFree), e)

    // Indirect-1 tier ----------------------------------------------
    val (ino1, ind1Addr) = ensureInd1(ino, dev, bm)
    val xs1 = readExtBlock(dev, ind1Addr)
    val slot1 = firstEmptySlotArr(xs1)
    if slot1 >= 0 then
      xs1(slot1) = e
      writeExtBlock(dev, ind1Addr, xs1)
      return ino1

    // Indirect-2 tier ----------------------------------------------
    val (ino2, ind2Addr) = ensureInd2(ino1, dev, bm)
    val ptrs2 = readPtrBlock(dev, ind2Addr)
    val tailPtr2 = lastNonZeroPtr(ptrs2)
    if tailPtr2 >= 0 then
      val tailInd1 = ptrs2(tailPtr2)
      val xs = readExtBlock(dev, tailInd1)
      val slot = firstEmptySlotArr(xs)
      if slot >= 0 then
        xs(slot) = e
        writeExtBlock(dev, tailInd1, xs)
        return ino2
    val nextPtr2 = tailPtr2 + 1
    if nextPtr2 < IndirectPointerBlock.Capacity then
      val newInd1 = bm.allocate().getOrElse {
        throw new SfsCorruptError("appendNewExtent: out of space (ind1 under ind2)")
      }
      initEmptyExtBlock(dev, newInd1)
      ptrs2(nextPtr2) = newInd1
      writePtrBlock(dev, ind2Addr, ptrs2)
      val xs = readExtBlock(dev, newInd1)
      xs(0) = e
      writeExtBlock(dev, newInd1, xs)
      return ino2

    // Indirect-3 tier ----------------------------------------------
    val (ino3, ind3Addr) = ensureInd3(ino2, dev, bm)
    val ptrs3 = readPtrBlock(dev, ind3Addr)
    val tailPtr3 = lastNonZeroPtr(ptrs3)
    if tailPtr3 >= 0 then
      val tailPtrs2Addr = ptrs3(tailPtr3)
      val tailPtrs2 = readPtrBlock(dev, tailPtrs2Addr)
      val tailPtr2InTail = lastNonZeroPtr(tailPtrs2)
      if tailPtr2InTail >= 0 then
        val tailInd1 = tailPtrs2(tailPtr2InTail)
        val xs = readExtBlock(dev, tailInd1)
        val slot = firstEmptySlotArr(xs)
        if slot >= 0 then
          xs(slot) = e
          writeExtBlock(dev, tailInd1, xs)
          return ino3
      val nextPtr2InTail = tailPtr2InTail + 1
      if nextPtr2InTail < IndirectPointerBlock.Capacity then
        val newInd1 = bm.allocate().getOrElse {
          throw new SfsCorruptError(
            "appendNewExtent: out of space (ind1 under ind3-leaf-ptrs)",
          )
        }
        initEmptyExtBlock(dev, newInd1)
        tailPtrs2(nextPtr2InTail) = newInd1
        writePtrBlock(dev, tailPtrs2Addr, tailPtrs2)
        val xs = readExtBlock(dev, newInd1)
        xs(0) = e
        writeExtBlock(dev, newInd1, xs)
        return ino3
    val nextPtr3 = tailPtr3 + 1
    if nextPtr3 >= IndirectPointerBlock.Capacity then
      throw new SfsCorruptError(
        "appendNewExtent: file exceeds maximum extent map capacity",
      )
    val newPtrs2Addr = bm.allocate().getOrElse {
      throw new SfsCorruptError("appendNewExtent: out of space (ind3-leaf-ptrs)")
    }
    initEmptyPtrBlock(dev, newPtrs2Addr)
    val newInd1Addr = bm.allocate().getOrElse {
      throw new SfsCorruptError("appendNewExtent: out of space (ind1 under fresh ind3-leaf-ptrs)")
    }
    initEmptyExtBlock(dev, newInd1Addr)
    val freshPtrs2 = new Array[Int](IndirectPointerBlock.Capacity)
    freshPtrs2(0) = newInd1Addr
    writePtrBlock(dev, newPtrs2Addr, freshPtrs2)
    ptrs3(nextPtr3) = newPtrs2Addr
    writePtrBlock(dev, ind3Addr, ptrs3)
    val xs = readExtBlock(dev, newInd1Addr)
    xs(0) = e
    writeExtBlock(dev, newInd1Addr, xs)
    ino3

  /** Allocate `indirect1` if not yet present. Returns updated inode and
    * the ind1 block address. */
  private def ensureInd1(ino: Inode, dev: BlockDevice, bm: Bitmap): (Inode, Int) =
    if (ino.flags & InodeFlagHasIndirect1) != 0 then (ino, ino.indirect1)
    else
      val addr = bm.allocate().getOrElse {
        throw new SfsCorruptError("ensureInd1: out of free blocks")
      }
      initEmptyExtBlock(dev, addr)
      val updated = ino.copy(
        indirect1 = addr,
        flags = ino.flags | InodeFlagHasIndirect1,
      )
      (updated, addr)

  private def ensureInd2(ino: Inode, dev: BlockDevice, bm: Bitmap): (Inode, Int) =
    if (ino.flags & InodeFlagHasIndirect2) != 0 then (ino, ino.indirect2)
    else
      val addr = bm.allocate().getOrElse {
        throw new SfsCorruptError("ensureInd2: out of free blocks")
      }
      initEmptyPtrBlock(dev, addr)
      val updated = ino.copy(
        indirect2 = addr,
        flags = ino.flags | InodeFlagHasIndirect2,
      )
      (updated, addr)

  private def ensureInd3(ino: Inode, dev: BlockDevice, bm: Bitmap): (Inode, Int) =
    if (ino.flags & InodeFlagHasIndirect3) != 0 then (ino, ino.indirect3)
    else
      val addr = bm.allocate().getOrElse {
        throw new SfsCorruptError("ensureInd3: out of free blocks")
      }
      initEmptyPtrBlock(dev, addr)
      val updated = ino.copy(
        indirect3 = addr,
        flags = ino.flags | InodeFlagHasIndirect3,
      )
      (updated, addr)

  // ---- Last-extent and write-back helpers ----------------------------

  private def findLastExtent(
      ino: Inode,
      dev: BlockDevice,
  ): Option[(ExtentLoc, Extent)] =
    if (ino.flags & InodeFlagHasIndirect3) != 0 then
      val ptrs3 = readPtrBlock(dev, ino.indirect3)
      val ptr3Slot = lastNonZeroPtr(ptrs3)
      if ptr3Slot < 0 then
        throw new SfsCorruptError(
          "findLastExtent: HAS_INDIRECT3 set but ind3 ptr block empty",
        )
      val ptrs2 = readPtrBlock(dev, ptrs3(ptr3Slot))
      val ptr2Slot = lastNonZeroPtr(ptrs2)
      if ptr2Slot < 0 then
        throw new SfsCorruptError(
          "findLastExtent: ind3-leaf ptr block empty",
        )
      val xs = readExtBlock(dev, ptrs2(ptr2Slot))
      val slot = lastFilledExtArr(xs)
      if slot < 0 then
        throw new SfsCorruptError(
          "findLastExtent: ind3 leaf extent block empty",
        )
      Some((Ind3Loc(ptr3Slot, ptr2Slot, slot), xs(slot)))
    else if (ino.flags & InodeFlagHasIndirect2) != 0 then
      val ptrs2 = readPtrBlock(dev, ino.indirect2)
      val ptrSlot = lastNonZeroPtr(ptrs2)
      if ptrSlot < 0 then
        throw new SfsCorruptError(
          "findLastExtent: HAS_INDIRECT2 set but ind2 ptr block empty",
        )
      val xs = readExtBlock(dev, ptrs2(ptrSlot))
      val slot = lastFilledExtArr(xs)
      if slot < 0 then
        throw new SfsCorruptError(
          "findLastExtent: ind2 leaf extent block empty",
        )
      Some((Ind2Loc(ptrSlot, slot), xs(slot)))
    else if (ino.flags & InodeFlagHasIndirect1) != 0 then
      val xs = readExtBlock(dev, ino.indirect1)
      val slot = lastFilledExtArr(xs)
      if slot < 0 then
        throw new SfsCorruptError(
          "findLastExtent: HAS_INDIRECT1 set but ind1 block empty",
        )
      Some((Ind1Loc(slot), xs(slot)))
    else
      ino.body match
        case InodeBody.Extents(xs) =>
          val slot = lastFilledExt(xs)
          if slot < 0 then None else Some((InlineLoc(slot), xs(slot)))
        case InodeBody.InlineSymlink(_) => None

  private def writeExtentAt(
      ino: Inode,
      dev: BlockDevice,
      loc: ExtentLoc,
      e: Extent,
  ): Inode =
    loc match
      case InlineLoc(slot) =>
        ino.body match
          case InodeBody.Extents(xs) =>
            ino.copy(body = InodeBody.Extents(xs.updated(slot, e)))
          case InodeBody.InlineSymlink(_) =>
            throw new SfsCorruptError(
              "writeExtentAt: cannot write extent to inline-symlink inode",
            )
      case Ind1Loc(slot) =>
        val xs = readExtBlock(dev, ino.indirect1)
        xs(slot) = e
        writeExtBlock(dev, ino.indirect1, xs)
        ino
      case Ind2Loc(ptrSlot, slot) =>
        val ptrs = readPtrBlock(dev, ino.indirect2)
        val ind1Addr = ptrs(ptrSlot)
        val xs = readExtBlock(dev, ind1Addr)
        xs(slot) = e
        writeExtBlock(dev, ind1Addr, xs)
        ino
      case Ind3Loc(ptr3Slot, ptr2Slot, slot) =>
        val ptrs3 = readPtrBlock(dev, ino.indirect3)
        val ptrs2 = readPtrBlock(dev, ptrs3(ptr3Slot))
        val ind1Addr = ptrs2(ptr2Slot)
        val xs = readExtBlock(dev, ind1Addr)
        xs(slot) = e
        writeExtBlock(dev, ind1Addr, xs)
        ino

  // ---- Truncate -------------------------------------------------------

  private def truncateImpl(
      ino: Inode,
      dev: BlockDevice,
      bm: Bitmap,
      target: Long,
  ): Inode =
    // Inline tier -------------------------------------------------
    var cur = ino
    var rem = target
    val inlineXs = cur.body match
      case InodeBody.Extents(xs)        => xs.toArray
      case InodeBody.InlineSymlink(_)   => return cur
    val (remAfterInline, inlineMod) = trimExtents(inlineXs, rem, dev, bm)
    if inlineMod then
      cur = cur.copy(body = InodeBody.Extents(inlineXs.toIndexedSeq))
    rem = remAfterInline

    // Indirect-1 tier ---------------------------------------------
    if (cur.flags & InodeFlagHasIndirect1) != 0 then
      val ind1Addr = cur.indirect1
      val xs = readExtBlock(dev, ind1Addr)
      val (newRem, mod) = trimExtents(xs, rem, dev, bm)
      rem = newRem
      val emptyAfter = isEmptyExtArr(xs)
      if emptyAfter then
        bm.free(ind1Addr)
        cur = cur.copy(
          indirect1 = 0,
          flags = cur.flags & ~InodeFlagHasIndirect1,
        )
      else if mod then writeExtBlock(dev, ind1Addr, xs)

    // Indirect-2 tier ---------------------------------------------
    if (cur.flags & InodeFlagHasIndirect2) != 0 then
      val ind2Addr = cur.indirect2
      val ptrs = readPtrBlock(dev, ind2Addr)
      var ptrsMod = false
      var i = 0
      while i < ptrs.length && ptrs(i) != 0 do
        val ind1Addr = ptrs(i)
        val xs = readExtBlock(dev, ind1Addr)
        val (newRem, mod) = trimExtents(xs, rem, dev, bm)
        rem = newRem
        if isEmptyExtArr(xs) then
          bm.free(ind1Addr)
          ptrs(i) = 0
          ptrsMod = true
        else if mod then writeExtBlock(dev, ind1Addr, xs)
        i += 1
      val emptyAfter = isEmptyPtrArr(ptrs)
      if emptyAfter then
        bm.free(ind2Addr)
        cur = cur.copy(
          indirect2 = 0,
          flags = cur.flags & ~InodeFlagHasIndirect2,
        )
      else if ptrsMod then writePtrBlock(dev, ind2Addr, ptrs)

    // Indirect-3 tier ---------------------------------------------
    if (cur.flags & InodeFlagHasIndirect3) != 0 then
      val ind3Addr = cur.indirect3
      val ptrs3 = readPtrBlock(dev, ind3Addr)
      var ptrs3Mod = false
      var i = 0
      while i < ptrs3.length && ptrs3(i) != 0 do
        val ptrs2Addr = ptrs3(i)
        val ptrs2 = readPtrBlock(dev, ptrs2Addr)
        var ptrs2Mod = false
        var j = 0
        while j < ptrs2.length && ptrs2(j) != 0 do
          val ind1Addr = ptrs2(j)
          val xs = readExtBlock(dev, ind1Addr)
          val (newRem, mod) = trimExtents(xs, rem, dev, bm)
          rem = newRem
          if isEmptyExtArr(xs) then
            bm.free(ind1Addr)
            ptrs2(j) = 0
            ptrs2Mod = true
          else if mod then writeExtBlock(dev, ind1Addr, xs)
          j += 1
        if isEmptyPtrArr(ptrs2) then
          bm.free(ptrs2Addr)
          ptrs3(i) = 0
          ptrs3Mod = true
        else if ptrs2Mod then writePtrBlock(dev, ptrs2Addr, ptrs2)
        i += 1
      val emptyAfter = isEmptyPtrArr(ptrs3)
      if emptyAfter then
        bm.free(ind3Addr)
        cur = cur.copy(
          indirect3 = 0,
          flags = cur.flags & ~InodeFlagHasIndirect3,
        )
      else if ptrs3Mod then writePtrBlock(dev, ind3Addr, ptrs3)

    cur

  /** Trim the given extent slice to the first `rem` logical blocks.
    * Frees physical blocks of fully-discarded concrete extents and
    * the freed suffix of a straddling concrete extent. Returns the
    * leftover `rem` (logical blocks still owed past these extents)
    * and whether any slot was mutated. */
  private def trimExtents(
      xs: Array[Extent],
      rem0: Long,
      dev: BlockDevice,
      bm: Bitmap,
  ): (Long, Boolean) =
    var rem = rem0
    var modified = false
    var i = 0
    while i < xs.length do
      val e = xs(i)
      if e.count == 0 then
        // End of this tier's contents. Loop drops out below; remaining
        // slots are already empty so nothing to do.
        i = xs.length
      else if e.count.toLong <= rem then
        rem -= e.count.toLong
        i += 1
      else if rem > 0 then
        val keep = rem.toInt
        val freed = e.count - keep
        if !e.sparse && !e.uninitialized then
          bm.freeRange(e.start + keep, freed)
        xs(i) = e.copy(count = keep)
        modified = true
        rem = 0
        i += 1
      else // rem == 0
        if !e.sparse && !e.uninitialized then
          bm.freeRange(e.start, e.count)
        xs(i) = Extent.Empty
        modified = true
        i += 1
    (rem, modified)

  // ---- Block-level scans ----------------------------------------------

  private def firstEmptySlot(xs: IndexedSeq[Extent]): Int =
    var i = 0
    while i < xs.length do
      if xs(i).count == 0 then return i
      i += 1
    -1

  private def firstEmptySlotArr(xs: Array[Extent]): Int =
    var i = 0
    while i < xs.length do
      if xs(i).count == 0 then return i
      i += 1
    -1

  private def lastFilledExt(xs: IndexedSeq[Extent]): Int =
    var i = xs.length - 1
    while i >= 0 && xs(i).count == 0 do i -= 1
    i

  private def lastFilledExtArr(xs: Array[Extent]): Int =
    var i = xs.length - 1
    while i >= 0 && xs(i).count == 0 do i -= 1
    i

  private def lastNonZeroPtr(ptrs: Array[Int]): Int =
    var i = ptrs.length - 1
    while i >= 0 && ptrs(i) == 0 do i -= 1
    i

  private def isEmptyExtArr(xs: Array[Extent]): Boolean =
    var i = 0
    while i < xs.length do
      if xs(i).count != 0 then return false
      i += 1
    true

  private def isEmptyPtrArr(ptrs: Array[Int]): Boolean =
    var i = 0
    while i < ptrs.length do
      if ptrs(i) != 0 then return false
      i += 1
    true

  // ---- Low-level block I/O --------------------------------------------

  private def readExtBlock(dev: BlockDevice, blockAddr: Int): Array[Extent] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockAddr.toLong, buf)
    IndirectExtentBlock.unpack(buf, 0).toArray

  private def writeExtBlock(
      dev: BlockDevice,
      blockAddr: Int,
      xs: Array[Extent],
  ): Unit =
    val buf = new Array[Byte](BlockSize)
    IndirectExtentBlock.pack(xs.toIndexedSeq, buf, 0)
    dev.writeBlock(blockAddr.toLong, buf)

  private def readPtrBlock(dev: BlockDevice, blockAddr: Int): Array[Int] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockAddr.toLong, buf)
    IndirectPointerBlock.unpack(buf, 0).toArray

  private def writePtrBlock(
      dev: BlockDevice,
      blockAddr: Int,
      ptrs: Array[Int],
  ): Unit =
    val buf = new Array[Byte](BlockSize)
    IndirectPointerBlock.pack(ptrs.toIndexedSeq, buf, 0)
    dev.writeBlock(blockAddr.toLong, buf)

  private def initEmptyExtBlock(dev: BlockDevice, blockAddr: Int): Unit =
    val buf = new Array[Byte](BlockSize)
    dev.writeBlock(blockAddr.toLong, buf)

  private def initEmptyPtrBlock(dev: BlockDevice, blockAddr: Int): Unit =
    val buf = new Array[Byte](BlockSize)
    dev.writeBlock(blockAddr.toLong, buf)
