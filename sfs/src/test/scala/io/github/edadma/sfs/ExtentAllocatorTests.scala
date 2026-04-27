package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class ExtentAllocatorTests extends AnyFreeSpec with Matchers:

  // ---- Test helpers ----------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "rttest",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  /** Format a fresh device + load its block bitmap. */
  private def fresh(): (RamBlockDevice, Layout, Bitmap) =
    val dev = RamBlockDevice.default()
    val layout = Sfs.format(dev, smallOpts)
    val bm = new Bitmap(
      dev,
      startBlock = layout.blockBitmapStart.toLong,
      lengthBlocks = layout.blockBitmapLen,
      totalBits = layout.totalBlocks,
    )
    bm.load()
    (dev, layout, bm)

  /** A fresh, empty regular-file inode with no extents. */
  private def blankInode(): Inode = Inode(
    mode = 0x81a4,
    linkCount = 1,
    uid = 0,
    gid = 0,
    flags = 0,
    size = 0L,
    blockCount = 0,
    generation = 0,
    atimeSec = 0,
    atimeNsec = 0,
    mtimeSec = 0,
    mtimeNsec = 0,
    ctimeSec = 0,
    ctimeNsec = 0,
    crtimeSec = 0,
    crtimeNsec = 0,
    body = InodeBody.EmptyExtents,
    indirect1 = 0,
    indirect2 = 0,
    indirect3 = 0,
    xattrBlock = 0,
  )

  /** Helper to read inline extents out of an inode body. */
  private def inlineXs(ino: Inode): IndexedSeq[Extent] =
    ino.body match
      case InodeBody.Extents(xs)      => xs
      case InodeBody.InlineSymlink(_) =>
        fail("expected Extents body, got InlineSymlink")

  /** Read the extents stored in an indirect-1 block. */
  private def readInd1(dev: BlockDevice, addr: Int): IndexedSeq[Extent] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(addr.toLong, buf)
    IndirectExtentBlock.unpack(buf, 0)

  /** Read the pointers stored in an indirect-{2,3} pointer block. */
  private def readPtrs(dev: BlockDevice, addr: Int): IndexedSeq[Int] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(addr.toLong, buf)
    IndirectPointerBlock.unpack(buf, 0)

  /** Pre-mark a long run of every-other data blocks as allocated, so
    * that subsequent `bm.allocate()` calls return non-contiguous odd
    * physical addresses (gap of 2 between successive allocations). The
    * cushion past `n` covers interleaved metadata allocations (e.g.
    * indirect-block creation) so that the file-block allocs near the
    * tail of the alternating fence stay non-contiguous too. */
  private def reserveAlternating(bm: Bitmap, layout: Layout, n: Int): Unit =
    var b = layout.dataStart
    var i = 0
    val total = n + 5
    while i < total do
      bm.set(b)
      i += 1
      b += 2

  /** Hand-craft a fresh ind1 leaf block on disk, fully filled with sparse
    * extents (count=1 each). Returns the block's disk address (which is
    * also marked allocated in the bitmap). */
  private def buildFullSparseInd1Leaf(dev: BlockDevice, bm: Bitmap): Int =
    val addr = bm.allocate().getOrElse(throw new RuntimeException("no space"))
    val xs = IndexedSeq.fill(IndirectExtentBlock.Capacity)(Extent(0, 1, sparse = true))
    val buf = new Array[Byte](BlockSize)
    IndirectExtentBlock.pack(xs, buf, 0)
    dev.writeBlock(addr.toLong, buf)
    addr

  /** Hand-craft a fresh extent block with a single sparse extent at slot 0. */
  private def buildSingleSparseInd1Leaf(dev: BlockDevice, bm: Bitmap): Int =
    val addr = bm.allocate().getOrElse(throw new RuntimeException("no space"))
    val xs = Extent(0, 1, sparse = true) +:
      IndexedSeq.fill(IndirectExtentBlock.Capacity - 1)(Extent.Empty)
    val buf = new Array[Byte](BlockSize)
    IndirectExtentBlock.pack(xs, buf, 0)
    dev.writeBlock(addr.toLong, buf)
    addr

  /** Hand-craft a pointer block with `ptrs(0) = first`, the rest 0. */
  private def buildSinglePtrBlock(
      dev: BlockDevice,
      bm: Bitmap,
      first: Int,
  ): Int =
    val addr = bm.allocate().getOrElse(throw new RuntimeException("no space"))
    val ptrs = first +: IndexedSeq.fill(IndirectPointerBlock.Capacity - 1)(0)
    val buf = new Array[Byte](BlockSize)
    IndirectPointerBlock.pack(ptrs, buf, 0)
    dev.writeBlock(addr.toLong, buf)
    addr

  // ---- append (concrete) -----------------------------------------------

  "append" - {

    "0 leaves the inode unchanged" in {
      val (dev, _, bm) = fresh()
      val ino = blankInode()
      val before = bm.freeCount
      val after = ExtentAllocator.append(ino, dev, bm, 0)
      after shouldBe ino
      bm.freeCount shouldBe before
    }

    "1 places a single concrete extent in inline slot 0" in {
      val (dev, layout, bm) = fresh()
      val freeBefore = bm.freeCount
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 1)
      val xs = inlineXs(ino)
      xs(0).count shouldBe 1
      xs(0).sparse shouldBe false
      xs(0).uninitialized shouldBe false
      xs(0).start should be >= layout.dataStart
      xs.drop(1).foreach(_.count shouldBe 0)
      bm.freeCount shouldBe (freeBefore - 1)
      (ino.flags & InodeFlagHasIndirect1) shouldBe 0
    }

    "5 contiguous blocks coalesce into a single extent (count=5)" in {
      val (dev, _, bm) = fresh()
      val freeBefore = bm.freeCount
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 5)
      val xs = inlineXs(ino)
      xs(0).count shouldBe 5
      xs(0).sparse shouldBe false
      xs(1).count shouldBe 0
      bm.freeCount shouldBe (freeBefore - 5)
    }

    "16 contiguous blocks still coalesce into a single inline extent" in {
      val (dev, _, bm) = fresh()
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 16)
      val xs = inlineXs(ino)
      xs(0).count shouldBe 16
      xs(1).count shouldBe 0
      (ino.flags & InodeFlagHasIndirect1) shouldBe 0
    }

    "non-contiguous block allocations land in separate extents" in {
      val (dev, layout, bm) = fresh()
      // Reserve 16 alternating blocks → next 16 allocations are odd-block
      // addresses, none contiguous, so 16 separate inline extents.
      reserveAlternating(bm, layout, 16)
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 16)
      val xs = inlineXs(ino)
      // All 16 inline slots filled with count=1 each, distinct starts.
      xs.foreach(_.count shouldBe 1)
      xs.map(_.start).toSet.size shouldBe 16
      // Inline tier exactly full, no spill into ind1 yet.
      (ino.flags & InodeFlagHasIndirect1) shouldBe 0
      ino.indirect1 shouldBe 0
    }

    "spilling past 16 separate extents allocates an indirect-1 block" in {
      val (dev, layout, bm) = fresh()
      // Reserve enough alternating bits for 17 non-contig allocations.
      reserveAlternating(bm, layout, 17)
      val freeBefore = bm.freeCount
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 17)
      val xs = inlineXs(ino)
      xs.foreach(_.count shouldBe 1)
      (ino.flags & InodeFlagHasIndirect1) shouldBe InodeFlagHasIndirect1
      ino.indirect1 should not be 0
      val ind1 = readInd1(dev, ino.indirect1)
      ind1(0).count shouldBe 1
      ind1(0).sparse shouldBe false
      ind1(1).count shouldBe 0
      // 17 file blocks + 1 indirect-1 block = 18 allocations total.
      bm.freeCount shouldBe (freeBefore - 18)
    }
  }

  // ---- coalescing across calls ----------------------------------------

  "successive append(1) calls coalesce into one growing extent" in {
    val (dev, _, bm) = fresh()
    var ino = blankInode()
    ino = ExtentAllocator.append(ino, dev, bm, 1)
    ino = ExtentAllocator.append(ino, dev, bm, 1)
    ino = ExtentAllocator.append(ino, dev, bm, 1)
    val xs = inlineXs(ino)
    xs(0).count shouldBe 3
    xs(1).count shouldBe 0
  }

  // ---- appendSparse ----------------------------------------------------

  "appendSparse" - {

    "0 leaves the inode unchanged" in {
      val (dev, _, bm) = fresh()
      val ino = blankInode()
      ExtentAllocator.appendSparse(ino, dev, bm, 0) shouldBe ino
    }

    "creates a sparse extent (no physical block allocated)" in {
      val (dev, _, bm) = fresh()
      val freeBefore = bm.freeCount
      val ino = ExtentAllocator.appendSparse(blankInode(), dev, bm, 100)
      val xs = inlineXs(ino)
      xs(0).count shouldBe 100
      xs(0).sparse shouldBe true
      xs(0).start shouldBe 0
      bm.freeCount shouldBe freeBefore
    }

    "successive sparse appends coalesce into one slot" in {
      val (dev, _, bm) = fresh()
      var ino = blankInode()
      ino = ExtentAllocator.appendSparse(ino, dev, bm, 50)
      ino = ExtentAllocator.appendSparse(ino, dev, bm, 70)
      val xs = inlineXs(ino)
      xs(0).count shouldBe 120
      xs(0).sparse shouldBe true
      xs(1).count shouldBe 0
    }

    "sparse + concrete + sparse → 3 distinct extents" in {
      val (dev, _, bm) = fresh()
      var ino = blankInode()
      ino = ExtentAllocator.append(ino, dev, bm, 5)
      ino = ExtentAllocator.appendSparse(ino, dev, bm, 100)
      ino = ExtentAllocator.append(ino, dev, bm, 5)
      val xs = inlineXs(ino)
      xs(0).count shouldBe 5
      xs(0).sparse shouldBe false
      xs(1).count shouldBe 100
      xs(1).sparse shouldBe true
      xs(2).count shouldBe 5
      xs(2).sparse shouldBe false
      xs(3).count shouldBe 0
    }
  }

  // ---- inline → indirect-1 transition (already covered above) --------
  // ---- indirect-1 → indirect-2 transition ----------------------------

  "indirect-1 → indirect-2 spill" - {

    "appending past a fully-packed inline+ind1 allocates ind2 + new ind1" in {
      val (dev, _, bm) = fresh()

      // Hand-craft an inode that has all 16 inline extents filled with
      // sparse(count=1) and an indirect-1 block fully filled with 512
      // sparse(count=1) extents. Logically this is a 528-block sparse file.
      val ind1Addr = buildFullSparseInd1Leaf(dev, bm)
      val sparseExt = Extent(0, 1, sparse = true)
      val fullInline = InodeBody.Extents(IndexedSeq.fill(InlineExtents)(sparseExt))
      val base = blankInode().copy(
        body = fullInline,
        flags = InodeFlagHasIndirect1,
        indirect1 = ind1Addr,
      )

      val freeBefore = bm.freeCount
      // One more concrete block — must spill into the ind2 tier.
      val ino = ExtentAllocator.append(base, dev, bm, 1)

      (ino.flags & InodeFlagHasIndirect2) shouldBe InodeFlagHasIndirect2
      ino.indirect2 should not be 0
      val ptrs2 = readPtrs(dev, ino.indirect2)
      ptrs2(0) should not be 0
      ptrs2(1) shouldBe 0
      // The new ind1 leaf under ptrs2(0) should hold one concrete extent.
      val newLeaf = readInd1(dev, ptrs2(0))
      newLeaf(0).count shouldBe 1
      newLeaf(0).sparse shouldBe false
      newLeaf(1).count shouldBe 0
      // Ind1 stays where it was; HAS_INDIRECT1 still set.
      (ino.flags & InodeFlagHasIndirect1) shouldBe InodeFlagHasIndirect1
      ino.indirect1 shouldBe ind1Addr
      // 1 file data block + 1 ind2 ptr block + 1 new ind1 leaf = 3 allocations.
      bm.freeCount shouldBe (freeBefore - 3)
    }
  }

  // ---- truncate -------------------------------------------------------

  "truncate" - {

    "to 0 on a fresh inode is a no-op" in {
      val (dev, _, bm) = fresh()
      val ino = blankInode()
      val before = bm.freeCount
      ExtentAllocator.truncate(ino, dev, bm, 0L) shouldBe ino
      bm.freeCount shouldBe before
    }

    "to current size leaves blocks intact" in {
      val (dev, _, bm) = fresh()
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 5)
      val before = bm.freeCount
      val same = ExtentAllocator.truncate(ino, dev, bm, 5L)
      same shouldBe ino
      bm.freeCount shouldBe before
    }

    "shrinking a single extent trims its count and frees the suffix" in {
      val (dev, _, bm) = fresh()
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 10)
      val freeAfterAppend = bm.freeCount
      val after = ExtentAllocator.truncate(ino, dev, bm, 4L)
      val xs = inlineXs(after)
      xs(0).count shouldBe 4
      xs(0).start shouldBe inlineXs(ino)(0).start
      xs(1).count shouldBe 0
      // 6 trailing blocks freed.
      bm.freeCount shouldBe (freeAfterAppend + 6)
    }

    "truncating to 0 frees every concrete block (single inline extent)" in {
      val (dev, _, bm) = fresh()
      val freshFree = bm.freeCount
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 8)
      val after = ExtentAllocator.truncate(ino, dev, bm, 0L)
      after.body match
        case InodeBody.Extents(xs) => xs.foreach(_.count shouldBe 0)
        case _                     => fail("expected Extents body")
      bm.freeCount shouldBe freshFree
    }

    "truncating across an extent boundary: keep first, drop second" in {
      val (dev, _, bm) = fresh()
      var ino = blankInode()
      ino = ExtentAllocator.append(ino, dev, bm, 3) // extent 0: count=3
      ino = ExtentAllocator.appendSparse(ino, dev, bm, 4) // extent 1: sparse(4)
      ino = ExtentAllocator.append(ino, dev, bm, 2) // extent 2: count=2
      val freeBefore = bm.freeCount
      // Logical layout: [0..2] concrete(3) | [3..6] sparse(4) | [7..8] concrete(2)
      val after = ExtentAllocator.truncate(ino, dev, bm, 5L)
      val xs = inlineXs(after)
      xs(0).count shouldBe 3 // untouched
      xs(0).sparse shouldBe false
      xs(1).count shouldBe 2 // sparse trimmed from 4 → 2
      xs(1).sparse shouldBe true
      xs(2).count shouldBe 0 // tail concrete extent dropped
      // Only the 2 trailing concrete blocks are freed (sparse cost nothing).
      bm.freeCount shouldBe (freeBefore + 2)
    }

    "truncating an ind1-using file back to inline reclaims the indirect block" in {
      val (dev, layout, bm) = fresh()
      // Force 17 non-contig extents to spill into ind1.
      reserveAlternating(bm, layout, 17)
      val ino = ExtentAllocator.append(blankInode(), dev, bm, 17)
      (ino.flags & InodeFlagHasIndirect1) shouldBe InodeFlagHasIndirect1
      val freeAfterFill = bm.freeCount

      // Truncate to 0 → all 17 file blocks + the ind1 block are freed.
      val after = ExtentAllocator.truncate(ino, dev, bm, 0L)
      (after.flags & InodeFlagHasIndirect1) shouldBe 0
      after.indirect1 shouldBe 0
      after.body match
        case InodeBody.Extents(xs) => xs.foreach(_.count shouldBe 0)
        case _                     => fail("expected Extents body")
      bm.freeCount shouldBe (freeAfterFill + 17 + 1)
    }

    "truncating a hand-crafted ind3 inode to 0 reclaims every metadata block" in {
      val (dev, _, bm) = fresh()

      // A minimal "all four tiers" inode: each indirect tier holds exactly
      // one chain (single ptr / single sparse extent). This deliberately
      // violates the dense-fill invariant (real allocator output never looks
      // like this) but it's the cheapest way to exercise truncate's per-tier
      // reclamation paths without writing 1024+ block leaves.
      val ind1LeafTopAddr = buildSingleSparseInd1Leaf(dev, bm)
      val ind1LeafUnderInd2 = buildSingleSparseInd1Leaf(dev, bm)
      val ind2PtrAddr = buildSinglePtrBlock(dev, bm, ind1LeafUnderInd2)
      val ind1LeafUnderInd3 = buildSingleSparseInd1Leaf(dev, bm)
      val ind2MidUnderInd3 = buildSinglePtrBlock(dev, bm, ind1LeafUnderInd3)
      val ind3PtrAddr = buildSinglePtrBlock(dev, bm, ind2MidUnderInd3)

      val sparseHead = InodeBody.Extents(
        Extent(0, 1, sparse = true) +:
          IndexedSeq.fill(InlineExtents - 1)(Extent.Empty),
      )
      val ino = blankInode().copy(
        body = sparseHead,
        flags = InodeFlagHasIndirect1 | InodeFlagHasIndirect2 | InodeFlagHasIndirect3,
        indirect1 = ind1LeafTopAddr,
        indirect2 = ind2PtrAddr,
        indirect3 = ind3PtrAddr,
      )
      val freeBefore = bm.freeCount
      val after = ExtentAllocator.truncate(ino, dev, bm, 0L)

      (after.flags & InodeFlagHasIndirect1) shouldBe 0
      (after.flags & InodeFlagHasIndirect2) shouldBe 0
      (after.flags & InodeFlagHasIndirect3) shouldBe 0
      after.indirect1 shouldBe 0
      after.indirect2 shouldBe 0
      after.indirect3 shouldBe 0
      after.body match
        case InodeBody.Extents(xs) => xs.foreach(_.count shouldBe 0)
        case _                     => fail("expected Extents body")
      // 6 metadata blocks (3 ind1 leaves, 2 ind2 ptr blocks, 1 ind3 ptr
      // block) all reclaimed; sparse extents had no physical blocks.
      bm.freeCount shouldBe (freeBefore + 6)
    }
  }
