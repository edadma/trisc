package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*
import BlockMapping.*

class ExtentReaderTests extends AnyFreeSpec with Matchers:

  // ---- inode/disk helpers --------------------------------------------

  /** Build an inode whose inline extents are `extents` followed by enough
    * Empty slots to fill the 16-slot union. */
  private def fileInode(
      extents: Seq[Extent],
      flags: Int = 0,
      indirect1: Int = 0,
      indirect2: Int = 0,
      indirect3: Int = 0,
  ): Inode =
    val padded =
      (extents ++ Seq.fill(InlineExtents - extents.length)(Extent.Empty)).take(InlineExtents).toIndexedSeq
    Inode(
      mode = 0x81a4,
      linkCount = 1,
      uid = 0,
      gid = 0,
      flags = flags,
      size = 0L,
      blockCount = 0,
      generation = 0,
      atimeSec = 0, atimeNsec = 0,
      mtimeSec = 0, mtimeNsec = 0,
      ctimeSec = 0, ctimeNsec = 0,
      crtimeSec = 0, crtimeNsec = 0,
      body = InodeBody.Extents(padded),
      indirect1 = indirect1,
      indirect2 = indirect2,
      indirect3 = indirect3,
      xattrBlock = 0,
    )

  /** Pack `xs` into an [[IndirectExtentBlock]] at `addr`, padding with
    * Empty extents. */
  private def writeExtentBlock(dev: BlockDevice, addr: Int, xs: Seq[Extent]): Unit =
    val buf = new Array[Byte](BlockSize)
    IndirectExtentBlock.pack(xs, buf, 0)
    dev.writeBlock(addr.toLong, buf)

  /** Pack `ptrs` into an [[IndirectPointerBlock]] at `addr`. */
  private def writePointerBlock(dev: BlockDevice, addr: Int, ptrs: Seq[Int]): Unit =
    val buf = new Array[Byte](BlockSize)
    IndirectPointerBlock.pack(ptrs, buf, 0)
    dev.writeBlock(addr.toLong, buf)

  // A device large enough to host a few indirect blocks. We don't actually
  // care that it isn't a formatted SFS — ExtentReader only does block reads.
  private def freshDevice(): RamBlockDevice = new RamBlockDevice(1024)

  // ---- tests -----------------------------------------------------------

  "inline extents only" - {

    "maps logical blocks within a single concrete extent" in {
      val dev = freshDevice()
      val ino = fileInode(Seq(Extent(start = 100, count = 5)))
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(0) shouldBe Concrete(100L)
      r.physicalBlock(1) shouldBe Concrete(101L)
      r.physicalBlock(4) shouldBe Concrete(104L)
    }

    "spans multiple inline extents" in {
      val dev = freshDevice()
      val ino = fileInode(
        Seq(Extent(100, 3), Extent(200, 2), Extent(300, 4)),
      )
      val r = new ExtentReader(dev, ino)
      // extent 0 covers logical 0..2 → physical 100..102
      r.physicalBlock(2) shouldBe Concrete(102L)
      // extent 1 covers logical 3..4 → physical 200..201
      r.physicalBlock(3) shouldBe Concrete(200L)
      r.physicalBlock(4) shouldBe Concrete(201L)
      // extent 2 covers logical 5..8 → physical 300..303
      r.physicalBlock(5) shouldBe Concrete(300L)
      r.physicalBlock(8) shouldBe Concrete(303L)
    }

    "returns OutOfRange past the last inline extent" in {
      val dev = freshDevice()
      val ino = fileInode(Seq(Extent(100, 5)))
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(5) shouldBe OutOfRange
      r.physicalBlock(1000) shouldBe OutOfRange
    }

    "returns Sparse for a sparse extent" in {
      val dev = freshDevice()
      val ino = fileInode(
        Seq(Extent(100, 2), Extent(0, 10, sparse = true), Extent(200, 1)),
      )
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(0) shouldBe Concrete(100L)
      r.physicalBlock(2) shouldBe Sparse
      r.physicalBlock(11) shouldBe Sparse
      r.physicalBlock(12) shouldBe Concrete(200L)
    }

    "returns Uninitialized for a preallocated extent" in {
      val dev = freshDevice()
      val ino = fileInode(
        Seq(Extent(100, 3), Extent(500, 5, uninitialized = true)),
      )
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(2) shouldBe Concrete(102L)
      r.physicalBlock(3) shouldBe Uninitialized
      r.physicalBlock(7) shouldBe Uninitialized
      r.physicalBlock(8) shouldBe OutOfRange
    }

    "fills all 16 inline slots" in {
      val dev = freshDevice()
      // 16 extents of 1 block each; logical 0..15 maps to physical 1000..1015
      val xs = (0 until InlineExtents).map(i => Extent(start = 1000 + i, count = 1))
      val ino = fileInode(xs)
      val r = new ExtentReader(dev, ino)
      for i <- 0 until InlineExtents do r.physicalBlock(i) shouldBe Concrete((1000 + i).toLong)
      r.physicalBlock(InlineExtents) shouldBe OutOfRange
    }

    "rejects negative logical block indices" in {
      val dev = freshDevice()
      val r = new ExtentReader(dev, fileInode(Seq(Extent(100, 1))))
      an[IllegalArgumentException] should be thrownBy r.physicalBlock(-1)
    }
  }

  "single-indirect" - {

    "indirect1 picks up where inline extents end" in {
      val dev = freshDevice()
      val ind1Addr = 50
      // inline: 16 extents × 1 block = 16 logical blocks at physical 100..115
      val inline = (0 until InlineExtents).map(i => Extent(100 + i, 1))
      // indirect1: 5 extents × 1 block at physical 200..204
      writeExtentBlock(dev, ind1Addr, (0 until 5).map(i => Extent(200 + i, 1)))
      val ino = fileInode(inline, flags = InodeFlagHasIndirect1, indirect1 = ind1Addr)
      val r = new ExtentReader(dev, ino)
      // last inline block
      r.physicalBlock(15) shouldBe Concrete(115L)
      // first indirect1 block
      r.physicalBlock(16) shouldBe Concrete(200L)
      // last indirect1 block
      r.physicalBlock(20) shouldBe Concrete(204L)
      // past indirect1
      r.physicalBlock(21) shouldBe OutOfRange
    }

    "fills the full 512-extent capacity of indirect1" in {
      val dev = freshDevice()
      val ind1Addr = 50
      // No inline extents (count=0); indirect1 has 512 × 1 block at 1000..1511
      val ext = (0 until IndirectExtentBlock.Capacity).map(i => Extent(1000 + i, 1))
      writeExtentBlock(dev, ind1Addr, ext)
      val ino = fileInode(Seq.empty, flags = InodeFlagHasIndirect1, indirect1 = ind1Addr)
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(0) shouldBe Concrete(1000L)
      r.physicalBlock(511) shouldBe Concrete(1511L)
      r.physicalBlock(512) shouldBe OutOfRange
    }
  }

  "double-indirect" - {

    "walks indirect2 → indirect-extent-block → extent" in {
      val dev = freshDevice()
      val ind2Addr = 60
      val leaf0Addr = 70
      val leaf1Addr = 80
      writeExtentBlock(dev, leaf0Addr, Seq(Extent(start = 5000, count = 4)))
      writeExtentBlock(dev, leaf1Addr, Seq(Extent(start = 6000, count = 3)))
      writePointerBlock(dev, ind2Addr, Seq(leaf0Addr, leaf1Addr))
      val ino = fileInode(Seq.empty, flags = InodeFlagHasIndirect2, indirect2 = ind2Addr)
      val r = new ExtentReader(dev, ino)
      // first leaf, 4 blocks
      r.physicalBlock(0) shouldBe Concrete(5000L)
      r.physicalBlock(3) shouldBe Concrete(5003L)
      // second leaf, 3 blocks
      r.physicalBlock(4) shouldBe Concrete(6000L)
      r.physicalBlock(6) shouldBe Concrete(6002L)
      r.physicalBlock(7) shouldBe OutOfRange
    }

    "treats a zero pointer as end-of-map" in {
      val dev = freshDevice()
      val ind2Addr = 60
      val leafAddr = 70
      writeExtentBlock(dev, leafAddr, Seq(Extent(7000, 2)))
      // first pointer valid; rest of pointer block is zero
      writePointerBlock(dev, ind2Addr, Seq(leafAddr))
      val ino = fileInode(Seq.empty, flags = InodeFlagHasIndirect2, indirect2 = ind2Addr)
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(1) shouldBe Concrete(7001L)
      r.physicalBlock(2) shouldBe OutOfRange
    }
  }

  "triple-indirect" - {

    "walks indirect3 → indirect2 → indirect-extent-block → extent" in {
      val dev = freshDevice()
      val ind3Addr = 90
      val ind2Addr = 100
      val leafAddr = 110
      writeExtentBlock(dev, leafAddr, Seq(Extent(start = 50_000, count = 8)))
      writePointerBlock(dev, ind2Addr, Seq(leafAddr))
      writePointerBlock(dev, ind3Addr, Seq(ind2Addr))
      val ino = fileInode(Seq.empty, flags = InodeFlagHasIndirect3, indirect3 = ind3Addr)
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(0) shouldBe Concrete(50_000L)
      r.physicalBlock(7) shouldBe Concrete(50_007L)
      r.physicalBlock(8) shouldBe OutOfRange
    }
  }

  "all four tiers chained" - {

    "logical address space rolls inline → ind1 → ind2 → ind3 in order" in {
      val dev = freshDevice()
      val ind1Addr = 200
      val ind2Addr = 300
      val ind2LeafAddr = 310
      val ind3Addr = 400
      val ind3MidAddr = 410
      val ind3LeafAddr = 420

      // inline: 1 extent of 16 blocks at 1000
      val inline = Seq(Extent(start = 1000, count = 16))
      // indirect1: 1 extent of 32 blocks at 2000
      writeExtentBlock(dev, ind1Addr, Seq(Extent(start = 2000, count = 32)))
      // indirect2 → leaf with 1 extent of 64 blocks at 3000
      writeExtentBlock(dev, ind2LeafAddr, Seq(Extent(start = 3000, count = 64)))
      writePointerBlock(dev, ind2Addr, Seq(ind2LeafAddr))
      // indirect3 → indirect2 → leaf with 1 extent of 128 blocks at 4000
      writeExtentBlock(dev, ind3LeafAddr, Seq(Extent(start = 4000, count = 128)))
      writePointerBlock(dev, ind3MidAddr, Seq(ind3LeafAddr))
      writePointerBlock(dev, ind3Addr, Seq(ind3MidAddr))

      val ino = fileInode(
        inline,
        flags = InodeFlagHasIndirect1 | InodeFlagHasIndirect2 | InodeFlagHasIndirect3,
        indirect1 = ind1Addr,
        indirect2 = ind2Addr,
        indirect3 = ind3Addr,
      )
      val r = new ExtentReader(dev, ino)

      // inline: 0..15 → 1000..1015
      r.physicalBlock(0) shouldBe Concrete(1000L)
      r.physicalBlock(15) shouldBe Concrete(1015L)
      // indirect1: 16..47 → 2000..2031
      r.physicalBlock(16) shouldBe Concrete(2000L)
      r.physicalBlock(47) shouldBe Concrete(2031L)
      // indirect2: 48..111 → 3000..3063
      r.physicalBlock(48) shouldBe Concrete(3000L)
      r.physicalBlock(111) shouldBe Concrete(3063L)
      // indirect3: 112..239 → 4000..4127
      r.physicalBlock(112) shouldBe Concrete(4000L)
      r.physicalBlock(239) shouldBe Concrete(4127L)
      // past the last extent
      r.physicalBlock(240) shouldBe OutOfRange
    }
  }

  "inline-symlink inode" - {

    "returns OutOfRange for any logical block" in {
      val dev = freshDevice()
      val ino = Inode(
        mode = 0xa1ff,
        linkCount = 1,
        uid = 0,
        gid = 0,
        flags = InodeFlagInlineSymlink,
        size = 4L,
        blockCount = 0,
        generation = 0,
        atimeSec = 0, atimeNsec = 0,
        mtimeSec = 0, mtimeNsec = 0,
        ctimeSec = 0, ctimeNsec = 0,
        crtimeSec = 0, crtimeNsec = 0,
        body = InodeBody.InlineSymlink("/etc"),
        indirect1 = 0,
        indirect2 = 0,
        indirect3 = 0,
        xattrBlock = 0,
      )
      val r = new ExtentReader(dev, ino)
      r.physicalBlock(0) shouldBe OutOfRange
      r.physicalBlock(99) shouldBe OutOfRange
    }
  }
