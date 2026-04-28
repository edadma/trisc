package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class FileIOTests extends AnyFreeSpec with Matchers:

  // ---- helpers --------------------------------------------------------

  private val FixedSec: Int = 0x6800_4321
  private val FixedNsec: Int = 1234

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "rttest",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def fresh(): (RamBlockDevice, Layout, Bitmap, Sfs) =
    val dev = RamBlockDevice.default()
    val layout = Sfs.format(dev, smallOpts)
    val sfs = Sfs.mount(dev)
    (dev, layout, sfs.blockBitmap, sfs)

  /** A larger device for tests that need more headroom (multi-block files,
    * non-contig allocator pre-fills, indirect-1 spillover). */
  private def freshBig(blocks: Long = 4096L): (RamBlockDevice, Layout, Bitmap, Sfs) =
    val dev = new RamBlockDevice(blocks)
    val layout = Sfs.format(dev, smallOpts)
    val sfs = Sfs.mount(dev)
    (dev, layout, sfs.blockBitmap, sfs)

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

  /** Pre-mark alternating data blocks set, with a +5-block cushion past
    * `n`, so the next `n` allocations come out non-contiguous. */
  private def reserveAlternating(bm: Bitmap, layout: Layout, n: Int): Unit =
    var b = layout.dataStart
    val total = n + 5
    var i = 0
    while i < total do
      bm.set(b)
      i += 1
      b += 2

  /** A predictable byte pattern of `len` bytes, indexed from `seed`. */
  private def pattern(len: Int, seed: Int = 0): Array[Byte] =
    val out = new Array[Byte](len)
    var i = 0
    while i < len do
      out(i) = ((seed + i) & 0xff).toByte
      i += 1
    out

  // ---- readFile basics ------------------------------------------------

  "readFile" - {

    "returns empty array on a zero-size inode" in {
      val (dev, _, _, sfs) = fresh()
      FileIO.readFile(blankInode(), dev, 0L, 100).length shouldBe 0
    }

    "returns empty when offset is at or past EOF" in {
      val (dev, _, bm, sfs) = fresh()
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, pattern(10), FixedSec, FixedNsec)
      FileIO.readFile(ino, dev, 10L, 5).length shouldBe 0
      FileIO.readFile(ino, dev, 100L, 5).length shouldBe 0
    }

    "clamps a read that extends past EOF to ino.size" in {
      val (dev, _, bm, sfs) = fresh()
      val data = pattern(50)
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      val got = FileIO.readFile(ino, dev, 40L, 100)
      got.length shouldBe 10
      got.toSeq shouldBe data.slice(40, 50).toSeq
    }

    "round-trips a single-block byte pattern" in {
      val (dev, _, bm, sfs) = fresh()
      val data = pattern(BlockSize)
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      FileIO.readFile(ino, dev, 0L, BlockSize).toSeq shouldBe data.toSeq
    }

    "round-trips a multi-block byte pattern" in {
      val (dev, _, bm, sfs) = freshBig()
      val data = pattern(BlockSize * 3 + 17)
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      ino.size shouldBe data.length.toLong
      FileIO.readFile(ino, dev, 0L, data.length).toSeq shouldBe data.toSeq
    }

    "reads sparse holes as zero" in {
      val (dev, _, bm, sfs) = fresh()
      // Put a single byte at offset 8KiB → 2 sparse blocks then 1 concrete.
      val ino = FileIO.writeFile(
        blankInode(),
        sfs,
        offset = (BlockSize * 2).toLong,
        bytes = Array[Byte](0x42.toByte),
        timeSec = FixedSec,
        timeNsec = FixedNsec,
      )
      ino.size shouldBe (BlockSize * 2 + 1).toLong
      // Bytes [0, 8K) are sparse → all zero.
      val hole = FileIO.readFile(ino, dev, 0L, BlockSize * 2)
      hole.length shouldBe BlockSize * 2
      hole.forall(_ == 0.toByte) shouldBe true
      // Byte at 8K is the one we wrote.
      FileIO.readFile(ino, dev, (BlockSize * 2).toLong, 1).toSeq shouldBe
        Seq(0x42.toByte)
    }

    "supports byte-misaligned reads inside one block" in {
      val (dev, _, bm, sfs) = fresh()
      val data = pattern(200, seed = 7)
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      FileIO.readFile(ino, dev, 50L, 80).toSeq shouldBe data.slice(50, 130).toSeq
    }

    "supports byte-misaligned reads across block boundaries" in {
      val (dev, _, bm, sfs) = freshBig()
      val data = pattern(BlockSize * 2 + 100, seed = 11)
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      val got = FileIO.readFile(ino, dev, BlockSize - 5L, 30)
      got.toSeq shouldBe data.slice(BlockSize - 5, BlockSize - 5 + 30).toSeq
    }
  }

  // ---- writeFile basics -----------------------------------------------

  "writeFile" - {

    "zero-length write is a no-op (no time bumps)" in {
      val (dev, _, bm, sfs) = fresh()
      val ino = blankInode()
      val freeBefore = bm.freeCount
      FileIO.writeFile(ino, sfs, 0L, new Array[Byte](0), FixedSec, FixedNsec) shouldBe ino
      bm.freeCount shouldBe freeBefore
    }

    "stamps mtime, ctime, size, and blockCount on a fresh write" in {
      val (dev, _, bm, sfs) = fresh()
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, pattern(100), FixedSec, FixedNsec)
      ino.size shouldBe 100L
      ino.mtimeSec shouldBe FixedSec
      ino.mtimeNsec shouldBe FixedNsec
      ino.ctimeSec shouldBe FixedSec
      ino.ctimeNsec shouldBe FixedNsec
      ino.blockCount shouldBe 8 // 1 block × 8 (512-byte units)
    }

    "writing into the middle of an existing concrete file overwrites" in {
      val (dev, _, bm, sfs) = freshBig()
      val orig = pattern(BlockSize * 2, seed = 0)
      var ino = FileIO.writeFile(blankInode(), sfs, 0L, orig, FixedSec, FixedNsec)
      val freeBefore = bm.freeCount
      // Overwrite bytes [100, 200) with all 0xee.
      val patch = Array.fill[Byte](100)(0xee.toByte)
      ino = FileIO.writeFile(ino, sfs, 100L, patch, FixedSec + 1, 0)
      // No new allocation — just RMW.
      bm.freeCount shouldBe freeBefore
      val got = FileIO.readFile(ino, dev, 0L, BlockSize * 2)
      got.slice(0, 100).toSeq shouldBe orig.slice(0, 100).toSeq
      got.slice(100, 200).toSeq shouldBe patch.toSeq
      got.slice(200, BlockSize * 2).toSeq shouldBe orig.slice(200, BlockSize * 2).toSeq
    }

    "extends past EOF with a sparse hole when offset > size" in {
      val (dev, _, bm, sfs) = fresh()
      // Empty file → write at byte offset 8K + 50 = 1 byte.
      val data = pattern(1, seed = 0xaa)
      val ino = FileIO.writeFile(
        blankInode(),
        sfs,
        offset = (BlockSize * 2 + 50).toLong,
        bytes = data,
        timeSec = FixedSec,
        timeNsec = FixedNsec,
      )
      ino.size shouldBe (BlockSize * 2 + 50 + 1).toLong
      // Only 1 physical block was allocated for the data; the sparse hole costs nothing.
      ino.blockCount shouldBe 8
      FileIO.readFile(ino, dev, 0L, BlockSize * 2).forall(_ == 0.toByte) shouldBe true
      FileIO.readFile(ino, dev, (BlockSize * 2 + 50).toLong, 1).toSeq shouldBe data.toSeq
    }

    "appending at exactly EOF grows the file in place" in {
      val (dev, _, bm, sfs) = freshBig()
      val first = pattern(100, seed = 0)
      val second = pattern(50, seed = 200)
      var ino = FileIO.writeFile(blankInode(), sfs, 0L, first, FixedSec, FixedNsec)
      ino = FileIO.writeFile(ino, sfs, ino.size, second, FixedSec, FixedNsec)
      ino.size shouldBe 150L
      val all = FileIO.readFile(ino, dev, 0L, 200)
      all.length shouldBe 150
      all.slice(0, 100).toSeq shouldBe first.toSeq
      all.slice(100, 150).toSeq shouldBe second.toSeq
    }

    "writing into an existing sparse hole converts it to concrete" in {
      val (dev, _, bm, sfs) = fresh()
      // Build a 3-block sparse file by truncate-extend.
      var ino = FileIO.truncateFile(
        blankInode(),
        sfs,
        newSize = (BlockSize * 3).toLong,
        timeSec = FixedSec,
        timeNsec = FixedNsec,
      )
      ino.size shouldBe (BlockSize * 3).toLong
      ino.blockCount shouldBe 0 // sparse — no physical blocks
      val freeBefore = bm.freeCount

      // Write into the middle sparse block.
      val patch = pattern(10, seed = 0x55)
      ino = FileIO.writeFile(
        ino,
        sfs,
        offset = (BlockSize + 100).toLong,
        bytes = patch,
        timeSec = FixedSec,
        timeNsec = FixedNsec,
      )
      // One new physical block allocated for the converted sparse block.
      bm.freeCount shouldBe (freeBefore - 1)
      ino.blockCount shouldBe 8
      // Bytes around the patch read as zero.
      FileIO.readFile(ino, dev, BlockSize.toLong, 100).forall(_ == 0.toByte) shouldBe true
      FileIO.readFile(ino, dev, (BlockSize + 100).toLong, 10).toSeq shouldBe patch.toSeq
      FileIO.readFile(ino, dev, (BlockSize + 110).toLong, 100).forall(_ == 0.toByte) shouldBe true
      // The other two blocks remain sparse — read as all zero.
      FileIO.readFile(ino, dev, 0L, BlockSize).forall(_ == 0.toByte) shouldBe true
      FileIO.readFile(ino, dev, (BlockSize * 2).toLong, BlockSize)
        .forall(_ == 0.toByte) shouldBe true
    }

    "byte-misaligned writes inside one block preserve surrounding bytes" in {
      val (dev, _, bm, sfs) = freshBig()
      val orig = pattern(BlockSize, seed = 0x40)
      var ino = FileIO.writeFile(blankInode(), sfs, 0L, orig, FixedSec, FixedNsec)
      val patch = Array.fill[Byte](32)(0x77.toByte)
      ino = FileIO.writeFile(ino, sfs, 1000L, patch, FixedSec, FixedNsec)
      val got = FileIO.readFile(ino, dev, 0L, BlockSize)
      got.slice(0, 1000).toSeq shouldBe orig.slice(0, 1000).toSeq
      got.slice(1000, 1032).toSeq shouldBe patch.toSeq
      got.slice(1032, BlockSize).toSeq shouldBe orig.slice(1032, BlockSize).toSeq
    }

    "writes spanning a block boundary stitch correctly" in {
      val (dev, _, bm, sfs) = freshBig()
      val orig = pattern(BlockSize * 2, seed = 0x80)
      var ino = FileIO.writeFile(blankInode(), sfs, 0L, orig, FixedSec, FixedNsec)
      val patch = pattern(50, seed = 0x33)
      // Span byte 4090..4140 → covers tail of block 0 and head of block 1.
      ino = FileIO.writeFile(ino, sfs, 4090L, patch, FixedSec, FixedNsec)
      val got = FileIO.readFile(ino, dev, 0L, BlockSize * 2)
      got.slice(0, 4090).toSeq shouldBe orig.slice(0, 4090).toSeq
      got.slice(4090, 4140).toSeq shouldBe patch.toSeq
      got.slice(4140, BlockSize * 2).toSeq shouldBe orig.slice(4140, BlockSize * 2).toSeq
    }
  }

  // ---- truncateFile ---------------------------------------------------

  "truncateFile" - {

    "to the same size is a no-op" in {
      val (dev, _, bm, sfs) = fresh()
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, pattern(50), FixedSec, FixedNsec)
      val freeBefore = bm.freeCount
      val same = FileIO.truncateFile(ino, sfs, ino.size, FixedSec + 9, 0)
      same shouldBe ino
      bm.freeCount shouldBe freeBefore
    }

    "shrinking a single-block file frees the block when truncating to 0" in {
      val (dev, _, bm, sfs) = fresh()
      val freeBeforeAll = bm.freeCount
      val ino = FileIO.writeFile(blankInode(), sfs, 0L, pattern(100), FixedSec, FixedNsec)
      val after = FileIO.truncateFile(ino, sfs, 0L, FixedSec + 1, 0)
      after.size shouldBe 0L
      after.blockCount shouldBe 0
      bm.freeCount shouldBe freeBeforeAll
    }

    "shrinking past a partial block zeroes the trailing bytes on disk" in {
      val (dev, _, bm, sfs) = fresh()
      val data = pattern(BlockSize, seed = 0xab) // a full block of non-zero bytes
      var ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      // Truncate to 1000 bytes — the block stays but bytes [1000, 4096) must zero.
      ino = FileIO.truncateFile(ino, sfs, 1000L, FixedSec + 1, 0)
      ino.size shouldBe 1000L
      // Re-extend to full block to expose the underlying stale bytes (if any).
      ino = FileIO.truncateFile(ino, sfs, BlockSize.toLong, FixedSec + 2, 0)
      val tail = FileIO.readFile(ino, dev, 1000L, BlockSize - 1000)
      tail.forall(_ == 0.toByte) shouldBe true
    }

    "extending with truncate adds a sparse hole, not physical blocks" in {
      val (dev, _, bm, sfs) = fresh()
      val freeBefore = bm.freeCount
      val ino = FileIO.truncateFile(
        blankInode(),
        sfs,
        newSize = (BlockSize * 4).toLong,
        timeSec = FixedSec,
        timeNsec = FixedNsec,
      )
      ino.size shouldBe (BlockSize * 4).toLong
      ino.blockCount shouldBe 0
      bm.freeCount shouldBe freeBefore
      FileIO.readFile(ino, dev, 0L, BlockSize * 4).forall(_ == 0.toByte) shouldBe true
    }

    "shrinking a multi-block file frees the blocks past the cut point" in {
      val (dev, _, bm, sfs) = freshBig()
      val data = pattern(BlockSize * 4, seed = 0xc0)
      var ino = FileIO.writeFile(blankInode(), sfs, 0L, data, FixedSec, FixedNsec)
      val freeAfterWrite = bm.freeCount
      ino = FileIO.truncateFile(ino, sfs, BlockSize.toLong, FixedSec + 1, 0)
      ino.size shouldBe BlockSize.toLong
      ino.blockCount shouldBe 8
      bm.freeCount shouldBe (freeAfterWrite + 3) // 3 trailing blocks freed
    }
  }

  // ---- spillover into indirect-1 -------------------------------------

  ">64 KiB file with non-contiguous data spills into indirect-1" in {
    val (dev, layout, bm, sfs) = freshBig(8192L)
    // 17 logical blocks, each its own extent (forces inline → ind1 spill).
    reserveAlternating(bm, layout, 17)
    var ino = blankInode()
    val sliceLen = BlockSize
    var b = 0
    while b < 17 do
      val data = pattern(sliceLen, seed = b)
      ino = FileIO.writeFile(
        ino,
        sfs,
        offset = (b.toLong * BlockSize),
        bytes = data,
        timeSec = FixedSec,
        timeNsec = FixedNsec,
      )
      b += 1
    ino.size shouldBe (17L * BlockSize)
    (ino.flags & InodeFlagHasIndirect1) shouldBe InodeFlagHasIndirect1
    ino.indirect1 should not be 0
    // Read back every block and verify it matches the seed pattern.
    var i = 0
    while i < 17 do
      val got = FileIO.readFile(ino, dev, (i.toLong * BlockSize), BlockSize)
      got.toSeq shouldBe pattern(sliceLen, seed = i).toSeq
      i += 1
  }

  // ---- > 2 GiB sparse file (built sparsely, fits in test device) -----

  "sparse file >2 GiB stores data only at the populated tail" in {
    val (dev, _, bm, sfs) = fresh()
    // 2 GiB = 524288 blocks of 4 KiB = 0x80000000 bytes.
    val twoGiB = (1L << 31)
    val freeBefore = bm.freeCount
    val data = pattern(64, seed = 0x9c)
    val ino = FileIO.writeFile(
      blankInode(),
      sfs,
      offset = twoGiB,
      bytes = data,
      timeSec = FixedSec,
      timeNsec = FixedNsec,
    )
    ino.size shouldBe (twoGiB + data.length)
    // Only one physical block was allocated for the data; the rest is a
    // single sparse extent covering 524288 logical blocks.
    bm.freeCount shouldBe (freeBefore - 1)
    ino.blockCount shouldBe 8
    // The extent map fits inline: 1 sparse extent + 1 concrete extent = 2.
    (ino.flags & InodeFlagHasIndirect1) shouldBe 0
    val xs = ExtentAllocator.listExtents(ino, dev)
    xs.length shouldBe 2
    xs(0).sparse shouldBe true
    xs(0).count.toLong shouldBe (twoGiB / BlockSize)
    xs(1).sparse shouldBe false
    xs(1).count shouldBe 1
    // Sample reads inside the hole and at the tail.
    FileIO.readFile(ino, dev, 0L, 4096).forall(_ == 0.toByte) shouldBe true
    FileIO.readFile(ino, dev, (twoGiB - 100), 100).forall(_ == 0.toByte) shouldBe true
    FileIO.readFile(ino, dev, twoGiB, 64).toSeq shouldBe data.toSeq
  }
