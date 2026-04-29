package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class FormatTests extends AnyFreeSpec with Matchers:

  /** A small but spec-valid format options shape that fits inside the 2 MiB
    * default RAM device. The defaults in [[FormatOptions]] are tuned for a
    * realistically-sized volume (1 Mi inodes, 128 MiB journal) and don't fit
    * the tiny test device. */
  private def smallOpts(volumeName: String = "test"): FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = volumeName,
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_1234L,
    )

  "Sfs.format" - {

    "lays out a 2 MiB volume and the superblock parses cleanly" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())

      // re-read the superblock and check it agrees with the computed layout
      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0)
      sb.totalBlocks shouldBe layout.totalBlocks
      sb.totalInodes shouldBe layout.totalInodes
      sb.blockBitmapStart shouldBe layout.blockBitmapStart
      sb.blockBitmapLen shouldBe layout.blockBitmapLen
      sb.inodeBitmapStart shouldBe layout.inodeBitmapStart
      sb.inodeBitmapLen shouldBe layout.inodeBitmapLen
      sb.inodeTableStart shouldBe layout.inodeTableStart
      sb.inodeTableLen shouldBe layout.inodeTableLen
      sb.journalStart shouldBe layout.journalStart
      sb.journalLen shouldBe layout.journalLen
      sb.dataStart shouldBe layout.dataStart
      sb.rootInode shouldBe InoRoot
      sb.fsState shouldBe FsClean
      sb.hashAlgorithm shouldBe HashFnv1a
      sb.volumeName shouldBe "test"
      sb.uuid.length shouldBe 16
    }

    "writes an identical backup superblock at block 1" in {
      val dev = RamBlockDevice.default()
      Sfs.format(dev, smallOpts())
      val a = new Array[Byte](BlockSize)
      val b = new Array[Byte](BlockSize)
      dev.readBlock(0, a)
      dev.readBlock(1, b)
      a shouldBe b
    }

    "reserves every metadata block + the root dir's two data blocks in the block bitmap" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val bm = new Bitmap(dev, layout.blockBitmapStart, layout.blockBitmapLen, layout.totalBlocks)
      bm.load()
      // every metadata block 0..(dataStart-1) is set
      for i <- 0 until layout.dataStart do bm.isSet(i) shouldBe true
      // the two root-dir data blocks are set
      bm.isSet(layout.dataStart) shouldBe true
      bm.isSet(layout.dataStart + 1) shouldBe true
      // the next block is free
      bm.isSet(layout.dataStart + 2) shouldBe false
      bm.freeCount shouldBe (layout.totalBlocks - layout.dataStart - 2)
    }

    "reserves inodes 0, 1, 2 in the inode bitmap" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val ibm = new Bitmap(dev, layout.inodeBitmapStart, layout.inodeBitmapLen, layout.totalInodes)
      ibm.load()
      ibm.isSet(InoNull) shouldBe true
      ibm.isSet(InoBadBlocks) shouldBe true
      ibm.isSet(InoRoot) shouldBe true
      ibm.isSet(3) shouldBe false
      ibm.freeCount shouldBe (layout.totalInodes - 3)
    }

    "free counts in the SB match the bitmap free counts" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0)
      sb.freeBlocks shouldBe (layout.totalBlocks - layout.dataStart - 2)
      sb.freeInodes shouldBe (layout.totalInodes - 3)
    }

    "writes a parseable bad-blocks inode (inode 1)" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val (blk, off) = layout.inodeLocation(InoBadBlocks)
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(blk, buf)
      val ino = Inode.unpack(buf, off)
      ino.mode shouldBe 0x81a4
      ino.linkCount shouldBe 1
      ino.size shouldBe 0L
      ino.blockCount shouldBe 0
      ino.body shouldBe InodeBody.EmptyExtents
      (ino.flags & InodeFlagInlineSymlink) shouldBe 0
    }

    "writes a parseable root inode (inode 2) pointing at its dir blocks" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val (blk, off) = layout.inodeLocation(InoRoot)
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(blk, buf)
      val ino = Inode.unpack(buf, off)
      ino.mode shouldBe 0x41ed
      ino.linkCount shouldBe 2
      ino.size shouldBe (2 * BlockSize).toLong
      ino.blockCount shouldBe (2 * BlockSize / 512)
      ino.body match
        case InodeBody.Extents(xs) =>
          xs(0) shouldBe Extent(start = layout.dataStart, count = 2)
          xs.tail.forall(_ == Extent.Empty) shouldBe true
        case _ => fail("root inode body should be Extents")
    }

    "writes a parseable root directory block at the first allocated data block" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val rootBuf = new Array[Byte](BlockSize)
      dev.readBlock(layout.dataStart.toLong, rootBuf)
      val root = DirRootBlock.unpack(rootBuf, InoRoot)
      root.dot.name shouldBe "."
      root.dot.inode shouldBe InoRoot
      root.dotdot.name shouldBe ".."
      root.dotdot.inode shouldBe InoRoot
      root.hashVersion shouldBe HashFnv1a
      root.treeDepth shouldBe 0
      root.indexEntries.head shouldBe ((0, 1)) // first leaf at file-block 1
    }

    "writes a parseable empty leaf block at the second allocated data block" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val leafBuf = new Array[Byte](BlockSize)
      dev.readBlock((layout.dataStart + 1).toLong, leafBuf)
      val entries = DirLeafBlock.unpack(leafBuf, InoRoot)
      entries.length shouldBe 1
      entries.head.inode shouldBe 0 // tombstone
      entries.head.recLen shouldBe DirLeafBlock.UsableSize
    }

    "writes a parseable journal superblock with sequence=0" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts())
      val jBuf = new Array[Byte](BlockSize)
      dev.readBlock(layout.journalStart.toLong, jBuf)
      val jsb = JournalSuperblock.unpack(jBuf, 0)
      jsb.head shouldBe 0
      jsb.tail shouldBe 0
      jsb.sequence shouldBe 0
      jsb.blockCount shouldBe (layout.journalLen - 1)
      jsb.fsUuid shouldBe smallOpts().uuid
    }

    "rejects format options that don't fit the device" in {
      val dev = RamBlockDevice.default()
      // default opts ask for 1 Mi inodes + 128 MiB journal — won't fit a 2 MiB device
      an[IllegalArgumentException] should be thrownBy Sfs.format(dev, FormatOptions())
    }
  }
