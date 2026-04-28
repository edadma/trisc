package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class MountTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "rttest",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def fresh(): (RamBlockDevice, Layout) =
    val dev = RamBlockDevice.default()
    val layout = Sfs.format(dev, smallOpts)
    (dev, layout)

  "mount" - {

    "succeeds on a freshly-formatted clean volume" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      fs.isMounted shouldBe true
      fs.superblock.totalBlocks shouldBe 512
      fs.superblock.fsState shouldBe FsDirty // mount flips it
      fs.unmount()
    }

    "loads the layout from the SB and matches what format wrote" in {
      val (dev, layout) = fresh()
      val fs = Sfs.mount(dev)
      fs.layout shouldBe layout
      fs.unmount()
    }

    "loads bitmaps reflecting the post-format reservations" in {
      val (dev, layout) = fresh()
      val fs = Sfs.mount(dev)
      fs.blockBitmap.freeCount shouldBe (layout.totalBlocks - layout.dataStart - 2)
      fs.inodeBitmap.freeCount shouldBe (layout.totalInodes - 3)
      fs.unmount()
    }

    "writes fs_state=dirty to disk" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      fs.unmount()
      // re-read SB and check that mount flipped fs_state to dirty
      // (after unmount it's back to clean, so re-mount and inspect on disk
      // before that)
      val dev2 = RamBlockDevice.default()
      Sfs.format(dev2, smallOpts)
      val fs2 = Sfs.mount(dev2)
      val buf = new Array[Byte](BlockSize)
      dev2.readBlock(0, buf)
      Superblock.unpack(buf, 0).fsState shouldBe FsDirty
      fs2.unmount()
    }

    "recovers a dirty volume on remount (Phase 13d)" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      // skip unmount → leave fsState=dirty on disk
      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0, sbBuf)
      Superblock.unpack(sbBuf, 0).fsState shouldBe FsDirty
      // Recovery on next mount: walks the journal, replays nothing
      // (no committed txns since the prior mount didn't do anything),
      // and re-marks the SB dirty for our new mount session.
      val fs2 = Sfs.mount(dev)
      fs2.isMounted shouldBe true
      fs2.unmount()
    }

    "rejects an error-state volume" in {
      val (dev, _) = fresh()
      // hand-craft an SB with fsState=error
      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsError)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)
      a[SfsCorruptError] should be thrownBy Sfs.mount(dev)
    }

    "falls back to the backup SB when the primary's CRC is corrupt" in {
      val (dev, _) = fresh()
      // corrupt block 0; backup at block 1 is identical so mount should still work
      val zeros = new Array[Byte](BlockSize)
      dev.writeBlock(0L, zeros)
      val fs = Sfs.mount(dev)
      fs.isMounted shouldBe true
      fs.unmount()
    }
  }

  "readInode" - {

    "returns the bad-blocks inode (1) as written by format" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      val ino = fs.readInode(InoBadBlocks)
      ino.mode shouldBe 0x81a4
      ino.linkCount shouldBe 1
      ino.size shouldBe 0L
      fs.unmount()
    }

    "returns the root inode (2) as written by format" in {
      val (dev, layout) = fresh()
      val fs = Sfs.mount(dev)
      val ino = fs.readInode(InoRoot)
      ino.mode shouldBe 0x41ed
      ino.size shouldBe (2 * BlockSize).toLong
      ino.body match
        case InodeBody.Extents(xs) =>
          xs.head shouldBe Extent(start = layout.dataStart, count = 2)
        case _ => fail("root inode should have Extents body")
      fs.unmount()
    }
  }

  "writeInode" - {

    "round-trips through the inode table without affecting neighbors" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      val rootBefore = fs.readInode(InoRoot)
      val updated = fs.readInode(InoBadBlocks).copy(linkCount = 7, size = 99L)
      fs.writeInode(InoBadBlocks, updated)
      // bad-blocks inode reflects the change
      val readBack = fs.readInode(InoBadBlocks)
      readBack.linkCount shouldBe 7
      readBack.size shouldBe 99L
      // the root inode is untouched (lives in the same block at a different offset)
      fs.readInode(InoRoot) shouldBe rootBefore
      fs.unmount()
    }
  }

  "unmount" - {

    "writes fs_state=clean back to disk" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      fs.unmount()
      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0, sbBuf)
      Superblock.unpack(sbBuf, 0).fsState shouldBe FsClean
    }

    "rejects further calls after unmount" in {
      val (dev, _) = fresh()
      val fs = Sfs.mount(dev)
      fs.unmount()
      fs.isMounted shouldBe false
      an[IllegalStateException] should be thrownBy fs.readInode(InoRoot)
      an[IllegalStateException] should be thrownBy fs.unmount()
    }

    "preserves bitmap state across unmount/remount" in {
      val (dev, _) = fresh()
      val fs1 = Sfs.mount(dev)
      // mutate the bitmap then unmount
      fs1.inodeBitmap.set(5)
      fs1.inodeBitmap.set(6)
      fs1.unmount()
      val fs2 = Sfs.mount(dev)
      fs2.inodeBitmap.isSet(5) shouldBe true
      fs2.inodeBitmap.isSet(6) shouldBe true
      fs2.inodeBitmap.isSet(7) shouldBe false
      fs2.unmount()
    }

    "writes the up-to-date freeBlocks / freeInodes to the SB" in {
      val (dev, layout) = fresh()
      val fs = Sfs.mount(dev)
      fs.inodeBitmap.set(5)
      fs.inodeBitmap.set(6)
      fs.unmount()
      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0)
      sb.freeInodes shouldBe (layout.totalInodes - 5)
      sb.fsState shouldBe FsClean
    }
  }

  "format → mount → readInode → unmount → re-mount round-trip" in {
    val dev = RamBlockDevice.default()
    val layout = Sfs.format(dev, smallOpts)
    // first mount cycle
    val fs1 = Sfs.mount(dev)
    val rootIno = fs1.readInode(InoRoot)
    fs1.unmount()
    // second mount cycle
    val fs2 = Sfs.mount(dev)
    fs2.layout shouldBe layout
    fs2.readInode(InoRoot) shouldBe rootIno
    fs2.superblock.totalBlocks shouldBe layout.totalBlocks
    fs2.unmount()
  }
