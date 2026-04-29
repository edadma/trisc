package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for [[Sfs.statfs]] (Phase 17a). */
class StatfsTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "stat",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0

  "statfs" - {

    "reports BlockSize as the filesystem block size" in {
      val (_, sfs) = mounted()
      sfs.statfs.blockSize shouldBe BlockSize
      sfs.unmount()
    }

    "reports totals matching the layout" in {
      val (_, sfs) = mounted()
      val s = sfs.statfs
      s.totalBlocks shouldBe sfs.layout.totalBlocks
      s.totalInodes shouldBe sfs.layout.totalInodes
      sfs.unmount()
    }

    "reports free counts matching the live bitmaps" in {
      val (_, sfs) = mounted()
      val s = sfs.statfs
      s.freeBlocks shouldBe sfs.blockBitmap.freeCount
      s.freeInodes shouldBe sfs.inodeBitmap.freeCount
      sfs.unmount()
    }

    "freeInodes drops by 1 after creating a new file" in {
      val (_, sfs) = mounted()
      val before = sfs.statfs.freeInodes
      val (_, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.statfs.freeInodes shouldBe (before - 1)
      sfs.unmount()
    }

    "freeBlocks drops after writing data, returns after unlink" in {
      val (_, sfs) = mounted()
      val baseline = sfs.statfs.freeBlocks

      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "data",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      val payload = new Array[Byte](BlockSize * 3)
      var i = 0
      while i < payload.length do
        payload(i) = (i & 0xff).toByte
        i += 1
      val withData = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, payload, Now, Nsec)
      sfs.writeInode(ino, withData)

      val afterWrite = sfs.statfs.freeBlocks
      afterWrite should be < baseline

      FileOps.unlink(root1, InoRoot, sfs, "data", Now, Nsec)
      sfs.statfs.freeBlocks shouldBe baseline
      sfs.unmount()
    }

    "usedBlocks + freeBlocks = totalBlocks" in {
      val (_, sfs) = mounted()
      val s = sfs.statfs
      (s.usedBlocks + s.freeBlocks) shouldBe s.totalBlocks
      sfs.unmount()
    }

    "usedInodes + freeInodes = totalInodes" in {
      val (_, sfs) = mounted()
      val s = sfs.statfs
      (s.usedInodes + s.freeInodes) shouldBe s.totalInodes
      sfs.unmount()
    }

    "throws after unmount" in {
      val (_, sfs) = mounted()
      sfs.unmount()
      an[IllegalStateException] should be thrownBy sfs.statfs
    }
  }
