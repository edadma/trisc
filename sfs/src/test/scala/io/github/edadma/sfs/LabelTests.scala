package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for the volume label and UUID accessors and [[Sfs.relabel]]
  * (Phase 17b). */
class LabelTests extends AnyFreeSpec with Matchers:

  private val FixedUuid: IndexedSeq[Byte] =
    (1 to 16).map(_.toByte).toIndexedSeq

  private def smallOpts(name: String = "lbltest"): FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = name,
      uuid = FixedUuid,
      formatTime = 0x6800_4321L,
    )

  private def mounted(name: String = "lbltest"): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts(name))
    (dev, Sfs.mount(dev))

  "volumeName / uuid" - {

    "return the values set at format time" in {
      val (_, sfs) = mounted("hello")
      sfs.volumeName shouldBe "hello"
      sfs.uuid shouldBe FixedUuid
      sfs.unmount()
    }

    "match what is in the superblock" in {
      val (_, sfs) = mounted("foo")
      sfs.volumeName shouldBe sfs.superblock.volumeName
      sfs.uuid shouldBe sfs.superblock.uuid
      sfs.unmount()
    }
  }

  "relabel" - {

    "updates the in-memory superblock immediately" in {
      val (_, sfs) = mounted("old")
      sfs.relabel("new")
      sfs.volumeName shouldBe "new"
      sfs.superblock.volumeName shouldBe "new"
      sfs.unmount()
    }

    "persists across unmount/remount" in {
      val (dev, sfs) = mounted("before")
      sfs.relabel("after")
      sfs.unmount()
      val sfs2 = Sfs.mount(dev)
      sfs2.volumeName shouldBe "after"
      sfs2.unmount()
    }

    "leaves UUID unchanged" in {
      val (_, sfs) = mounted("x")
      sfs.relabel("y")
      sfs.uuid shouldBe FixedUuid
      sfs.unmount()
    }

    "rejects names longer than VolumeNameMax" in {
      val (_, sfs) = mounted("x")
      val tooLong = "a" * (Superblock.VolumeNameMax + 1)
      an[IllegalArgumentException] should be thrownBy sfs.relabel(tooLong)
      sfs.volumeName shouldBe "x" // unchanged after rejection
      sfs.unmount()
    }

    "accepts a name exactly at VolumeNameMax bytes" in {
      val (_, sfs) = mounted("x")
      val maxName = "a" * Superblock.VolumeNameMax
      sfs.relabel(maxName)
      sfs.volumeName shouldBe maxName
      sfs.unmount()
    }

    "accepts an empty name" in {
      val (_, sfs) = mounted("nonempty")
      sfs.relabel("")
      sfs.volumeName shouldBe ""
      sfs.unmount()
    }

    "throws after unmount" in {
      val (_, sfs) = mounted()
      sfs.unmount()
      an[IllegalStateException] should be thrownBy sfs.relabel("any")
    }

    "writes through both SB copies (primary + backup)" in {
      val (dev, sfs) = mounted("orig")
      sfs.relabel("dual")
      sfs.unmount()

      // Both block 0 and block 1 should now decode the new label.
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(0L, buf)
      Superblock.unpack(buf, 0).volumeName shouldBe "dual"
      dev.readBlock(1L, buf)
      Superblock.unpack(buf, 0).volumeName shouldBe "dual"
    }
  }
