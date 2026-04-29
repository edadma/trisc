package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 15 — bad-block tracking tests.
  *
  * Inode 1 holds a packed array of u32 block addresses; the block
  * allocator must never hand out a block that's been marked bad.
  * The list is loaded at mount time (bits set in the in-memory
  * bitmap), so all the existing allocator paths automatically skip
  * marked blocks.
  */
class BadBlockOpsTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "badblock",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0

  // ---- happy path -----------------------------------------------------

  "list" - {

    "is empty on a freshly formatted volume" in {
      val (_, sfs) = mounted()
      BadBlockOps.list(sfs) shouldBe IndexedSeq.empty
      sfs.unmount()
    }

    "returns marks in insertion order" in {
      val (_, sfs) = mounted()
      val a = sfs.layout.dataStart + 100
      val b = sfs.layout.dataStart + 200
      val c = sfs.layout.dataStart + 50
      BadBlockOps.mark(sfs, a, Now, Nsec)
      BadBlockOps.mark(sfs, b, Now, Nsec)
      BadBlockOps.mark(sfs, c, Now, Nsec)
      BadBlockOps.list(sfs) shouldBe IndexedSeq(a, b, c)
      sfs.unmount()
    }
  }

  "mark" - {

    "appends the address to inode 1's file content" in {
      val (_, sfs) = mounted()
      val sizeBefore = sfs.readInode(InoBadBlocks).size
      val target = sfs.layout.dataStart + 42
      BadBlockOps.mark(sfs, target, Now, Nsec)
      sfs.readInode(InoBadBlocks).size shouldBe (sizeBefore + 4L)
      sfs.unmount()
    }

    "is idempotent: marking the same block twice does not duplicate it" in {
      val (_, sfs) = mounted()
      val target = sfs.layout.dataStart + 7
      BadBlockOps.mark(sfs, target, Now, Nsec)
      val sizeAfterFirst = sfs.readInode(InoBadBlocks).size
      BadBlockOps.mark(sfs, target, Now, Nsec)
      sfs.readInode(InoBadBlocks).size shouldBe sizeAfterFirst
      BadBlockOps.list(sfs) shouldBe IndexedSeq(target)
      sfs.unmount()
    }

    "refuses addresses inside the metadata region" in {
      val (_, sfs) = mounted()
      // Try to mark block 0 (superblock) — refuse.
      an[IllegalArgumentException] should be thrownBy
        BadBlockOps.mark(sfs, 0, Now, Nsec)
      // Just before dataStart is also metadata — refuse.
      an[IllegalArgumentException] should be thrownBy
        BadBlockOps.mark(sfs, sfs.layout.dataStart - 1, Now, Nsec)
      // dataStart itself is OK.
      BadBlockOps.mark(sfs, sfs.layout.dataStart, Now, Nsec)
      sfs.unmount()
    }

    "refuses addresses past totalBlocks" in {
      val (_, sfs) = mounted()
      an[IllegalArgumentException] should be thrownBy
        BadBlockOps.mark(sfs, sfs.layout.totalBlocks, Now, Nsec)
      an[IllegalArgumentException] should be thrownBy
        BadBlockOps.mark(sfs, sfs.layout.totalBlocks + 100, Now, Nsec)
      sfs.unmount()
    }
  }

  // ---- allocator interaction ------------------------------------------

  "allocator skips bad blocks" - {

    "marking a free block makes it un-allocatable" in {
      val (_, sfs) = mounted()
      // Find a block that the allocator WOULD give us next.
      val wouldGet = sfs.blockBitmap.allocate().get
      sfs.blockBitmap.clear(wouldGet) // put it back
      BadBlockOps.mark(sfs, wouldGet, Now, Nsec)
      // Allocator must not return it now.
      val firstAlloc = sfs.blockBitmap.allocate().get
      firstAlloc should not be wouldGet
      sfs.unmount()
    }

    "an entire run of bad blocks is skipped" in {
      val (_, sfs) = mounted()
      // Mark blocks 100..104 (relative to dataStart) bad.
      val base = sfs.layout.dataStart + 100
      var i = 0
      while i < 5 do
        BadBlockOps.mark(sfs, base + i, Now, Nsec)
        i += 1
      // Allocate a bunch and verify none of them fall in the bad range.
      val allocated = scala.collection.mutable.ArrayBuffer.empty[Int]
      var n = 0
      while n < 50 do
        val b = sfs.blockBitmap.allocate().get
        allocated += b
        n += 1
      val badSet = (base until base + 5).toSet
      allocated.toSet.intersect(badSet) shouldBe empty
      sfs.unmount()
    }

    "loadAtMount sets the bits even if the on-disk bitmap is stale" in {
      // Build a volume by hand: mark a block bad (which sets its bit),
      // then forcibly clear the bit on disk to simulate an older
      // bitmap snapshot, mount, and verify loadAtMount restored the
      // bit from inode 1's file content.
      val dev = new RamBlockDevice(8192L)
      Sfs.format(dev, smallOpts)
      val sfs1 = Sfs.mount(dev)
      val target = sfs1.layout.dataStart + 77
      val bbStart = sfs1.layout.blockBitmapStart
      BadBlockOps.mark(sfs1, target, Now, Nsec)
      sfs1.unmount()

      // Scribble the bitmap bit back to free.
      val bbBuf = new Array[Byte](BlockSize)
      val byteOffGlobal = target >>> 3
      val bbBlockIdx = byteOffGlobal / BlockSize
      val byteInBlock = byteOffGlobal % BlockSize
      val bitInByte = target & 7
      dev.readBlock(bbStart.toLong + bbBlockIdx, bbBuf)
      bbBuf(byteInBlock) = (bbBuf(byteInBlock) & ~(1 << bitInByte)).toByte
      dev.writeBlock(bbStart.toLong + bbBlockIdx, bbBuf)
      dev.flush()

      // Mount: loadAtMount should re-mark the bit in memory.
      val sfs2 = Sfs.mount(dev)
      sfs2.blockBitmap.isSet(target) shouldBe true
      // And the allocator must not return it.
      val seen = scala.collection.mutable.Set.empty[Int]
      var i = 0
      while i < 200 do
        seen += sfs2.blockBitmap.allocate().get
        i += 1
      seen.contains(target) shouldBe false
      sfs2.unmount()
    }
  }

  // ---- persistence ----------------------------------------------------

  "persistence" - {

    "marks survive a clean unmount/remount cycle" in {
      val (dev, sfs) = mounted()
      val a = sfs.layout.dataStart + 1
      val b = sfs.layout.dataStart + 7
      val c = sfs.layout.dataStart + 99
      BadBlockOps.mark(sfs, a, Now, Nsec)
      BadBlockOps.mark(sfs, b, Now, Nsec)
      BadBlockOps.mark(sfs, c, Now, Nsec)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      BadBlockOps.list(sfs2) shouldBe IndexedSeq(a, b, c)
      sfs2.blockBitmap.isSet(a) shouldBe true
      sfs2.blockBitmap.isSet(b) shouldBe true
      sfs2.blockBitmap.isSet(c) shouldBe true
      sfs2.unmount()
    }

    "marks survive journal replay on a dirty mount" in {
      // Mark a block, then DON'T unmount (simulate a power loss).
      // Recovery should replay the txn so list() returns the mark.
      val dev = new RamBlockDevice(8192L)
      Sfs.format(dev, smallOpts)
      val sfs = Sfs.mount(dev)
      val target = sfs.layout.dataStart + 33
      BadBlockOps.mark(sfs, target, Now, Nsec)
      // No unmount.

      val sfs2 = Sfs.mount(dev)
      BadBlockOps.list(sfs2) shouldBe IndexedSeq(target)
      sfs2.blockBitmap.isSet(target) shouldBe true
      sfs2.unmount()
    }

    "many marks across multiple txns persist correctly" in {
      val (dev, sfs) = mounted()
      var i = 0
      while i < 25 do
        BadBlockOps.mark(sfs, sfs.layout.dataStart + i, Now, Nsec)
        i += 1
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      BadBlockOps.list(sfs2) shouldBe (sfs2.layout.dataStart until sfs2.layout.dataStart + 25).toIndexedSeq
      sfs2.unmount()
    }
  }

