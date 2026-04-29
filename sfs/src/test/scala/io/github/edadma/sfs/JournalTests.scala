package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 13a — Journal bookkeeping. Exercises the in-memory state
  * machine (head / tail / sequence with wrap-around math) and the
  * round-trip through the on-disk JournalSuperblock at [[Sfs.format]] /
  * [[Sfs.mount]] / [[Sfs.unmount]]. */
class JournalTests extends AnyFreeSpec with Matchers:

  // ---- helpers --------------------------------------------------------

  private val Uuid: IndexedSeq[Byte] = (1 to 16).map(_.toByte).toIndexedSeq
  private val OtherUuid: IndexedSeq[Byte] = (101 to 116).map(_.toByte).toIndexedSeq

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "jrnl",
      uuid = Uuid,
      formatTime = 0x6800_4321L,
    )

  /** Build a stand-alone Journal sitting on a fresh RamBlockDevice — no
    * full filesystem around it. The device is sized just large enough
    * to hold the journal SB at block 0 and the log blocks after it. */
  private def standalone(blockCount: Int): (RamBlockDevice, Journal) =
    val dev = new RamBlockDevice((blockCount + 1).toLong)
    val sb0 = JournalSuperblock(
      version = 1,
      blockCount = blockCount,
      head = 0, tail = 0, sequence = 0,
      fsUuid = Uuid,
    )
    val buf = new Array[Byte](BlockSize)
    JournalSuperblock.pack(sb0, buf, 0)
    dev.writeBlock(0L, buf)
    (dev, Journal.load(dev, 0L, Uuid))

  // ---- empty state ----------------------------------------------------

  "freshly loaded empty journal" - {

    "reports head == tail == sequence == 0" in {
      val (_, j) = standalone(8)
      j.head shouldBe 0
      j.tail shouldBe 0
      j.sequence shouldBe 0
    }

    "reports freeBlocks == blockCount - 1 (one slot of slack)" in {
      val (_, j) = standalone(8)
      j.freeBlocks shouldBe 7
    }

    "logPositionToDisk maps position 0 to journalStart + 1" in {
      val (_, j) = standalone(8)
      j.logPositionToDisk(0) shouldBe 1L
      j.logPositionToDisk(7) shouldBe 8L
    }

    "logPositionToDisk rejects out-of-range positions" in {
      val (_, j) = standalone(8)
      an[IllegalArgumentException] should be thrownBy j.logPositionToDisk(-1)
      an[IllegalArgumentException] should be thrownBy j.logPositionToDisk(8)
    }
  }

  // ---- reserve --------------------------------------------------------

  "reserve" - {

    "returns the absolute disk block of the current tail without mutating state" in {
      val (_, j) = standalone(8)
      val before = (j.head, j.tail, j.sequence)
      val start = j.reserve(3)
      start shouldBe 1L // journalStart=0, +1 for SB, tail=0
      (j.head, j.tail, j.sequence) shouldBe before
    }

    "rejects n <= 0" in {
      val (_, j) = standalone(8)
      an[IllegalArgumentException] should be thrownBy j.reserve(0)
      an[IllegalArgumentException] should be thrownBy j.reserve(-1)
    }

    "throws SfsNoSpaceError when n > freeBlocks" in {
      val (_, j) = standalone(8)
      // free = 7 (slack)
      an[SfsNoSpaceError] should be thrownBy j.reserve(8)
    }

    "allows reserving exactly freeBlocks blocks (max contiguous fit)" in {
      val (_, j) = standalone(8)
      val start = j.reserve(7)
      start shouldBe 1L
    }

    "after advance, reserve returns the new tail's disk block" in {
      val (_, j) = standalone(8)
      j.reserve(3) // does NOT mutate
      j.advance(newTail = 3, newSeq = 1)
      j.reserve(2) shouldBe 4L // journalStart=0, +1 for SB, +tail=3 → 4
    }
  }

  // ---- wrap-around math ----------------------------------------------

  "wrap-around" - {

    "advance to tail near end leaves correct freeBlocks" in {
      val (_, j) = standalone(8)
      j.advance(newTail = 7, newSeq = 1)
      // head=0, tail=7: free = (0 - 7 - 1 + 8) mod 8 = 0
      j.freeBlocks shouldBe 0
      an[SfsNoSpaceError] should be thrownBy j.reserve(1)
    }

    "advance to a wrapped tail leaves correct freeBlocks" in {
      val (_, j) = standalone(8)
      // simulate: a transaction wrote starting at tail=6, ran 3 blocks
      // (6, 7, 0), advance(1, 1) → wrapped tail
      j.advance(newTail = 1, newSeq = 1)
      // head=0, tail=1: free = (0 - 1 - 1 + 8) mod 8 = 6
      j.freeBlocks shouldBe 6
    }

    "freeBlocks correct mid-fill (head=0, tail=3)" in {
      val (_, j) = standalone(8)
      j.advance(newTail = 3, newSeq = 1)
      // free = (0 - 3 - 1 + 8) mod 8 = 4
      j.freeBlocks shouldBe 4
    }

    "freeBlocks correct after replay caught up to tail (empty again)" in {
      val (_, j) = standalone(8)
      j.advance(newTail = 5, newSeq = 1)
      j.replayHead(5)
      j.head shouldBe 5
      j.tail shouldBe 5
      // empty: free = blockCount - 1 = 7
      j.freeBlocks shouldBe 7
    }

    "reserve at wrap point honours circular distance" in {
      val (_, j) = standalone(8)
      // head=2, tail=6: used=4, free=(2-6-1+8) mod 8 = 3
      j.advance(newTail = 6, newSeq = 1)
      j.replayHead(2)
      j.freeBlocks shouldBe 3
      // reserving 3 succeeds; 4 doesn't
      j.reserve(3) shouldBe 7L // logPositionToDisk(6) = 0 + 1 + 6 = 7
      an[SfsNoSpaceError] should be thrownBy j.reserve(4)
    }

    "exact-fit: 1 block free on a non-empty journal" in {
      val (_, j) = standalone(8)
      // head=3, tail=2: free = (3-2-1+8) mod 8 = 0  (full)
      j.advance(newTail = 2, newSeq = 1)
      j.replayHead(3)
      j.freeBlocks shouldBe 0
      // free up one block: head=4
      j.replayHead(4)
      j.freeBlocks shouldBe 1
      j.reserve(1) shouldBe 3L // logPositionToDisk(2) = 0 + 1 + 2 = 3
      an[SfsNoSpaceError] should be thrownBy j.reserve(2)
    }
  }

  // ---- replayHead -----------------------------------------------------

  "replayHead" - {

    "rejects out-of-range positions" in {
      val (_, j) = standalone(8)
      an[IllegalArgumentException] should be thrownBy j.replayHead(-1)
      an[IllegalArgumentException] should be thrownBy j.replayHead(8)
    }
  }

  // ---- advance --------------------------------------------------------

  "advance" - {

    "rejects out-of-range tail" in {
      val (_, j) = standalone(8)
      an[IllegalArgumentException] should be thrownBy j.advance(-1, 0)
      an[IllegalArgumentException] should be thrownBy j.advance(8, 0)
    }

    "updates both tail and sequence" in {
      val (_, j) = standalone(8)
      j.advance(5, 42)
      j.tail shouldBe 5
      j.sequence shouldBe 42
    }
  }

  // ---- flush + load round-trip ---------------------------------------

  "flush + load" - {

    "round-trips head, tail, sequence through the JournalSuperblock" in {
      val (dev, j) = standalone(16)
      j.advance(newTail = 11, newSeq = 7)
      j.replayHead(3)
      j.flush()
      val j2 = Journal.load(dev, 0L, Uuid)
      j2.head shouldBe 3
      j2.tail shouldBe 11
      j2.sequence shouldBe 7
      j2.blockCount shouldBe 16
      j2.fsUuid shouldBe Uuid
    }

    "load rejects mismatched fs_uuid" in {
      val (dev, _) = standalone(8)
      an[SfsCorruptError] should be thrownBy Journal.load(dev, 0L, OtherUuid)
    }

    "load rejects a corrupted journal SB" in {
      val (dev, _) = standalone(8)
      val zeros = new Array[Byte](BlockSize)
      dev.writeBlock(0L, zeros)
      an[SfsCorruptError] should be thrownBy Journal.load(dev, 0L, Uuid)
    }
  }

  // ---- integration with Sfs.mount / Sfs.format / Sfs.unmount ---------

  "Sfs integration" - {

    "format creates a clean journal at journalStart" in {
      val dev = RamBlockDevice.default()
      val layout = Sfs.format(dev, smallOpts)
      val fs = Sfs.mount(dev)
      fs.journal.journalStart shouldBe layout.journalStart.toLong
      fs.journal.blockCount shouldBe (layout.journalLen - 1)
      fs.journal.head shouldBe 0
      fs.journal.tail shouldBe 0
      fs.journal.sequence shouldBe 0
      fs.journal.fsUuid shouldBe Uuid
      fs.unmount()
    }

    "unmount persists journal head/tail/sequence to disk" in {
      val dev = RamBlockDevice.default()
      Sfs.format(dev, smallOpts)
      val fs1 = Sfs.mount(dev)
      fs1.journal.advance(newTail = 5, newSeq = 3)
      fs1.journal.replayHead(2)
      fs1.unmount()

      val fs2 = Sfs.mount(dev)
      fs2.journal.head shouldBe 2
      fs2.journal.tail shouldBe 5
      fs2.journal.sequence shouldBe 3
      fs2.unmount()
    }

    "mount fails if journal SB UUID does not match the filesystem UUID" in {
      val dev = RamBlockDevice.default()
      Sfs.format(dev, smallOpts)
      // hand-craft a journal SB with a different UUID
      val fs1 = Sfs.mount(dev)
      val js = JournalSuperblock(
        version = 1,
        blockCount = fs1.journal.blockCount,
        head = 0, tail = 0, sequence = 0,
        fsUuid = OtherUuid,
      )
      val buf = new Array[Byte](BlockSize)
      JournalSuperblock.pack(js, buf, 0)
      // need to skip unmount to leave the wrong-UUID SB in place; but
      // mount() flips fsState=dirty, so do a clean unmount first then
      // poke the SB while the volume is clean
      fs1.unmount()
      dev.writeBlock(fs1.journal.journalStart, buf)
      a[SfsCorruptError] should be thrownBy Sfs.mount(dev)
    }
  }
