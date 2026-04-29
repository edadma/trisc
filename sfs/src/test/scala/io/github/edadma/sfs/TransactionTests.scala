package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 13b — Transaction API. Exercises the descriptor + metadata +
  * commit-block layout, the transaction-wide CRC, the in-place
  * write-after-commit step, and abort / no-op / wrap-around / large /
  * escape-flag corner cases. */
class TransactionTests extends AnyFreeSpec with Matchers:

  // ---- helpers --------------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "txn",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  /** A larger journal so we can comfortably do multi-descriptor
    * transactions (need numDescriptors=2 + 600 metadata + 1 commit = 603
    * journal blocks). */
  private def largeOpts: FormatOptions =
    smallOpts.copy(
      totalInodes = 32,
      journalBlocks = 1024,
      volumeName = "txnlarge",
    )

  private def mounted(opts: FormatOptions = smallOpts): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(2048L)
    Sfs.format(dev, opts)
    (dev, Sfs.mount(dev))

  private def mountedLarge(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, largeOpts)
    (dev, Sfs.mount(dev))

  /** Pick an arbitrary block number in the data region for use as a
    * "metadata" target in tests. The journal doesn't care whether the
    * block is real metadata or data — the test just wants a valid disk
    * block to write to. */
  private def dataBlock(sfs: Sfs, offset: Int): Long =
    (sfs.layout.dataStart + offset).toLong

  private def buf(fill: Byte): Array[Byte] =
    Array.fill(BlockSize)(fill)

  /** Build a 4 KiB buffer whose first 4 bytes equal the given int (LE)
    * and the rest is a byte-pattern. */
  private def bufWithLeadingMagic(magic: Int, fill: Byte): Array[Byte] =
    val b = buf(fill)
    Le.putU32(b, 0, magic)
    b

  private def readBlock(dev: RamBlockDevice, n: Long): Array[Byte] =
    val b = new Array[Byte](BlockSize)
    dev.readBlock(n, b)
    b

  // ---- empty commit ---------------------------------------------------

  "empty commit" - {

    "is a no-op (does not touch journal head/tail/seq)" in {
      val (dev, sfs) = mounted()
      val before = (sfs.journal.head, sfs.journal.tail, sfs.journal.sequence)
      val tx = sfs.beginTxn()
      tx.size shouldBe 0
      tx.commit()
      tx.isOpen shouldBe false
      (sfs.journal.head, sfs.journal.tail, sfs.journal.sequence) shouldBe before
      sfs.unmount()
    }
  }

  // ---- single-block txn ---------------------------------------------

  "single-block transaction" - {

    "writes descriptor + metadata + commit to journal and bumps tail" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val journalStart = journal.journalStart
      val target = dataBlock(sfs, 1)
      val payload = buf(0x42.toByte)

      val tx = sfs.beginTxn()
      tx.writeMetadata(target, payload)
      tx.commit()

      // descriptor at log position 0
      val descBuf = readBlock(dev, journal.logPositionToDisk(0))
      val desc = TxnDescriptor.unpack(descBuf, 0, entriesInBlock = 1)
      desc.sequence shouldBe 1
      desc.blockCount shouldBe 1
      desc.entries.length shouldBe 1
      desc.entries.head.fsBlock shouldBe target.toInt
      desc.entries.head.flags shouldBe 0

      // metadata at log position 1 — the journal copy matches what was staged
      val metaBuf = readBlock(dev, journal.logPositionToDisk(1))
      metaBuf.toSeq shouldBe payload.toSeq

      // commit block at log position 2
      val commitBuf = readBlock(dev, journal.logPositionToDisk(2))
      val commit = CommitBlock.unpack(commitBuf, 0)
      commit.sequence shouldBe 1

      // tail advanced by 3 (1 desc + 1 meta + 1 commit), seq bumped to 1.
      // After in-place writes lands, head also advances to tail —
      // the journal record is no longer needed for recovery (the
      // canonical state lives at the fs_block locations).
      journal.tail shouldBe 3
      journal.sequence shouldBe 1
      journal.head shouldBe 3

      sfs.unmount()
    }

    "writes the staged buffer to the in-place fs_block location" in {
      val (dev, sfs) = mounted()
      val target = dataBlock(sfs, 5)
      val payload = buf(0x77.toByte)

      val tx = sfs.beginTxn()
      tx.writeMetadata(target, payload)
      tx.commit()

      readBlock(dev, target).toSeq shouldBe payload.toSeq
      sfs.unmount()
    }

    "commit-block CRC matches an independently computed CRC" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val target = dataBlock(sfs, 1)
      val payload = buf(0x33.toByte)

      val tx = sfs.beginTxn()
      tx.writeMetadata(target, payload)
      tx.commit()

      val descBuf = readBlock(dev, journal.logPositionToDisk(0))
      val metaBuf = readBlock(dev, journal.logPositionToDisk(1))
      val commitBuf = readBlock(dev, journal.logPositionToDisk(2))
      val storedCrc = Le.u32(commitBuf, CommitBlock.CrcOff)

      // Reproduce the txn-wide CRC: descriptor + metadata + commit-with-crc-zeroed
      val commitCopy = commitBuf.clone()
      Le.putU32(commitCopy, CommitBlock.CrcOff, 0)
      var c = Crc32.start
      c = Crc32.update(c, descBuf, 0, BlockSize)
      c = Crc32.update(c, metaBuf, 0, BlockSize)
      c = Crc32.update(c, commitCopy, 0, BlockSize)
      val expected = Crc32.finish(c)

      storedCrc shouldBe expected
      sfs.unmount()
    }
  }

  // ---- multi-block txn ----------------------------------------------

  "multi-block transaction" - {

    "10 distinct blocks land in one descriptor + 10 metadata + 1 commit" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val targets = (0 until 10).map(i => dataBlock(sfs, i + 1))
      val payloads = (0 until 10).map(i => buf((0x10 + i).toByte))

      val tx = sfs.beginTxn()
      targets.zip(payloads).foreach((blk, p) => tx.writeMetadata(blk, p))
      tx.commit()

      val descBuf = readBlock(dev, journal.logPositionToDisk(0))
      val desc = TxnDescriptor.unpack(descBuf, 0, entriesInBlock = 10)
      desc.entries.length shouldBe 10
      desc.entries.zip(targets).foreach((e, t) => e.fsBlock shouldBe t.toInt)

      // metadata blocks 1..10
      (0 until 10).foreach { i =>
        val metaBuf = readBlock(dev, journal.logPositionToDisk(1 + i))
        metaBuf.toSeq shouldBe payloads(i).toSeq
      }

      // in-place writes
      targets.zip(payloads).foreach { (t, p) =>
        readBlock(dev, t).toSeq shouldBe p.toSeq
      }

      journal.tail shouldBe 12
      journal.sequence shouldBe 1
      sfs.unmount()
    }

    "coalesces same-block writes (last write wins)" in {
      val (dev, sfs) = mounted()
      val target = dataBlock(sfs, 1)

      val tx = sfs.beginTxn()
      tx.writeMetadata(target, buf(0x11.toByte))
      tx.writeMetadata(target, buf(0x22.toByte))
      tx.size shouldBe 1
      tx.commit()

      readBlock(dev, target).toSeq shouldBe buf(0x22.toByte).toSeq
      // 1 desc + 1 meta + 1 commit
      sfs.journal.tail shouldBe 3
      sfs.unmount()
    }
  }

  // ---- ESCAPED flag --------------------------------------------------

  "ESCAPED flag" - {

    "stages a magic-leading metadata block with ESCAPED set, XOR'd in journal" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val target = dataBlock(sfs, 1)
      val orig = bufWithLeadingMagic(MagicTxnDescriptor, 0x55.toByte)

      val tx = sfs.beginTxn()
      tx.writeMetadata(target, orig)
      tx.commit()

      val descBuf = readBlock(dev, journal.logPositionToDisk(0))
      val desc = TxnDescriptor.unpack(descBuf, 0, entriesInBlock = 1)
      desc.entries.head.flags shouldBe TxnEntry.FlagEscaped

      // The journal-side metadata has its first word XOR'd; the rest matches.
      val metaBuf = readBlock(dev, journal.logPositionToDisk(1))
      Le.u32(metaBuf, 0) shouldBe (MagicTxnDescriptor ^ Transaction.EscapeSentinel)
      metaBuf.drop(4).toSeq shouldBe orig.drop(4).toSeq

      // The in-place fs_block has the original (unescaped) bytes.
      readBlock(dev, target).toSeq shouldBe orig.toSeq
      sfs.unmount()
    }

    "checks all four magics (SFSJ / SFST / SFSC / SFSD)" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val magics = Seq(
        MagicJournalSuperblock,
        MagicTxnDescriptor,
        MagicCommit,
        MagicDirTail,
      )
      val tx = sfs.beginTxn()
      magics.zipWithIndex.foreach { (m, i) =>
        tx.writeMetadata(dataBlock(sfs, i + 1), bufWithLeadingMagic(m, 0xa5.toByte))
      }
      tx.commit()

      val descBuf = readBlock(dev, journal.logPositionToDisk(0))
      val desc = TxnDescriptor.unpack(descBuf, 0, entriesInBlock = 4)
      desc.entries.foreach(_.flags shouldBe TxnEntry.FlagEscaped)
      sfs.unmount()
    }

    "leaves non-magic blocks unflagged and un-XOR'd in journal" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val orig = buf(0x99.toByte) // first 4 bytes = 0x99999999, not a magic

      val tx = sfs.beginTxn()
      tx.writeMetadata(dataBlock(sfs, 1), orig)
      tx.commit()

      val descBuf = readBlock(dev, journal.logPositionToDisk(0))
      val desc = TxnDescriptor.unpack(descBuf, 0, entriesInBlock = 1)
      desc.entries.head.flags shouldBe 0

      val metaBuf = readBlock(dev, journal.logPositionToDisk(1))
      metaBuf.toSeq shouldBe orig.toSeq
      sfs.unmount()
    }
  }

  // ---- multi-descriptor (>510 entries) ------------------------------

  "multi-descriptor transaction" - {

    "600 blocks span two descriptor blocks" in {
      val (dev, sfs) = mountedLarge()
      val journal = sfs.journal
      val n = 600

      val tx = sfs.beginTxn()
      var i = 0
      while i < n do
        tx.writeMetadata(dataBlock(sfs, i + 1), buf((i & 0xff).toByte))
        i += 1
      tx.commit()

      // First descriptor: 510 entries, total blockCount = 600
      val d0 = TxnDescriptor.unpack(
        readBlock(dev, journal.logPositionToDisk(0)),
        0,
        entriesInBlock = TxnDescriptor.MaxEntriesPerBlock,
      )
      d0.blockCount shouldBe n
      d0.entries.length shouldBe TxnDescriptor.MaxEntriesPerBlock

      // Second descriptor: 600 - 510 = 90 entries, same blockCount + sequence
      val d1 = TxnDescriptor.unpack(
        readBlock(dev, journal.logPositionToDisk(1)),
        0,
        entriesInBlock = n - TxnDescriptor.MaxEntriesPerBlock,
      )
      d1.blockCount shouldBe n
      d1.sequence shouldBe d0.sequence
      d1.entries.length shouldBe (n - TxnDescriptor.MaxEntriesPerBlock)

      // Total log used: 2 desc + 600 meta + 1 commit = 603
      journal.tail shouldBe (2 + n + 1)

      sfs.unmount()
    }
  }

  // ---- wrap-around --------------------------------------------------

  "wrap-around" - {

    "transaction whose run straddles the journal end works correctly" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val bc = journal.blockCount

      // Position the journal so the next commit wraps:
      // small journal of journalBlocks=16 → blockCount=15.
      // Force tail near end via direct advance (this is allowed; it just
      // bumps the in-memory state; we'll write a real txn on top of it).
      // Pick startTail so a 3-block txn (1 desc + 1 meta + 1 commit) wraps.
      // Free a few blocks at the head so the 3-block reservation fits.
      val startTail = bc - 2 // log positions: bc-2 (desc), bc-1 (meta), 0 (commit)
      journal.replayHead(2)
      journal.advance(newTail = startTail, newSeq = 7)

      val target = dataBlock(sfs, 1)
      val payload = buf(0x5a.toByte)
      val tx = sfs.beginTxn()
      tx.writeMetadata(target, payload)
      tx.commit()

      // descriptor at startTail
      val descBuf = readBlock(dev, journal.logPositionToDisk(startTail))
      val desc = TxnDescriptor.unpack(descBuf, 0, entriesInBlock = 1)
      desc.sequence shouldBe 8

      // metadata at startTail+1 == bc-1
      val metaBuf = readBlock(dev, journal.logPositionToDisk(bc - 1))
      metaBuf.toSeq shouldBe payload.toSeq

      // commit at log position 0 (wrapped)
      val commitBuf = readBlock(dev, journal.logPositionToDisk(0))
      val commit = CommitBlock.unpack(commitBuf, 0)
      commit.sequence shouldBe 8

      // tail wrapped to position 1
      journal.tail shouldBe 1
      journal.sequence shouldBe 8

      sfs.unmount()
    }
  }

  // ---- abort --------------------------------------------------------

  "abort" - {

    "discards staged blocks; journal state untouched" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      val target = dataBlock(sfs, 1)
      val before = (journal.head, journal.tail, journal.sequence)

      // Read in-place block before the txn so we can verify it's untouched.
      val origInPlace = readBlock(dev, target).toSeq

      val tx = sfs.beginTxn()
      tx.writeMetadata(target, buf(0xaa.toByte))
      tx.abort()
      tx.isOpen shouldBe false

      (journal.head, journal.tail, journal.sequence) shouldBe before
      readBlock(dev, target).toSeq shouldBe origInPlace
      sfs.unmount()
    }

    "subsequent commit on an aborted tx throws" in {
      val (_, sfs) = mounted()
      val tx = sfs.beginTxn()
      tx.abort()
      an[IllegalArgumentException] should be thrownBy tx.commit()
      sfs.unmount()
    }

    "writeMetadata on a closed (committed) tx throws" in {
      val (_, sfs) = mounted()
      val tx = sfs.beginTxn()
      tx.commit()
      an[IllegalArgumentException] should be thrownBy tx.writeMetadata(
        dataBlock(sfs, 1),
        buf(0.toByte),
      )
      sfs.unmount()
    }
  }

  // ---- per-txn cap --------------------------------------------------

  "per-transaction cap" - {

    "rejects the (MaxBlocksPerTransaction + 1)-th distinct block" in {
      val (_, sfs) = mountedLarge()
      val tx = sfs.beginTxn()
      var i = 0
      while i < MaxBlocksPerTransaction do
        tx.writeMetadata(dataBlock(sfs, i + 1), buf(0.toByte))
        i += 1
      tx.size shouldBe MaxBlocksPerTransaction
      an[SfsNoSpaceError] should be thrownBy
        tx.writeMetadata(
          dataBlock(sfs, MaxBlocksPerTransaction + 1),
          buf(0.toByte),
        )
      tx.abort()
      sfs.unmount()
    }
  }

  // ---- multi-txn flow ----------------------------------------------

  "consecutive transactions" - {

    "two commits bump sequence by 2 and tail by their cumulative size" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal

      val tx1 = sfs.beginTxn()
      tx1.writeMetadata(dataBlock(sfs, 1), buf(0x01.toByte))
      tx1.commit()
      journal.sequence shouldBe 1
      journal.tail shouldBe 3

      val tx2 = sfs.beginTxn()
      tx2.writeMetadata(dataBlock(sfs, 2), buf(0x02.toByte))
      tx2.writeMetadata(dataBlock(sfs, 3), buf(0x03.toByte))
      tx2.commit()
      journal.sequence shouldBe 2
      // tx2 used 1 desc + 2 meta + 1 commit = 4 → tail = 3 + 4 = 7
      journal.tail shouldBe 7

      // Both in-place writes are visible
      readBlock(dev, dataBlock(sfs, 1)).toSeq shouldBe buf(0x01.toByte).toSeq
      readBlock(dev, dataBlock(sfs, 2)).toSeq shouldBe buf(0x02.toByte).toSeq
      readBlock(dev, dataBlock(sfs, 3)).toSeq shouldBe buf(0x03.toByte).toSeq

      sfs.unmount()
    }

    "tail/sequence persist across unmount + remount" in {
      val (dev, sfs) = mounted()
      val tx = sfs.beginTxn()
      tx.writeMetadata(dataBlock(sfs, 1), buf(0x77.toByte))
      tx.commit()
      val (head1, tail1, seq1) = (sfs.journal.head, sfs.journal.tail, sfs.journal.sequence)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      sfs2.journal.head shouldBe head1
      sfs2.journal.tail shouldBe tail1
      sfs2.journal.sequence shouldBe seq1
      sfs2.unmount()
    }
  }

  // ---- journal full ------------------------------------------------

  "journal full" - {

    "commit throws SfsNoSpaceError when reservation exceeds free blocks" in {
      val (dev, sfs) = mounted()
      val journal = sfs.journal
      // journalBlocks=16 → blockCount=15, freeBlocks at empty=14.
      // A 14-block run is OK; a 15-block one isn't (slack rule).
      // Need 1 desc + N meta + 1 commit = N + 2 ≤ 14 → N ≤ 12 fits, N=13 doesn't.
      val tx = sfs.beginTxn()
      var i = 0
      while i < 13 do
        tx.writeMetadata(dataBlock(sfs, i + 1), buf(0.toByte))
        i += 1
      // 13 + 2 = 15 > 14
      an[SfsNoSpaceError] should be thrownBy tx.commit()
      sfs.unmount()
    }
  }
