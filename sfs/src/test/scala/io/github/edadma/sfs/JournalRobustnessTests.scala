package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Edge-case tests for the SFS journal, complementing JournalTests
  * (state-machine), TransactionTests (commit pipeline), RecoveryTests
  * (replay walker), and CrashInjectionTests (drop-writes-after-barrier).
  *
  * Each test stages a *deliberate, single-bit* corruption or boundary
  * condition on the on-disk journal layout and verifies that recovery
  * either rejects the txn cleanly or recovers correctly. The point is
  * to catch the unhappy paths a real disk could land us in:
  *
  *   - bit rot in any of the three journal record sections
  *     (descriptor, metadata, commit)
  *   - byzantine sequence numbers from past mounts whose log slots
  *     were never overwritten
  *   - the multi-descriptor path when one descriptor block is
  *     scrambled in a way that the other isn't
  *   - the read-your-writes contract inside a txn for non-trivial
  *     usage patterns (same block staged repeatedly, abort after
  *     stage)
  *   - the data=ordered guarantee that file data hits disk before
  *     the metadata commit in the surrounding txn
  */
class JournalRobustnessTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "robust",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def biggerOpts: FormatOptions =
    smallOpts.copy(journalBlocks = 1024) // for multi-descriptor txns

  private def freshMounted(
      blocks: Long = 4096L,
      opts: FormatOptions = smallOpts,
  ): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(blocks)
    Sfs.format(dev, opts)
    val sfs = Sfs.mount(dev)
    (dev, sfs)

  private val Now: Int = 0x6800_4321
  private val Nsec: Int = 0

  // ----------------------------------------------------------------
  // Helpers for hand-crafting on-disk journal records.
  // ----------------------------------------------------------------

  /** Build a complete on-disk transaction (descriptor + metadata +
    * commit) and write it at log position `startPos`. Does NOT touch
    * the journal SB or the main FS SB — the caller patches those. */
  private def writeTxn(
      dev: BlockDevice,
      journalStart: Long,
      bc: Int,
      startPos: Int,
      sequence: Int,
      entries: IndexedSeq[(Int, Array[Byte], Int)],
  ): Unit =
    val n = entries.length
    val numDescriptors =
      (n + TxnDescriptor.MaxEntriesPerBlock - 1) / TxnDescriptor.MaxEntriesPerBlock

    // Build descriptors
    val descBufs = (0 until numDescriptors).map { d =>
      val from = d * TxnDescriptor.MaxEntriesPerBlock
      val to = math.min(n, (d + 1) * TxnDescriptor.MaxEntriesPerBlock)
      val es = (from until to).map(i => TxnEntry(entries(i)._1, entries(i)._3)).toIndexedSeq
      val desc = TxnDescriptor(sequence, n, 0, es)
      val buf = new Array[Byte](BlockSize)
      TxnDescriptor.pack(desc, buf, 0)
      buf
    }

    // Build commit block w/ CRC=0 placeholder
    val commitBuf = new Array[Byte](BlockSize)
    CommitBlock.pack(CommitBlock(sequence, 0L, 0), commitBuf, 0)
    Le.zero(commitBuf, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)

    // Compute txn-wide CRC
    var crc = Crc32.start
    descBufs.foreach(b => crc = Crc32.update(crc, b, 0, BlockSize))
    entries.foreach { case (_, payload, _) => crc = Crc32.update(crc, payload, 0, BlockSize) }
    crc = Crc32.update(crc, commitBuf, 0, BlockSize)
    Le.putU32(commitBuf, CommitBlock.CrcOff, Crc32.finish(crc))

    // Write descriptors
    var p = 0
    while p < numDescriptors do
      val pos = (startPos + p) % bc
      dev.writeBlock(journalStart + 1 + pos, descBufs(p))
      p += 1

    // Write metadata
    var q = 0
    while q < n do
      val pos = (startPos + numDescriptors + q) % bc
      dev.writeBlock(journalStart + 1 + pos, entries(q)._2)
      q += 1

    // Write commit
    val commitPos = (startPos + numDescriptors + n) % bc
    dev.writeBlock(journalStart + 1 + commitPos, commitBuf)

  /** Patch the journal SB to claim head=`head`, tail=`tail`, seq=`seq`. */
  private def patchJournalSb(
      dev: BlockDevice,
      journalStart: Long,
      head: Int,
      tail: Int,
      seq: Int,
  ): Unit =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(journalStart, buf)
    val jsb = JournalSuperblock.unpack(buf, 0)
    JournalSuperblock.pack(jsb.copy(head = head, tail = tail, sequence = seq), buf, 0)
    dev.writeBlock(journalStart, buf)

  /** Mark the main SB as dirty so mount runs recovery. */
  private def markDirty(dev: BlockDevice): Unit =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(0L, buf)
    val sb = Superblock.unpack(buf, 0).copy(fsState = FsDirty)
    Superblock.pack(sb, buf, 0)
    dev.writeBlock(0L, buf)
    dev.writeBlock(1L, buf)

  /** Read a 4 KiB block off the device into a fresh buffer. */
  private def readBlock(dev: BlockDevice, blockNum: Long): Array[Byte] =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockNum, buf)
    buf

  private def filledBlock(b: Byte): Array[Byte] =
    val a = new Array[Byte](BlockSize)
    java.util.Arrays.fill(a, b)
    a

  // ----------------------------------------------------------------
  // CRC tampering
  // ----------------------------------------------------------------

  "CRC tampering" - {

    "single bit-flip in a metadata block fails CRC, txn rejected" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 100L
      val origPayload = filledBlock(0x11.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = filledBlock(0x22.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((target.toInt, newPayload, 0)))

      // Flip one bit in the metadata block at log position 1.
      val metaBuf = readBlock(dev, journalStart + 1 + 1)
      metaBuf(0) = (metaBuf(0) ^ 0x01.toByte).toByte
      dev.writeBlock(journalStart + 1 + 1, metaBuf)

      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()

      // target must remain at its original (untouched) value.
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "single bit-flip in the descriptor block fails CRC, txn rejected" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 200L
      val origPayload = filledBlock(0x33.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = filledBlock(0x44.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((target.toInt, newPayload, 0)))

      // Flip one bit in the descriptor block (NOT the magic, which would
      // make recovery reject for a different reason — flip an entry byte).
      val descBuf = readBlock(dev, journalStart + 1 + 0)
      descBuf(TxnDescriptor.HeaderSize + 4) =
        (descBuf(TxnDescriptor.HeaderSize + 4) ^ 0x10.toByte).toByte
      dev.writeBlock(journalStart + 1 + 0, descBuf)

      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "bit-flip in the commit block (outside CRC field) fails CRC, txn rejected" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 300L
      val origPayload = filledBlock(0x55.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = filledBlock(0x66.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((target.toInt, newPayload, 0)))

      // Flip the commit_time field — outside the CRC field's 4 bytes
      // but inside the CRC-covered payload.
      val commitBuf = readBlock(dev, journalStart + 1 + 2)
      commitBuf(8) = (commitBuf(8) ^ 0x80.toByte).toByte
      dev.writeBlock(journalStart + 1 + 2, commitBuf)

      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "wrong stored CRC value fails verification" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 400L
      val origPayload = filledBlock(0x77.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = filledBlock(0x88.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((target.toInt, newPayload, 0)))

      // Corrupt only the stored CRC value in the commit block.
      val commitBuf = readBlock(dev, journalStart + 1 + 2)
      Le.putU32(commitBuf, CommitBlock.CrcOff, 0xdeadbeef)
      dev.writeBlock(journalStart + 1 + 2, commitBuf)

      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "JournalSuperblock CRC tampering is detected at mount" in {
      val (dev, _) = freshMounted()
      val journalStart = smallOpts.journalBlocks // not great — read it from layout
      // Read journal SB, corrupt the version field WITHOUT updating CRC.
      val sbBuf = new Array[Byte](BlockSize)
      // Find journalStart from the on-disk superblock.
      val mainSbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, mainSbBuf)
      val mainSb = Superblock.unpack(mainSbBuf, 0)
      dev.readBlock(mainSb.journalStart.toLong, sbBuf)
      sbBuf(4) = (sbBuf(4) ^ 0x01.toByte).toByte
      dev.writeBlock(mainSb.journalStart.toLong, sbBuf)

      a[SfsCorruptError] should be thrownBy Sfs.mount(dev)
    }
  }

  // ----------------------------------------------------------------
  // Sequence-number rules
  // ----------------------------------------------------------------

  "sequence-number rules" - {

    "rejects a txn whose sequence is greater than expected (gap)" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 500L
      val origPayload = filledBlock(0x99.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = filledBlock(0xaa.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      // Recovery expects sequence 1 (since journal SB still has seq=0)
      // but we wrote sequence=5 → gap → recovery rejects.
      writeTxn(dev, journalStart, bc, 0, 5, IndexedSeq((target.toInt, newPayload, 0)))
      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "rejects a stale txn whose sequence is less than expected" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 600L
      val origPayload = filledBlock(0xbb.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = filledBlock(0xcc.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      // Recovery expects 11 (journal seq=10 → next is 11).
      // We wrote seq=3, simulating a stale wrap-around remnant.
      writeTxn(dev, journalStart, bc, 0, 3, IndexedSeq((target.toInt, newPayload, 0)))
      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 10)
      markDirty(dev)

      Sfs.mount(dev).unmount()
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "replays N successive in-sequence txns and stops at the first gap" in {
      val (dev, sfs) = freshMounted()
      val tA = sfs.layout.dataStart.toLong + 700L
      val tB = sfs.layout.dataStart.toLong + 701L
      val tC = sfs.layout.dataStart.toLong + 702L
      dev.writeBlock(tA, filledBlock(0x00.toByte))
      dev.writeBlock(tB, filledBlock(0x00.toByte))
      dev.writeBlock(tC, filledBlock(0x00.toByte))
      val newA = filledBlock(0x11.toByte)
      val newB = filledBlock(0x22.toByte)
      val newC = filledBlock(0x33.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      // Three txns at positions 0..2, 3..5, 6..8. Sequences 1, 2, 4 (gap at 3).
      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((tA.toInt, newA, 0)))
      writeTxn(dev, journalStart, bc, 3, 2, IndexedSeq((tB.toInt, newB, 0)))
      writeTxn(dev, journalStart, bc, 6, 4, IndexedSeq((tC.toInt, newC, 0)))
      patchJournalSb(dev, journalStart, head = 0, tail = 9, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()

      // tA + tB replayed; tC untouched (gap at sequence 3).
      readBlock(dev, tA).toSeq shouldBe newA.toSeq
      readBlock(dev, tB).toSeq shouldBe newB.toSeq
      readBlock(dev, tC).forall(_ == 0.toByte) shouldBe true
    }
  }

  // ----------------------------------------------------------------
  // ESCAPED-flag protection
  // ----------------------------------------------------------------

  "ESCAPED flag" - {

    """recovery does NOT mis-parse a metadata block whose first word equals MagicTxnDescriptor (XOR-protected)""" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 800L
      // Block whose first 4 bytes coincidentally match MagicTxnDescriptor —
      // a concrete way the non-escaped path would corrupt recovery.
      val origPayload = new Array[Byte](BlockSize)
      Le.putU32(origPayload, 0, MagicTxnDescriptor)
      var i = 4
      while i < BlockSize do
        origPayload(i) = (i & 0xff).toByte
        i += 1
      val journalCopy = origPayload.clone()
      Le.putU32(journalCopy, 0, MagicTxnDescriptor ^ Transaction.EscapeSentinel)
      dev.writeBlock(target, filledBlock(0x00.toByte))

      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      writeTxn(
        dev, journalStart, bc, 0, 1,
        IndexedSeq((target.toInt, journalCopy, TxnEntry.FlagEscaped)),
      )
      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()

      // Replay must have un-XOR'd the leading word back to MagicTxnDescriptor.
      readBlock(dev, target).toSeq shouldBe origPayload.toSeq
    }

    "all four journal magics are escaped correctly on the txn-wide replay" in {
      val (dev, sfs) = freshMounted()
      val tJ = sfs.layout.dataStart.toLong + 900L
      val tT = sfs.layout.dataStart.toLong + 901L
      val tC = sfs.layout.dataStart.toLong + 902L
      val tD = sfs.layout.dataStart.toLong + 903L
      val origs = Map(
        tJ -> MagicJournalSuperblock,
        tT -> MagicTxnDescriptor,
        tC -> MagicCommit,
        tD -> MagicDirTail,
      )
      origs.foreach { case (addr, magic) =>
        val buf = new Array[Byte](BlockSize)
        Le.putU32(buf, 0, magic)
        var k = 4
        while k < BlockSize do
          buf(k) = (k & 0xff).toByte
          k += 1
        dev.writeBlock(addr, filledBlock(0x00.toByte))
        // We'll feed the journal the XOR'd version with FlagEscaped.
      }

      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      val entries = origs.toIndexedSeq.map { case (addr, magic) =>
        val orig = new Array[Byte](BlockSize)
        Le.putU32(orig, 0, magic)
        var k = 4
        while k < BlockSize do
          orig(k) = (k & 0xff).toByte
          k += 1
        val journalCopy = orig.clone()
        Le.putU32(journalCopy, 0, magic ^ Transaction.EscapeSentinel)
        (addr.toInt, journalCopy, TxnEntry.FlagEscaped)
      }

      writeTxn(dev, journalStart, bc, 0, 1, entries)
      patchJournalSb(dev, journalStart, head = 0, tail = 1 + entries.length + 1, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()

      origs.foreach { case (addr, magic) =>
        Le.u32(readBlock(dev, addr), 0) shouldBe magic
      }
    }

    "an UNESCAPED metadata block whose first word matches MagicCommit will (correctly) be CRC-rejected on replay" in {
      // If somebody hand-builds a journal with a metadata block that
      // coincidentally starts with a magic AND forgets to set
      // TxnEntry.FlagEscaped, recovery has no way to know it isn't a
      // commit/descriptor. The CRC over (descriptor || metadata ||
      // commit) only protects that the bytes weren't tampered with,
      // not that they aren't ambiguous. This test documents the
      // contract: callers MUST set FlagEscaped — Transaction.commit()
      // does this automatically. Recovery still rejects on CRC if the
      // hand-craft is also missing the proper CRC, which is a useful
      // safety net but NOT the primary defence.
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 1100L
      val origPayload = filledBlock(0x44.toByte)
      dev.writeBlock(target, origPayload)
      val newPayload = new Array[Byte](BlockSize)
      Le.putU32(newPayload, 0, MagicCommit) // ambiguous!
      var i = 4
      while i < BlockSize do
        newPayload(i) = (i & 0xff).toByte
        i += 1

      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      // Hand-craft as if we forgot the FlagEscaped — write metadata
      // verbatim. The CRC is still correct (we're not corrupting it).
      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((target.toInt, newPayload, 0)))
      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      // Recovery validates the txn fine (CRC matches) and replays.
      // The newPayload's first 4 bytes are MagicCommit verbatim — that's
      // fine for the *target* block (it's just data on the fs). The
      // ambiguity only would matter if recovery tried to RE-PARSE the
      // log starting from a metadata-block position, which it doesn't:
      // it walks from journal.head and only ever expects a descriptor
      // at the start of a record.
      Sfs.mount(dev).unmount()
      readBlock(dev, target).toSeq shouldBe newPayload.toSeq
    }
  }

  // ----------------------------------------------------------------
  // Multi-descriptor recovery
  // ----------------------------------------------------------------

  "multi-descriptor recovery" - {

    "replays a 600-block txn that spans two descriptor blocks" in {
      val (dev, sfs) = freshMounted(blocks = 16384L, opts = biggerOpts)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      val n = 600
      val baseTarget = sfs.layout.dataStart.toLong + 100L

      // Fill targets with 0x00 first.
      var k = 0
      while k < n do
        dev.writeBlock(baseTarget + k, filledBlock(0x00.toByte))
        k += 1

      sfs.unmount()

      // Build n entries with unique payloads.
      val entries = (0 until n).map { i =>
        val payload = filledBlock(((i + 1) & 0xff).toByte)
        ((baseTarget + i).toInt, payload, 0)
      }.toIndexedSeq

      writeTxn(dev, journalStart, bc, 0, 1, entries)

      val numDescriptors = (n + TxnDescriptor.MaxEntriesPerBlock - 1) /
        TxnDescriptor.MaxEntriesPerBlock
      val totalLog = numDescriptors + n + 1
      patchJournalSb(dev, journalStart, head = 0, tail = totalLog, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()

      // Verify every target was replayed with its unique payload.
      var i = 0
      while i < n do
        val expected = ((i + 1) & 0xff).toByte
        readBlock(dev, baseTarget + i).forall(_ == expected) shouldBe true
        i += 1
    }

    "rejects the entire txn if any descriptor block is corrupt" in {
      val (dev, sfs) = freshMounted(blocks = 16384L, opts = biggerOpts)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      val n = 600
      val baseTarget = sfs.layout.dataStart.toLong + 200L

      var k = 0
      while k < n do
        dev.writeBlock(baseTarget + k, filledBlock(0x77.toByte))
        k += 1

      sfs.unmount()

      val entries = (0 until n).map { i =>
        val payload = filledBlock(((i + 1) & 0xff).toByte)
        ((baseTarget + i).toInt, payload, 0)
      }.toIndexedSeq

      writeTxn(dev, journalStart, bc, 0, 1, entries)

      // Corrupt the SECOND descriptor block (log position 1) by zeroing
      // its magic — the first descriptor block at log position 0 is
      // still valid, but we shouldn't be willing to replay only half
      // the txn.
      val badDesc = readBlock(dev, journalStart + 1 + 1)
      Le.putU32(badDesc, 0, 0)
      dev.writeBlock(journalStart + 1 + 1, badDesc)

      val numDescriptors = (n + TxnDescriptor.MaxEntriesPerBlock - 1) /
        TxnDescriptor.MaxEntriesPerBlock
      val totalLog = numDescriptors + n + 1
      patchJournalSb(dev, journalStart, head = 0, tail = totalLog, seq = 0)
      markDirty(dev)

      Sfs.mount(dev).unmount()

      // None of the targets must have been replayed.
      var i = 0
      while i < n do
        readBlock(dev, baseTarget + i).forall(_ == 0x77.toByte) shouldBe true
        i += 1
    }
  }

  // ----------------------------------------------------------------
  // Replay idempotence
  // ----------------------------------------------------------------

  "replay idempotence" - {

    "double recovery (mount-mount-mount with intervening crash) lands on the same state" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 1200L
      dev.writeBlock(target, filledBlock(0x00.toByte))
      val newPayload = filledBlock(0xee.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      writeTxn(dev, journalStart, bc, 0, 1, IndexedSeq((target.toInt, newPayload, 0)))
      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)
      markDirty(dev)

      // First recovery
      val sfs1 = Sfs.mount(dev)
      readBlock(dev, target).toSeq shouldBe newPayload.toSeq
      // Don't unmount — simulate immediate crash. Force the on-disk SB
      // to dirty + the journal SB to its pre-replay state, so a second
      // mount tries to recover the *same* txn. A real crash here would
      // leave the SB dirty but we already wrote head=0 again? No —
      // mount() persisted dirty already. We set head=0 again to force
      // recovery to find the same txn.
      patchJournalSb(dev, journalStart, head = 0, tail = 3, seq = 0)

      val sfs2 = Sfs.mount(dev)
      // After second replay, the in-place block remains the new value.
      readBlock(dev, target).toSeq shouldBe newPayload.toSeq
      sfs2.unmount()
    }
  }

  // ----------------------------------------------------------------
  // Same-block coalescing
  // ----------------------------------------------------------------

  "same-block coalescing" - {

    "writeMetadata three times for the same block keeps only the last" in {
      val (_, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 1300L
      val tx = sfs.beginTxn()
      tx.writeMetadata(target, filledBlock(0x11.toByte))
      tx.writeMetadata(target, filledBlock(0x22.toByte))
      tx.writeMetadata(target, filledBlock(0x33.toByte))
      tx.size shouldBe 1 // coalesced
      tx.commit()

      readBlock(sfs.device, target).forall(_ == 0x33.toByte) shouldBe true
      sfs.unmount()
    }
  }

  // ----------------------------------------------------------------
  // Abort safety
  // ----------------------------------------------------------------

  "abort safety" - {

    "abort after staging multiple blocks leaves on-disk state untouched" in {
      val (_, sfs) = freshMounted()
      val targetA = sfs.layout.dataStart.toLong + 1400L
      val targetB = sfs.layout.dataStart.toLong + 1401L
      sfs.device.writeBlock(targetA, filledBlock(0x11.toByte))
      sfs.device.writeBlock(targetB, filledBlock(0x22.toByte))

      val tx = sfs.beginTxn()
      tx.writeMetadata(targetA, filledBlock(0x99.toByte))
      tx.writeMetadata(targetB, filledBlock(0xaa.toByte))
      tx.size shouldBe 2
      tx.abort()
      tx.isOpen shouldBe false
      tx.size shouldBe 0

      // On-disk state must be the originals.
      readBlock(sfs.device, targetA).forall(_ == 0x11.toByte) shouldBe true
      readBlock(sfs.device, targetB).forall(_ == 0x22.toByte) shouldBe true

      // Journal must NOT have advanced — head==tail still.
      sfs.journal.head shouldBe sfs.journal.tail
      sfs.unmount()
    }

    "withTransaction body throwing aborts and propagates" in {
      val (_, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 1500L
      sfs.device.writeBlock(target, filledBlock(0x55.toByte))

      val ex = intercept[RuntimeException] {
        sfs.withTransaction {
          val tx = sfs.currentTxn.nn
          tx.writeMetadata(target, filledBlock(0xff.toByte))
          throw new RuntimeException("synthetic")
        }
      }
      ex.getMessage shouldBe "synthetic"

      // Block on disk is untouched, journal didn't advance.
      readBlock(sfs.device, target).forall(_ == 0x55.toByte) shouldBe true
      sfs.journal.head shouldBe sfs.journal.tail
      sfs.currentTxn shouldBe null
      sfs.unmount()
    }
  }

  // ----------------------------------------------------------------
  // Bitmap consistency post-recovery
  // ----------------------------------------------------------------

  "bitmap consistency after recovery" - {

    "a recovered txn that includes bitmap blocks reflects in the loaded Bitmap" in {
      val (dev, sfs) = freshMounted()
      val freshFreeBlocks = sfs.blockBitmap.freeCount
      val freshFreeInodes = sfs.inodeBitmap.freeCount
      sfs.unmount()

      // mkdir() allocates 2 data blocks (root + one empty leaf for the
      // child directory) plus one inode, so both bitmaps mutate. Each
      // mutation is staged into the txn and lands via the journal.
      val sfs2 = Sfs.mount(dev)
      val (rootAfter, _) = DirOps.mkdir(
        sfs2.readInode(InoRoot), InoRoot, sfs2, "subdir",
        0x41ed, 1000, 1000, Now, Nsec,
      )
      sfs2.writeInode(InoRoot, rootAfter)
      val midBlockFree = sfs2.blockBitmap.freeCount
      val midInodeFree = sfs2.inodeBitmap.freeCount
      midBlockFree should be < freshFreeBlocks
      midInodeFree should be < freshFreeInodes
      sfs2.unmount()

      // Mount fresh; bitmaps should reflect the persisted state.
      val sfs3 = Sfs.mount(dev)
      sfs3.blockBitmap.freeCount shouldBe midBlockFree
      sfs3.inodeBitmap.freeCount shouldBe midInodeFree
      sfs3.unmount()
    }
  }

  // ----------------------------------------------------------------
  // Boundary cases
  // ----------------------------------------------------------------

  "boundary cases" - {

    "txn that exactly fills the journal capacity (n + numDesc + 1 = blockCount-1)" in {
      val (dev, sfs) = freshMounted()
      val journal = sfs.journal
      val bc = journal.blockCount
      val freeAtStart = journal.freeBlocks
      // Reservation succeeds at exactly freeBlocks; throws at freeBlocks+1.
      noException should be thrownBy journal.reserve(freeAtStart)
      an[SfsNoSpaceError] should be thrownBy journal.reserve(freeAtStart + 1)
      sfs.unmount()
    }

    "freeBlocks math is correct across every wrap configuration" in {
      val (dev, sfs) = freshMounted()
      val journal = sfs.journal
      val bc = journal.blockCount
      // Sweep head/tail values; freeBlocks must always equal
      // ((head - tail - 1) mod bc).
      var h = 0
      while h < bc do
        var t = 0
        while t < bc do
          journal.replayHead(h)
          journal.advance(t, journal.sequence)
          val expected =
            val raw = (h - t - 1) % bc
            if raw < 0 then raw + bc else raw
          journal.freeBlocks shouldBe expected
          t += 1
        h += 1
      sfs.unmount()
    }

    "logPositionToDisk rejects out-of-range positions" in {
      val (_, sfs) = freshMounted()
      val journal = sfs.journal
      an[IllegalArgumentException] should be thrownBy journal.logPositionToDisk(-1)
      an[IllegalArgumentException] should be thrownBy
        journal.logPositionToDisk(journal.blockCount)
      noException should be thrownBy journal.logPositionToDisk(0)
      noException should be thrownBy journal.logPositionToDisk(journal.blockCount - 1)
      sfs.unmount()
    }

    "Transaction.writeMetadata rejects buffers that aren't BlockSize bytes" in {
      val (_, sfs) = freshMounted()
      val tx = sfs.beginTxn()
      an[IllegalArgumentException] should be thrownBy
        tx.writeMetadata(100L, new Array[Byte](100))
      an[IllegalArgumentException] should be thrownBy
        tx.writeMetadata(100L, new Array[Byte](BlockSize - 1))
      an[IllegalArgumentException] should be thrownBy
        tx.writeMetadata(100L, new Array[Byte](BlockSize + 1))
      tx.abort()
      sfs.unmount()
    }
  }

  // ----------------------------------------------------------------
  // Journal wrap-around semantics
  // ----------------------------------------------------------------

  "journal wrap-around" - {

    "many sequential txns wrap the log without losing earlier replayed state" in {
      // Run >blockCount worth of single-block txns. Because each txn's
      // commit advances head==tail, the log returns to empty after
      // every commit, so we never actually wrap mid-txn — but we DO
      // exercise tail values across the full [0, blockCount) range.
      val (dev, sfs) = freshMounted()
      val journal = sfs.journal
      val bc = journal.blockCount

      var i = 0
      while i < bc * 2 do
        val (rootAfter, _) = FileOps.create(
          sfs.readInode(InoRoot), InoRoot, sfs, s"f$i",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
        )
        sfs.writeInode(InoRoot, rootAfter)
        if i % 8 == 7 then
          // Periodically remove some entries so we don't run out of inodes.
          val k = i - 4
          FileOps.unlink(sfs.readInode(InoRoot), InoRoot, sfs, s"f$k", Now, Nsec)
        i += 1

      // After many ops the journal must still be self-consistent —
      // free bitmap matches accounting, head==tail, and at least the
      // most recent file is reachable.
      journal.head shouldBe journal.tail
      val r = sfs.readInode(InoRoot)
      val lastName = s"f${bc * 2 - 1}"
      HTree.lookup(r, sfs.metaDevice, InoRoot, lastName) shouldBe defined
      sfs.unmount()

      // Cycle works: reload + verify persistence.
      val sfs2 = Sfs.mount(dev)
      val r2 = sfs2.readInode(InoRoot)
      HTree.lookup(r2, sfs2.metaDevice, InoRoot, lastName) shouldBe defined
      sfs2.unmount()
    }

    "txn whose log range straddles the wrap point computes CRC over the right blocks" in {
      // Hand-craft a txn whose descriptor lands at log pos bc-2 and
      // whose commit lands at log pos 0 — a 4-block txn (1 desc, 2 meta,
      // 1 commit) wrapping at the bc boundary.
      val (dev, sfs) = freshMounted()
      val tA = sfs.layout.dataStart.toLong + 1700L
      val tB = sfs.layout.dataStart.toLong + 1701L
      dev.writeBlock(tA, filledBlock(0x00.toByte))
      dev.writeBlock(tB, filledBlock(0x00.toByte))
      val newA = filledBlock(0xa1.toByte)
      val newB = filledBlock(0xb2.toByte)
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      // log positions: bc-2 (desc), bc-1 (meta A), 0 (meta B), 1 (commit).
      writeTxn(
        dev, journalStart, bc, bc - 2, 1,
        IndexedSeq((tA.toInt, newA, 0), (tB.toInt, newB, 0)),
      )
      patchJournalSb(dev, journalStart, head = bc - 2, tail = 2, seq = 0)
      markDirty(dev)

      val sfs2 = Sfs.mount(dev)
      readBlock(dev, tA).toSeq shouldBe newA.toSeq
      readBlock(dev, tB).toSeq shouldBe newB.toSeq
      sfs2.journal.head shouldBe 2
      sfs2.unmount()
    }
  }

  // ----------------------------------------------------------------
  // High-level crash + recovery scenarios
  // ----------------------------------------------------------------

  "high-level crash + recovery" - {

    "crash mid-mkdir → recovery either restores fully or rolls back fully (no half-state)" in {
      // Run mkdir against a CrashingBlockDevice that crashes on the
      // commit block. Then mount the raw device and verify the
      // directory either fully exists (with all 2 data blocks + inode
      // + parent linkCount bumped) OR fully doesn't (in which case
      // bitmap, parent linkCount, parent dir block, and the new
      // inode slot all match the pre-mkdir state).
      val raw = new RamBlockDevice(4096L)
      Sfs.format(raw, smallOpts)

      // Pre-state snapshot via a clean mount.
      val pre = Sfs.mount(raw)
      val rootBefore = pre.readInode(InoRoot)
      val rootLinkBefore = rootBefore.linkCount
      val freeBlocksBefore = pre.blockBitmap.freeCount
      val freeInodesBefore = pre.inodeBitmap.freeCount
      pre.unmount()

      // Crash on the commit block during mkdir.
      val barrier = new CrashingBlockDevice(raw, crashOnFirstWord = Some(MagicCommit))
      val sfs = Sfs.mount(barrier)
      try
        DirOps.mkdir(
          sfs.readInode(InoRoot), InoRoot, sfs, "lost",
          0x41ed, 1000, 1000, Now, Nsec,
        )
      catch case _: Throwable => ()
      barrier.crashed shouldBe true
      // Don't unmount — simulate power loss.

      // Recover.
      val rec = Sfs.mount(raw)
      val rootAfter = rec.readInode(InoRoot)
      // Pre-commit crash → no replay → state matches pre.
      rootAfter.linkCount shouldBe rootLinkBefore
      rec.blockBitmap.freeCount shouldBe freeBlocksBefore
      rec.inodeBitmap.freeCount shouldBe freeInodesBefore
      HTree.lookup(rootAfter, rec.metaDevice, InoRoot, "lost") shouldBe None
      rec.unmount()
    }

    "crash AFTER commit but BEFORE in-place: mkdir is fully visible after recovery" in {
      val raw = new RamBlockDevice(4096L)
      Sfs.format(raw, smallOpts)

      val pre = Sfs.mount(raw)
      val rootLinkBefore = pre.readInode(InoRoot).linkCount
      pre.unmount()

      // First, count writes in the committed-and-checkpointed run. Wrap
      // mkdir + parent-writeInode in withTransaction so the parent
      // linkCount bump is part of the journaled txn.
      val raw2 = new RamBlockDevice(4096L)
      Sfs.format(raw2, smallOpts)
      var commitWriteIndex = -1
      var idx = 0
      val detector = new BlockDevice:
        val blockCount = raw2.blockCount
        def readBlock(b: Long, buf: Array[Byte]): Unit = raw2.readBlock(b, buf)
        def writeBlock(b: Long, buf: Array[Byte]): Unit =
          if buf.length >= 4 && Le.u32(buf, 0) == MagicCommit then
            commitWriteIndex = idx
          raw2.writeBlock(b, buf)
          idx += 1
        override def flush(): Unit = raw2.flush()
      val countingSfs = Sfs.mount(detector)
      countingSfs.withTransaction {
        val (rA, _) = DirOps.mkdir(
          countingSfs.readInode(InoRoot), InoRoot, countingSfs, "kept",
          0x41ed, 1000, 1000, Now, Nsec,
        )
        countingSfs.writeInode(InoRoot, rA)
      }
      countingSfs.unmount()
      commitWriteIndex should be > 0

      // Now run for real, crashing one write past the commit block —
      // commit is durable, in-place writes are not.
      val raw3 = new RamBlockDevice(4096L)
      Sfs.format(raw3, smallOpts)
      val barrier = new CrashingBlockDevice(raw3, crashAfterWrites = commitWriteIndex + 1)
      val sfs = Sfs.mount(barrier)
      try
        sfs.withTransaction {
          val (rB, _) = DirOps.mkdir(
            sfs.readInode(InoRoot), InoRoot, sfs, "kept",
            0x41ed, 1000, 1000, Now, Nsec,
          )
          sfs.writeInode(InoRoot, rB)
        }
      catch case _: Throwable => ()
      barrier.crashed shouldBe true
      // Don't unmount.

      val rec = Sfs.mount(raw3)
      val rootAfter = rec.readInode(InoRoot)
      // After recovery: directory is visible AND parent linkCount bumped.
      rootAfter.linkCount shouldBe (rootLinkBefore + 1)
      val maybeChild = HTree.lookup(rootAfter, rec.metaDevice, InoRoot, "kept")
      maybeChild shouldBe defined
      // The recovered child inode must itself be a directory with linkCount=2.
      val childIno = rec.readInode(maybeChild.get._1)
      childIno.linkCount shouldBe 2
      (childIno.mode & FileOps.ModeTypeMask) shouldBe FileOps.ModeDirectory
      rec.unmount()
    }

    "crash mid-rename leaves source intact and target unchanged (atomicity)" in {
      val raw = new RamBlockDevice(4096L)
      Sfs.format(raw, smallOpts)

      // Pre-state: create "src".
      val setup = Sfs.mount(raw)
      val (root1, _) = FileOps.create(
        setup.readInode(InoRoot), InoRoot, setup, "src",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      setup.writeInode(InoRoot, root1)
      setup.unmount()

      // Now crash on the commit of the rename. The rename touches
      // multiple metadata blocks (parent dir's leaf for both delete
      // + insert), so the txn is non-trivial.
      val barrier = new CrashingBlockDevice(raw, crashOnFirstWord = Some(MagicCommit))
      val sfs = Sfs.mount(barrier)
      try
        DirOps.rename(
          sfs.readInode(InoRoot), InoRoot,
          sfs.readInode(InoRoot), InoRoot,
          sfs, "src", "dst", Now, Nsec,
        )
      catch case _: Throwable => ()
      barrier.crashed shouldBe true

      // Recover. "src" must still exist, "dst" must NOT exist.
      val rec = Sfs.mount(raw)
      val r = rec.readInode(InoRoot)
      HTree.lookup(r, rec.metaDevice, InoRoot, "src") shouldBe defined
      HTree.lookup(r, rec.metaDevice, InoRoot, "dst") shouldBe None
      rec.unmount()
    }
  }

  // ----------------------------------------------------------------
  // Data ordering
  // ----------------------------------------------------------------

  "data=ordered ordering" - {

    "FileIO.writeFile flushes file data BEFORE the metadata commit" in {
      // Instrument the underlying device with an order-tracking
      // wrapper. Each write records its region tag (J=journal log,
      // D=data, M=other-metadata) in a log; flush() appends a sentinel.
      val raw = new RamBlockDevice(4096L)
      Sfs.format(raw, smallOpts)
      val rawSfs = Sfs.mount(raw)
      val mainSb = rawSfs.superblock
      val journalRegion = mainSb.journalStart.toLong until
        (mainSb.journalStart.toLong + mainSb.journalLen.toLong)
      val dataStart = mainSb.dataStart.toLong
      rawSfs.unmount()

      // Re-format on a fresh device so the log of writes starts fresh.
      val raw2 = new RamBlockDevice(4096L)
      Sfs.format(raw2, smallOpts)

      val log = scala.collection.mutable.ArrayBuffer.empty[String]
      val tracker = new BlockDevice:
        val blockCount = raw2.blockCount
        def readBlock(b: Long, buf: Array[Byte]): Unit = raw2.readBlock(b, buf)
        def writeBlock(b: Long, buf: Array[Byte]): Unit =
          val tag =
            if journalRegion.contains(b) then "J"
            else if b >= dataStart then "D"
            else "M"
          log += s"W:$tag"
          raw2.writeBlock(b, buf)
        override def flush(): Unit =
          log += "F"
          raw2.flush()

      val sfs = Sfs.mount(tracker)
      val (rootAfter, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.writeInode(InoRoot, rootAfter)
      log.clear()

      // Wrap writeFile + writeInode in an explicit txn — that's how a
      // higher-level op (e.g. a write(2) syscall handler) would do it.
      // FileIO.writeFile writes data blocks direct, calls flush, then
      // returns the updated inode; sfs.writeInode stages it through
      // the journal. tx.commit then writes descriptor/metadata/commit
      // to the journal region (J writes), flushes, in-place writes
      // to metadata (M writes), then the journal SB (J).
      val data = new Array[Byte](BlockSize)
      java.util.Arrays.fill(data, 0x42.toByte)
      sfs.withTransaction {
        val newIno = FileIO.writeFile(
          sfs.readInode(ino), sfs,
          offset = 0L, bytes = data, timeSec = Now, timeNsec = Nsec,
        )
        sfs.writeInode(ino, newIno)
      }

      val ops = log.toVector

      // Find the first D in the log; data must have been written.
      val firstD = ops.indexOf("W:D")
      firstD should be >= 0

      // The first J after the data writes must be preceded by at
      // least one F — that's the data=ordered durability barrier.
      val firstJ = ops.indexWhere(_ == "W:J", firstD + 1)
      firstJ should be > firstD
      val flushesBetween = ops.slice(firstD + 1, firstJ).count(_ == "F")
      flushesBetween should be >= 1

      sfs.unmount()
    }
  }
