package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 13d: dirty-mount recovery tests.
  *
  * The unit-level tests reach below the public API to drive the
  * journal directly so we can simulate "crashed before in-place
  * write" / "crashed before commit lands" / "stale txn past tail"
  * scenarios that would be hard to reproduce through the regular
  * mutator path.
  *
  * The end-to-end tests use the public mutators, then simulate a
  * crash by skipping `unmount()` and remounting, and verify that
  * recovery + replay leaves the volume in the expected state.
  */
class RecoveryTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "rectest",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  /** A device large enough for plenty of operations + a roomy journal. */
  private def freshMounted(blocks: Long = 4096L): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(blocks)
    Sfs.format(dev, smallOpts)
    val sfs = Sfs.mount(dev)
    (dev, sfs)

  private val Now: Int = 0x6800_4321
  private val Nsec: Int = 0

  // ---- Public-API end-to-end recovery --------------------------------

  "end-to-end recovery" - {

    "no-op on a dirty mount with an empty journal" in {
      val (dev, sfs) = freshMounted()
      // Don't unmount — superblock stays dirty, journal log is empty
      // (head == tail, no committed txns since the last clean state).
      val sfs2 = Sfs.mount(dev)
      sfs2.isMounted shouldBe true
      // No exception, no spurious changes.
      sfs2.readInode(InoRoot).linkCount shouldBe 2
      sfs2.unmount()
    }

    "create + crash + remount: file visible via journal replay" in {
      val (dev, sfs) = freshMounted()
      val (rootAfter, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "kept",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.writeInode(InoRoot, rootAfter)
      // Skip unmount — but the journal commit advances head==tail, so
      // the journal log itself is empty when we crash here. The
      // in-place writes already landed (data=ordered + commit().
      // Recovery is therefore a no-op, but the metadata is durable.
      val sfs2 = Sfs.mount(dev)
      val r = sfs2.readInode(InoRoot)
      HTree.lookup(r, sfs2.device, InoRoot, "kept") shouldBe Some((ino, DirEntry.TypeRegular))
      sfs2.unmount()
    }
  }

  // ---- Direct-journal injection tests --------------------------------

  "journal replay" - {

    "replays a single committed txn that was never checkpointed" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 100L
      val journalStart = sfs.journal.journalStart
      // Unmount FIRST so on-disk SB + journal SB are at known clean state.
      // Anything we patch after this is the post-crash state we want
      // recovery to handle.
      sfs.unmount()

      // Hand-craft post-crash state:
      //   - data block `target` has bytes "OLD"
      //   - the journal has a committed txn that says it should be "NEW"
      //   - JournalSuperblock points at this txn as unreplayed
      //   - main SB.fsState = dirty so recovery runs at next mount
      val oldBuf = new Array[Byte](BlockSize)
      java.util.Arrays.fill(oldBuf, 0xaa.toByte)
      dev.writeBlock(target, oldBuf)

      val newBuf = new Array[Byte](BlockSize)
      java.util.Arrays.fill(newBuf, 0xbb.toByte)

      val descBuf = new Array[Byte](BlockSize)
      val seq = 999
      val desc = TxnDescriptor(
        sequence = seq,
        blockCount = 1,
        flags = 0,
        entries = IndexedSeq(TxnEntry(target.toInt, 0)),
      )
      TxnDescriptor.pack(desc, descBuf, 0)

      val commitBuf = new Array[Byte](BlockSize)
      CommitBlock.pack(CommitBlock(seq, 0L, 0), commitBuf, 0)
      Le.zero(commitBuf, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)

      var crc = Crc32.start
      crc = Crc32.update(crc, descBuf, 0, BlockSize)
      crc = Crc32.update(crc, newBuf, 0, BlockSize)
      crc = Crc32.update(crc, commitBuf, 0, BlockSize)
      Le.putU32(commitBuf, CommitBlock.CrcOff, Crc32.finish(crc))

      dev.writeBlock(journalStart + 1, descBuf)
      dev.writeBlock(journalStart + 2, newBuf)
      dev.writeBlock(journalStart + 3, commitBuf)

      val jsbBuf = new Array[Byte](BlockSize)
      dev.readBlock(journalStart, jsbBuf)
      val jsb = JournalSuperblock.unpack(jsbBuf, 0)
      JournalSuperblock.pack(jsb.copy(head = 0, tail = 3, sequence = seq - 1), jsbBuf, 0)
      dev.writeBlock(journalStart, jsbBuf)

      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsDirty)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)

      val sfs2 = Sfs.mount(dev)
      val readBack = new Array[Byte](BlockSize)
      dev.readBlock(target, readBack)
      readBack.toSeq shouldBe newBuf.toSeq
      sfs2.unmount()
    }

    "stops replay at the first invalid commit (no replay past the bad one)" in {
      val (dev, sfs) = freshMounted()
      val tA = sfs.layout.dataStart.toLong + 200L
      val tB = sfs.layout.dataStart.toLong + 201L
      val journalStart = sfs.journal.journalStart
      sfs.unmount()

      val origA = new Array[Byte](BlockSize)
      java.util.Arrays.fill(origA, 0x11.toByte)
      dev.writeBlock(tA, origA)
      val origB = new Array[Byte](BlockSize)
      java.util.Arrays.fill(origB, 0x22.toByte)
      dev.writeBlock(tB, origB)

      val newA = new Array[Byte](BlockSize)
      java.util.Arrays.fill(newA, 0x33.toByte)
      val newB = new Array[Byte](BlockSize)
      java.util.Arrays.fill(newB, 0x44.toByte)

      // Build txn 1: valid, target tA → newA, seq=10
      val desc1 = new Array[Byte](BlockSize)
      TxnDescriptor.pack(
        TxnDescriptor(10, 1, 0, IndexedSeq(TxnEntry(tA.toInt, 0))),
        desc1, 0,
      )
      val commit1 = new Array[Byte](BlockSize)
      CommitBlock.pack(CommitBlock(10, 0L, 0), commit1, 0)
      Le.zero(commit1, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)
      var crc = Crc32.start
      crc = Crc32.update(crc, desc1, 0, BlockSize)
      crc = Crc32.update(crc, newA, 0, BlockSize)
      crc = Crc32.update(crc, commit1, 0, BlockSize)
      Le.putU32(commit1, CommitBlock.CrcOff, Crc32.finish(crc))

      // Build txn 2: BAD CRC. Target tB → newB, seq=11, but commit's CRC is wrong.
      val desc2 = new Array[Byte](BlockSize)
      TxnDescriptor.pack(
        TxnDescriptor(11, 1, 0, IndexedSeq(TxnEntry(tB.toInt, 0))),
        desc2, 0,
      )
      val commit2 = new Array[Byte](BlockSize)
      CommitBlock.pack(CommitBlock(11, 0L, 0xdeadbeef), commit2, 0)
      Le.zero(commit2, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)

      // Layout: positions 0..2 = txn 1 (desc, meta, commit); 3..5 = txn 2.
      dev.writeBlock(journalStart + 1, desc1)
      dev.writeBlock(journalStart + 2, newA)
      dev.writeBlock(journalStart + 3, commit1)
      dev.writeBlock(journalStart + 4, desc2)
      dev.writeBlock(journalStart + 5, newB)
      dev.writeBlock(journalStart + 6, commit2)

      // Patch journal SB: head=0, tail=6, seq=9
      val jsbBuf = new Array[Byte](BlockSize)
      dev.readBlock(journalStart, jsbBuf)
      val jsb = JournalSuperblock.unpack(jsbBuf, 0)
      JournalSuperblock.pack(jsb.copy(head = 0, tail = 6, sequence = 9), jsbBuf, 0)
      dev.writeBlock(journalStart, jsbBuf)

      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsDirty)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)

      val sfs2 = Sfs.mount(dev)

      // tA must be replayed (newA), tB must be untouched (origB).
      val readA = new Array[Byte](BlockSize)
      dev.readBlock(tA, readA)
      readA.toSeq shouldBe newA.toSeq

      val readB = new Array[Byte](BlockSize)
      dev.readBlock(tB, readB)
      readB.toSeq shouldBe origB.toSeq

      sfs2.unmount()
    }

    "undoes the ESCAPED XOR sentinel during replay" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 300L
      val journalStart = sfs.journal.journalStart
      sfs.unmount()

      // Create an "original" payload whose first 4 bytes = MagicCommit
      // (so the journal would have escaped it) and unique tail bytes.
      val origPayload = new Array[Byte](BlockSize)
      Le.putU32(origPayload, 0, MagicCommit) // would coincide with a journal magic
      var i = 4
      while i < BlockSize do
        origPayload(i) = (i & 0xff).toByte
        i += 1

      // Stage what *the journal* would have written: the first word is XOR'd.
      val journalCopy = origPayload.clone()
      Le.putU32(journalCopy, 0, MagicCommit ^ Transaction.EscapeSentinel)

      val seq = 42
      val desc = new Array[Byte](BlockSize)
      TxnDescriptor.pack(
        TxnDescriptor(seq, 1, 0, IndexedSeq(TxnEntry(target.toInt, TxnEntry.FlagEscaped))),
        desc, 0,
      )
      val commit = new Array[Byte](BlockSize)
      CommitBlock.pack(CommitBlock(seq, 0L, 0), commit, 0)
      Le.zero(commit, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)
      var crc = Crc32.start
      crc = Crc32.update(crc, desc, 0, BlockSize)
      crc = Crc32.update(crc, journalCopy, 0, BlockSize)
      crc = Crc32.update(crc, commit, 0, BlockSize)
      Le.putU32(commit, CommitBlock.CrcOff, Crc32.finish(crc))

      dev.writeBlock(journalStart + 1, desc)
      dev.writeBlock(journalStart + 2, journalCopy)
      dev.writeBlock(journalStart + 3, commit)

      val jsbBuf = new Array[Byte](BlockSize)
      dev.readBlock(journalStart, jsbBuf)
      val jsb = JournalSuperblock.unpack(jsbBuf, 0)
      JournalSuperblock.pack(jsb.copy(head = 0, tail = 3, sequence = seq - 1), jsbBuf, 0)
      dev.writeBlock(journalStart, jsbBuf)

      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsDirty)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)

      Sfs.mount(dev).unmount()

      val readBack = new Array[Byte](BlockSize)
      dev.readBlock(target, readBack)
      // The XOR must have been undone: in-place block matches the original.
      readBack.toSeq shouldBe origPayload.toSeq
    }

    "stops at a partial txn that has descriptor + metadata but no commit block" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 500L
      val journalStart = sfs.journal.journalStart
      sfs.unmount()

      val orig = new Array[Byte](BlockSize)
      java.util.Arrays.fill(orig, 0x77.toByte)
      dev.writeBlock(target, orig)

      val newBuf = new Array[Byte](BlockSize)
      java.util.Arrays.fill(newBuf, 0x88.toByte)

      val seq = 5
      val desc = new Array[Byte](BlockSize)
      TxnDescriptor.pack(
        TxnDescriptor(seq, 1, 0, IndexedSeq(TxnEntry(target.toInt, 0))),
        desc, 0,
      )
      // No commit block — leave position 3 as whatever-was-there. The
      // device freshly-formatted has zeros there, which is NOT MagicCommit.
      dev.writeBlock(journalStart + 1, desc)
      dev.writeBlock(journalStart + 2, newBuf)
      // Position 3 stays zeros → magic mismatch → txn rejected.

      val jsbBuf = new Array[Byte](BlockSize)
      dev.readBlock(journalStart, jsbBuf)
      val jsb = JournalSuperblock.unpack(jsbBuf, 0)
      JournalSuperblock.pack(jsb.copy(head = 0, tail = 3, sequence = seq - 1), jsbBuf, 0)
      dev.writeBlock(journalStart, jsbBuf)

      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsDirty)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)

      Sfs.mount(dev).unmount()

      // target must NOT have been replayed.
      val readBack = new Array[Byte](BlockSize)
      dev.readBlock(target, readBack)
      readBack.toSeq shouldBe orig.toSeq
    }

    "replays a wrap-around txn (descriptor at end, commit at start of log)" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 600L
      val journalStart = sfs.journal.journalStart
      val bc = sfs.journal.blockCount
      sfs.unmount()

      val orig = new Array[Byte](BlockSize)
      java.util.Arrays.fill(orig, 0x99.toByte)
      dev.writeBlock(target, orig)

      val newBuf = new Array[Byte](BlockSize)
      java.util.Arrays.fill(newBuf, 0xcc.toByte)

      val seq = 33
      val desc = new Array[Byte](BlockSize)
      TxnDescriptor.pack(
        TxnDescriptor(seq, 1, 0, IndexedSeq(TxnEntry(target.toInt, 0))),
        desc, 0,
      )
      val commit = new Array[Byte](BlockSize)
      CommitBlock.pack(CommitBlock(seq, 0L, 0), commit, 0)
      Le.zero(commit, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)
      var crc = Crc32.start
      crc = Crc32.update(crc, desc, 0, BlockSize)
      crc = Crc32.update(crc, newBuf, 0, BlockSize)
      crc = Crc32.update(crc, commit, 0, BlockSize)
      Le.putU32(commit, CommitBlock.CrcOff, Crc32.finish(crc))

      // Place the txn at log positions (bc-2, bc-1, 0) so it wraps.
      // Disk-level positions: journalStart + 1 + (logPos % bc).
      val descPos = bc - 2
      val metaPos = bc - 1
      val commitPos = 0
      dev.writeBlock(journalStart + 1 + descPos, desc)
      dev.writeBlock(journalStart + 1 + metaPos, newBuf)
      dev.writeBlock(journalStart + 1 + commitPos, commit)

      // head=bc-2, tail=1 (so the run [bc-2, bc-1, 0] is unreplayed).
      val jsbBuf = new Array[Byte](BlockSize)
      dev.readBlock(journalStart, jsbBuf)
      val jsb = JournalSuperblock.unpack(jsbBuf, 0)
      JournalSuperblock.pack(jsb.copy(head = bc - 2, tail = 1, sequence = seq - 1), jsbBuf, 0)
      dev.writeBlock(journalStart, jsbBuf)

      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsDirty)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)

      val sfs2 = Sfs.mount(dev)
      val readBack = new Array[Byte](BlockSize)
      dev.readBlock(target, readBack)
      readBack.toSeq shouldBe newBuf.toSeq
      // After replay, head moved to tail.
      sfs2.journal.head shouldBe 1
      sfs2.unmount()
    }

    "advances head to tail after a successful full-log replay" in {
      val (dev, sfs) = freshMounted()
      val target = sfs.layout.dataStart.toLong + 400L
      val journalStart = sfs.journal.journalStart
      sfs.unmount()

      val payload = new Array[Byte](BlockSize)
      java.util.Arrays.fill(payload, 0x55.toByte)

      val seq = 7
      val desc = new Array[Byte](BlockSize)
      TxnDescriptor.pack(
        TxnDescriptor(seq, 1, 0, IndexedSeq(TxnEntry(target.toInt, 0))),
        desc, 0,
      )
      val commit = new Array[Byte](BlockSize)
      CommitBlock.pack(CommitBlock(seq, 0L, 0), commit, 0)
      Le.zero(commit, CommitBlock.PayloadSize, BlockSize - CommitBlock.PayloadSize)
      var crc = Crc32.start
      crc = Crc32.update(crc, desc, 0, BlockSize)
      crc = Crc32.update(crc, payload, 0, BlockSize)
      crc = Crc32.update(crc, commit, 0, BlockSize)
      Le.putU32(commit, CommitBlock.CrcOff, Crc32.finish(crc))

      dev.writeBlock(journalStart + 1, desc)
      dev.writeBlock(journalStart + 2, payload)
      dev.writeBlock(journalStart + 3, commit)

      val jsbBuf = new Array[Byte](BlockSize)
      dev.readBlock(journalStart, jsbBuf)
      val jsb = JournalSuperblock.unpack(jsbBuf, 0)
      JournalSuperblock.pack(jsb.copy(head = 0, tail = 3, sequence = seq - 1), jsbBuf, 0)
      dev.writeBlock(journalStart, jsbBuf)

      val sbBuf = new Array[Byte](BlockSize)
      dev.readBlock(0L, sbBuf)
      val sb = Superblock.unpack(sbBuf, 0).copy(fsState = FsDirty)
      Superblock.pack(sb, sbBuf, 0)
      dev.writeBlock(0L, sbBuf)
      dev.writeBlock(1L, sbBuf)

      val sfs2 = Sfs.mount(dev)
      // After full replay, head must equal tail (3) — log is empty.
      sfs2.journal.head shouldBe 3
      sfs2.journal.tail shouldBe 3
      sfs2.unmount()
    }
  }
