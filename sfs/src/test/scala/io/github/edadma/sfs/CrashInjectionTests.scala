package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 13e: crash-injection tests.
  *
  * Wraps a real [[RamBlockDevice]] in a [[CrashingBlockDevice]] that
  * silently drops writes past a configurable barrier. The Sfs API is
  * unaware of the crash — it sees `flush()` succeed (the inner device
  * buffered the write, but the wrapper drops the underlying flush
  * post-crash) and `writeBlock` returns normally — so this exactly
  * mimics a power-loss event. Then we discard the live Sfs without
  * `unmount()`, mount the underlying device fresh, and let recovery
  * decide what survived.
  */
class CrashInjectionTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "crashtest",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  /** Fresh format, mount the *crashable* wrapper. The underlying real
    * device is returned separately so the test can mount it cleanly
    * after the crash. */
  private def freshCrashable(
      blocks: Long = 4096L,
      crashAfterWrites: Int = Int.MaxValue,
      crashOnFirstWord: Option[Int] = None,
  ): (RamBlockDevice, CrashingBlockDevice, Sfs) =
    val raw = new RamBlockDevice(blocks)
    Sfs.format(raw, smallOpts)
    val wrapper = new CrashingBlockDevice(raw, crashAfterWrites, crashOnFirstWord)
    val sfs = Sfs.mount(wrapper)
    (raw, wrapper, sfs)

  private val Now: Int = 0x6800_4321
  private val Nsec: Int = 0

  "crash before commit block lands" - {

    "txn does NOT replay (file was never created from recovery's POV)" in {
      // Crash on the FIRST commit-block write attempted by the
      // FileOps.create call below. Anything before that (descriptor +
      // metadata in the journal log, all data writes) is durable;
      // the commit block never touches disk.
      val (raw, wrapper, sfs) =
        freshCrashable(crashOnFirstWord = Some(MagicCommit))

      // Try to create a file. This builds a transaction, writes the
      // descriptor + metadata blocks, then tries to write the commit
      // block — the wrapper swallows that write. Subsequent in-place
      // writes are also swallowed (`crashed` flag is sticky).
      val rootBefore = sfs.readInode(InoRoot)
      try
        FileOps.create(rootBefore, InoRoot, sfs, "ghost",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec)
      catch case _: Throwable => () // we don't care if anything threw

      wrapper.crashed shouldBe true
      // Don't unmount; simulate a power loss. The on-disk SB is still
      // dirty (from the original mount), the journal SB still points
      // at head/tail of the in-progress txn, and the in-place writes
      // for the new file's inode + dir block were swallowed too.

      // Mount the raw device and verify the file does NOT exist.
      val sfs2 = Sfs.mount(raw)
      val r = sfs2.readInode(InoRoot)
      HTree.lookup(r, sfs2.device, InoRoot, "ghost") shouldBe None
      sfs2.unmount()
    }
  }

  "crash after commit but before in-place writes" - {

    "recovery replays the txn — file IS visible" in {
      // The strategy: count the writes the txn does before the commit
      // block, then crash one write *after* the commit. That leaves
      // the journal log fully durable but no in-place writes.
      //
      // For FileOps.create on a fresh dir, a typical sequence is:
      //   N journal log writes (descriptor + metadata + commit)
      //   then in-place writes (inode block, dir leaf block, bitmaps).
      // We don't know N exactly, so use a 2-pass approach: run the
      // op once on a clean device to count, then redo it with the
      // crash barrier set to (commit-write-index + 1).
      val raw1 = new RamBlockDevice(4096L)
      Sfs.format(raw1, smallOpts)
      val counter = new CrashingBlockDevice(raw1)
      val sfs1 = Sfs.mount(counter)
      val (rootAfter, ino) = FileOps.create(
        sfs1.readInode(InoRoot), InoRoot, sfs1, "kept",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs1.writeInode(InoRoot, rootAfter)
      val totalWrites = counter.writes

      // We don't know precisely how many writes belonged to the txn
      // vs. the in-place phase, but we DO know the journal-log writes
      // happen first (descriptor → metadata → commit) and then
      // `device.flush()` lands, then the in-place writes. So the
      // commit block lands somewhere in the first half. Picking
      // `commitWriteIndex` = total - 1 (i.e. let everything but the
      // last in-place write land) is overkill but lets us focus on
      // the post-commit-pre-replay scenario by comparison.
      //
      // For "after commit, before in-place": find the actual commit
      // write by scanning. We do this by re-running with magic-match
      // and a counter on top so we know its index.
      val raw2 = new RamBlockDevice(4096L)
      Sfs.format(raw2, smallOpts)

      // Detector wrapper: forwards every write but increments a
      // counter and records the index of any commit-block write.
      var commitWriteIndex: Int = -1
      var idx: Int = 0
      val detector = new BlockDevice:
        val blockCount = raw2.blockCount
        def readBlock(b: Long, buf: Array[Byte]): Unit = raw2.readBlock(b, buf)
        def writeBlock(b: Long, buf: Array[Byte]): Unit =
          if buf.length >= 4 && Le.u32(buf, 0) == MagicCommit then
            commitWriteIndex = idx
          raw2.writeBlock(b, buf)
          idx += 1
        override def flush(): Unit = raw2.flush()
      val sfs2 = Sfs.mount(detector)
      val (rootAfter2, _) = FileOps.create(
        sfs2.readInode(InoRoot), InoRoot, sfs2, "kept",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs2.writeInode(InoRoot, rootAfter2)
      sfs2.unmount()
      commitWriteIndex should be > 0

      // Third run: crash one write *after* the commit. This means
      // the commit block IS durable, but the in-place writes (inode,
      // dir block, bitmaps, journal SB head-advance) get swallowed.
      val raw3 = new RamBlockDevice(4096L)
      Sfs.format(raw3, smallOpts)
      val barrier = new CrashingBlockDevice(raw3, crashAfterWrites = commitWriteIndex + 1)
      val sfs3 = Sfs.mount(barrier)
      try
        val (r3, _) = FileOps.create(
          sfs3.readInode(InoRoot), InoRoot, sfs3, "kept",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
        )
        sfs3.writeInode(InoRoot, r3)
      catch case _: Throwable => ()
      barrier.crashed shouldBe true
      // Don't unmount.

      // Now mount raw3 cleanly. Recovery should walk the journal,
      // find the committed txn, and replay every metadata block —
      // including the dir leaf and the inode that holds "kept".
      val sfs4 = Sfs.mount(raw3)
      val r = sfs4.readInode(InoRoot)
      HTree.lookup(r, sfs4.device, InoRoot, "kept") shouldBe defined
      sfs4.unmount()
    }
  }

  "crash mid-multi-txn" - {

    "the first txn (already committed + checkpointed) survives, the second is rolled back" in {
      // Strategy: do one full create() (txn 1 — fully durable + checkpointed),
      // then start a second create and crash on its commit block.
      val (raw, wrapper, sfs) =
        freshCrashable(crashOnFirstWord = Some(MagicCommit))

      // Hmm — but the FIRST create's commit block also has MagicCommit.
      // So this barrier crashes the first one, not the second. Need
      // a different strategy: do txn 1 against the raw device first,
      // unmount, then mount with the magic-on-commit wrapper and try
      // txn 2.

      // Actually freshCrashable already mounted with the wrapper. The
      // first create will crash too. Let's restructure.

      sfs.unmount() // dispose; we built the wrapper too eagerly.

      val raw2 = new RamBlockDevice(4096L)
      Sfs.format(raw2, smallOpts)
      val firstSfs = Sfs.mount(raw2)
      val (root1, _) = FileOps.create(
        firstSfs.readInode(InoRoot), InoRoot, firstSfs, "kept",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      firstSfs.writeInode(InoRoot, root1)
      firstSfs.unmount()

      // Now wrap, mount, attempt the second create — crash on its commit.
      val wrapper2 = new CrashingBlockDevice(raw2, crashOnFirstWord = Some(MagicCommit))
      val secondSfs = Sfs.mount(wrapper2)
      try
        FileOps.create(
          secondSfs.readInode(InoRoot), InoRoot, secondSfs, "lost",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
        )
      catch case _: Throwable => ()
      wrapper2.crashed shouldBe true

      // Mount cleanly. txn 1 ("kept") survives, txn 2 ("lost") is rolled back.
      val rec = Sfs.mount(raw2)
      val r = rec.readInode(InoRoot)
      HTree.lookup(r, rec.device, InoRoot, "kept") shouldBe defined
      HTree.lookup(r, rec.device, InoRoot, "lost") shouldBe None
      rec.unmount()
    }
  }
