package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Integration tests for Phase 13c: every public mutator now runs
  * inside `Sfs.withTransaction`, metadata writes are journaled (and
  * staged-then-replayed in-place), and reads inside the same txn see
  * the staged changes. */
class Phase13cWireThroughTests extends AnyFreeSpec with Matchers:

  private val Now: Int = 0x6800_4321
  private val Nsec: Int = 0

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "phase13c",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def freshMounted(blocks: Long = 4096L): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(blocks)
    Sfs.format(dev, smallOpts)
    val sfs = Sfs.mount(dev)
    (dev, sfs)

  "withTransaction" - {

    "is re-entrant: nested calls don't open a second txn" in {
      val (_, sfs) = freshMounted()
      sfs.currentTxn shouldBe null
      val outerTxn = sfs.withTransaction {
        val outer = sfs.currentTxn
        outer should not be null
        sfs.withTransaction {
          // Same txn observed inside the nested call.
          sfs.currentTxn shouldBe outer
        }
        outer
      }
      sfs.currentTxn shouldBe null
      outerTxn should not be null
      sfs.unmount()
    }

    "stages bitmap writes via Bitmap.stageInto at commit time" in {
      val (_, sfs) = freshMounted()
      // create() wraps in withTransaction. Commit advances head==tail
      // because the in-place writes succeed.
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "phase13c",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      ino should be > 0
      sfs.journal.head shouldBe sfs.journal.tail
      sfs.journal.sequence should be > 0
      sfs.unmount()
    }
  }

  "read-your-writes within a txn" - {

    "rename-overwrite sees the just-deleted entry as gone" in {
      val (_, sfs) = freshMounted()
      // Create two regular files.
      val (root1, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.writeInode(InoRoot, root1)
      val (root2, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "dst",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.writeInode(InoRoot, root2)

      // Rename src → dst (same parent, overwriting non-dir target).
      // Inside one txn, FileOps.unlink stages the dst-removal, then
      // HTree.insert reads back via metaDevice and sees the staged
      // state — the new dst entry replaces the old without a duplicate
      // collision.
      val (_, finalRoot) = DirOps.rename(
        sfs.readInode(InoRoot), InoRoot,
        sfs.readInode(InoRoot), InoRoot,
        sfs, "src", "dst", Now, Nsec,
      )
      sfs.writeInode(InoRoot, finalRoot)

      val rootAfter = sfs.readInode(InoRoot)
      HTree.lookup(rootAfter, sfs.device, InoRoot, "src") shouldBe None
      HTree.lookup(rootAfter, sfs.device, InoRoot, "dst") shouldBe defined
      sfs.unmount()
    }

    "writeInode sees other inode updates already staged in same 4 KiB block" in {
      val (_, sfs) = freshMounted()
      // The first inode-table block holds 16 inodes (208 B each, plus
      // padding). Several creates in one txn → same table block —
      // each writeInode does a read-modify-write and must see the
      // earlier inode updates from the same txn or it would clobber
      // them.
      sfs.withTransaction {
        val (r1, ino1) = FileOps.create(
          sfs.readInode(InoRoot), InoRoot, sfs, "a",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
        )
        sfs.writeInode(InoRoot, r1)
        val (r2, ino2) = FileOps.create(
          sfs.readInode(InoRoot), InoRoot, sfs, "b",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
        )
        sfs.writeInode(InoRoot, r2)
        val (r3, ino3) = FileOps.create(
          sfs.readInode(InoRoot), InoRoot, sfs, "c",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
        )
        sfs.writeInode(InoRoot, r3)
        ino1 should not be ino2
        ino2 should not be ino3
      }

      // After the outer txn commits, all three inodes are persisted.
      val rootIno = sfs.readInode(InoRoot)
      Set("a", "b", "c").foreach { n =>
        HTree.lookup(rootIno, sfs.device, InoRoot, n) shouldBe defined
      }
      sfs.unmount()
    }
  }

  "data=ordered" - {

    "writeFile writes data blocks directly (not through the journal)" in {
      val (dev, sfs) = freshMounted()
      val (rootAfterCreate, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.writeInode(InoRoot, rootAfterCreate)

      val data = Array.tabulate(BlockSize)(i => (i & 0xff).toByte)
      val withData = FileIO.writeFile(
        sfs.readInode(ino), sfs,
        offset = 0L, bytes = data, timeSec = Now, timeNsec = Nsec,
      )
      sfs.writeInode(ino, withData)

      // Read the data back — it must be the exact bytes we wrote,
      // proving the data block was written *somewhere* (the raw
      // device, since the journal is metadata-only).
      val readBack = FileIO.readFile(sfs.readInode(ino), sfs.device, 0L, BlockSize)
      readBack.toSeq shouldBe data.toSeq
      sfs.unmount()
    }
  }

  "persistence across remount" - {

    "every per-op metadata change survives unmount + mount" in {
      val (dev, sfs) = freshMounted()
      val rootBefore = sfs.readInode(InoRoot)
      val (root1, _) = FileOps.create(rootBefore, InoRoot, sfs, "alpha",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec)
      sfs.writeInode(InoRoot, root1)
      val (root2, dirIno) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs,
        "subdir", 0x41ed, 1000, 1000, Now, Nsec)
      sfs.writeInode(InoRoot, root2)
      val (root3, _) = SymlinkOps.symlink(sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", "/etc/hosts", 1000, 1000, Now, Nsec)
      sfs.writeInode(InoRoot, root3)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      val r = sfs2.readInode(InoRoot)
      HTree.lookup(r, sfs2.device, InoRoot, "alpha") shouldBe defined
      HTree.lookup(r, sfs2.device, InoRoot, "subdir") shouldBe defined
      HTree.lookup(r, sfs2.device, InoRoot, "lnk") shouldBe defined
      sfs2.unmount()
    }
  }
