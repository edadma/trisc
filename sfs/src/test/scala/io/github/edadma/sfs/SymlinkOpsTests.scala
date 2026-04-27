package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for symlink ops (Phase 12). Two storage paths to exercise:
  *
  *   - Inline (target ≤ 127 UTF-8 bytes): no data block allocated.
  *   - Extent (128 .. 4095 bytes): blocks allocated through FileIO.
  *
  * The plan calls out four sizes: 1, 127, 128, and 4095.
  */
class SymlinkOpsTests extends AnyFreeSpec with Matchers:

  // ---- Test setup -----------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "symlink",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0

  // ---- inline symlinks (≤ 127 bytes) ----------------------------------

  "inline symlinks" - {

    "1-byte target round-trips through symlink + readlink" in {
      val (_, sfs) = mounted()
      val (root1, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        name = "lnk", target = "x",
        uid = 0, gid = 0, timeSec = Now, timeNsec = Nsec,
      )
      HTree.lookup(root1, sfs.device, InoRoot, "lnk") shouldBe
        Some((ino, DirEntry.TypeSymlink))

      val ln = sfs.readInode(ino)
      (ln.flags & InodeFlagInlineSymlink) should not be 0
      ln.size shouldBe 1L
      ln.blockCount shouldBe 0
      SymlinkOps.readlink(ln, sfs.device) shouldBe "x"
      sfs.unmount()
    }

    "127-byte target uses inline storage (no block allocated)" in {
      val (_, sfs) = mounted()
      val target = "a" * 127
      val freeBefore = sfs.blockBitmap.freeCount
      val (_, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", target, 0, 0, Now, Nsec,
      )
      val ln = sfs.readInode(ino)
      (ln.flags & InodeFlagInlineSymlink) should not be 0
      ln.size shouldBe 127L
      ln.blockCount shouldBe 0
      // No data block allocated for the target itself (HTree.insert
      // may have grown the parent directory if its leaf split, but on
      // a fresh root with one entry, no growth is needed).
      sfs.blockBitmap.freeCount shouldBe freeBefore
      SymlinkOps.readlink(ln, sfs.device) shouldBe target
      sfs.unmount()
    }

    "stamps mode = S_IFLNK | 0o777 by default" in {
      val (_, sfs) = mounted()
      val (_, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", "target", 1000, 1000, Now, Nsec,
      )
      val ln = sfs.readInode(ino)
      (ln.mode & FileOps.ModeTypeMask) shouldBe FileOps.ModeSymlink
      (ln.mode & FileOps.ModePermMask) shouldBe SymlinkOps.DefaultPerms
      ln.uid shouldBe 1000
      ln.gid shouldBe 1000
      sfs.unmount()
    }

    "non-ASCII UTF-8 target round-trips" in {
      val (_, sfs) = mounted()
      val target = "café/日本語"
      val (_, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", target, 0, 0, Now, Nsec,
      )
      val ln = sfs.readInode(ino)
      (ln.flags & InodeFlagInlineSymlink) should not be 0
      SymlinkOps.readlink(ln, sfs.device) shouldBe target
      sfs.unmount()
    }
  }

  // ---- extent symlinks (128 .. 4095 bytes) ----------------------------

  "extent symlinks" - {

    "128-byte target switches to extent storage (1 block allocated)" in {
      val (_, sfs) = mounted()
      val target = "b" * 128
      val freeBefore = sfs.blockBitmap.freeCount
      val (_, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", target, 0, 0, Now, Nsec,
      )
      val ln = sfs.readInode(ino)
      (ln.flags & InodeFlagInlineSymlink) shouldBe 0
      ln.size shouldBe 128L
      ln.blockCount shouldBe 8 // one 4 KiB block in 512-byte units
      sfs.blockBitmap.freeCount shouldBe (freeBefore - 1)
      SymlinkOps.readlink(ln, sfs.device) shouldBe target
      sfs.unmount()
    }

    "PATH_MAX-1 (4095-byte) target round-trips through readlink" in {
      val (_, sfs) = mounted()
      val target = (0 until 4095).map(i => ('a' + (i % 26)).toChar).mkString
      val (_, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", target, 0, 0, Now, Nsec,
      )
      val ln = sfs.readInode(ino)
      (ln.flags & InodeFlagInlineSymlink) shouldBe 0
      ln.size shouldBe 4095L
      SymlinkOps.readlink(ln, sfs.device) shouldBe target
      sfs.unmount()
    }

    "rejects target longer than PATH_MAX-1" in {
      val (_, sfs) = mounted()
      val tooLong = "x" * (SymlinkOps.MaxTargetBytes + 1)
      an[IllegalArgumentException] should be thrownBy
        SymlinkOps.symlink(
          sfs.readInode(InoRoot), InoRoot, sfs,
          "lnk", tooLong, 0, 0, Now, Nsec,
        )
      sfs.unmount()
    }
  }

  // ---- unlink interaction ---------------------------------------------

  "unlink + symlink" - {

    "unlink of an inline symlink frees the inode without freeing data blocks" in {
      val (_, sfs) = mounted()
      val (root1, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", "short", 0, 0, Now, Nsec,
      )
      val freeBefore = sfs.blockBitmap.freeCount
      sfs.inodeBitmap.isSet(ino) shouldBe true

      FileOps.unlink(root1, InoRoot, sfs, "lnk", Now, Nsec)
      sfs.inodeBitmap.isSet(ino) shouldBe false
      sfs.blockBitmap.freeCount shouldBe freeBefore // no data blocks to free
      sfs.unmount()
    }

    "unlink of an extent symlink frees its data blocks" in {
      val (_, sfs) = mounted()
      val (root1, ino) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", "b" * 200, 0, 0, Now, Nsec,
      )
      val freeBefore = sfs.blockBitmap.freeCount
      FileOps.unlink(root1, InoRoot, sfs, "lnk", Now, Nsec)
      sfs.inodeBitmap.isSet(ino) shouldBe false
      sfs.blockBitmap.freeCount should be > freeBefore
      sfs.unmount()
    }
  }

  // ---- error handling -------------------------------------------------

  "errors" - {

    "rejects empty target" in {
      val (_, sfs) = mounted()
      an[IllegalArgumentException] should be thrownBy
        SymlinkOps.symlink(
          sfs.readInode(InoRoot), InoRoot, sfs,
          "lnk", "", 0, 0, Now, Nsec,
        )
      sfs.unmount()
    }

    "raises SfsExistsError on duplicate name" in {
      val (_, sfs) = mounted()
      val (root1, _) = SymlinkOps.symlink(
        sfs.readInode(InoRoot), InoRoot, sfs,
        "lnk", "first", 0, 0, Now, Nsec,
      )
      an[SfsExistsError] should be thrownBy
        SymlinkOps.symlink(
          root1, InoRoot, sfs,
          "lnk", "second", 0, 0, Now, Nsec,
        )
      sfs.unmount()
    }

    "readlink raises SfsNotSymlinkError on a non-symlink inode" in {
      val (_, sfs) = mounted()
      val (_, fileIno) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "regular",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      an[SfsNotSymlinkError] should be thrownBy
        SymlinkOps.readlink(sfs.readInode(fileIno), sfs.device)
      sfs.unmount()
    }
  }
