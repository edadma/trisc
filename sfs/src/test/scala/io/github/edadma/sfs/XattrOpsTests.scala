package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** End-to-end tests for [[XattrOps]] over a live mounted filesystem.
  *
  * Phase 14 acceptance per `PLAN.md`:
  *   - round-trip set/get
  *   - multiple attributes
  *   - deletion
  *
  * Plus edge cases that we want covered before moving on:
  *   - replace-existing semantics + Create/Replace flags
  *   - block exhaustion → SfsNoSpaceError
  *   - free the xattr block when the last attr is removed
  *   - free the xattr block when the inode itself is unlinked / rmdir'd
  *   - durability across remount
  *   - journal-replay safety (dirty mount + Recovery)
  */
class XattrOpsTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "xattr",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0

  /** Create a fresh regular file under the root and return its
    * inode number. Persists the (root) parent so subsequent reads
    * resolve. */
  private def createFile(sfs: Sfs, name: String): Int =
    val (newRoot, ino) = FileOps.create(
      sfs.readInode(InoRoot), InoRoot, sfs, name,
      FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
    )
    sfs.writeInode(InoRoot, newRoot)
    ino

  // ---- happy path -----------------------------------------------------

  "round-trip" - {

    "set then get returns the stored value" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.foo", "bar".getBytes("UTF-8"), Now, Nsec)
      val got = XattrOps.get(sfs, ino, "user.foo")
      got.map(new String(_, "UTF-8")) shouldBe Some("bar")
      sfs.unmount()
    }

    "get on unset name returns None (no xattr block at all)" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.get(sfs, ino, "user.absent") shouldBe None
      sfs.unmount()
    }

    "get on unset name returns None when other attrs are present" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.a", Array[Byte](1), Now, Nsec)
      XattrOps.get(sfs, ino, "user.b") shouldBe None
      sfs.unmount()
    }

    "list returns the empty seq when no xattrs are set" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.list(sfs, ino) shouldBe IndexedSeq.empty
      sfs.unmount()
    }
  }

  "multiple attributes" - {

    "list returns insertion order" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.a", Array[Byte](1), Now, Nsec)
      XattrOps.set(sfs, ino, "user.b", Array[Byte](2), Now, Nsec)
      XattrOps.set(sfs, ino, "security.label", "x".getBytes("UTF-8"), Now, Nsec)
      XattrOps.list(sfs, ino) shouldBe IndexedSeq("user.a", "user.b", "security.label")
      sfs.unmount()
    }

    "get retrieves each independently" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.a", Array[Byte](1), Now, Nsec)
      XattrOps.set(sfs, ino, "user.b", Array[Byte](2, 3, 4), Now, Nsec)
      XattrOps.get(sfs, ino, "user.a").get shouldBe Array[Byte](1)
      XattrOps.get(sfs, ino, "user.b").get shouldBe Array[Byte](2, 3, 4)
      sfs.unmount()
    }
  }

  // ---- replace semantics ----------------------------------------------

  "replace" - {

    "default mode (CreateOrReplace) overwrites existing value in place" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1, 2, 3), Now, Nsec)
      XattrOps.set(sfs, ino, "user.k", Array[Byte](9, 9), Now, Nsec)
      XattrOps.list(sfs, ino) shouldBe IndexedSeq("user.k")
      XattrOps.get(sfs, ino, "user.k").get shouldBe Array[Byte](9, 9)
      sfs.unmount()
    }

    "Create mode rejects when name already exists" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1), Now, Nsec)
      a[SfsExistsError] should be thrownBy
        XattrOps.set(sfs, ino, "user.k", Array[Byte](2), Now, Nsec, XattrOps.SetMode.Create)
      sfs.unmount()
    }

    "Replace mode rejects when name does not exist" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      a[SfsNotFoundError] should be thrownBy
        XattrOps.set(sfs, ino, "user.absent", Array[Byte](1), Now, Nsec, XattrOps.SetMode.Replace)
      sfs.unmount()
    }
  }

  // ---- removal --------------------------------------------------------

  "remove" - {

    "drops the named attribute" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.a", Array[Byte](1), Now, Nsec)
      XattrOps.set(sfs, ino, "user.b", Array[Byte](2), Now, Nsec)
      XattrOps.remove(sfs, ino, "user.a", Now, Nsec)
      XattrOps.list(sfs, ino) shouldBe IndexedSeq("user.b")
      XattrOps.get(sfs, ino, "user.a") shouldBe None
      XattrOps.get(sfs, ino, "user.b").get shouldBe Array[Byte](2)
      sfs.unmount()
    }

    "freeing the last attr clears the xattr block + flag and returns the block" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      val freeBefore = sfs.blockBitmap.freeCount
      XattrOps.set(sfs, ino, "user.a", Array[Byte](1), Now, Nsec)
      val freeWhileSet = sfs.blockBitmap.freeCount
      freeWhileSet shouldBe (freeBefore - 1) // one xattr block allocated

      XattrOps.remove(sfs, ino, "user.a", Now, Nsec)
      sfs.blockBitmap.freeCount shouldBe freeBefore
      val ino2 = sfs.readInode(ino)
      (ino2.flags & InodeFlagHasXattr) shouldBe 0
      ino2.xattrBlock shouldBe 0
      sfs.unmount()
    }

    "remove on missing attribute throws SfsNotFoundError" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      a[SfsNotFoundError] should be thrownBy
        XattrOps.remove(sfs, ino, "user.absent", Now, Nsec)
      sfs.unmount()
    }

    "remove on inode with no xattr block at all throws SfsNotFoundError" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      a[SfsNotFoundError] should be thrownBy
        XattrOps.remove(sfs, ino, "user.x", Now, Nsec)
      sfs.unmount()
    }
  }

  // ---- inode lifecycle ------------------------------------------------

  "xattr block is freed on inode deletion" - {

    "unlink → linkCount=0 returns the xattr block" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "victim")
      val freeBefore = sfs.blockBitmap.freeCount
      XattrOps.set(sfs, ino, "user.k", Array.fill(100)(7.toByte), Now, Nsec)
      sfs.blockBitmap.freeCount shouldBe (freeBefore - 1)
      val r = FileOps.unlink(sfs.readInode(InoRoot), InoRoot, sfs, "victim", Now, Nsec)
      sfs.writeInode(InoRoot, r)
      sfs.blockBitmap.freeCount shouldBe freeBefore
      sfs.readInode(ino).linkCount shouldBe 0
      sfs.unmount()
    }

    "rmdir frees an xattr-bearing directory's xattr block" in {
      val (_, sfs) = mounted()
      val (newRoot, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "d",
        FileOps.ModeDirectory | 0x1ed, 1000, 1000, Now, Nsec,
      )
      sfs.writeInode(InoRoot, newRoot)
      val freeBefore = sfs.blockBitmap.freeCount
      XattrOps.set(sfs, dirIno, "user.k", Array.fill(50)(3.toByte), Now, Nsec)
      sfs.blockBitmap.freeCount shouldBe (freeBefore - 1)
      val r = DirOps.rmdir(sfs.readInode(InoRoot), InoRoot, sfs, "d", Now, Nsec)
      sfs.writeInode(InoRoot, r)
      sfs.blockBitmap.freeCount should be > (freeBefore - 1) // block(s) returned
      sfs.unmount()
    }

    "linked file unlink does NOT free the xattr block while another link remains" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "a")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](42), Now, Nsec)
      val withLink = FileOps.link(sfs.readInode(InoRoot), InoRoot, sfs, ino, "b", Now, Nsec)
      sfs.writeInode(InoRoot, withLink)
      val freeBefore = sfs.blockBitmap.freeCount
      val afterUnlink = FileOps.unlink(sfs.readInode(InoRoot), InoRoot, sfs, "a", Now, Nsec)
      sfs.writeInode(InoRoot, afterUnlink)
      sfs.blockBitmap.freeCount shouldBe freeBefore // xattr block survives
      // Still readable through the surviving "b" entry.
      XattrOps.get(sfs, ino, "user.k").get shouldBe Array[Byte](42)
      sfs.unmount()
    }
  }

  // ---- exhaustion -----------------------------------------------------

  "block exhaustion" - {

    "set that overflows the block throws SfsNoSpaceError" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      // Pack ~ a half-block attribute first, then refuse a second one
      // that wouldn't fit.
      val halfPlus = XattrBlock.UsableSize / 2 + 100
      XattrOps.set(sfs, ino, "user.a", new Array[Byte](halfPlus), Now, Nsec)
      a[SfsNoSpaceError] should be thrownBy
        XattrOps.set(sfs, ino, "user.b", new Array[Byte](halfPlus), Now, Nsec)
      // The first attribute is unaffected.
      XattrOps.list(sfs, ino) shouldBe IndexedSeq("user.a")
      sfs.unmount()
    }

    "single max-size attribute fits exactly" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      val name = "x"
      val maxValueLen = XattrBlock.UsableSize - XattrEntry.HeaderSize - name.length
      XattrOps.set(sfs, ino, name, new Array[Byte](maxValueLen), Now, Nsec)
      XattrOps.get(sfs, ino, name).get.length shouldBe maxValueLen
      sfs.unmount()
    }
  }

  // ---- inode metadata side effects ------------------------------------

  "inode metadata" - {

    "set bumps ctime on the inode" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      val before = sfs.readInode(ino).ctimeSec
      val later = before + 100
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1), later, 0)
      sfs.readInode(ino).ctimeSec shouldBe later
      sfs.unmount()
    }

    "remove bumps ctime on the inode" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1), Now, Nsec)
      val later = Now + 200
      XattrOps.remove(sfs, ino, "user.k", later, 0)
      sfs.readInode(ino).ctimeSec shouldBe later
      sfs.unmount()
    }

    "HAS_XATTR flag and xattr_block pointer are stamped on first set" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      val before = sfs.readInode(ino)
      (before.flags & InodeFlagHasXattr) shouldBe 0
      before.xattrBlock shouldBe 0
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1), Now, Nsec)
      val after = sfs.readInode(ino)
      (after.flags & InodeFlagHasXattr) should not be 0
      after.xattrBlock should be > 0
      sfs.unmount()
    }
  }

  // ---- durability across remount --------------------------------------

  "persistence" - {

    "xattrs survive a clean unmount/remount cycle" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.a", "alpha".getBytes("UTF-8"), Now, Nsec)
      XattrOps.set(sfs, ino, "user.b", "beta".getBytes("UTF-8"), Now, Nsec)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      XattrOps.list(sfs2, ino) shouldBe IndexedSeq("user.a", "user.b")
      new String(XattrOps.get(sfs2, ino, "user.a").get, "UTF-8") shouldBe "alpha"
      new String(XattrOps.get(sfs2, ino, "user.b").get, "UTF-8") shouldBe "beta"
      sfs2.unmount()
    }

    "xattrs survive journal recovery on a dirty mount (Phase 13d integration)" in {
      // Simulate a crash by NOT calling unmount; the on-disk SB stays
      // dirty + the xattr writes are committed in the journal but not
      // checkpointed. Recovery should replay them on the next mount.
      val dev = new RamBlockDevice(8192L)
      Sfs.format(dev, smallOpts)
      val sfs = Sfs.mount(dev)
      val ino = createFile(sfs, "survivor")
      XattrOps.set(sfs, ino, "user.k", "kept".getBytes("UTF-8"), Now, Nsec)
      // Don't unmount.

      val sfs2 = Sfs.mount(dev)
      new String(XattrOps.get(sfs2, ino, "user.k").get, "UTF-8") shouldBe "kept"
      sfs2.unmount()
    }
  }
