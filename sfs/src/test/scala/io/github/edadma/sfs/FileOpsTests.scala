package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for the file-level ops layer (Phase 10): create, unlink, link,
  * stat, and the POSIX permission check. Build directly on top of the
  * [[Sfs]] mount point — every test goes through `format → mount → ops
  * → unmount`. */
class FileOpsTests extends AnyFreeSpec with Matchers:

  // ---- Test setup -----------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "fileops",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  /** Format a 32 MiB device, mount, return the live Sfs instance. */
  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0

  // ---- create ---------------------------------------------------------

  "create" - {

    "allocates a new inode and inserts the directory entry" in {
      val (_, sfs) = mounted()
      val root = sfs.readInode(InoRoot)
      val (newRoot, ino) = FileOps.create(
        root, InoRoot, sfs, "hello",
        FileOps.ModeRegular | 0x1a4, // 0o644
        uid = 1000, gid = 1000,
        Now, Nsec,
      )
      ino should be > InoRoot
      HTree.lookup(newRoot, sfs.device, InoRoot, "hello") shouldBe
        Some((ino, DirEntry.TypeRegular))
      sfs.unmount()
    }

    "starts new inodes with linkCount = 1" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "hello",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.readInode(ino).linkCount shouldBe 1
      sfs.unmount()
    }

    "stamps the requested mode/uid/gid and times" in {
      val (_, sfs) = mounted()
      val mode = FileOps.ModeRegular | 0x180 // 0o600
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "hello",
        mode, uid = 7, gid = 13, Now, Nsec,
      )
      val written = sfs.readInode(ino)
      written.mode shouldBe mode
      written.uid shouldBe 7
      written.gid shouldBe 13
      written.mtimeSec shouldBe Now
      written.ctimeSec shouldBe Now
      written.crtimeSec shouldBe Now
      sfs.unmount()
    }

    "rejects mode with S_IFDIR (use mkdir instead)" in {
      val (_, sfs) = mounted()
      an[IllegalArgumentException] should be thrownBy
        FileOps.create(
          sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
          FileOps.ModeDirectory | 0x1ed, 0, 0, Now, Nsec,
        )
      sfs.unmount()
    }

    "raises SfsExistsError on a duplicate name" in {
      val (_, sfs) = mounted()
      val (root1, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "hello",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      an[SfsExistsError] should be thrownBy
        FileOps.create(root1, InoRoot, sfs, "hello",
          FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec)
      sfs.unmount()
    }

    "decrements inodeBitmap.freeCount by 1" in {
      val (_, sfs) = mounted()
      val before = sfs.inodeBitmap.freeCount
      FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "hello",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.inodeBitmap.freeCount shouldBe (before - 1)
      sfs.unmount()
    }

    "bumps the parent's mtime/ctime" in {
      val (_, sfs) = mounted()
      val (newRoot, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "hello",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      newRoot.mtimeSec shouldBe Now
      newRoot.ctimeSec shouldBe Now
      sfs.unmount()
    }

    "reuses a previously-freed inode slot, bumping generation" in {
      val (_, sfs) = mounted()
      var root = sfs.readInode(InoRoot)
      val (root1, ino1) = FileOps.create(
        root, InoRoot, sfs, "victim",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      val gen1 = sfs.readInode(ino1).generation
      val root2 = FileOps.unlink(root1, InoRoot, sfs, "victim", Now, Nsec)
      val (_, ino2) = FileOps.create(
        root2, InoRoot, sfs, "phoenix",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now + 1, Nsec,
      )
      ino2 shouldBe ino1
      sfs.readInode(ino2).generation shouldBe (gen1 + 1)
      sfs.unmount()
    }
  }

  // ---- unlink ---------------------------------------------------------

  "unlink" - {

    "decrements linkCount when target has multiple hard links" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      val root2 = FileOps.link(root1, InoRoot, sfs, ino, "alias", Now, Nsec)
      sfs.readInode(ino).linkCount shouldBe 2

      val root3 = FileOps.unlink(root2, InoRoot, sfs, "alias", Now, Nsec)
      sfs.readInode(ino).linkCount shouldBe 1
      // Inode is still allocated.
      sfs.inodeBitmap.isSet(ino) shouldBe true
      // Source name still resolves.
      HTree.lookup(root3, sfs.device, InoRoot, "src") shouldBe
        Some((ino, DirEntry.TypeRegular))
      sfs.unmount()
    }

    "frees the inode when the last link is removed" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "lonely",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      sfs.inodeBitmap.isSet(ino) shouldBe true

      val root2 = FileOps.unlink(root1, InoRoot, sfs, "lonely", Now, Nsec)
      sfs.inodeBitmap.isSet(ino) shouldBe false
      sfs.readInode(ino).linkCount shouldBe 0
      HTree.lookup(root2, sfs.device, InoRoot, "lonely") shouldBe None
      sfs.unmount()
    }

    "frees the target's data blocks when the last link is removed" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "fat_file",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )

      // Write 32 KiB to the file (8 concrete blocks via FileIO).
      val data = Array.tabulate(32 * 1024)(i => (i & 0xff).toByte)
      val withData = FileIO.writeFile(
        sfs.readInode(ino), sfs,
        offset = 0L, bytes = data, timeSec = Now, timeNsec = Nsec,
      )
      sfs.writeInode(ino, withData)
      val freeAfterWrite = sfs.blockBitmap.freeCount

      FileOps.unlink(root1, InoRoot, sfs, "fat_file", Now, Nsec)
      sfs.blockBitmap.freeCount should be > freeAfterWrite
      sfs.unmount()
    }

    "raises SfsNotFoundError when the name doesn't exist" in {
      val (_, sfs) = mounted()
      an[SfsNotFoundError] should be thrownBy
        FileOps.unlink(sfs.readInode(InoRoot), InoRoot, sfs, "ghost", Now, Nsec)
      sfs.unmount()
    }

    "rejects directories with SfsIsDirectoryError" in {
      val (_, sfs) = mounted()
      // Create a fake directory entry pointing at the root itself —
      // that's enough to drive the type-check.
      val rootIno = sfs.readInode(InoRoot)
      val withFakeDir = HTree.insert(
        rootIno, sfs, InoRoot,
        "fake_subdir", InoRoot, DirEntry.TypeDirectory,
      )
      an[SfsIsDirectoryError] should be thrownBy
        FileOps.unlink(withFakeDir, InoRoot, sfs, "fake_subdir", Now, Nsec)
      sfs.unmount()
    }
  }

  // ---- link -----------------------------------------------------------

  "link" - {

    "bumps target.linkCount and inserts the directory entry" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      val root2 = FileOps.link(root1, InoRoot, sfs, ino, "alias", Now, Nsec)
      sfs.readInode(ino).linkCount shouldBe 2
      HTree.lookup(root2, sfs.device, InoRoot, "alias") shouldBe
        Some((ino, DirEntry.TypeRegular))
      HTree.lookup(root2, sfs.device, InoRoot, "src") shouldBe
        Some((ino, DirEntry.TypeRegular))
      sfs.unmount()
    }

    "stamps target's ctime but not its mtime" in {
      val (_, sfs) = mounted()
      val createTime = Now
      val linkTime = Now + 1000
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, createTime, Nsec,
      )
      FileOps.link(root1, InoRoot, sfs, ino, "alias", linkTime, Nsec)
      val target = sfs.readInode(ino)
      target.mtimeSec shouldBe createTime
      target.ctimeSec shouldBe linkTime
      sfs.unmount()
    }

    "rejects linking a directory" in {
      val (_, sfs) = mounted()
      // Root itself is a directory (mode 0x41ed). Try to link it.
      an[SfsIsDirectoryError] should be thrownBy
        FileOps.link(
          sfs.readInode(InoRoot), InoRoot, sfs,
          targetInodeNum = InoRoot, name = "alias",
          Now, Nsec,
        )
      sfs.unmount()
    }

    "raises SfsExistsError when the new name already exists" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      val (root2, _) = FileOps.create(
        root1, InoRoot, sfs, "other",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      an[SfsExistsError] should be thrownBy
        FileOps.link(root2, InoRoot, sfs, ino, "other", Now, Nsec)
      sfs.unmount()
    }
  }

  // ---- stat -----------------------------------------------------------

  "stat" - {

    "returns the inode contents written by create" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "data",
        FileOps.ModeRegular | 0x180, 7, 13, Now, Nsec,
      )
      val s = FileOps.stat(sfs, ino)
      s.uid shouldBe 7
      s.gid shouldBe 13
      s.linkCount shouldBe 1
      s.mode shouldBe (FileOps.ModeRegular | 0x180)
      sfs.unmount()
    }
  }

  // ---- canAccess ------------------------------------------------------

  "canAccess" - {

    /** Build a fake inode at a given owner uid/gid and permission bits. */
    def ino(uid: Int, gid: Int, perms: Int): Inode =
      Inode(
        mode = FileOps.ModeRegular | perms,
        linkCount = 1, uid = uid, gid = gid, flags = 0,
        size = 0L, blockCount = 0, generation = 1,
        atimeSec = 0, atimeNsec = 0,
        mtimeSec = 0, mtimeNsec = 0,
        ctimeSec = 0, ctimeNsec = 0,
        crtimeSec = 0, crtimeNsec = 0,
        body = InodeBody.EmptyExtents,
        indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
      )

    "root (uid 0) always passes" in {
      val i = ino(uid = 1000, gid = 1000, perms = 0x000)
      FileOps.canAccess(i, 0, 0, FileOps.AccessRead) shouldBe true
      FileOps.canAccess(i, 0, 0, FileOps.AccessWrite) shouldBe true
      FileOps.canAccess(i, 0, 0, FileOps.AccessExec) shouldBe true
    }

    "owner uid uses the owner triplet" in {
      // 0o600 → owner rw, group --, other --
      val i = ino(uid = 1000, gid = 2000, perms = 0x180)
      FileOps.canAccess(i, 1000, 9999, FileOps.AccessRead) shouldBe true
      FileOps.canAccess(i, 1000, 9999, FileOps.AccessWrite) shouldBe true
      FileOps.canAccess(i, 1000, 9999, FileOps.AccessExec) shouldBe false
    }

    "matching gid uses the group triplet (and ignores other)" in {
      // 0o060 → group rw only
      val i = ino(uid = 1000, gid = 2000, perms = 0x030)
      FileOps.canAccess(i, 1234, 2000, FileOps.AccessRead) shouldBe true
      FileOps.canAccess(i, 1234, 2000, FileOps.AccessExec) shouldBe false
    }

    "non-owner non-group uses the other triplet" in {
      // 0o004 → other read only
      val i = ino(uid = 1000, gid = 2000, perms = 0x004)
      FileOps.canAccess(i, 9999, 9999, FileOps.AccessRead) shouldBe true
      FileOps.canAccess(i, 9999, 9999, FileOps.AccessWrite) shouldBe false
    }

    "owner triplet trumps even when also in the group" in {
      // 0o060 (no owner perms, full group). Owner uid matches, so
      // the owner triplet (0) wins — owner has no access.
      val i = ino(uid = 1000, gid = 2000, perms = 0x030)
      FileOps.canAccess(i, 1000, 2000, FileOps.AccessRead) shouldBe false
    }
  }

  // ---- multi-link cycle ----------------------------------------------

  "create + multiple links + repeated unlinks" - {

    "repeatedly add and remove names; inode persists until last unlink" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "primary",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now, Nsec,
      )
      // Add 3 hard links.
      val root2 = FileOps.link(root1, InoRoot, sfs, ino, "link1", Now, Nsec)
      val root3 = FileOps.link(root2, InoRoot, sfs, ino, "link2", Now, Nsec)
      val root4 = FileOps.link(root3, InoRoot, sfs, ino, "link3", Now, Nsec)
      sfs.readInode(ino).linkCount shouldBe 4

      // Remove three of the four names.
      val root5 = FileOps.unlink(root4, InoRoot, sfs, "link2", Now, Nsec)
      val root6 = FileOps.unlink(root5, InoRoot, sfs, "primary", Now, Nsec)
      val root7 = FileOps.unlink(root6, InoRoot, sfs, "link1", Now, Nsec)
      sfs.readInode(ino).linkCount shouldBe 1
      sfs.inodeBitmap.isSet(ino) shouldBe true
      // Last surviving name still resolves.
      HTree.lookup(root7, sfs.device, InoRoot, "link3") shouldBe
        Some((ino, DirEntry.TypeRegular))

      // Removing the last name finally frees the inode.
      val root8 = FileOps.unlink(root7, InoRoot, sfs, "link3", Now, Nsec)
      sfs.inodeBitmap.isSet(ino) shouldBe false
      sfs.readInode(ino).linkCount shouldBe 0
      HTree.lookup(root8, sfs.device, InoRoot, "link3") shouldBe None
      sfs.unmount()
    }
  }
