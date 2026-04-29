package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for the directory ops layer (Phase 11): mkdir, rmdir, readdir.
  * Rename is a follow-up. */
class DirOpsTests extends AnyFreeSpec with Matchers:

  // ---- Test setup -----------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "dirops",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0
  private val DirMode: Int = FileOps.ModeDirectory | 0x1ed // 0o755

  // ---- mkdir ----------------------------------------------------------

  "mkdir" - {

    "creates a subdirectory under the parent and returns its inode number" in {
      val (_, sfs) = mounted()
      val (newRoot, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 1000, 1000, Now, Nsec,
      )
      dirIno should be > InoRoot
      HTree.lookup(newRoot, sfs.device, InoRoot, "subdir") shouldBe
        Some((dirIno, DirEntry.TypeDirectory))
      sfs.unmount()
    }

    "starts the new directory with linkCount = 2 (. + entry-in-parent)" in {
      val (_, sfs) = mounted()
      val (_, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 1000, 1000, Now, Nsec,
      )
      sfs.readInode(dirIno).linkCount shouldBe 2
      sfs.unmount()
    }

    "bumps parent.linkCount by 1 (for the new ..)" in {
      val (_, sfs) = mounted()
      val rootBefore = sfs.readInode(InoRoot)
      val (newRoot, _) = DirOps.mkdir(
        rootBefore, InoRoot, sfs, "subdir",
        DirMode, 1000, 1000, Now, Nsec,
      )
      newRoot.linkCount shouldBe (rootBefore.linkCount + 1)
      sfs.unmount()
    }

    "lays down a working HTree (lookup of '.' and '..' inside the new dir)" in {
      val (_, sfs) = mounted()
      val (_, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 1000, 1000, Now, Nsec,
      )
      val dirInode = sfs.readInode(dirIno)
      HTree.lookup(dirInode, sfs.device, dirIno, ".") shouldBe
        Some((dirIno, DirEntry.TypeDirectory))
      HTree.lookup(dirInode, sfs.device, dirIno, "..") shouldBe
        Some((InoRoot, DirEntry.TypeDirectory))
      sfs.unmount()
    }

    "stamps the requested mode/uid/gid + crtime on the new directory" in {
      val (_, sfs) = mounted()
      val mode = FileOps.ModeDirectory | 0x180 // 0o600
      val (_, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        mode, uid = 7, gid = 13, Now, Nsec,
      )
      val ino = sfs.readInode(dirIno)
      ino.mode shouldBe mode
      ino.uid shouldBe 7
      ino.gid shouldBe 13
      ino.crtimeSec shouldBe Now
      sfs.unmount()
    }

    "rejects duplicate names" in {
      val (_, sfs) = mounted()
      val (root1, _) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 1000, 1000, Now, Nsec,
      )
      an[SfsExistsError] should be thrownBy
        DirOps.mkdir(root1, InoRoot, sfs, "subdir", DirMode, 1000, 1000, Now, Nsec)
      sfs.unmount()
    }

    "rejects non-directory mode (must encode S_IFDIR or no type bits)" in {
      val (_, sfs) = mounted()
      an[IllegalArgumentException] should be thrownBy
        DirOps.mkdir(
          sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
          FileOps.ModeRegular | 0x1ed, 0, 0, Now, Nsec,
        )
      sfs.unmount()
    }

    "treats no-type-bits mode as implicit S_IFDIR" in {
      val (_, sfs) = mounted()
      val (_, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        mode = 0x1ed, // 0o755 with no type bits
        1000, 1000, Now, Nsec,
      )
      (sfs.readInode(dirIno).mode & FileOps.ModeTypeMask) shouldBe FileOps.ModeDirectory
      sfs.unmount()
    }

    "nested mkdir: subdir's '..' points to the right parent" in {
      val (_, sfs) = mounted()
      val (root1, sub1) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "level1",
        DirMode, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, root1)
      val (sub1after, sub2) = DirOps.mkdir(
        sfs.readInode(sub1), sub1, sfs, "level2",
        DirMode, 0, 0, Now, Nsec,
      )
      sfs.writeInode(sub1, sub1after)
      val sub2Inode = sfs.readInode(sub2)
      HTree.lookup(sub2Inode, sfs.device, sub2, "..") shouldBe
        Some((sub1, DirEntry.TypeDirectory))
      // sub1's linkCount has been bumped twice now (level2's "..").
      sfs.readInode(sub1).linkCount shouldBe 3
      sfs.unmount()
    }
  }

  // ---- rmdir ----------------------------------------------------------

  "rmdir" - {

    "removes an empty subdirectory and frees its inode" in {
      val (_, sfs) = mounted()
      val (root1, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 0, 0, Now, Nsec,
      )
      sfs.inodeBitmap.isSet(dirIno) shouldBe true

      val root2 = DirOps.rmdir(root1, InoRoot, sfs, "subdir", Now, Nsec)
      sfs.inodeBitmap.isSet(dirIno) shouldBe false
      HTree.lookup(root2, sfs.device, InoRoot, "subdir") shouldBe None
      sfs.unmount()
    }

    "decrements parent.linkCount by 1" in {
      val (_, sfs) = mounted()
      val rootBefore = sfs.readInode(InoRoot)
      val (root1, _) = DirOps.mkdir(
        rootBefore, InoRoot, sfs, "subdir",
        DirMode, 0, 0, Now, Nsec,
      )
      val root2 = DirOps.rmdir(root1, InoRoot, sfs, "subdir", Now, Nsec)
      root2.linkCount shouldBe rootBefore.linkCount
      sfs.unmount()
    }

    "frees the directory's data blocks (block bitmap free count rises)" in {
      val (_, sfs) = mounted()
      val (root1, _) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 0, 0, Now, Nsec,
      )
      val freeBefore = sfs.blockBitmap.freeCount
      DirOps.rmdir(root1, InoRoot, sfs, "subdir", Now, Nsec)
      val freeAfter = sfs.blockBitmap.freeCount
      // 2 data blocks (root + leaf) freed.
      freeAfter shouldBe (freeBefore + 2)
      sfs.unmount()
    }

    "raises SfsNotEmptyError when the subdirectory still has children" in {
      val (_, sfs) = mounted()
      val (root1, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, root1)

      // Drop a file inside the subdirectory.
      val (subAfter, _) = FileOps.create(
        sfs.readInode(dirIno), dirIno, sfs, "child",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      sfs.writeInode(dirIno, subAfter)

      an[SfsNotEmptyError] should be thrownBy
        DirOps.rmdir(sfs.readInode(InoRoot), InoRoot, sfs, "subdir", Now, Nsec)
      sfs.unmount()
    }

    "raises SfsNotDirectoryError on a regular-file target" in {
      val (_, sfs) = mounted()
      val (root1, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "file",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      an[SfsNotDirectoryError] should be thrownBy
        DirOps.rmdir(root1, InoRoot, sfs, "file", Now, Nsec)
      sfs.unmount()
    }

    "raises SfsNotFoundError when the name doesn't exist" in {
      val (_, sfs) = mounted()
      an[SfsNotFoundError] should be thrownBy
        DirOps.rmdir(sfs.readInode(InoRoot), InoRoot, sfs, "ghost", Now, Nsec)
      sfs.unmount()
    }

    "rmdir + mkdir reuses the inode slot, with bumped generation" in {
      val (_, sfs) = mounted()
      val (root1, dirIno1) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "first",
        DirMode, 0, 0, Now, Nsec,
      )
      val gen1 = sfs.readInode(dirIno1).generation
      val root2 = DirOps.rmdir(root1, InoRoot, sfs, "first", Now, Nsec)
      val (_, dirIno2) = DirOps.mkdir(
        root2, InoRoot, sfs, "second",
        DirMode, 0, 0, Now + 1, Nsec,
      )
      dirIno2 shouldBe dirIno1
      sfs.readInode(dirIno2).generation shouldBe (gen1 + 1)
      sfs.unmount()
    }
  }

  // ---- readdir --------------------------------------------------------

  "readdir" - {

    "yields just dot/dotdot on a freshly mkdir'd directory" in {
      val (_, sfs) = mounted()
      val (_, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 0, 0, Now, Nsec,
      )
      val xs = DirOps.readdir(sfs.readInode(dirIno), sfs.device, dirIno)
      xs.map(_.name).toSet shouldBe Set(".", "..")
      sfs.unmount()
    }

    "reflects entries inserted via FileOps.create" in {
      val (_, sfs) = mounted()
      val (root1, dirIno) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "subdir",
        DirMode, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, root1)

      var subInode = sfs.readInode(dirIno)
      val names = Vector("a.txt", "b.txt", "c.txt")
      for n <- names do
        val (updated, _) = FileOps.create(
          subInode, dirIno, sfs, n,
          FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
        )
        subInode = updated

      val listed = DirOps.readdir(subInode, sfs.device, dirIno).map(_.name).toSet
      listed shouldBe (names.toSet + "." + "..")
      sfs.unmount()
    }
  }

  // ---- rename ---------------------------------------------------------

  "rename" - {

    "same-parent rename of a regular file" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "old",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      val (_, root2) = DirOps.rename(
        root1, InoRoot, root1, InoRoot, sfs,
        "old", "new", Now, Nsec,
      )
      HTree.lookup(root2, sfs.device, InoRoot, "old") shouldBe None
      HTree.lookup(root2, sfs.device, InoRoot, "new") shouldBe
        Some((ino, DirEntry.TypeRegular))
      sfs.unmount()
    }

    "same-name same-parent rename is a no-op" in {
      val (_, sfs) = mounted()
      val (root1, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "x",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      val (_, root2) = DirOps.rename(
        root1, InoRoot, root1, InoRoot, sfs,
        "x", "x", Now, Nsec,
      )
      HTree.lookup(root2, sfs.device, InoRoot, "x") shouldBe
        Some((ino, DirEntry.TypeRegular))
      sfs.unmount()
    }

    "rename overwriting an existing regular file unlinks the target" in {
      val (_, sfs) = mounted()
      val (root1, srcIno) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      val (root2, dstIno) = FileOps.create(
        root1, InoRoot, sfs, "dst",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      sfs.inodeBitmap.isSet(dstIno) shouldBe true

      val (_, root3) = DirOps.rename(
        root2, InoRoot, root2, InoRoot, sfs,
        "src", "dst", Now, Nsec,
      )
      HTree.lookup(root3, sfs.device, InoRoot, "src") shouldBe None
      HTree.lookup(root3, sfs.device, InoRoot, "dst") shouldBe
        Some((srcIno, DirEntry.TypeRegular))
      // Overwritten target's inode is freed.
      sfs.inodeBitmap.isSet(dstIno) shouldBe false
      sfs.unmount()
    }

    "rename refuses to overwrite a directory" in {
      val (_, sfs) = mounted()
      val (root1, _) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "src",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      val (root2, _) = DirOps.mkdir(root1, InoRoot, sfs, "dst", DirMode, 0, 0, Now, Nsec)
      an[SfsIsDirectoryError] should be thrownBy
        DirOps.rename(root2, InoRoot, root2, InoRoot, sfs,
          "src", "dst", Now, Nsec)
      sfs.unmount()
    }

    "rename refuses to put a directory over a non-directory" in {
      val (_, sfs) = mounted()
      val (root1, _) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs, "src_dir", DirMode, 0, 0, Now, Nsec)
      val (root2, _) = FileOps.create(
        root1, InoRoot, sfs, "dst_file",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      an[SfsNotDirectoryError] should be thrownBy
        DirOps.rename(root2, InoRoot, root2, InoRoot, sfs,
          "src_dir", "dst_file", Now, Nsec)
      sfs.unmount()
    }

    "raises SfsNotFoundError when the source name is missing" in {
      val (_, sfs) = mounted()
      an[SfsNotFoundError] should be thrownBy
        DirOps.rename(sfs.readInode(InoRoot), InoRoot,
          sfs.readInode(InoRoot), InoRoot, sfs,
          "ghost", "phoenix", Now, Nsec)
      sfs.unmount()
    }

    "cross-directory rename of a regular file works" in {
      val (_, sfs) = mounted()
      val (rootA, srcDir) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs, "src_dir", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(InoRoot, rootA)
      val (rootB, dstDir) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs, "dst_dir", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(InoRoot, rootB)
      val (srcAfter, fileIno) = FileOps.create(
        sfs.readInode(srcDir), srcDir, sfs, "the_file",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )
      sfs.writeInode(srcDir, srcAfter)

      val (newSrc, newDst) = DirOps.rename(
        sfs.readInode(srcDir), srcDir,
        sfs.readInode(dstDir), dstDir,
        sfs,
        "the_file", "moved_file",
        Now, Nsec,
      )
      HTree.lookup(newSrc, sfs.device, srcDir, "the_file") shouldBe None
      HTree.lookup(newDst, sfs.device, dstDir, "moved_file") shouldBe
        Some((fileIno, DirEntry.TypeRegular))
      sfs.unmount()
    }

    "cross-directory rename of a subdirectory updates its '..'" in {
      val (_, sfs) = mounted()
      val (rootA, srcDir) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs, "src_dir", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(InoRoot, rootA)
      val (rootB, dstDir) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs, "dst_dir", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(InoRoot, rootB)
      val (srcAfter, movedDir) = DirOps.mkdir(sfs.readInode(srcDir), srcDir, sfs, "moving", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(srcDir, srcAfter)

      val srcLinkBefore = sfs.readInode(srcDir).linkCount
      val dstLinkBefore = sfs.readInode(dstDir).linkCount

      val (newSrc, newDst) = DirOps.rename(
        sfs.readInode(srcDir), srcDir,
        sfs.readInode(dstDir), dstDir,
        sfs,
        "moving", "moved",
        Now, Nsec,
      )

      // Subdirectory's '..' now points at dstDir.
      HTree.lookup(sfs.readInode(movedDir), sfs.device, movedDir, "..") shouldBe
        Some((dstDir, DirEntry.TypeDirectory))
      // Parents' linkCounts updated.
      newSrc.linkCount shouldBe (srcLinkBefore - 1)
      newDst.linkCount shouldBe (dstLinkBefore + 1)
      sfs.unmount()
    }
  }

  // ---- end-to-end -----------------------------------------------------

  "end-to-end mkdir tree" - {

    "build a 3-level deep directory tree, list each level" in {
      val (_, sfs) = mounted()
      val (rootA, l1) = DirOps.mkdir(sfs.readInode(InoRoot), InoRoot, sfs, "etc", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(InoRoot, rootA)
      val (l1A, l2) = DirOps.mkdir(sfs.readInode(l1), l1, sfs, "ssh", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(l1, l1A)
      val (l2A, l3) = DirOps.mkdir(sfs.readInode(l2), l2, sfs, "config.d", DirMode, 0, 0, Now, Nsec)
      sfs.writeInode(l2, l2A)
      val (_, fileIno) = FileOps.create(
        sfs.readInode(l3), l3, sfs, "tunnel.conf",
        FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
      )

      DirOps.readdir(sfs.readInode(InoRoot), sfs.device, InoRoot).map(_.name).toSet shouldBe
        Set(".", "..", "etc")
      DirOps.readdir(sfs.readInode(l1), sfs.device, l1).map(_.name).toSet shouldBe
        Set(".", "..", "ssh")
      DirOps.readdir(sfs.readInode(l2), sfs.device, l2).map(_.name).toSet shouldBe
        Set(".", "..", "config.d")
      DirOps.readdir(sfs.readInode(l3), sfs.device, l3).map(_.name).toSet shouldBe
        Set(".", "..", "tunnel.conf")
      val _ = fileIno
      sfs.unmount()
    }
  }
