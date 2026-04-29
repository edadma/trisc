package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 17d denial coverage: every public op refuses an unauthorized
  * [[Caller]] with [[SfsPermissionError]], and root bypasses. Sticky-bit
  * semantics on `unlink` / `rmdir` / `rename` are exercised separately.
  *
  * Build pattern: format → mount → (as root) seed a directory with a
  * specific mode → switch to a non-root caller and assert denial → also
  * assert root still passes. */
class PermissionsTests extends AnyFreeSpec with Matchers:

  // ---- setup ----------------------------------------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "perms",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x6800_5000
  private val Nsec: Int = 0

  /** Stranger caller — never matches owner uid/gid, never root. */
  private val Stranger: Caller = Caller(uid = 5555, gid = 6666)

  /** Make a fresh subdirectory of `parent` owned by `(uid, gid)` with
    * the given perm bits, and return (newDirInode, newDirNum, parentAfter). */
  private def mkdirAs(
      sfs: Sfs,
      parent: Inode,
      parentNum: Int,
      name: String,
      uid: Int,
      gid: Int,
      perm: Int,
  ): (Inode, Int, Inode) =
    val (parentAfter, dirNum) = DirOps.mkdir(
      parent, parentNum, sfs, name,
      FileOps.ModeDirectory | (perm & 0xfff),
      uid, gid, Now, Nsec,
    )
    sfs.writeInode(parentNum, parentAfter)
    (sfs.readInode(dirNum), dirNum, parentAfter)

  /** Make a fresh regular file of `parent` owned by `(uid, gid)` with
    * the given perm bits. Returns (fileInode, fileNum, parentAfter). */
  private def createAs(
      sfs: Sfs,
      parent: Inode,
      parentNum: Int,
      name: String,
      uid: Int,
      gid: Int,
      perm: Int,
  ): (Inode, Int, Inode) =
    val (parentAfter, fileNum) = FileOps.create(
      parent, parentNum, sfs, name,
      FileOps.ModeRegular | (perm & 0xfff),
      uid, gid, Now, Nsec,
    )
    sfs.writeInode(parentNum, parentAfter)
    (sfs.readInode(fileNum), fileNum, parentAfter)

  // ---- FileOps.create -------------------------------------------------

  "FileOps.create" - {

    "denies a stranger when parent lacks W+X for them" in {
      val (_, sfs) = mounted()
      // mkdir a subdir 0o700 owned by uid=1000 — strangers have nothing.
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1c0)
      intercept[SfsPermissionError] {
        FileOps.create(parent, parentNum, sfs, "f",
          FileOps.ModeRegular | 0x1a4, 5555, 6666, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "root bypasses even on a 0-perm parent" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0)
      noException should be thrownBy {
        FileOps.create(parent, parentNum, sfs, "f",
          FileOps.ModeRegular | 0x1a4, 1, 1, Now, Nsec, Caller.Root)
      }
      sfs.unmount()
    }

    "non-root passes when parent has W+X for them via other-bits" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1ff)
      noException should be thrownBy {
        FileOps.create(parent, parentNum, sfs, "f",
          FileOps.ModeRegular | 0x1a4, 5555, 6666, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  // ---- FileOps.unlink (sticky) ----------------------------------------

  "FileOps.unlink" - {

    "denies stranger on parent lacking W+X" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1c0)
      val (_, _, parentAfter) = createAs(sfs, parent, parentNum, "f", 1000, 1000, 0x1ff)
      intercept[SfsPermissionError] {
        FileOps.unlink(parentAfter, parentNum, sfs, "f", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "denies non-owner on a sticky parent (writable)" in {
      val (_, sfs) = mounted()
      // /tmp-style: world-writable + sticky.
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "tmp", 0, 0, 0x1ff | Perms.ModeSticky)
      // f belongs to uid=1000.
      val (_, _, parentAfter) = createAs(sfs, parent, parentNum, "f", 1000, 1000, 0x1ff)
      intercept[SfsPermissionError] {
        FileOps.unlink(parentAfter, parentNum, sfs, "f", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "permits the file's owner on a sticky parent" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "tmp", 0, 0, 0x1ff | Perms.ModeSticky)
      val (_, _, parentAfter) = createAs(sfs, parent, parentNum, "mine", 1000, 1000, 0x1ff)
      noException should be thrownBy {
        FileOps.unlink(parentAfter, parentNum, sfs, "mine", Now, Nsec, Caller(uid = 1000, gid = 1000))
      }
      sfs.unmount()
    }

    "permits root on a sticky parent even when not the owner" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "tmp", 0, 0, 0x1ff | Perms.ModeSticky)
      val (_, _, parentAfter) = createAs(sfs, parent, parentNum, "f", 1000, 1000, 0x1ff)
      noException should be thrownBy {
        FileOps.unlink(parentAfter, parentNum, sfs, "f", Now, Nsec, Caller.Root)
      }
      sfs.unmount()
    }
  }

  // ---- FileOps.link ---------------------------------------------------

  "FileOps.link" - {

    "denies stranger on parent lacking W+X" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1c0)
      val (_, fileNum, parentAfter) = createAs(sfs, parent, parentNum, "f", 1000, 1000, 0x1ff)
      intercept[SfsPermissionError] {
        FileOps.link(parentAfter, parentNum, sfs, fileNum, "alias", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  // ---- FileIO.readFile / writeFile / truncateFile ---------------------

  "FileIO.readFile" - {

    "denies stranger when file lacks R" in {
      val (_, sfs) = mounted()
      val (file, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "secret", 1000, 1000, 0x1c0)
      val written = FileIO.writeFile(file, sfs, 0L, "x".getBytes, Now, Nsec)
      sfs.writeInode(fileNum, written)
      intercept[SfsPermissionError] {
        FileIO.readFile(sfs.readInode(fileNum), fileNum, sfs, 0L, 1, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "permits stranger when file is world-readable" in {
      val (_, sfs) = mounted()
      val (file, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "open", 1000, 1000, 0x1ff)
      val written = FileIO.writeFile(file, sfs, 0L, "x".getBytes, Now, Nsec)
      sfs.writeInode(fileNum, written)
      noException should be thrownBy {
        FileIO.readFile(sfs.readInode(fileNum), fileNum, sfs, 0L, 1, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  "FileIO.writeFile" - {

    "denies stranger when file lacks W" in {
      val (_, sfs) = mounted()
      val (file, _, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "ro", 1000, 1000, 0x124) // 0o444
      intercept[SfsPermissionError] {
        FileIO.writeFile(file, sfs, 0L, "x".getBytes, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  "FileIO.truncateFile" - {

    "denies stranger when file lacks W" in {
      val (_, sfs) = mounted()
      val (file, _, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "ro", 1000, 1000, 0x124)
      intercept[SfsPermissionError] {
        FileIO.truncateFile(file, sfs, 0L, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  // ---- DirOps.mkdir / rmdir / rename ----------------------------------

  "DirOps.mkdir" - {

    "denies stranger on parent lacking W+X" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1c0)
      intercept[SfsPermissionError] {
        DirOps.mkdir(parent, parentNum, sfs, "sub",
          FileOps.ModeDirectory | 0x1ff, 5555, 6666, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  "DirOps.rmdir" - {

    "denies stranger on sticky parent for a dir they don't own" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "tmp", 0, 0, 0x1ff | Perms.ModeSticky)
      val (_, _, parentAfter) = mkdirAs(sfs, parent, parentNum, "sub", 1000, 1000, 0x1ff)
      intercept[SfsPermissionError] {
        DirOps.rmdir(parentAfter, parentNum, sfs, "sub", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "permits the dir's owner on a sticky parent" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "tmp", 0, 0, 0x1ff | Perms.ModeSticky)
      val (_, _, parentAfter) = mkdirAs(sfs, parent, parentNum, "mine", 1000, 1000, 0x1ff)
      noException should be thrownBy {
        DirOps.rmdir(parentAfter, parentNum, sfs, "mine", Now, Nsec, Caller(uid = 1000, gid = 1000))
      }
      sfs.unmount()
    }
  }

  "DirOps.rename" - {

    "denies stranger on a parent they cannot write" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1c0)
      val (_, _, parentAfter) = createAs(sfs, parent, parentNum, "f", 1000, 1000, 0x1ff)
      intercept[SfsPermissionError] {
        DirOps.rename(parentAfter, parentNum, parentAfter, parentNum, sfs, "f", "g", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "denies non-owner on a sticky source parent" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "tmp", 0, 0, 0x1ff | Perms.ModeSticky)
      val (_, _, parentAfter) = createAs(sfs, parent, parentNum, "f", 1000, 1000, 0x1ff)
      intercept[SfsPermissionError] {
        DirOps.rename(parentAfter, parentNum, parentAfter, parentNum, sfs, "f", "g", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  // ---- HTree.list (atime overload) ------------------------------------

  "HTree.list (atime-aware)" - {

    "denies stranger on a directory lacking R for them" in {
      val (_, sfs) = mounted()
      val (dir, dirNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "noread", 1000, 1000, 0x140) // 0o500 owner only
      intercept[SfsPermissionError] {
        HTree.list(dir, dirNum, sfs, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "permits stranger on a world-readable directory" in {
      val (_, sfs) = mounted()
      val (dir, dirNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "open", 1000, 1000, 0x1ff)
      noException should be thrownBy {
        HTree.list(dir, dirNum, sfs, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "primitive (ino, dev, ownerInode) overload still has no perm check" in {
      val (_, sfs) = mounted()
      val (dir, dirNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "noread", 1000, 1000, 0x140)
      // No caller — internal/fsck overload — no perm check.
      noException should be thrownBy {
        HTree.list(dir, sfs.device, dirNum)
      }
      sfs.unmount()
    }
  }

  // ---- SymlinkOps.symlink ---------------------------------------------

  "SymlinkOps.symlink" - {

    "denies stranger on parent lacking W+X" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "d", 1000, 1000, 0x1c0)
      intercept[SfsPermissionError] {
        SymlinkOps.symlink(parent, parentNum, sfs, "lnk", "/x", 5555, 6666, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  // ---- XattrOps -------------------------------------------------------

  "XattrOps" - {

    "get denies stranger when inode lacks R" in {
      val (_, sfs) = mounted()
      val (_, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "f", 1000, 1000, 0x1c0)
      // Seed an attribute as root so there is something to read.
      XattrOps.set(sfs, fileNum, "user.k", "v".getBytes, Now, Nsec)
      intercept[SfsPermissionError] {
        XattrOps.get(sfs, fileNum, "user.k", Stranger)
      }
      sfs.unmount()
    }

    "list denies stranger when inode lacks R" in {
      val (_, sfs) = mounted()
      val (_, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "f", 1000, 1000, 0x1c0)
      intercept[SfsPermissionError] {
        XattrOps.list(sfs, fileNum, Stranger)
      }
      sfs.unmount()
    }

    "set denies stranger when inode lacks W" in {
      val (_, sfs) = mounted()
      val (_, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "f", 1000, 1000, 0x124) // 0o444 — no W
      intercept[SfsPermissionError] {
        XattrOps.set(sfs, fileNum, "user.k", "v".getBytes, Now, Nsec, caller = Stranger)
      }
      sfs.unmount()
    }

    "remove denies stranger when inode lacks W" in {
      val (_, sfs) = mounted()
      val (_, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "f", 1000, 1000, 0x1ff)
      XattrOps.set(sfs, fileNum, "user.k", "v".getBytes, Now, Nsec)
      // Tighten perms to 0o444 (no W) by rewriting the inode.
      val ino = sfs.readInode(fileNum)
      sfs.writeInode(fileNum, ino.copy(mode = (ino.mode & ~0xfff) | 0x124))
      intercept[SfsPermissionError] {
        XattrOps.remove(sfs, fileNum, "user.k", Now, Nsec, Stranger)
      }
      sfs.unmount()
    }

    "root bypasses even on a no-perms inode" in {
      val (_, sfs) = mounted()
      val (_, fileNum, _) = createAs(sfs, sfs.readInode(InoRoot), InoRoot, "f", 1000, 1000, 0)
      noException should be thrownBy {
        XattrOps.set(sfs, fileNum, "user.k", "v".getBytes, Now, Nsec, caller = Caller.Root)
      }
      noException should be thrownBy {
        XattrOps.get(sfs, fileNum, "user.k", Caller.Root)
      }
      sfs.unmount()
    }
  }

  // ---- BadBlockOps + Sfs.relabel (root-only) --------------------------

  "BadBlockOps" - {

    "list denies non-root" in {
      val (_, sfs) = mounted()
      intercept[SfsPermissionError] { BadBlockOps.list(sfs, Stranger) }
      sfs.unmount()
    }

    "mark denies non-root" in {
      val (_, sfs) = mounted()
      intercept[SfsPermissionError] {
        BadBlockOps.mark(sfs, sfs.layout.dataStart + 5, Now, Nsec, Stranger)
      }
      sfs.unmount()
    }
  }

  "Sfs.relabel" - {

    "denies non-root" in {
      val (_, sfs) = mounted()
      intercept[SfsPermissionError] { sfs.relabel("nope", Stranger) }
      sfs.unmount()
    }

    "permits root" in {
      val (_, sfs) = mounted()
      noException should be thrownBy { sfs.relabel("yep", Caller.Root) }
      sfs.volumeName shouldBe "yep"
      sfs.unmount()
    }
  }

  // ---- root-bypass coverage on a single op ----------------------------

  "Caller.Root" - {

    "passes every op even on an inode with mode 0" in {
      val (_, sfs) = mounted()
      val (parent, parentNum, _) = mkdirAs(sfs, sfs.readInode(InoRoot), InoRoot, "vault", 1000, 1000, 0)
      noException should be thrownBy {
        FileOps.create(parent, parentNum, sfs, "f",
          FileOps.ModeRegular | 0, 0, 0, Now, Nsec, Caller.Root)
      }
      sfs.unmount()
    }
  }
