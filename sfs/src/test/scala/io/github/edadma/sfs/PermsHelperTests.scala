package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import FileOps.{AccessExec, AccessRead, AccessWrite, ModeRegular, ModeDirectory}

/** Tests for the Phase 17d permission helpers: [[FileOps.canAccess]],
  * [[Perms.requireAccess]], and [[Perms.requireStickyOk]]. These are
  * pure functions over [[Inode]] + [[Caller]]; no filesystem needed. */
class PermsHelperTests extends AnyFreeSpec with Matchers:

  private def ino(uid: Int, gid: Int, perm: Int, modeType: Int = ModeRegular): Inode =
    Inode(
      mode = modeType | (perm & 0xfff),
      linkCount = 1,
      uid = uid, gid = gid, flags = 0,
      size = 0L, blockCount = 0, generation = 1,
      atimeSec = 0, atimeNsec = 0,
      mtimeSec = 0, mtimeNsec = 0,
      ctimeSec = 0, ctimeNsec = 0,
      crtimeSec = 0, crtimeNsec = 0,
      body = InodeBody.EmptyExtents,
      indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
    )

  "FileOps.canAccess" - {

    "root (uid==0) bypasses every check, even mode 0" in {
      val i = ino(uid = 1000, gid = 1000, perm = 0)
      FileOps.canAccess(i, uid = 0, gid = 0, want = AccessRead) shouldBe true
      FileOps.canAccess(i, uid = 0, gid = 999, want = AccessRead | AccessWrite | AccessExec) shouldBe true
    }

    "owner triplet selected when uid matches" in {
      // mode 0o400 — only owner-read. gid mismatched too (so it can't fall to group).
      val i = ino(uid = 1000, gid = 1000, perm = 0x100)
      FileOps.canAccess(i, uid = 1000, gid = 9999, want = AccessRead) shouldBe true
      FileOps.canAccess(i, uid = 1000, gid = 9999, want = AccessWrite) shouldBe false
    }

    "group triplet selected when gid matches and uid does not" in {
      // 0o040 — group-read only.
      val i = ino(uid = 1000, gid = 2000, perm = 0x020)
      FileOps.canAccess(i, uid = 1234, gid = 2000, want = AccessRead) shouldBe true
      FileOps.canAccess(i, uid = 1234, gid = 2000, want = AccessWrite) shouldBe false
    }

    "other triplet selected when neither uid nor gid match" in {
      // 0o007 — others can rwx.
      val i = ino(uid = 1000, gid = 2000, perm = 0x007)
      FileOps.canAccess(i, uid = 5555, gid = 6666, want = AccessRead | AccessWrite | AccessExec) shouldBe true
    }

    "owner perms beat group perms (owner check is first)" in {
      // 0o074 — owner has 0, group has rwx. As owner, we should be denied.
      val i = ino(uid = 1000, gid = 2000, perm = 0x074)
      FileOps.canAccess(i, uid = 1000, gid = 2000, want = AccessRead) shouldBe false
    }

    "group perms beat other perms when only gid matches" in {
      // 0o007 — group has nothing, others have rwx.
      val i = ino(uid = 1000, gid = 2000, perm = 0x007)
      FileOps.canAccess(i, uid = 9999, gid = 2000, want = AccessRead) shouldBe false
    }

    "want is treated as a bit set — all requested bits must be present" in {
      // 0o100 — owner-read.
      val i = ino(uid = 1000, gid = 1000, perm = 0x100)
      FileOps.canAccess(i, uid = 1000, gid = 1000, want = AccessRead) shouldBe true
      FileOps.canAccess(i, uid = 1000, gid = 1000, want = AccessRead | AccessWrite) shouldBe false
    }
  }

  "Perms.requireAccess" - {

    "throws SfsPermissionError on denial" in {
      val i = ino(uid = 1000, gid = 1000, perm = 0)
      val caller = Caller(uid = 5555, gid = 6666)
      val ex = intercept[SfsPermissionError] {
        Perms.requireAccess(caller, i, AccessRead, "test", "ino#42")
      }
      ex.getMessage should include("uid=5555")
      ex.getMessage should include("R")
      ex.getMessage should include("ino#42")
    }

    "passes silently when caller has the right bits" in {
      val i = ino(uid = 1000, gid = 1000, perm = 0x1ff) // 0o777
      val caller = Caller(uid = 1234, gid = 5678)
      noException should be thrownBy {
        Perms.requireAccess(caller, i, AccessRead | AccessWrite | AccessExec, "test", "ino")
      }
    }

    "Caller.Root bypasses even on a mode-0 inode" in {
      val i = ino(uid = 1000, gid = 1000, perm = 0)
      noException should be thrownBy {
        Perms.requireAccess(Caller.Root, i, AccessWrite, "test", "ino")
      }
    }
  }

  "Perms.requireStickyOk" - {

    "passes when sticky bit is not set on the parent dir" in {
      val parent = ino(uid = 999, gid = 999, perm = 0x1ff, modeType = ModeDirectory)
      val target = ino(uid = 1000, gid = 1000, perm = 0x1a4)
      noException should be thrownBy {
        Perms.requireStickyOk(Caller(uid = 5555, gid = 5555), parent, target, "test", "f")
      }
    }

    "passes for root even on a sticky dir not owning the file" in {
      val parent = ino(uid = 999, gid = 999, perm = 0x1ff | Perms.ModeSticky, modeType = ModeDirectory)
      val target = ino(uid = 1000, gid = 1000, perm = 0x1a4)
      noException should be thrownBy {
        Perms.requireStickyOk(Caller.Root, parent, target, "test", "f")
      }
    }

    "passes when caller owns the target file" in {
      val parent = ino(uid = 999, gid = 999, perm = 0x1ff | Perms.ModeSticky, modeType = ModeDirectory)
      val target = ino(uid = 1000, gid = 1000, perm = 0x1a4)
      noException should be thrownBy {
        Perms.requireStickyOk(Caller(uid = 1000, gid = 1234), parent, target, "test", "f")
      }
    }

    "passes when caller owns the parent directory" in {
      val parent = ino(uid = 999, gid = 999, perm = 0x1ff | Perms.ModeSticky, modeType = ModeDirectory)
      val target = ino(uid = 1000, gid = 1000, perm = 0x1a4)
      noException should be thrownBy {
        Perms.requireStickyOk(Caller(uid = 999, gid = 1234), parent, target, "test", "f")
      }
    }

    "throws when sticky is set and caller is not root, target owner, or dir owner" in {
      val parent = ino(uid = 999, gid = 999, perm = 0x1ff | Perms.ModeSticky, modeType = ModeDirectory)
      val target = ino(uid = 1000, gid = 1000, perm = 0x1a4)
      val ex = intercept[SfsPermissionError] {
        Perms.requireStickyOk(Caller(uid = 5555, gid = 5555), parent, target, "test", "f")
      }
      ex.getMessage should include("sticky")
    }
  }
