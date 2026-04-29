package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for the relatime rule and its plumbing through
  * [[FileIO.readFile]] and [[HTree.list]] (Phase 17c). */
class AtimeTests extends AnyFreeSpec with Matchers:

  // ---- pure rule -----------------------------------------------------

  private def baseInode(atime: Int, mtime: Int, ctime: Int): Inode =
    Inode(
      mode = FileOps.ModeRegular | 0x1a4,
      linkCount = 1,
      uid = 0, gid = 0, flags = 0,
      size = 0L, blockCount = 0, generation = 1,
      atimeSec = atime, atimeNsec = 0,
      mtimeSec = mtime, mtimeNsec = 0,
      ctimeSec = ctime, ctimeNsec = 0,
      crtimeSec = 0, crtimeNsec = 0,
      body = InodeBody.EmptyExtents,
      indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
    )

  "Atime.relatimeUpdate" - {

    "updates when atime < mtime" in {
      val ino = baseInode(atime = 100, mtime = 200, ctime = 100)
      val out = Atime.relatimeUpdate(ino, nowSec = 150, nowNsec = 0)
      out.atimeSec shouldBe 150
    }

    "updates when atime < ctime" in {
      val ino = baseInode(atime = 100, mtime = 100, ctime = 200)
      val out = Atime.relatimeUpdate(ino, nowSec = 150, nowNsec = 0)
      out.atimeSec shouldBe 150
    }

    "updates when more than 24 h have passed since atime" in {
      val ino = baseInode(atime = 100, mtime = 100, ctime = 100)
      val out = Atime.relatimeUpdate(ino, nowSec = 100 + Atime.OneDaySec, nowNsec = 0)
      out.atimeSec shouldBe (100 + Atime.OneDaySec)
    }

    "leaves atime alone if newer than both mtime and ctime and within 24 h" in {
      val ino = baseInode(atime = 1000, mtime = 500, ctime = 600)
      val out = Atime.relatimeUpdate(ino, nowSec = 1500, nowNsec = 0)
      out should be theSameInstanceAs ino
      out.atimeSec shouldBe 1000
    }

    "updates when atime equals mtime+1 staleness — boundary at exactly 86400 s" in {
      val ino = baseInode(atime = 100, mtime = 100, ctime = 100)
      val justUnder = Atime.relatimeUpdate(ino, nowSec = 100 + Atime.OneDaySec - 1, nowNsec = 0)
      justUnder should be theSameInstanceAs ino // < 24 h, no update
      val atBoundary = Atime.relatimeUpdate(ino, nowSec = 100 + Atime.OneDaySec, nowNsec = 0)
      atBoundary.atimeSec shouldBe (100 + Atime.OneDaySec) // ≥ 24 h, updates
    }
  }

  // ---- end-to-end through readFile + list -----------------------------

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "atime",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  // Pick "now" timestamps that sit comfortably after the smallOpts
  // formatTime (0x6800_4321) so the root inode's initial atime is in
  // the past relative to all reads/writes the tests perform.
  private val Now0: Int = 0x6800_5000
  private val Now1: Int = Now0 + 100
  private val Now2: Int = Now0 + Atime.OneDaySec + 100

  "FileIO.readFile (atime-aware)" - {

    "does NOT bump atime when atime == mtime == ctime and within 24 h" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val written = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, "hi".getBytes, Now0, 0)
      sfs.writeInode(ino, written)

      val before = sfs.readInode(ino).atimeSec
      FileIO.readFile(sfs.readInode(ino), ino, sfs, 0L, 2, Now1, 0)
      sfs.readInode(ino).atimeSec shouldBe before // unchanged — relatime suppresses
      sfs.unmount()
    }

    "bumps atime on read after a write that advanced mtime past atime" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      // Initial write at Now0; atime = Now0, mtime = Now0.
      val w0 = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, "hi".getBytes, Now0, 0)
      sfs.writeInode(ino, w0)
      // Second write at Now1 advances mtime/ctime past atime.
      val w1 = FileIO.writeFile(sfs.readInode(ino), sfs, 2L, "x".getBytes, Now1, 0)
      sfs.writeInode(ino, w1)

      val before = sfs.readInode(ino).atimeSec
      before shouldBe Now0
      FileIO.readFile(sfs.readInode(ino), ino, sfs, 0L, 3, Now1 + 1, 0)
      sfs.readInode(ino).atimeSec shouldBe (Now1 + 1) // atime < mtime → bumped
      sfs.unmount()
    }

    "bumps atime when more than 24 h have passed since the last bump" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val written = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, "hi".getBytes, Now0, 0)
      sfs.writeInode(ino, written)

      FileIO.readFile(sfs.readInode(ino), ino, sfs, 0L, 2, Now2, 0)
      sfs.readInode(ino).atimeSec shouldBe Now2 // 24-h rule fired
      sfs.unmount()
    }

    "second read within 24 h after a bump does NOT bump again" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val written = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, "hi".getBytes, Now0, 0)
      sfs.writeInode(ino, written)

      FileIO.readFile(sfs.readInode(ino), ino, sfs, 0L, 2, Now2, 0)
      val first = sfs.readInode(ino).atimeSec
      FileIO.readFile(sfs.readInode(ino), ino, sfs, 0L, 2, Now2 + 100, 0)
      sfs.readInode(ino).atimeSec shouldBe first // suppressed: atime ≥ mtime/ctime, < 24 h
      sfs.unmount()
    }

    "atime persists across remount" in {
      val (dev, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val written = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, "hi".getBytes, Now0, 0)
      sfs.writeInode(ino, written)
      FileIO.readFile(sfs.readInode(ino), ino, sfs, 0L, 2, Now2, 0)
      sfs.unmount()

      val sfs2 = Sfs.mount(dev)
      sfs2.readInode(ino).atimeSec shouldBe Now2
      sfs2.unmount()
    }

    "primitive (ino, dev, offset, len) overload never touches atime" in {
      val (_, sfs) = mounted()
      val (_, ino) = FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "f",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val written = FileIO.writeFile(sfs.readInode(ino), sfs, 0L, "hi".getBytes, Now0, 0)
      sfs.writeInode(ino, written)

      val before = sfs.readInode(ino).atimeSec
      FileIO.readFile(sfs.readInode(ino), sfs.device, 0L, 2) // primitive
      sfs.readInode(ino).atimeSec shouldBe before
      sfs.unmount()
    }
  }

  "HTree.list (atime-aware)" - {

    "bumps directory atime when 24 h have passed" in {
      val (_, sfs) = mounted()
      FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "child",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val before = sfs.readInode(InoRoot).atimeSec
      HTree.list(sfs.readInode(InoRoot), InoRoot, sfs, Now2, 0)
      val after = sfs.readInode(InoRoot).atimeSec
      after should be > before
      after shouldBe Now2
      sfs.unmount()
    }

    "does NOT bump directory atime when within 24 h and atime ≥ mtime/ctime" in {
      val (_, sfs) = mounted()
      // Create then bump atime to Now2 first to put it past mtime/ctime.
      FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "child",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      HTree.list(sfs.readInode(InoRoot), InoRoot, sfs, Now2, 0)
      val first = sfs.readInode(InoRoot).atimeSec
      // A second list a short time later — atime is fresh, no bump.
      HTree.list(sfs.readInode(InoRoot), InoRoot, sfs, Now2 + 100, 0)
      sfs.readInode(InoRoot).atimeSec shouldBe first
      sfs.unmount()
    }

    "primitive (ino, dev, ownerInode) overload never touches atime" in {
      val (_, sfs) = mounted()
      FileOps.create(
        sfs.readInode(InoRoot), InoRoot, sfs, "child",
        FileOps.ModeRegular | 0x1a4, 1000, 1000, Now0, 0,
      )
      val before = sfs.readInode(InoRoot).atimeSec
      HTree.list(sfs.readInode(InoRoot), sfs.device, InoRoot) // primitive
      sfs.readInode(InoRoot).atimeSec shouldBe before
      sfs.unmount()
    }
  }
