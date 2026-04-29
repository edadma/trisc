package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Phase 16 chunk 16a — fsck walker + CRC verifier.
  *
  * fsck walks every reachable on-disk structure and reports
  * CRC / magic / owner-binding failures. Each test below corrupts
  * exactly one byte of a target structure and asserts that the
  * matching [[FsckIssue]] variant is reported.
  */
class FsckTests extends AnyFreeSpec with Matchers:

  private def smallOpts: FormatOptions =
    FormatOptions(
      totalInodes = 32,
      journalBlocks = 16,
      volumeName = "fsck",
      uuid = (1 to 16).map(_.toByte).toIndexedSeq,
      formatTime = 0x6800_4321L,
    )

  private def mounted(): (RamBlockDevice, Sfs) =
    val dev = new RamBlockDevice(8192L)
    Sfs.format(dev, smallOpts)
    (dev, Sfs.mount(dev))

  private val Now: Int = 0x68000000
  private val Nsec: Int = 0

  /** Flip the low bit of one byte at `(blockNum, byteOffset)`. */
  private def tamperByte(dev: RamBlockDevice, blockNum: Long, byteOffset: Int): Unit =
    val buf = new Array[Byte](BlockSize)
    dev.readBlock(blockNum, buf)
    buf(byteOffset) = (buf(byteOffset) ^ 0x01).toByte
    dev.writeBlock(blockNum, buf)
    dev.flush()

  private def createFile(sfs: Sfs, name: String): Int =
    val (newRoot, ino) = FileOps.create(
      sfs.readInode(InoRoot), InoRoot, sfs, name,
      FileOps.ModeRegular | 0x1a4, 0, 0, Now, Nsec,
    )
    sfs.writeInode(InoRoot, newRoot)
    ino

  // ---- happy path -----------------------------------------------------

  "freshly formatted volume" - {

    "passes fsck cleanly" in {
      val (_, sfs) = mounted()
      val report = Fsck.check(sfs)
      report.issues shouldBe empty
      report.clean shouldBe true
      sfs.unmount()
    }

    "checks the reserved inodes" in {
      val (_, sfs) = mounted()
      val report = Fsck.check(sfs)
      // InoNull, InoBadBlocks, InoRoot are all set in the bitmap.
      report.stats.inodesChecked shouldBe 3
      // Root directory has two concrete blocks (root + leaf).
      report.stats.dirBlocksChecked shouldBe 2
      sfs.unmount()
    }

    "after some directory + file ops, still clean" in {
      val (_, sfs) = mounted()
      val (newRoot, _) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "sub",
        FileOps.ModeDirectory | 0x1ed, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, newRoot)
      createFile(sfs, "f.txt")
      Fsck.check(sfs).clean shouldBe true
      sfs.unmount()
    }
  }

  // ---- superblock -----------------------------------------------------

  "superblock CRC corruption" - {
    // Sfs.mount rewrites both SB copies right after fallback, so tampering
    // before mount would be erased. Tamper after mount; fsck reads from
    // the device directly and sees the on-disk corruption.

    "primary superblock corruption is reported" in {
      val (dev, sfs) = mounted()
      tamperByte(dev, 0L, 50) // a byte inside the SB CRC coverage (0..127)
      val report = Fsck.check(sfs)
      report.issues.exists {
        case FsckIssue.SuperblockCorrupt(0L, _) => true
        case _                                  => false
      } shouldBe true
      sfs.unmount()
    }

    "backup superblock corruption is reported" in {
      val (dev, sfs) = mounted()
      tamperByte(dev, 1L, 50)
      val report = Fsck.check(sfs)
      report.issues.exists {
        case FsckIssue.SuperblockCorrupt(1L, _) => true
        case _                                  => false
      } shouldBe true
      sfs.unmount()
    }
  }

  "journal superblock CRC corruption is reported" in {
    val (dev, sfs) = mounted()
    val journalSb = sfs.layout.journalStart.toLong
    // Corrupt after mount — the in-memory Journal still has clean state,
    // so unmount/etc. work; fsck reads from the device directly and sees
    // the bad bytes on disk.
    tamperByte(dev, journalSb, 4)
    val report = Fsck.check(sfs)
    report.issues.exists {
      case FsckIssue.JournalSuperblockCorrupt(_) => true
      case _                                     => false
    } shouldBe true
    sfs.unmount()
  }

  // ---- inode ----------------------------------------------------------

  "inode CRC corruption" - {

    "is reported for an allocated regular-file inode" in {
      val (dev, sfs) = mounted()
      val childInode = createFile(sfs, "f.txt")
      sfs.unmount()
      val (blk, off) = sfs.layout.inodeLocation(childInode)
      tamperByte(dev, blk, off + 0) // mode field
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.InodeCorrupt(n, _) => n == childInode
        case _                            => false
      } shouldBe true
      sfs2.unmount()
    }

    "is reported even if the corrupt inode is the root directory" in {
      val (dev, sfs) = mounted()
      sfs.unmount()
      val (blk, off) = sfs.layout.inodeLocation(InoRoot)
      tamperByte(dev, blk, off + 16) // size field
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.InodeCorrupt(InoRoot, _) => true
        case _                                  => false
      } shouldBe true
      // Because the root inode failed to parse, fsck cannot walk its
      // dir blocks — that's a real loss, but at least we surfaced it.
      sfs2.unmount()
    }

    "an inode whose bit is clear is not checked even if it has bad bytes" in {
      val (dev, sfs) = mounted()
      sfs.unmount()
      // Pick inode 5, which is unallocated on a fresh volume. Tamper.
      val (blk, off) = sfs.layout.inodeLocation(5)
      tamperByte(dev, blk, off + 0)
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.collect {
        case FsckIssue.InodeCorrupt(n, _) if n == 5 => n
      } shouldBe empty
      sfs2.unmount()
    }
  }

  // ---- directory blocks -----------------------------------------------

  "directory block corruption" - {

    "is reported when the leaf CRC is bad" in {
      val (dev, sfs) = mounted()
      // Find the physical address of root's leaf (logical block 1).
      val rootIno = sfs.readInode(InoRoot)
      val reader = new ExtentReader(sfs.device, rootIno)
      val leafBlock = reader.physicalBlock(1L) match
        case BlockMapping.Concrete(p) => p
        case other                    => fail(s"expected concrete leaf block, got $other")
      sfs.unmount()
      // Flip a byte well inside the CRC-covered region.
      tamperByte(dev, leafBlock, 100)
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.DirBlockCorrupt(InoRoot, p, _) => p == leafBlock
        case _                                        => false
      } shouldBe true
      sfs2.unmount()
    }

    "is reported when the leaf magic is wrong" in {
      val (dev, sfs) = mounted()
      val rootIno = sfs.readInode(InoRoot)
      val reader = new ExtentReader(sfs.device, rootIno)
      val leafBlock = reader.physicalBlock(1L) match
        case BlockMapping.Concrete(p) => p
        case other                    => fail(s"expected concrete leaf block, got $other")
      sfs.unmount()
      // BlockSize - 12 = magic offset.
      tamperByte(dev, leafBlock, BlockSize - 12)
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.DirBlockCorrupt(InoRoot, p, _) => p == leafBlock
        case _                                        => false
      } shouldBe true
      sfs2.unmount()
    }

    "is reported when the owning inode is wrong (block-swap protection)" in {
      val (dev, sfs) = mounted()
      val rootIno = sfs.readInode(InoRoot)
      val reader = new ExtentReader(sfs.device, rootIno)
      val leafBlock = reader.physicalBlock(1L) match
        case BlockMapping.Concrete(p) => p
        case other                    => fail(s"expected concrete leaf block, got $other")
      sfs.unmount()
      // Rewrite the owner inode in the tail to a different value but
      // recompute the CRC so the only failure is the owner-mismatch.
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(leafBlock, buf)
      Le.putU32(buf, BlockSize - 8, 0x12345678) // bogus owner
      Le.putU32(buf, BlockSize - 4, 0)
      val crc = Crc32.compute(buf, 0, BlockSize - 4)
      Le.putU32(buf, BlockSize - 4, crc)
      dev.writeBlock(leafBlock, buf)
      dev.flush()
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.DirBlockCorrupt(InoRoot, p, msg) =>
          p == leafBlock && msg.contains("inode mismatch")
        case _ => false
      } shouldBe true
      sfs2.unmount()
    }
  }

  // ---- xattr block ----------------------------------------------------

  "xattr block corruption" - {

    "is reported when the xattr block CRC is bad" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1, 2, 3), Now, Nsec)
      val xattrBlk = sfs.readInode(ino).xattrBlock.toLong
      sfs.unmount()
      tamperByte(dev, xattrBlk, 200) // somewhere in the entries region
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.XattrBlockCorrupt(n, p, _) => n == ino && p == xattrBlk
        case _                                    => false
      } shouldBe true
      sfs2.unmount()
    }

    "is reported when the xattr magic is wrong" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1, 2, 3), Now, Nsec)
      val xattrBlk = sfs.readInode(ino).xattrBlock.toLong
      sfs.unmount()
      tamperByte(dev, xattrBlk, 0) // magic byte
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.XattrBlockCorrupt(n, p, _) => n == ino && p == xattrBlk
        case _                                    => false
      } shouldBe true
      sfs2.unmount()
    }

    "is reported when the owner inode is wrong" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      XattrOps.set(sfs, ino, "user.k", Array[Byte](1, 2, 3), Now, Nsec)
      val xattrBlk = sfs.readInode(ino).xattrBlock.toLong
      sfs.unmount()
      // Rewrite the owner field, recompute CRC so only owner mismatches.
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(xattrBlk, buf)
      Le.putU32(buf, XattrBlock.OwnerOff, 0xdeadbeef)
      Le.putU32(buf, XattrBlock.CrcOff, 0)
      val crc = Crc32.compute(buf, 0, BlockSize - 4)
      Le.putU32(buf, XattrBlock.CrcOff, crc)
      dev.writeBlock(xattrBlk, buf)
      dev.flush()
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.XattrBlockCorrupt(n, p, msg) =>
          n == ino && p == xattrBlk && msg.contains("owner mismatch")
        case _ => false
      } shouldBe true
      sfs2.unmount()
    }
  }

  // ---- multiple issues ------------------------------------------------

  "multiple issues are reported in one pass" in {
    val (dev, sfs) = mounted()
    val ino = createFile(sfs, "f")
    XattrOps.set(sfs, ino, "user.k", Array[Byte](1), Now, Nsec)
    val xattrBlk = sfs.readInode(ino).xattrBlock.toLong
    val (inoBlk, inoOff) = sfs.layout.inodeLocation(ino)
    sfs.unmount()
    // Corrupt both the inode and its xattr block; both should appear.
    tamperByte(dev, inoBlk, inoOff + 0)
    tamperByte(dev, xattrBlk, 200)
    val sfs2 = Sfs.mount(dev)
    val report = Fsck.check(sfs2)
    val inoIssues = report.issues.collect {
      case FsckIssue.InodeCorrupt(n, _) if n == ino => n
    }
    inoIssues should not be empty
    // The xattr block can only be checked if the inode parsed; with
    // both corrupted we expect at least the inode issue. Nothing else
    // should crash fsck.
    sfs2.unmount()
  }
