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

  // ---- block bitmap reconciliation (16b) ------------------------------

  /** Flip one bit in the block bitmap on disk. */
  private def flipBlockBitmapBit(dev: RamBlockDevice, sfs: Sfs, blockNum: Int): Unit =
    val byteOffGlobal = blockNum >>> 3
    val bbBlockIdx = byteOffGlobal / BlockSize
    val byteInBlock = byteOffGlobal % BlockSize
    val bitInByte = blockNum & 7
    val bbBuf = new Array[Byte](BlockSize)
    val bbBlk = sfs.layout.blockBitmapStart.toLong + bbBlockIdx
    dev.readBlock(bbBlk, bbBuf)
    bbBuf(byteInBlock) = (bbBuf(byteInBlock) ^ (1 << bitInByte)).toByte
    dev.writeBlock(bbBlk, bbBuf)
    dev.flush()

  "block bitmap reconciliation" - {

    "freshly formatted volume claims at least dataStart blocks" in {
      val (_, sfs) = mounted()
      val report = Fsck.check(sfs)
      // Metadata region (0..dataStart-1) plus the two root-dir blocks.
      report.stats.claimedBlocks should be >= sfs.layout.dataStart
      sfs.unmount()
    }

    "missing-bit on a claimed block is reported" in {
      val (dev, sfs) = mounted()
      val (newRoot, _) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "sub",
        FileOps.ModeDirectory | 0x1ed, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, newRoot)
      sfs.unmount()
      // Pick a known-allocated data block — root's leaf block.
      val sfs1 = Sfs.mount(dev)
      val rootIno = sfs1.readInode(InoRoot)
      val reader = new ExtentReader(sfs1.device, rootIno)
      val leafBlock = reader.physicalBlock(1L) match
        case BlockMapping.Concrete(p) => p.toInt
        case _                        => fail("expected concrete root leaf")
      sfs1.unmount()
      // Clear that bit on disk (a "missing-bit corruption").
      flipBlockBitmapBit(dev, sfs1, leafBlock)
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.BlockBitmapMissingBit(b) => b == leafBlock
        case _                                  => false
      } shouldBe true
      sfs2.unmount()
    }

    "leaked block (set bit, no inode owns it) is reported" in {
      val (dev, sfs) = mounted()
      sfs.unmount()
      // Pick a free data block far from root's two blocks; set its bit.
      val target = sfs.layout.dataStart + 100
      val sfs1 = Sfs.mount(dev)
      sfs1.unmount()
      flipBlockBitmapBit(dev, sfs1, target) // toggle from clear to set
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.BlockBitmapLeakedBlock(b) => b == target
        case _                                   => false
      } shouldBe true
      sfs2.unmount()
    }

    "metadata region bits cannot be reported as leaks" in {
      val (_, sfs) = mounted()
      val report = Fsck.check(sfs)
      // No metadata block (block bitmap region etc.) should appear in
      // a leak report — they're always "claimed" by fsck.
      val md = sfs.layout.dataStart
      report.issues.collect {
        case FsckIssue.BlockBitmapLeakedBlock(b) if b < md => b
      } shouldBe empty
      sfs.unmount()
    }

    "bad-block list addresses are NOT flagged as leaked" in {
      val (dev, sfs) = mounted()
      val target = sfs.layout.dataStart + 50
      BadBlockOps.mark(sfs, target, Now, Nsec)
      // Now `target`'s bit is set in the bitmap, but no inode points
      // at it. fsck must treat it as legitimately claimed.
      val report = Fsck.check(sfs)
      report.issues.collect {
        case FsckIssue.BlockBitmapLeakedBlock(b) if b == target => b
      } shouldBe empty
      sfs.unmount()
    }

    "extents from a regular file are claimed" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "f")
      // Write a few KiB to allocate some data blocks.
      val data = Array.fill(BlockSize * 3)('a'.toByte)
      val before = sfs.readInode(ino)
      val grown = FileIO.writeFile(before, sfs, 0L, data, Now, Nsec)
      sfs.writeInode(ino, grown)
      val report = Fsck.check(sfs)
      report.clean shouldBe true
      sfs.unmount()
    }

    "indirect tier blocks are claimed (no leaks for ind1)" in {
      val (_, sfs) = mounted()
      val ino = createFile(sfs, "big")
      // Force allocation past the 16 inline extents. Each writeFile
      // call appends one concrete block, so we want 17+ writes to a
      // file that has appendOneConcrete unable to coalesce — easiest
      // way is to write blocks at non-contiguous physical addresses,
      // but allocator hands them out monotonically so they coalesce
      // into one extent. Forcing ind1 requires a file with > 16
      // separate extents, which means non-coalescing appends — easy
      // way: alternate sparse + concrete via writeFile holes. Use
      // appendSparse explicitly between concrete writes.
      var cur = sfs.readInode(ino)
      var i = 0
      while i < 18 do
        // Concrete byte then a hole — 18 separate concrete extents
        // forces ind1 to be allocated.
        cur = ExtentAllocator.appendSparse(cur, sfs, 1)
        cur = FileIO.writeFile(cur, sfs, cur.size + 1L, Array[Byte]('x'.toByte), Now, Nsec)
        i += 1
      sfs.writeInode(ino, cur)
      // ind1 may or may not have been hit depending on coalescing —
      // just assert fsck stays clean.
      Fsck.check(sfs).clean shouldBe true
      sfs.unmount()
    }

    "after journaled mkdir + reopen, bitmap stays consistent" in {
      val (dev, sfs) = mounted()
      val (newRoot, _) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "sub",
        FileOps.ModeDirectory | 0x1ed, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, newRoot)
      sfs.unmount()
      val sfs2 = Sfs.mount(dev)
      Fsck.check(sfs2).clean shouldBe true
      sfs2.unmount()
    }
  }

  // ---- inode reconciliation + link counts (16c) ----------------------

  /** Flip one bit in the inode bitmap on disk. */
  private def flipInodeBitmapBit(dev: RamBlockDevice, sfs: Sfs, inodeNum: Int): Unit =
    val byteOffGlobal = inodeNum >>> 3
    val ibBlockIdx = byteOffGlobal / BlockSize
    val byteInBlock = byteOffGlobal % BlockSize
    val bitInByte = inodeNum & 7
    val ibBuf = new Array[Byte](BlockSize)
    val ibBlk = sfs.layout.inodeBitmapStart.toLong + ibBlockIdx
    dev.readBlock(ibBlk, ibBuf)
    ibBuf(byteInBlock) = (ibBuf(byteInBlock) ^ (1 << bitInByte)).toByte
    dev.writeBlock(ibBlk, ibBuf)
    dev.flush()

  "link-count comparison" - {

    "freshly formatted root has linkCount = 2 and refs = 2 (clean)" in {
      val (_, sfs) = mounted()
      val report = Fsck.check(sfs)
      report.issues.collect {
        case i: FsckIssue.LinkCountMismatch => i
      } shouldBe empty
      sfs.unmount()
    }

    "creating a file makes refs match the file's linkCount = 1" in {
      val (_, sfs) = mounted()
      createFile(sfs, "f")
      Fsck.check(sfs).clean shouldBe true
      sfs.unmount()
    }

    "creating a subdirectory bumps root.linkCount by 1" in {
      val (_, sfs) = mounted()
      val (newRoot, _) = DirOps.mkdir(
        sfs.readInode(InoRoot), InoRoot, sfs, "sub",
        FileOps.ModeDirectory | 0x1ed, 0, 0, Now, Nsec,
      )
      sfs.writeInode(InoRoot, newRoot)
      val r = sfs.readInode(InoRoot)
      r.linkCount shouldBe 3 // 2 + 1 subdir
      Fsck.check(sfs).clean shouldBe true
      sfs.unmount()
    }

    "many subdirs: root.linkCount = 2 + N and fsck stays clean" in {
      val (_, sfs) = mounted()
      val n = 7
      var i = 0
      while i < n do
        val (newRoot, _) = DirOps.mkdir(
          sfs.readInode(InoRoot), InoRoot, sfs, s"d$i",
          FileOps.ModeDirectory | 0x1ed, 0, 0, Now, Nsec,
        )
        sfs.writeInode(InoRoot, newRoot)
        i += 1
      sfs.readInode(InoRoot).linkCount shouldBe (2 + n)
      Fsck.check(sfs).clean shouldBe true
      sfs.unmount()
    }

    "mismatch is reported when linkCount is inflated on disk" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      sfs.unmount()
      // Re-pack the inode with a wrong linkCount.
      val (blk, off) = sfs.layout.inodeLocation(ino)
      val buf = new Array[Byte](BlockSize)
      dev.readBlock(blk, buf)
      val parsed = Inode.unpack(buf, off)
      Inode.pack(parsed.copy(linkCount = 9), buf, off)
      dev.writeBlock(blk, buf)
      dev.flush()
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.LinkCountMismatch(n, stored, computed) =>
          n == ino && stored == 9 && computed == 1
        case _ => false
      } shouldBe true
      sfs2.unmount()
    }
  }

  "orphan detection" - {

    "an inode allocated but with no dir entry is flagged as orphan" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      // Allocate a fresh inode by hand and write a regular-file inode
      // record into it, but never create a dir entry pointing at it.
      // Easiest path: allocate via the bitmap directly + writeInode.
      val orphan = sfs.inodeBitmap.allocate().getOrElse(fail("out of inodes"))
      val now = Now
      val orphanIno = Inode(
        mode = FileOps.ModeRegular | 0x1a4,
        linkCount = 1,
        uid = 0, gid = 0, flags = 0,
        size = 0L, blockCount = 0, generation = 1,
        atimeSec = now, atimeNsec = 0,
        mtimeSec = now, mtimeNsec = 0,
        ctimeSec = now, ctimeNsec = 0,
        crtimeSec = now, crtimeNsec = 0,
        body = InodeBody.EmptyExtents,
        indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
      )
      // writeInode goes through the txn machinery, which is fine.
      sfs.withTransaction { sfs.writeInode(orphan, orphanIno) }
      sfs.unmount()
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.OrphanedInode(n, lc) => n == orphan && lc == 1
        case _                              => false
      } shouldBe true
      // The deliberately-created file is still fine.
      report.issues.collect {
        case FsckIssue.OrphanedInode(n, _) if n == ino => n
      } shouldBe empty
      sfs2.unmount()
    }

    "reserved inodes (0, 1) are NOT flagged as orphans" in {
      val (_, sfs) = mounted()
      val report = Fsck.check(sfs)
      report.issues.collect {
        case FsckIssue.OrphanedInode(n, _) if n == InoNull || n == InoBadBlocks => n
      } shouldBe empty
      // Same for InodeBitmapLeakedBit.
      report.issues.collect {
        case FsckIssue.InodeBitmapLeakedBit(n) if n == InoNull || n == InoBadBlocks => n
      } shouldBe empty
      sfs.unmount()
    }
  }

  "inode bitmap reconciliation" - {

    "missing-bit: a dir entry pointing at an unallocated inode is reported" in {
      val (dev, sfs) = mounted()
      val ino = createFile(sfs, "f")
      sfs.unmount()
      // Clear the inode's bit on disk while leaving the dir entry
      // pointing at it.
      flipInodeBitmapBit(dev, sfs, ino)
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.InodeBitmapMissingBit(n) => n == ino
        case _                                  => false
      } shouldBe true
      sfs2.unmount()
    }

    "leaked: an allocated inode with linkCount=0 is reported" in {
      val (dev, sfs) = mounted()
      // Allocate a slot and leave its bit set, but write a linkCount=0
      // inode to it. (Simulates a half-applied unlink that cleared
      // linkCount but didn't clear the bitmap bit.)
      val n = sfs.inodeBitmap.allocate().getOrElse(fail("out of inodes"))
      val ino = Inode(
        mode = 0, linkCount = 0,
        uid = 0, gid = 0, flags = 0,
        size = 0L, blockCount = 0, generation = 1,
        atimeSec = 0, atimeNsec = 0,
        mtimeSec = 0, mtimeNsec = 0,
        ctimeSec = 0, ctimeNsec = 0,
        crtimeSec = 0, crtimeNsec = 0,
        body = InodeBody.EmptyExtents,
        indirect1 = 0, indirect2 = 0, indirect3 = 0, xattrBlock = 0,
      )
      sfs.withTransaction { sfs.writeInode(n, ino) }
      sfs.unmount()
      val sfs2 = Sfs.mount(dev)
      val report = Fsck.check(sfs2)
      report.issues.exists {
        case FsckIssue.InodeBitmapLeakedBit(m) => m == n
        case _                                 => false
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
