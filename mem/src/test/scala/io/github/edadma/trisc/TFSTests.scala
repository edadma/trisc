package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TFSTests extends AnyFreeSpec with Matchers {

  private val BS = 512 // default block size
  private val BLOCKS = 128 // default total blocks
  private val INODES = 64 // default max inodes
  private val NOW = 1000000 // fixed timestamp

  // ---- Binary read helpers ----

  private def readShort(d: Array[Byte], off: Int): Int =
    ((d(off) & 0xff) << 8) | (d(off + 1) & 0xff)

  private def readInt(d: Array[Byte], off: Int): Int =
    ((d(off) & 0xff) << 24) | ((d(off + 1) & 0xff) << 16) |
      ((d(off + 2) & 0xff) << 8) | (d(off + 3) & 0xff)

  // ---- Superblock reader ----

  private def sbOffset(disk: Array[Byte]): Int = BS // block 1

  private def sbMagic(d: Array[Byte]): Int = readInt(d, BS + TFS.SB_MAGIC)
  private def sbVersion(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_VERSION)
  private def sbBlockSize(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_BLOCK_SIZE)
  private def sbTotalBlocks(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_TOTAL_BLOCKS)
  private def sbTotalInodes(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_TOTAL_INODES)
  private def sbInodeBitmap(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_INODE_BITMAP)
  private def sbBlockBitmap(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_BLOCK_BITMAP)
  private def sbInodeTable(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_INODE_TABLE)
  private def sbFirstData(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_FIRST_DATA)
  private def sbFreeBlocks(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_FREE_BLOCKS)
  private def sbFreeInodes(d: Array[Byte]): Int = readShort(d, BS + TFS.SB_FREE_INODES)

  // ---- Inode reader ----

  private case class Inode(
      mode: Int, nlinks: Int, uid: Int, gid: Int,
      size: Int, mtime: Int, ctime: Int,
      direct: Seq[Int], indirect: Int,
  )

  private def readInode(d: Array[Byte], ino: Int): Inode =
    val tableStart = sbInodeTable(d)
    val off = tableStart * BS + ino * TFS.INODE_SIZE
    Inode(
      mode = readShort(d, off + TFS.INO_MODE),
      nlinks = d(off + TFS.INO_NLINKS) & 0xff,
      uid = d(off + TFS.INO_UID) & 0xff,
      gid = d(off + TFS.INO_GID) & 0xff,
      size = readInt(d, off + TFS.INO_SIZE),
      mtime = readInt(d, off + TFS.INO_MTIME),
      ctime = readInt(d, off + TFS.INO_CTIME),
      direct = (0 until TFS.NUM_DIRECT).map(i => readShort(d, off + TFS.INO_DIRECT0 + i * 2)),
      indirect = readShort(d, off + TFS.INO_INDIRECT),
    )

  // ---- Directory entry reader ----

  private case class DirEntry(inode: Int, name: String)

  private def readDirEntry(d: Array[Byte], block: Int, slot: Int): DirEntry =
    val off = block * BS + slot * TFS.DIR_ENTRY_SIZE
    val ino = readShort(d, off)
    val nameBytes = d.slice(off + 2, off + 2 + TFS.DIR_NAME_LEN)
    val end = nameBytes.indexOf(0.toByte)
    val name = if end < 0 then new String(nameBytes, "UTF-8") else new String(nameBytes, 0, end, "UTF-8")
    DirEntry(ino, name)

  // ---- Bitmap helpers ----

  private def inodeBitSet(d: Array[Byte], ino: Int): Boolean =
    val off = sbInodeBitmap(d) * BS + ino / 8
    (d(off) & (1 << (ino % 8))) != 0

  private def blockBitSet(d: Array[Byte], blk: Int): Boolean =
    val off = sbBlockBitmap(d) * BS + blk / 8
    (d(off) & (1 << (blk % 8))) != 0

  // ---- Convenience ----

  private def fmt(prefill: String = "", blocks: Int = BLOCKS, inodes: Int = INODES): Array[Byte] =
    TFS.format(BS, blocks, inodes, prefill, NOW)

  // ===================== SUPERBLOCK =====================

  "superblock has correct magic" in {
    sbMagic(fmt()) shouldBe TFS.MAGIC
  }

  "superblock has correct version" in {
    sbVersion(fmt()) shouldBe TFS.VERSION
  }

  "superblock has correct block size" in {
    sbBlockSize(fmt()) shouldBe BS
  }

  "superblock has correct total blocks" in {
    sbTotalBlocks(fmt()) shouldBe BLOCKS
  }

  "superblock has correct total inodes" in {
    sbTotalInodes(fmt()) shouldBe INODES
  }

  "superblock layout: inode bitmap at block 2" in {
    sbInodeBitmap(fmt()) shouldBe 2
  }

  "superblock layout: block bitmap follows inode bitmap" in {
    val d = fmt()
    sbBlockBitmap(d) shouldBe sbInodeBitmap(d) + 1 // 64 inodes fits in 1 block
  }

  "superblock layout: inode table follows block bitmap" in {
    val d = fmt()
    sbInodeTable(d) shouldBe sbBlockBitmap(d) + 1 // 128 blocks fits in 1 block
  }

  "superblock layout: first data follows inode table" in {
    val d = fmt()
    val inodeTableBlocks = (INODES + (BS / TFS.INODE_SIZE) - 1) / (BS / TFS.INODE_SIZE)
    sbFirstData(d) shouldBe sbInodeTable(d) + inodeTableBlocks
  }

  "superblock free inodes = total - 2 when empty" in {
    // inode 0 reserved + inode 1 root = 2 used
    sbFreeInodes(fmt()) shouldBe INODES - 2
  }

  "superblock free blocks decremented by root data block" in {
    val d = fmt()
    val metaBlocks = sbFirstData(d)
    // 1 data block allocated for root directory
    sbFreeBlocks(d) shouldBe BLOCKS - metaBlocks - 1
  }

  "disk array has correct size" in {
    fmt().length shouldBe BS * BLOCKS
  }

  "boot block (block 0) is all zeros" in {
    val d = fmt()
    d.slice(0, BS).forall(_ == 0) shouldBe true
  }

  // ===================== INODE BITMAP =====================

  "inode 0 marked used in bitmap" in {
    inodeBitSet(fmt(), 0) shouldBe true
  }

  "root inode marked used in bitmap" in {
    inodeBitSet(fmt(), TFS.ROOT_INODE) shouldBe true
  }

  "inode 2 not used when empty" in {
    inodeBitSet(fmt(), 2) shouldBe false
  }

  "allocated inodes marked in bitmap" in {
    val d = fmt("/dev/tty0 char 0 0")
    // inodes: 0 (reserved), 1 (root), 2 (dev dir), 3 (tty0)
    inodeBitSet(d, 2) shouldBe true
    inodeBitSet(d, 3) shouldBe true
    inodeBitSet(d, 4) shouldBe false
  }

  // ===================== BLOCK BITMAP =====================

  "metadata blocks marked used in block bitmap" in {
    val d = fmt()
    for b <- 0 until sbFirstData(d) do
      blockBitSet(d, b) shouldBe true
  }

  "root data block marked used" in {
    val d = fmt()
    val rootIno = readInode(d, TFS.ROOT_INODE)
    blockBitSet(d, rootIno.direct(0)) shouldBe true
  }

  "free data blocks not marked" in {
    val d = fmt()
    val lastUsed = readInode(d, TFS.ROOT_INODE).direct(0)
    blockBitSet(d, lastUsed + 1) shouldBe false
  }

  // ===================== ROOT INODE =====================

  "root inode has directory type" in {
    val root = readInode(fmt(), TFS.ROOT_INODE)
    (root.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
  }

  "root inode has default dir permissions" in {
    val root = readInode(fmt(), TFS.ROOT_INODE)
    (root.mode & 0x1ff) shouldBe TFS.DEFAULT_DIR_PERM
  }

  "root inode has nlinks=2 when empty" in {
    readInode(fmt(), TFS.ROOT_INODE).nlinks shouldBe 2
  }

  "root inode has uid=0 gid=0" in {
    val root = readInode(fmt(), TFS.ROOT_INODE)
    root.uid shouldBe 0
    root.gid shouldBe 0
  }

  "root inode size = 2 dir entries when empty" in {
    readInode(fmt(), TFS.ROOT_INODE).size shouldBe 2 * TFS.DIR_ENTRY_SIZE
  }

  "root inode has correct timestamps" in {
    val root = readInode(fmt(), TFS.ROOT_INODE)
    root.mtime shouldBe NOW
    root.ctime shouldBe NOW
  }

  "root inode direct[0] points to data block" in {
    val d = fmt()
    val root = readInode(d, TFS.ROOT_INODE)
    root.direct(0) should be >= sbFirstData(d)
  }

  // ===================== ROOT DIRECTORY ENTRIES =====================

  "root dir has . entry pointing to self" in {
    val d = fmt()
    val root = readInode(d, TFS.ROOT_INODE)
    val dot = readDirEntry(d, root.direct(0), 0)
    dot.inode shouldBe TFS.ROOT_INODE
    dot.name shouldBe "."
  }

  "root dir has .. entry pointing to self" in {
    val d = fmt()
    val root = readInode(d, TFS.ROOT_INODE)
    val dotdot = readDirEntry(d, root.direct(0), 1)
    dotdot.inode shouldBe TFS.ROOT_INODE
    dotdot.name shouldBe ".."
  }

  // ===================== CHARACTER DEVICES =====================

  "char device has correct type in mode" in {
    val d = fmt("/dev/tty0 char 0 0")
    val tty = readInode(d, 3) // root=1, dev=2, tty0=3
    (tty.mode & TFS.S_IFMT) shouldBe TFS.S_IFCHR
  }

  "char device has default dev permissions" in {
    val d = fmt("/dev/tty0 char 0 0")
    val tty = readInode(d, 3)
    (tty.mode & 0x1ff) shouldBe TFS.DEFAULT_DEV_PERM
  }

  "char device has nlinks=1" in {
    val d = fmt("/dev/tty0 char 0 0")
    readInode(d, 3).nlinks shouldBe 1
  }

  "char device major/minor in direct[0]" in {
    val d = fmt("/dev/tty0 char 5 3")
    val dev = readInode(d, 3)
    dev.direct(0) shouldBe ((5 << 8) | 3)
  }

  "char device has size 0" in {
    readInode(fmt("/dev/tty0 char 0 0"), 3).size shouldBe 0
  }

  // ===================== BLOCK DEVICES =====================

  "block device has correct type" in {
    val d = fmt("/dev/disk0 block 1 0")
    val dev = readInode(d, 3)
    (dev.mode & TFS.S_IFMT) shouldBe TFS.S_IFBLK
  }

  "block device major/minor in direct[0]" in {
    val d = fmt("/dev/disk0 block 1 0")
    readInode(d, 3).direct(0) shouldBe ((1 << 8) | 0)
  }

  // ===================== DIRECTORIES =====================

  "implicit directory created from path" in {
    val d = fmt("/dev/tty0 char 0 0")
    val dev = readInode(d, 2) // dev dir
    (dev.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
    (dev.mode & 0x1ff) shouldBe TFS.DEFAULT_DIR_PERM
  }

  "subdirectory has . and .. entries" in {
    val d = fmt("/dev/tty0 char 0 0")
    val dev = readInode(d, 2)
    val dot = readDirEntry(d, dev.direct(0), 0)
    val dotdot = readDirEntry(d, dev.direct(0), 1)
    dot.inode shouldBe 2
    dot.name shouldBe "."
    dotdot.inode shouldBe TFS.ROOT_INODE
    dotdot.name shouldBe ".."
  }

  "subdirectory contains child entry" in {
    val d = fmt("/dev/tty0 char 0 0")
    val dev = readInode(d, 2)
    val entry = readDirEntry(d, dev.direct(0), 2) // after . and ..
    entry.inode shouldBe 3
    entry.name shouldBe "tty0"
  }

  "parent nlinks increases with subdirectories" in {
    val d = fmt(
      """/dev/tty0 char 0 0
        |/dev/sub dir
        |""".stripMargin)
    // root has /dev as child dir -> nlinks = 2 (. + ..) + 1 (dev's ..) = 3
    readInode(d, TFS.ROOT_INODE).nlinks shouldBe 3
    // /dev has /dev/sub as child dir -> nlinks = 2 + 1 = 3
    readInode(d, 2).nlinks shouldBe 3
  }

  "shared parent not duplicated" in {
    val d = fmt(
      """/dev/tty0 char 0 0
        |/dev/null char 0 1
        |/dev/disk0 block 1 0
        |""".stripMargin)
    // 1=root, 2=dev, 3=tty0, 4=null, 5=disk0
    readInode(d, 2).nlinks shouldBe 2 // no subdirs, just device nodes
    val dev = readInode(d, 2)
    dev.size shouldBe 5 * TFS.DIR_ENTRY_SIZE // . + .. + tty0 + null + disk0
  }

  "deep nesting works" in {
    val d = fmt("/a/b/c/d file")
    // 1=root, 2=a, 3=b, 4=c, 5=d(file)
    val a = readInode(d, 2)
    (a.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
    val b = readInode(d, 3)
    (b.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
    val c = readInode(d, 4)
    (c.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
    val f = readInode(d, 5)
    (f.mode & TFS.S_IFMT) shouldBe TFS.S_IFREG
  }

  "explicit dir creates directory" in {
    val d = fmt("/tmp dir")
    val tmp = readInode(d, 2) // root=1, tmp=2
    (tmp.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
    tmp.nlinks shouldBe 2
  }

  "explicit dir after implicit is no-op" in {
    val d = fmt(
      """/dev/tty0 char 0 0
        |/dev dir
        |""".stripMargin)
    // /dev was already created implicitly; explicit dir should not create a duplicate
    sbFreeInodes(d) shouldBe INODES - 4 // root + dev + tty0 = 3 used, inode 0 reserved
  }

  "root dir entry lists children" in {
    val d = fmt(
      """/dev/tty0 char 0 0
        |/tmp dir
        |""".stripMargin)
    val root = readInode(d, TFS.ROOT_INODE)
    val e0 = readDirEntry(d, root.direct(0), 0)
    val e1 = readDirEntry(d, root.direct(0), 1)
    val e2 = readDirEntry(d, root.direct(0), 2)
    val e3 = readDirEntry(d, root.direct(0), 3)
    e0.name shouldBe "."
    e1.name shouldBe ".."
    e2.name shouldBe "dev"
    e3.name shouldBe "tmp"
  }

  // ===================== REGULAR FILES =====================

  "empty file has size 0 and no blocks" in {
    val d = fmt("/hello file")
    val f = readInode(d, 2) // root=1, hello=2
    (f.mode & TFS.S_IFMT) shouldBe TFS.S_IFREG
    f.size shouldBe 0
    f.direct(0) shouldBe 0
  }

  "file has default file permissions" in {
    val d = fmt("/hello file")
    (readInode(d, 2).mode & 0x1ff) shouldBe TFS.DEFAULT_FILE_PERM
  }

  "file with content has correct size" in {
    val d = fmt("""/etc/motd file "Hello from TOS"""")
    val f = readInode(d, 3) // root=1, etc=2, motd=3
    f.size shouldBe "Hello from TOS".length
  }

  "file content written to data block" in {
    val content = "Hello from TOS"
    val d = fmt(s"""/f file "$content"""")
    val f = readInode(d, 2)
    val dataOff = f.direct(0) * BS
    new String(d, dataOff, content.length, "UTF-8") shouldBe content
  }

  "file content zero-padded in block" in {
    val d = fmt("""/f file "Hi"""")
    val f = readInode(d, 2)
    val dataOff = f.direct(0) * BS
    d(dataOff) shouldBe 'H'.toByte
    d(dataOff + 1) shouldBe 'i'.toByte
    d(dataOff + 2) shouldBe 0
  }

  "multi-block file uses multiple direct pointers" in {
    val content = "X" * 600 // > 512 bytes, needs 2 blocks
    val d = fmt(s"""/big file "$content"""")
    val f = readInode(d, 2)
    f.size shouldBe 600
    f.direct(0) should be > 0
    f.direct(1) should be > 0
    f.direct(1) shouldBe f.direct(0) + 1 // contiguous
  }

  "multi-block file content fully written" in {
    val content = "A" * 600
    val d = fmt(s"""/big file "$content"""")
    val f = readInode(d, 2)
    // Read first 512 bytes from block 0
    val blk0 = new String(d, f.direct(0) * BS, 512, "UTF-8")
    blk0 shouldBe "A" * 512
    // Read remaining 88 bytes from block 1
    val blk1 = new String(d, f.direct(1) * BS, 88, "UTF-8")
    blk1 shouldBe "A" * 88
  }

  "file using all 6 direct pointers" in {
    val content = "B" * (BS * 6) // exactly 6 blocks
    val d = fmt(s"""/full file "$content"""", blocks = 256)
    val f = readInode(d, 2)
    f.size shouldBe BS * 6
    for i <- 0 until 6 do
      f.direct(i) should be > 0
    f.indirect shouldBe 0 // no indirect needed
  }

  // ===================== INDIRECT BLOCKS =====================

  "file exceeding direct pointers uses indirect block" in {
    val content = "C" * (BS * 7) // needs 7 data blocks + 1 indirect block
    val d = fmt(s"""/huge file "$content"""", blocks = 256)
    val f = readInode(d, 2)
    f.size shouldBe BS * 7
    f.indirect should be > 0

    // Indirect block contains pointer to 7th data block
    val indOff = f.indirect * BS
    val blk6 = readShort(d, indOff)
    blk6 should be > 0

    // Verify content in 7th block
    val actual = new String(d, blk6 * BS, BS, "UTF-8")
    actual shouldBe "C" * BS
  }

  "indirect block pointers are correct" in {
    val content = "D" * (BS * 8) // 8 blocks: 6 direct + 2 indirect
    val d = fmt(s"""/huge file "$content"""", blocks = 256)
    val f = readInode(d, 2)
    f.indirect should be > 0

    val indOff = f.indirect * BS
    val indBlk0 = readShort(d, indOff)
    val indBlk1 = readShort(d, indOff + 2)
    indBlk0 should be > 0
    indBlk1 should be > 0
    indBlk1 shouldBe indBlk0 + 1 // contiguous
  }

  // ===================== TIMESTAMPS =====================

  "all inodes have correct mtime" in {
    val d = fmt("/dev/tty0 char 0 0")
    readInode(d, TFS.ROOT_INODE).mtime shouldBe NOW
    readInode(d, 2).mtime shouldBe NOW // dev dir
    readInode(d, 3).mtime shouldBe NOW // tty0
  }

  "all inodes have correct ctime" in {
    val d = fmt("/dev/tty0 char 0 0")
    readInode(d, TFS.ROOT_INODE).ctime shouldBe NOW
    readInode(d, 3).ctime shouldBe NOW
  }

  "custom timestamp passed through" in {
    val d = TFS.format(BS, BLOCKS, INODES, "", now = 42)
    readInode(d, TFS.ROOT_INODE).mtime shouldBe 42
    readInode(d, TFS.ROOT_INODE).ctime shouldBe 42
  }

  // ===================== FREE COUNTS =====================

  "free inodes tracks allocations" in {
    val d = fmt(
      """/dev/tty0 char 0 0
        |/dev/disk0 block 1 0
        |/etc/motd file "Hi"
        |""".stripMargin)
    // Used inodes: 0 (reserved), 1 (root), 2 (dev), 3 (tty0), 4 (disk0), 5 (etc), 6 (motd) = 7
    sbFreeInodes(d) shouldBe INODES - 7
  }

  "free blocks tracks allocations" in {
    val d = fmt("/f file \"Hi\"")
    val metaBlocks = sbFirstData(d)
    // Data blocks: 1 (root dir) + 1 (file data) = 2
    sbFreeBlocks(d) shouldBe BLOCKS - metaBlocks - 2
  }

  // ===================== MIXED =====================

  "full prefill with all types" in {
    val d = fmt(
      """/dev/tty0 char 0 0
        |/dev/disk0 block 1 0
        |/dev/null char 0 1
        |/etc/motd file "Welcome"
        |/tmp dir
        |""".stripMargin)

    // Check inode count: 0(rsv) + 1(root) + 2(dev) + 3(tty0) + 4(disk0) + 5(null) + 6(etc) + 7(motd) + 8(tmp) = 9
    sbFreeInodes(d) shouldBe INODES - 9

    // Verify motd content
    val motd = readInode(d, 7)
    (motd.mode & TFS.S_IFMT) shouldBe TFS.S_IFREG
    motd.size shouldBe "Welcome".length
    val content = new String(d, motd.direct(0) * BS, motd.size, "UTF-8")
    content shouldBe "Welcome"

    // Verify tmp is empty dir
    val tmp = readInode(d, 8)
    (tmp.mode & TFS.S_IFMT) shouldBe TFS.S_IFDIR
    tmp.nlinks shouldBe 2
  }

  // ===================== EDGE CASES =====================

  "blank lines and whitespace ignored" in {
    val d = fmt(
      """
        |  /dev/tty0 char 0 0
        |
        |  /dev/null char 0 1
        |
        |""".stripMargin)
    sbFreeInodes(d) shouldBe INODES - 5 // reserved + root + dev + tty0 + null
  }

  "empty prefill creates only root" in {
    val d = fmt()
    sbFreeInodes(d) shouldBe INODES - 2
    val root = readInode(d, TFS.ROOT_INODE)
    root.size shouldBe 2 * TFS.DIR_ENTRY_SIZE
  }

  "name truncated to 14 bytes" in {
    val d = fmt("/this_is_a_very_long_name file")
    val root = readInode(d, TFS.ROOT_INODE)
    val entry = readDirEntry(d, root.direct(0), 2)
    entry.name shouldBe "this_is_a_very" // 14 chars
  }

  "block size must be >= INODE_SIZE" in {
    an[IllegalArgumentException] should be thrownBy {
      TFS.format(16, 64, 8, "")
    }
  }

  "need at least 2 inodes" in {
    an[IllegalArgumentException] should be thrownBy {
      TFS.format(BS, 64, 1, "")
    }
  }

  "unknown type throws error" in {
    an[Exception] should be thrownBy {
      fmt("/foo symlink /bar")
    }
  }

  "malformed line throws error" in {
    an[Exception] should be thrownBy {
      fmt("/foo")
    }
  }

  "char device missing args throws error" in {
    an[IllegalArgumentException] should be thrownBy {
      fmt("/dev/tty0 char")
    }
  }

  // ===================== INODE 0 =====================

  "inode 0 is unused (reserved)" in {
    val d = fmt()
    val ino0 = readInode(d, 0)
    ino0.mode shouldBe 0
    ino0.nlinks shouldBe 0
    ino0.size shouldBe 0
  }

  // ===================== CUSTOM BLOCK SIZE =====================

  "works with 256-byte blocks" in {
    val d = TFS.format(256, 256, 32, "/dev/tty0 char 0 0", NOW)
    val sb = 256 // superblock at block 1
    readInt(d, sb + TFS.SB_MAGIC) shouldBe TFS.MAGIC
    readShort(d, sb + TFS.SB_BLOCK_SIZE) shouldBe 256
  }
}
