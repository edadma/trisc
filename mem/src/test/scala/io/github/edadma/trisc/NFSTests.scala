package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NFSTests extends AnyFreeSpec with Matchers {

  private def readShort(disk: Array[Byte], offset: Int): Int =
    ((disk(offset) & 0xff) << 8) | (disk(offset + 1) & 0xff)

  private def readInt(disk: Array[Byte], offset: Int): Int =
    ((disk(offset) & 0xff) << 24) | ((disk(offset + 1) & 0xff) << 16) |
      ((disk(offset + 2) & 0xff) << 8) | (disk(offset + 3) & 0xff)

  private def readName(disk: Array[Byte], offset: Int): String =
    val bytes = disk.slice(offset, offset + NFS.NAME_LEN)
    val end = bytes.indexOf(0.toByte)
    if end < 0 then new String(bytes, "UTF-8") else new String(bytes, 0, end, "UTF-8")

  private case class DirEntry(
      name: String,
      parent: Int,
      entryType: Byte,
      flags: Byte,
      fieldA: Int,
      fieldB: Int,
      fileSize: Int,
  )

  private def readEntry(disk: Array[Byte], sectorSize: Int, idx: Int): DirEntry =
    val offset = sectorSize + idx * NFS.ENTRY_SIZE
    DirEntry(
      name = readName(disk, offset),
      parent = readShort(disk, offset + 16),
      entryType = disk(offset + 18),
      flags = disk(offset + 19),
      fieldA = readShort(disk, offset + 20),
      fieldB = readShort(disk, offset + 22),
      fileSize = readInt(disk, offset + 24),
    )

  // ===== Superblock =====

  "empty filesystem has valid superblock" in {
    val disk = NFS.format(512, 64, "")
    readInt(disk, 0) shouldBe NFS.MAGIC
    readShort(disk, 4) shouldBe NFS.VERSION
  }

  "superblock entry count includes root" in {
    val disk = NFS.format(512, 64, "")
    readShort(disk, 8) shouldBe 1 // just root
  }

  "superblock max entries is sector-aligned" in {
    val disk = NFS.format(512, 64, "")
    val maxEntries = readShort(disk, 6)
    maxEntries shouldBe (512 / NFS.ENTRY_SIZE) // 16
  }

  "superblock first data sector follows directory" in {
    val disk = NFS.format(512, 64, "")
    val firstData = readShort(disk, 10)
    firstData shouldBe 2 // sector 0 = superblock, sector 1 = directory
  }

  "superblock total sectors matches input" in {
    val disk = NFS.format(512, 64, "")
    readShort(disk, 12) shouldBe 64
  }

  "superblock next free sector equals first data when no files" in {
    val disk = NFS.format(512, 64, "")
    readShort(disk, 14) shouldBe readShort(disk, 10)
  }

  "disk array has correct size" in {
    val disk = NFS.format(512, 64, "")
    disk.length shouldBe 512 * 64
  }

  "custom sector size works" in {
    val disk = NFS.format(256, 128, "")
    disk.length shouldBe 256 * 128
    readInt(disk, 0) shouldBe NFS.MAGIC
  }

  // ===== Root directory =====

  "root entry is always entry 0" in {
    val disk = NFS.format(512, 64, "")
    val root = readEntry(disk, 512, 0)
    root.name shouldBe "/"
    root.parent shouldBe NFS.NO_PARENT
    root.entryType shouldBe NFS.TYPE_DIR
    root.flags shouldBe NFS.FLAG_ACTIVE
  }

  // ===== Character devices =====

  "char device creates entry with correct type and major/minor" in {
    val disk = NFS.format(512, 64, "/dev/tty0 char 0 0")
    val entries = readShort(disk, 8)
    entries shouldBe 3 // root + dev + tty0

    val dev = readEntry(disk, 512, 1)
    dev.name shouldBe "dev"
    dev.parent shouldBe 0
    dev.entryType shouldBe NFS.TYPE_DIR

    val tty = readEntry(disk, 512, 2)
    tty.name shouldBe "tty0"
    tty.parent shouldBe 1
    tty.entryType shouldBe NFS.TYPE_CHAR
    tty.flags shouldBe NFS.FLAG_ACTIVE
    tty.fieldA shouldBe 0 // major
    tty.fieldB shouldBe 0 // minor
    tty.fileSize shouldBe 0
  }

  "char device with nonzero major/minor" in {
    val disk = NFS.format(512, 64, "/dev/null char 5 3")
    val entry = readEntry(disk, 512, 2) // root, dev, null
    entry.name shouldBe "null"
    entry.entryType shouldBe NFS.TYPE_CHAR
    entry.fieldA shouldBe 5
    entry.fieldB shouldBe 3
  }

  // ===== Block devices =====

  "block device creates entry with correct type and major/minor" in {
    val disk = NFS.format(512, 64, "/dev/disk0 block 1 0")
    val entry = readEntry(disk, 512, 2) // root, dev, disk0
    entry.name shouldBe "disk0"
    entry.entryType shouldBe NFS.TYPE_BLOCK
    entry.fieldA shouldBe 1
    entry.fieldB shouldBe 0
  }

  // ===== Implicit directory creation =====

  "implicit directories are created from path" in {
    val disk = NFS.format(512, 64, "/a/b/c/file.txt file")
    val entries = readShort(disk, 8)
    entries shouldBe 5 // root, a, b, c, file.txt

    val a = readEntry(disk, 512, 1)
    a.name shouldBe "a"
    a.parent shouldBe 0
    a.entryType shouldBe NFS.TYPE_DIR

    val b = readEntry(disk, 512, 2)
    b.name shouldBe "b"
    b.parent shouldBe 1
    b.entryType shouldBe NFS.TYPE_DIR

    val c = readEntry(disk, 512, 3)
    c.name shouldBe "c"
    c.parent shouldBe 2
    c.entryType shouldBe NFS.TYPE_DIR

    val f = readEntry(disk, 512, 4)
    f.name shouldBe "file.txt"
    f.parent shouldBe 3
    f.entryType shouldBe NFS.TYPE_REGULAR
  }

  "shared parent directories are not duplicated" in {
    val disk = NFS.format(512, 64,
      """/dev/tty0 char 0 0
        |/dev/disk0 block 1 0
        |/dev/null char 0 1
        |""".stripMargin)
    val entries = readShort(disk, 8)
    entries shouldBe 5 // root, dev, tty0, disk0, null

    val tty = readEntry(disk, 512, 2)
    tty.name shouldBe "tty0"
    tty.parent shouldBe 1

    val disk0 = readEntry(disk, 512, 3)
    disk0.name shouldBe "disk0"
    disk0.parent shouldBe 1 // same parent

    val nul = readEntry(disk, 512, 4)
    nul.name shouldBe "null"
    nul.parent shouldBe 1 // same parent
  }

  // ===== Explicit empty directory =====

  "explicit dir creates directory entry" in {
    val disk = NFS.format(512, 64, "/tmp dir")
    val entries = readShort(disk, 8)
    entries shouldBe 2 // root + tmp

    val tmp = readEntry(disk, 512, 1)
    tmp.name shouldBe "tmp"
    tmp.parent shouldBe 0
    tmp.entryType shouldBe NFS.TYPE_DIR
    tmp.flags shouldBe NFS.FLAG_ACTIVE
  }

  // ===== Regular files =====

  "empty file creates entry with zero size" in {
    val disk = NFS.format(512, 64, "/etc/config file")
    val entry = readEntry(disk, 512, 2) // root, etc, config
    entry.name shouldBe "config"
    entry.entryType shouldBe NFS.TYPE_REGULAR
    entry.fileSize shouldBe 0
    entry.fieldA shouldBe 0 // no sectors allocated
    entry.fieldB shouldBe 0
  }

  "file with inline content has correct size" in {
    val disk = NFS.format(512, 64, """/etc/motd file "Hello from TOS"""")
    val entry = readEntry(disk, 512, 2) // root, etc, motd
    entry.name shouldBe "motd"
    entry.entryType shouldBe NFS.TYPE_REGULAR
    entry.fileSize shouldBe "Hello from TOS".length
    entry.fieldA should be > 0 // start sector allocated
    entry.fieldB shouldBe 1 // 14 bytes fits in 1 sector
  }

  "file content is written to data region" in {
    val content = "Hello from TOS"
    val disk = NFS.format(512, 64, s"""/etc/motd file "$content"""")
    val entry = readEntry(disk, 512, 2)
    val dataOffset = entry.fieldA * 512
    val actual = new String(disk, dataOffset, content.length, "UTF-8")
    actual shouldBe content
  }

  "file content remainder of sector is zero-padded" in {
    val disk = NFS.format(512, 64, """/f file "Hi"""")
    val entry = readEntry(disk, 512, 1) // root, f
    val dataOffset = entry.fieldA * 512
    // Content is "Hi" (2 bytes), rest of sector should be 0
    disk(dataOffset) shouldBe 'H'.toByte
    disk(dataOffset + 1) shouldBe 'i'.toByte
    disk(dataOffset + 2) shouldBe 0
  }

  "multiple files allocate contiguous sectors" in {
    val disk = NFS.format(512, 64,
      """/a file "AAAA"
        |/b file "BBBB"
        |""".stripMargin)
    val a = readEntry(disk, 512, 1) // root, a, b
    val b = readEntry(disk, 512, 2)
    a.fieldA should be > 0
    b.fieldA shouldBe a.fieldA + a.fieldB
  }

  "file spanning multiple sectors allocates correctly" in {
    val bigContent = "X" * 600 // > 512 bytes
    val disk = NFS.format(512, 64, s"""/big file "$bigContent"""")
    val entry = readEntry(disk, 512, 1) // root, big
    entry.fileSize shouldBe 600
    entry.fieldB shouldBe 2 // ceil(600/512) = 2 sectors
  }

  "file content spanning sectors is fully written" in {
    val content = "A" * 600
    val disk = NFS.format(512, 64, s"""/big file "$content"""")
    val entry = readEntry(disk, 512, 1)
    val dataOffset = entry.fieldA * 512
    val actual = new String(disk, dataOffset, content.length, "UTF-8")
    actual shouldBe content
  }

  "next free sector advances past allocated files" in {
    val content = "X" * 600 // 2 sectors
    val disk = NFS.format(512, 64, s"""/big file "$content"""")
    val firstData = readShort(disk, 10)
    val nextFree = readShort(disk, 14)
    nextFree shouldBe firstData + 2
  }

  // ===== Mixed entries =====

  "full prefill with devices and files" in {
    val disk = NFS.format(512, 128,
      """/dev/tty0 char 0 0
        |/dev/disk0 block 1 0
        |/dev/null char 0 1
        |/etc/motd file "Welcome"
        |/tmp dir
        |""".stripMargin)
    val entries = readShort(disk, 8)
    entries shouldBe 8 // root, dev, tty0, disk0, null, etc, motd, tmp

    // Verify motd content
    val motd = readEntry(disk, 512, 6) // root=0, dev=1, tty0=2, disk0=3, null=4, etc=5, motd=6
    motd.name shouldBe "motd"
    motd.fileSize shouldBe "Welcome".length
    val dataOffset = motd.fieldA * 512
    new String(disk, dataOffset, motd.fileSize, "UTF-8") shouldBe "Welcome"
  }

  // ===== Directory sectors spilling =====

  "many entries spill to multiple directory sectors" in {
    // 512 / 32 = 16 entries per sector, create 20 files to need 2+ dir sectors
    val lines = (1 to 20).map(i => s"/f$i file").mkString("\n")
    val disk = NFS.format(512, 64, lines)
    val entries = readShort(disk, 8)
    entries shouldBe 21 // root + 20 files

    val maxEntries = readShort(disk, 6)
    maxEntries shouldBe 32 // 2 dir sectors * 16 entries each

    val firstData = readShort(disk, 10)
    firstData shouldBe 3 // superblock + 2 dir sectors
  }

  // ===== Edge cases =====

  "blank lines and whitespace are ignored" in {
    val disk = NFS.format(512, 64,
      """
        |  /dev/tty0 char 0 0
        |
        |  /dev/null char 0 1
        |
        |""".stripMargin)
    readShort(disk, 8) shouldBe 4 // root, dev, tty0, null
  }

  "file at root level has parent 0" in {
    val disk = NFS.format(512, 64, "/hello file")
    val entry = readEntry(disk, 512, 1)
    entry.name shouldBe "hello"
    entry.parent shouldBe 0
  }

  "name truncated to 16 bytes" in {
    val disk = NFS.format(512, 64, "/this_is_a_very_long_filename file")
    val entry = readEntry(disk, 512, 1)
    entry.name shouldBe "this_is_a_very_l" // 16 chars
  }

  "sector size must be >= ENTRY_SIZE" in {
    an[IllegalArgumentException] should be thrownBy {
      NFS.format(16, 64, "")
    }
  }

  "unknown entry type throws error" in {
    an[Exception] should be thrownBy {
      NFS.format(512, 64, "/foo symlink /bar")
    }
  }

  "malformed line throws error" in {
    an[Exception] should be thrownBy {
      NFS.format(512, 64, "/foo")
    }
  }

  "char device missing major/minor throws error" in {
    an[IllegalArgumentException] should be thrownBy {
      NFS.format(512, 64, "/dev/tty0 char")
    }
  }
}
