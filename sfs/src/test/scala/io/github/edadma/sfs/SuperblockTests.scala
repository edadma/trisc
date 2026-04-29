package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class SuperblockTests extends AnyFreeSpec with Matchers:

  private def sample: Superblock = Superblock(
    versionMajor = 1,
    versionMinor = 0,
    fsState = FsClean,
    totalBlocks = 1024,
    freeBlocks = 900,
    totalInodes = 64,
    freeInodes = 60,
    blockBitmapStart = 2,
    blockBitmapLen = 1,
    inodeBitmapStart = 3,
    inodeBitmapLen = 1,
    inodeTableStart = 4,
    inodeTableLen = 4,
    journalStart = 8,
    journalLen = 16,
    dataStart = 24,
    rootInode = InoRoot,
    hashAlgorithm = HashFnv1a,
    formatTime = 0x6800_0000L,
    lastMountTime = 0x6800_0010L,
    lastWriteTime = 0x6800_0020L,
    uuid = (0 until 16).map(i => (i * 17 + 3).toByte).toIndexedSeq,
    volumeName = "demo",
  )

  "round-trips every field" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    val got = Superblock.unpack(buf, 0)
    got.versionMajor shouldBe 1
    got.versionMinor shouldBe 0
    got.fsState shouldBe FsClean
    got.totalBlocks shouldBe 1024
    got.freeBlocks shouldBe 900
    got.totalInodes shouldBe 64
    got.freeInodes shouldBe 60
    got.blockBitmapStart shouldBe 2
    got.blockBitmapLen shouldBe 1
    got.inodeBitmapStart shouldBe 3
    got.inodeTableStart shouldBe 4
    got.journalStart shouldBe 8
    got.journalLen shouldBe 16
    got.dataStart shouldBe 24
    got.rootInode shouldBe InoRoot
    got.hashAlgorithm shouldBe HashFnv1a
    got.formatTime shouldBe 0x6800_0000L
    got.uuid shouldBe sample.uuid
    got.volumeName shouldBe "demo"
  }

  "writes magic at offset 0 in little-endian byte order" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    // "SFS\0" in little-endian = 0x00 0x53 0x46 0x53 (NUL byte first)
    buf(0) shouldBe 0x00.toByte
    buf(1) shouldBe 0x53.toByte
    buf(2) shouldBe 0x46.toByte
    buf(3) shouldBe 0x53.toByte
  }

  "writes block_size as 4096" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    Le.u16(buf, 8) shouldBe BlockSize
  }

  "stores CRC at offset 128" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    val storedCrc = Le.u32(buf, Superblock.CrcOff)
    storedCrc should not be 0
    // Recompute manually with CRC field zeroed and confirm match
    val tmp = buf.clone()
    Le.putU32(tmp, Superblock.CrcOff, 0)
    val recomputed = Crc32.compute(tmp, 0, Superblock.CrcCoverage)
    storedCrc shouldBe recomputed
  }

  "rejects a bad CRC" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    buf(20) = (buf(20) ^ 0xff).toByte // corrupt a covered byte
    a[SfsCorruptError] should be thrownBy Superblock.unpack(buf, 0)
  }

  "rejects a bad magic" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    Le.putU32(buf, 0, 0xdeadbeef)
    a[SfsCorruptError] should be thrownBy Superblock.unpack(buf, 0)
  }

  "rejects a wrong block_size on unpack" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample, buf, 0)
    Le.putU16(buf, 8, 1024)
    // Need to also re-CRC so it gets past CRC check first
    Le.putU32(buf, Superblock.CrcOff, 0)
    val newCrc = Crc32.compute(buf, 0, Superblock.CrcCoverage)
    Le.putU32(buf, Superblock.CrcOff, newCrc)
    a[SfsCorruptError] should be thrownBy Superblock.unpack(buf, 0)
  }

  "rejects a volume name longer than 15 bytes" in {
    an[IllegalArgumentException] should be thrownBy
      sample.copy(volumeName = "x" * 16)
  }

  "preserves an empty volume name" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample.copy(volumeName = ""), buf, 0)
    Superblock.unpack(buf, 0).volumeName shouldBe ""
  }

  "stores volume_name as null-terminated UTF-8" in {
    val buf = new Array[Byte](Superblock.PayloadSize)
    Superblock.pack(sample.copy(volumeName = "hello"), buf, 0)
    buf(112) shouldBe 'h'.toByte
    buf(116) shouldBe 'o'.toByte
    buf(117) shouldBe 0.toByte // NUL
  }
