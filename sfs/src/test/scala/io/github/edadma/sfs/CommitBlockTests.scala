package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class CommitBlockTests extends AnyFreeSpec with Matchers:

  "round-trips every field" in {
    val c = CommitBlock(sequence = 17, commitTime = 0x6800ABCDL, crc32 = 0xdeadbeef)
    val buf = new Array[Byte](CommitBlock.PayloadSize)
    CommitBlock.pack(c, buf, 0)
    CommitBlock.unpack(buf, 0) shouldBe c
  }

  "writes magic 'SFSC' at offset 0" in {
    val buf = new Array[Byte](CommitBlock.PayloadSize)
    CommitBlock.pack(CommitBlock(0, 0L, 0), buf, 0)
    Le.u32(buf, 0) shouldBe MagicCommit
  }

  "stores crc32 at offset 16" in {
    val buf = new Array[Byte](CommitBlock.PayloadSize)
    CommitBlock.pack(CommitBlock(0, 0L, 0xcafebabe), buf, 0)
    Le.u32(buf, CommitBlock.CrcOff) shouldBe 0xcafebabe
  }

  "rejects bad magic" in {
    val buf = new Array[Byte](CommitBlock.PayloadSize)
    CommitBlock.pack(CommitBlock(1, 1L, 1), buf, 0)
    Le.putU32(buf, 0, 0)
    a[SfsCorruptError] should be thrownBy CommitBlock.unpack(buf, 0)
  }

  "zero-fills bytes after the CRC field" in {
    val buf = Array.fill[Byte](CommitBlock.PayloadSize)(0xff.toByte)
    CommitBlock.pack(CommitBlock(1, 1L, 1), buf, 0)
    for i <- 20 until CommitBlock.PayloadSize do buf(i) shouldBe 0.toByte
  }
