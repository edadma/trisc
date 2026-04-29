package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class JournalSuperblockTests extends AnyFreeSpec with Matchers:

  private val sampleUuid: IndexedSeq[Byte] =
    (0 until 16).map(i => (i * 13 + 7).toByte).toIndexedSeq

  private val sample = JournalSuperblock(
    version = 1,
    blockCount = 1024,
    head = 0,
    tail = 0,
    sequence = 1,
    fsUuid = sampleUuid,
  )

  "round-trips every field" in {
    val buf = new Array[Byte](JournalSuperblock.PayloadSize)
    JournalSuperblock.pack(sample, buf, 0)
    JournalSuperblock.unpack(buf, 0) shouldBe sample
  }

  "writes magic 'SFSJ' little-endian at offset 0" in {
    val buf = new Array[Byte](JournalSuperblock.PayloadSize)
    JournalSuperblock.pack(sample, buf, 0)
    buf(0) shouldBe 'J'.toByte
    buf(1) shouldBe 'S'.toByte
    buf(2) shouldBe 'F'.toByte
    buf(3) shouldBe 'S'.toByte
    Le.u32(buf, 0) shouldBe MagicJournalSuperblock
  }

  "rejects bad magic" in {
    val buf = new Array[Byte](JournalSuperblock.PayloadSize)
    JournalSuperblock.pack(sample, buf, 0)
    Le.putU32(buf, 0, 0)
    a[SfsCorruptError] should be thrownBy JournalSuperblock.unpack(buf, 0)
  }

  "rejects bad CRC" in {
    val buf = new Array[Byte](JournalSuperblock.PayloadSize)
    JournalSuperblock.pack(sample, buf, 0)
    buf(20) = (buf(20) ^ 0xff).toByte // corrupt sequence (CRC-covered)
    a[SfsCorruptError] should be thrownBy JournalSuperblock.unpack(buf, 0)
  }

  "non-zero head/tail/sequence round-trip cleanly" in {
    val s = sample.copy(head = 5, tail = 100, sequence = 42)
    val buf = new Array[Byte](JournalSuperblock.PayloadSize)
    JournalSuperblock.pack(s, buf, 0)
    JournalSuperblock.unpack(buf, 0) shouldBe s
  }
