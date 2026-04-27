package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class IndirectPointerBlockTests extends AnyFreeSpec with Matchers:

  "Capacity equals BlockSize / 4" in {
    IndirectPointerBlock.Capacity shouldBe (BlockSize / 4)
    IndirectPointerBlock.Capacity shouldBe 1024
  }

  "round-trips 1024 distinct pointers" in {
    val ptrs = (0 until 1024).map(i => i * 13 + 1)
    val buf = new Array[Byte](BlockSize)
    IndirectPointerBlock.pack(ptrs, buf, 0)
    IndirectPointerBlock.unpack(buf, 0) shouldBe ptrs
  }

  "zero-fills slots beyond the supplied pointers" in {
    val ptrs = Seq(7, 13, 42)
    val buf = Array.fill[Byte](BlockSize)(0xa5.toByte)
    IndirectPointerBlock.pack(ptrs, buf, 0)
    val unpacked = IndirectPointerBlock.unpack(buf, 0)
    unpacked.take(3) shouldBe ptrs
    unpacked.drop(3).forall(_ == 0) shouldBe true
  }

  "writes pointers as little-endian u32" in {
    val buf = new Array[Byte](BlockSize)
    IndirectPointerBlock.pack(Seq(0x12345678), buf, 0)
    buf(0) shouldBe 0x78.toByte
    buf(1) shouldBe 0x56.toByte
    buf(2) shouldBe 0x34.toByte
    buf(3) shouldBe 0x12.toByte
  }

  "rejects more than Capacity pointers" in {
    val ptrs = Seq.fill(1025)(1)
    val buf = new Array[Byte](BlockSize)
    an[IllegalArgumentException] should be thrownBy IndirectPointerBlock.pack(ptrs, buf, 0)
  }
