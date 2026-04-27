package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class ExtentTests extends AnyFreeSpec with Matchers:

  "Extent — atomic 8-byte struct" - {

    "round-trips a plain extent" in {
      val buf = new Array[Byte](16)
      Extent.pack(Extent(start = 0x12345, count = 7), buf, 0)
      Extent.unpack(buf, 0) shouldBe Extent(start = 0x12345, count = 7)
    }

    "round-trips with UNINITIALIZED set" in {
      val buf = new Array[Byte](8)
      Extent.pack(Extent(100, 5, uninitialized = true), buf, 0)
      val got = Extent.unpack(buf, 0)
      got.start shouldBe 100
      got.count shouldBe 5
      got.uninitialized shouldBe true
      got.sparse shouldBe false
    }

    "round-trips with SPARSE set" in {
      val buf = new Array[Byte](8)
      Extent.pack(Extent(0, 1024, sparse = true), buf, 0)
      val got = Extent.unpack(buf, 0)
      got.sparse shouldBe true
      got.uninitialized shouldBe false
      got.count shouldBe 1024
    }

    "writes the flag bits at the top of word 0" in {
      val buf = new Array[Byte](8)
      Extent.pack(Extent(0x1, 0, uninitialized = true, sparse = true), buf, 0)
      // word 0 = (3 << 30) | 1 = 0xc0000001
      Le.u32(buf, 0) shouldBe 0xc0000001
    }

    "rejects start_block above the 29-bit ceiling" in {
      an[IllegalArgumentException] should be thrownBy Extent(ExtentStartMask + 1, 0)
    }

    "rejects negative count" in {
      an[IllegalArgumentException] should be thrownBy Extent(0, -1)
    }

    "Empty is start=0, count=0, no flags" in {
      Extent.Empty shouldBe Extent(0, 0)
      Extent.Empty.uninitialized shouldBe false
      Extent.Empty.sparse shouldBe false
    }
  }

  "IndirectExtentBlock — 512 packed extents per 4 KiB block" - {

    "Capacity equals BlockSize / ExtentSize" in {
      IndirectExtentBlock.Capacity shouldBe (BlockSize / ExtentSize)
      IndirectExtentBlock.Capacity shouldBe 512
    }

    "round-trips a full block of 512 distinct extents" in {
      val xs = (0 until 512).map(i => Extent(i + 1, i * 7 + 1))
      val buf = new Array[Byte](BlockSize)
      IndirectExtentBlock.pack(xs, buf, 0)
      IndirectExtentBlock.unpack(buf, 0) shouldBe xs
    }

    "zero-fills slots beyond the supplied extents" in {
      val xs = Seq(Extent(10, 1), Extent(20, 2))
      val buf = new Array[Byte](BlockSize)
      java.util.Arrays.fill(buf, 0xff.toByte)
      IndirectExtentBlock.pack(xs, buf, 0)
      val unpacked = IndirectExtentBlock.unpack(buf, 0)
      unpacked.length shouldBe 512
      unpacked.take(2) shouldBe xs
      unpacked.drop(2).forall(_ == Extent.Empty) shouldBe true
    }

    "rejects more than Capacity extents" in {
      val xs = Seq.fill(513)(Extent(1, 1))
      val buf = new Array[Byte](BlockSize)
      an[IllegalArgumentException] should be thrownBy IndirectExtentBlock.pack(xs, buf, 0)
    }
  }
