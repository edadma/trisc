package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.BlockSize

class RamBlockDeviceTests extends AnyFreeSpec with Matchers:

  "RamBlockDevice.default" - {

    "reports 2 MiB / 512 blocks of capacity" in {
      val dev = RamBlockDevice.default()
      dev.blockCount shouldBe 512L
      dev.sizeInBytes shouldBe (2L * 1024 * 1024)
    }

    "starts zero-initialized on every block" in {
      val dev = RamBlockDevice.default()
      val buf = new Array[Byte](BlockSize)
      for b <- 0L until dev.blockCount do
        dev.readBlock(b, buf)
        buf.forall(_ == 0.toByte) shouldBe true
    }
  }

  "read/write" - {

    "round-trips a written block byte-for-byte" in {
      val dev = RamBlockDevice.default()
      val pattern = Array.tabulate(BlockSize)(i => (i & 0xff).toByte)
      dev.writeBlock(7L, pattern)

      val readBack = new Array[Byte](BlockSize)
      dev.readBlock(7L, readBack)
      readBack shouldBe pattern
    }

    "isolates blocks from one another" in {
      val dev = RamBlockDevice.default()
      val a = Array.fill[Byte](BlockSize)(0xaa.toByte)
      val b = Array.fill[Byte](BlockSize)(0x55.toByte)
      dev.writeBlock(0L, a)
      dev.writeBlock(1L, b)

      val buf = new Array[Byte](BlockSize)
      dev.readBlock(0L, buf); buf shouldBe a
      dev.readBlock(1L, buf); buf shouldBe b
    }

    "does not alias the caller's buffer (read returns a copy)" in {
      val dev = RamBlockDevice.default()
      val written = Array.fill[Byte](BlockSize)(0x42.toByte)
      dev.writeBlock(3L, written)

      val buf = new Array[Byte](BlockSize)
      dev.readBlock(3L, buf)
      buf(0) = 0x00
      val again = new Array[Byte](BlockSize)
      dev.readBlock(3L, again)
      again(0) shouldBe 0x42.toByte
    }

    "does not alias the caller's buffer (write copies in)" in {
      val dev = RamBlockDevice.default()
      val src = Array.fill[Byte](BlockSize)(0x11.toByte)
      dev.writeBlock(5L, src)
      java.util.Arrays.fill(src, 0xff.toByte)

      val buf = new Array[Byte](BlockSize)
      dev.readBlock(5L, buf)
      buf.forall(_ == 0x11.toByte) shouldBe true
    }
  }

  "bounds checking" - {

    "rejects negative block numbers" in {
      val dev = RamBlockDevice.default()
      val buf = new Array[Byte](BlockSize)
      an[IndexOutOfBoundsException] should be thrownBy dev.readBlock(-1L, buf)
      an[IndexOutOfBoundsException] should be thrownBy dev.writeBlock(-1L, buf)
    }

    "rejects block numbers at or above blockCount" in {
      val dev = RamBlockDevice.default()
      val buf = new Array[Byte](BlockSize)
      an[IndexOutOfBoundsException] should be thrownBy
        dev.readBlock(dev.blockCount, buf)
      an[IndexOutOfBoundsException] should be thrownBy
        dev.writeBlock(dev.blockCount, buf)
    }
  }

  "buffer-length checking" - {

    "rejects buffers that are too small" in {
      val dev = RamBlockDevice.default()
      an[IllegalArgumentException] should be thrownBy
        dev.readBlock(0L, new Array[Byte](BlockSize - 1))
      an[IllegalArgumentException] should be thrownBy
        dev.writeBlock(0L, new Array[Byte](BlockSize - 1))
    }

    "rejects buffers that are too large" in {
      val dev = RamBlockDevice.default()
      an[IllegalArgumentException] should be thrownBy
        dev.readBlock(0L, new Array[Byte](BlockSize + 1))
      an[IllegalArgumentException] should be thrownBy
        dev.writeBlock(0L, new Array[Byte](BlockSize + 1))
    }
  }

  "construction" - {

    "rejects a zero or negative blockCount" in {
      an[IllegalArgumentException] should be thrownBy new RamBlockDevice(0L)
      an[IllegalArgumentException] should be thrownBy new RamBlockDevice(-1L)
    }
  }
