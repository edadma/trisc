package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class BitmapTests extends AnyFreeSpec with Matchers:

  // A small fresh device + bitmap configuration used by most tests.
  // Two bitmap blocks at startBlock=0; up to 5000 bits live, leaving the
  // last block partially used so trailing-bit edge cases get exercised.
  private def fresh(totalBits: Int = 5000): (RamBlockDevice, Bitmap) =
    val dev = new RamBlockDevice(4)
    val bm = new Bitmap(dev, startBlock = 0, lengthBlocks = 2, totalBits = totalBits)
    (dev, bm)

  "construction" - {

    "rejects lengthBlocks < 1" in {
      val dev = new RamBlockDevice(4)
      an[IllegalArgumentException] should be thrownBy
        new Bitmap(dev, 0, lengthBlocks = 0, totalBits = 1)
    }

    "rejects totalBits exceeding region capacity" in {
      val dev = new RamBlockDevice(4)
      // 1 block × 4096 bytes × 8 bits = 32 768 bits max
      an[IllegalArgumentException] should be thrownBy
        new Bitmap(dev, 0, lengthBlocks = 1, totalBits = 32_769)
    }

    "starts with every bit clear and freeCount == totalBits" in {
      val (_, bm) = fresh()
      bm.freeCount shouldBe 5000
      for b <- 0 until 5000 do bm.isSet(b) shouldBe false
    }
  }

  "set / clear / isSet" - {

    "set toggles a bit and decrements freeCount once" in {
      val (_, bm) = fresh()
      bm.set(42)
      bm.isSet(42) shouldBe true
      bm.freeCount shouldBe 4999
      // setting again is a no-op for freeCount
      bm.set(42)
      bm.freeCount shouldBe 4999
    }

    "clear toggles a set bit and increments freeCount once" in {
      val (_, bm) = fresh()
      bm.set(42)
      bm.clear(42)
      bm.isSet(42) shouldBe false
      bm.freeCount shouldBe 5000
      // clearing a clear bit is a no-op
      bm.clear(42)
      bm.freeCount shouldBe 5000
    }

    "rejects out-of-range bit indices" in {
      val (_, bm) = fresh()
      an[IndexOutOfBoundsException] should be thrownBy bm.isSet(-1)
      an[IndexOutOfBoundsException] should be thrownBy bm.isSet(5000)
      an[IndexOutOfBoundsException] should be thrownBy bm.set(-1)
      an[IndexOutOfBoundsException] should be thrownBy bm.set(5000)
      an[IndexOutOfBoundsException] should be thrownBy bm.clear(5000)
    }

    "isolates bits across byte boundaries" in {
      val (_, bm) = fresh()
      bm.set(7)
      bm.set(8) // first bit of next byte
      for b <- 0 until 16 if b != 7 && b != 8 do bm.isSet(b) shouldBe false
      bm.isSet(7) shouldBe true
      bm.isSet(8) shouldBe true
    }

    "isolates bits across block boundaries" in {
      val (_, bm) = fresh()
      val lastInBlock0 = BlockSize * 8 - 1 // 32767
      val firstInBlock1 = BlockSize * 8 // 32768
      // totalBits=5000 so these are out of range — switch to a bigger map
      val dev = new RamBlockDevice(4)
      val big = new Bitmap(dev, 0, lengthBlocks = 2, totalBits = 40_000)
      big.set(lastInBlock0)
      big.set(firstInBlock1)
      big.isSet(lastInBlock0) shouldBe true
      big.isSet(firstInBlock1) shouldBe true
      big.isSet(lastInBlock0 - 1) shouldBe false
      big.isSet(firstInBlock1 + 1) shouldBe false
    }
  }

  "freeRange" - {

    "clears every bit in the range and updates freeCount" in {
      val (_, bm) = fresh()
      for b <- 100 to 199 do bm.set(b)
      bm.freeCount shouldBe 4900
      bm.freeRange(100, 100)
      bm.freeCount shouldBe 5000
      for b <- 100 to 199 do bm.isSet(b) shouldBe false
    }

    "rejects ranges that overflow totalBits" in {
      val (_, bm) = fresh()
      an[IllegalArgumentException] should be thrownBy bm.freeRange(4990, 11)
    }

    "rejects negative count" in {
      val (_, bm) = fresh()
      an[IllegalArgumentException] should be thrownBy bm.freeRange(0, -1)
    }
  }

  "allocate" - {

    "returns 0 on a fresh bitmap" in {
      val (_, bm) = fresh()
      bm.allocate() shouldBe Some(0)
      bm.isSet(0) shouldBe true
      bm.freeCount shouldBe 4999
    }

    "skips already-set bits" in {
      val (_, bm) = fresh()
      bm.set(0)
      bm.set(1)
      bm.set(2)
      bm.allocate() shouldBe Some(3)
    }

    "returns None when the bitmap is full" in {
      val (_, bm) = fresh(totalBits = 64)
      for b <- 0 until 64 do bm.set(b)
      bm.allocate() shouldBe None
    }

    "honors the hint (locality)" in {
      val (_, bm) = fresh()
      bm.allocate(hint = 100) shouldBe Some(100)
      bm.allocate(hint = 100) shouldBe Some(101)
    }

    "wraps to the start when nothing is free above the hint" in {
      val (_, bm) = fresh()
      // mark all bits >= 50 as set
      for b <- 50 until 5000 do bm.set(b)
      // hint past the high water; allocate should wrap and find bit 0
      bm.allocate(hint = 4000) shouldBe Some(0)
    }

    "rejects hint outside [0, totalBits]" in {
      val (_, bm) = fresh()
      an[IllegalArgumentException] should be thrownBy bm.allocate(-1)
      an[IllegalArgumentException] should be thrownBy bm.allocate(5001)
    }
  }

  "allocateRange" - {

    "returns 0 on a fresh bitmap and marks the run set" in {
      val (_, bm) = fresh()
      bm.allocateRange(8) shouldBe Some(0)
      for b <- 0 until 8 do bm.isSet(b) shouldBe true
      bm.freeCount shouldBe 4992
    }

    "skips a hole that is too small" in {
      val (_, bm) = fresh()
      // make a 5-bit free hole at [10..14] surrounded by set bits, then
      // require a run of 6 — must skip past
      for b <- 0 until 10 do bm.set(b)
      bm.set(15)
      bm.set(16)
      // [17..] is free for a long way
      bm.allocateRange(6) shouldBe Some(17)
    }

    "returns None when no run of the requested length exists" in {
      val (_, bm) = fresh(totalBits = 100)
      // alternate set/clear so the longest free run is 1
      for b <- 0 until 100 by 2 do bm.set(b)
      bm.allocateRange(2) shouldBe None
    }

    "spans byte boundaries" in {
      val (_, bm) = fresh()
      bm.allocateRange(20) shouldBe Some(0)
      for b <- 0 until 20 do bm.isSet(b) shouldBe true
      bm.isSet(20) shouldBe false
    }

    "rejects n < 1" in {
      val (_, bm) = fresh()
      an[IllegalArgumentException] should be thrownBy bm.allocateRange(0)
      an[IllegalArgumentException] should be thrownBy bm.allocateRange(-3)
    }
  }

  "flush + load" - {

    "round-trips state through the device" in {
      val (dev, bm) = fresh()
      // make a non-trivial pattern
      for b <- Seq(0, 1, 2, 100, 3000, 4999) do bm.set(b)
      bm.freeCount shouldBe 5000 - 6
      bm.flush()

      // build a fresh bitmap over the same region and load
      val bm2 = new Bitmap(dev, 0, lengthBlocks = 2, totalBits = 5000)
      bm2.load()
      bm2.freeCount shouldBe 5000 - 6
      for b <- Seq(0, 1, 2, 100, 3000, 4999) do bm2.isSet(b) shouldBe true
      bm2.isSet(3) shouldBe false
      bm2.isSet(2999) shouldBe false
    }

    "only writes dirty blocks" in {
      val (dev, bm) = fresh()
      // touch a bit only in block 0 (bit < BlockSize*8 == 32768)
      bm.set(7)
      bm.flush()
      // write a sentinel directly to the device's block 1
      val sentinel = Array.fill[Byte](BlockSize)(0x5a.toByte)
      dev.writeBlock(1, sentinel)
      // a second flush must not touch block 1 again
      bm.set(8)
      bm.flush()
      val readBack = new Array[Byte](BlockSize)
      dev.readBlock(1, readBack)
      readBack shouldBe sentinel
    }

    "load recomputes freeCount from disk state" in {
      val (dev, _) = fresh()
      // hand-craft a device state: bits 0,1,2 set in block 0
      val buf = new Array[Byte](BlockSize)
      buf(0) = 0x07.toByte // bits 0, 1, 2
      dev.writeBlock(0, buf)
      val bm = new Bitmap(dev, 0, lengthBlocks = 2, totalBits = 5000)
      bm.load()
      bm.freeCount shouldBe 5000 - 3
      bm.isSet(0) shouldBe true
      bm.isSet(1) shouldBe true
      bm.isSet(2) shouldBe true
      bm.isSet(3) shouldBe false
    }

    "load ignores bits past totalBits when recomputing freeCount" in {
      val (dev, _) = fresh()
      // set every bit of block 0; bits 5000..32767 are out of range and
      // should not be counted against freeCount
      val full = Array.fill[Byte](BlockSize)(0xff.toByte)
      dev.writeBlock(0, full)
      dev.writeBlock(1, full)
      val bm = new Bitmap(dev, 0, lengthBlocks = 2, totalBits = 5000)
      bm.load()
      bm.freeCount shouldBe 0
    }
  }

  "brute-force oracle" - {

    "agrees with a Set[Int] across thousands of random ops" in {
      val (_, bm) = fresh(totalBits = 1000)
      val oracle = scala.collection.mutable.Set.empty[Int]
      val rng = new scala.util.Random(0xc0ffeeL)

      def check(): Unit =
        bm.freeCount shouldBe (1000 - oracle.size)
        for b <- 0 until 1000 do bm.isSet(b) shouldBe oracle.contains(b)

      for _ <- 0 until 5000 do
        rng.nextInt(4) match
          case 0 =>
            val b = rng.nextInt(1000)
            oracle += b; bm.set(b)
          case 1 =>
            val b = rng.nextInt(1000)
            oracle -= b; bm.clear(b)
          case 2 =>
            bm.allocate() match
              case Some(b) =>
                oracle should not contain b
                oracle += b
              case None =>
                oracle.size shouldBe 1000
          case 3 =>
            val n = rng.nextInt(8) + 1
            bm.allocateRange(n) match
              case Some(start) =>
                for i <- 0 until n do
                  oracle should not contain (start + i)
                  oracle += (start + i)
              case None => // ok, may not fit
      check()
    }
  }
