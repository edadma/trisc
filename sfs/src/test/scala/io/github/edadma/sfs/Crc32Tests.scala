package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** CRC-32 (IEEE 802.3, reflected polynomial 0xEDB88320) — same flavor used
  * by zlib, ext4, gzip, PNG, etc. Test vectors below are the standard
  * published values, so any future regression in the algorithm shows up
  * immediately. */
class Crc32Tests extends AnyFreeSpec with Matchers:

  "known vectors" - {

    "empty input has CRC zero" in {
      Crc32.compute(Array.empty[Byte]) shouldBe 0
    }

    "\"123456789\" → 0xCBF43926 (the canonical CRC-32 vector)" in {
      val s = "123456789".getBytes("US-ASCII")
      Crc32.compute(s) shouldBe 0xcbf43926
    }

    "single zero byte → 0xD202EF8D" in {
      Crc32.compute(Array[Byte](0)) shouldBe 0xd202ef8d
    }

    "differs across single-bit changes" in {
      // sanity: tiny input perturbations produce different CRCs
      val a = Array[Byte](0x00, 0x00, 0x00, 0x00)
      val b = Array[Byte](0x00, 0x00, 0x00, 0x01)
      Crc32.compute(a) should not equal Crc32.compute(b)
    }
  }

  "compute(buf, off, len)" - {

    "matches compute(buf) when off=0, len=buf.length" in {
      val buf = (0 until 100).map(i => (i * 37 + 13).toByte).toArray
      Crc32.compute(buf, 0, buf.length) shouldBe Crc32.compute(buf)
    }

    "ignores bytes outside the slice" in {
      val core = "123456789".getBytes("US-ASCII")
      val padded = Array.fill[Byte](4)(0xaa.toByte) ++ core ++ Array.fill[Byte](4)(0x55.toByte)
      Crc32.compute(padded, 4, core.length) shouldBe 0xcbf43926
    }
  }

  "incremental update / finish" - {

    "single chunk equals one-shot compute" in {
      val s = "123456789".getBytes("US-ASCII")
      val c = Crc32.update(Crc32.start, s, 0, s.length)
      Crc32.finish(c) shouldBe 0xcbf43926
    }

    "multiple chunks equal the concatenation" in {
      val a = "12345".getBytes("US-ASCII")
      val b = "6789".getBytes("US-ASCII")
      var c = Crc32.start
      c = Crc32.update(c, a, 0, a.length)
      c = Crc32.update(c, b, 0, b.length)
      Crc32.finish(c) shouldBe 0xcbf43926
    }

    "empty chunk is a no-op" in {
      val s = "123456789".getBytes("US-ASCII")
      var c = Crc32.start
      c = Crc32.update(c, Array.empty[Byte], 0, 0)
      c = Crc32.update(c, s, 0, s.length)
      c = Crc32.update(c, Array.empty[Byte], 0, 0)
      Crc32.finish(c) shouldBe 0xcbf43926
    }
  }
