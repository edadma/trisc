package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** FNV-1a 32-bit test vectors. The known-answer pairs come from the
  * reference C implementation distributed with the FNV draft. They are
  * the canonical sanity check that the multiplication is being done
  * mod 2^32 with the right offset and prime. */
class Fnv1aTests extends AnyFreeSpec with Matchers:

  "constants" - {

    "offset basis is 2166136261" in {
      Fnv1a.Offset shouldBe 0x811c9dc5
    }

    "prime is 16777619" in {
      Fnv1a.Prime shouldBe 0x01000193
    }
  }

  "known vectors" - {

    "empty string returns the offset basis" in {
      Fnv1a.hash("") shouldBe Fnv1a.Offset
      Fnv1a.hash(Array.empty[Byte]) shouldBe Fnv1a.Offset
    }

    "\"a\" → 0xe40c292c" in {
      Fnv1a.hash("a") shouldBe 0xe40c292c
    }

    "\"foobar\" → 0xbf9cf968" in {
      Fnv1a.hash("foobar") shouldBe 0xbf9cf968
    }

    "\"hello\" → 0x4f9f2cab" in {
      Fnv1a.hash("hello") shouldBe 0x4f9f2cab
    }
  }

  "slice form" - {

    "matches whole-array form when off=0, len=length" in {
      val buf = "the quick brown fox".getBytes("UTF-8")
      Fnv1a.hash(buf, 0, buf.length) shouldBe Fnv1a.hash(buf)
    }

    "ignores bytes outside the slice" in {
      val core = "foobar".getBytes("UTF-8")
      val padded = Array.fill[Byte](3)(0xaa.toByte) ++ core ++ Array.fill[Byte](3)(0x55.toByte)
      Fnv1a.hash(padded, 3, core.length) shouldBe 0xbf9cf968
    }
  }

  "distribution sanity" - {

    "1000 unique short names produce mostly-unique hashes" in {
      val names = (0 until 1000).map(i => f"file_$i%04d")
      val hashes = names.map(Fnv1a.hash).toSet
      // FNV-1a isn't cryptographic, but for 1000 short distinct strings we
      // expect well over 99% unique 32-bit hashes.
      hashes.size should be >= 990
    }

    "single-byte changes flip many bits" in {
      val a = Fnv1a.hash("namea")
      val b = Fnv1a.hash("nameb")
      // Hamming distance > 8 is essentially always true for this hash;
      // <= 8 would indicate the multiplication is broken.
      java.lang.Integer.bitCount(a ^ b) should be > 8
    }
  }
