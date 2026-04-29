package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Codec-level tests for [[XattrBlock]]: round-trip, integrity checks
  * (magic, owner-inode, CRC), and bounds validation. */
class XattrBlockTests extends AnyFreeSpec with Matchers:

  private def buf: Array[Byte] = new Array[Byte](BlockSize)

  "pack then unpack" - {

    "round-trips a single attribute" in {
      val b = buf
      val e = XattrEntry("user.foo", "hello".getBytes("UTF-8"))
      XattrBlock.pack(Seq(e), ownerInode = 7, b)
      val out = XattrBlock.unpack(b, expectedInode = 7)
      out should have size 1
      out(0).name shouldBe "user.foo"
      new String(out(0).value, "UTF-8") shouldBe "hello"
    }

    "round-trips multiple attributes preserving order" in {
      val b = buf
      val es = Seq(
        XattrEntry("user.a", Array[Byte](1, 2, 3)),
        XattrEntry("user.b", Array[Byte]()),
        XattrEntry("security.selinux", "system_u:object_r:file_t".getBytes("UTF-8")),
      )
      XattrBlock.pack(es, ownerInode = 42, b)
      val out = XattrBlock.unpack(b, 42)
      out.map(_.name) shouldBe Vector("user.a", "user.b", "security.selinux")
      out(0).value shouldBe Array[Byte](1, 2, 3)
      out(1).value shouldBe Array.empty[Byte]
    }

    "round-trips an empty entry list" in {
      val b = buf
      XattrBlock.pack(Seq.empty, ownerInode = 5, b)
      XattrBlock.unpack(b, 5) shouldBe IndexedSeq.empty
    }

    "round-trips a single max-size attribute" in {
      val b = buf
      val name = "user.x"
      val maxValueLen = XattrBlock.UsableSize - XattrEntry.HeaderSize - name.length
      val value = Array.tabulate(maxValueLen)(_.toByte)
      XattrBlock.pack(Seq(XattrEntry(name, value)), 1, b)
      val out = XattrBlock.unpack(b, 1)
      out should have size 1
      out(0).value.length shouldBe maxValueLen
      out(0).value shouldBe value
    }
  }

  "integrity checks" - {

    "detects magic mismatch" in {
      val b = buf
      XattrBlock.pack(Seq(XattrEntry("user.x", Array[Byte](0))), 1, b)
      Le.putU32(b, XattrBlock.MagicOff, 0xdeadbeef)
      // Recompute CRC so the only failure is magic.
      Le.putU32(b, XattrBlock.CrcOff, 0)
      Le.putU32(b, XattrBlock.CrcOff, Crc32.compute(b, 0, BlockSize - 4))
      a[SfsCorruptError] should be thrownBy XattrBlock.unpack(b, 1)
    }

    "detects owner-inode mismatch (block-swap protection)" in {
      val b = buf
      XattrBlock.pack(Seq(XattrEntry("user.x", Array[Byte](0))), ownerInode = 7, b)
      // Same valid block; just unpack with a different expected owner.
      a[SfsCorruptError] should be thrownBy XattrBlock.unpack(b, 8)
    }

    "detects CRC mismatch" in {
      val b = buf
      XattrBlock.pack(Seq(XattrEntry("user.x", "value".getBytes("UTF-8"))), 1, b)
      // Flip a byte in the entry payload after CRC was set.
      b(XattrBlock.HeaderSize + 4) = (b(XattrBlock.HeaderSize + 4) ^ 0xff).toByte
      a[SfsCorruptError] should be thrownBy XattrBlock.unpack(b, 1)
    }
  }

  "bounds validation" - {

    "refuses to pack entries whose combined size exceeds usable size" in {
      val b = buf
      // Two entries each of half-the-block + 100 bytes of value;
      // individually fine, together they overflow.
      val name = "x"
      val each = XattrBlock.UsableSize / 2 - XattrEntry.HeaderSize - name.length + 100
      val e1 = XattrEntry(name, new Array[Byte](each))
      val e2 = XattrEntry("y", new Array[Byte](each))
      an[IllegalArgumentException] should be thrownBy XattrBlock.pack(Seq(e1, e2), 1, b)
    }

    "single-entry value cap (MaxValueLen) is enforced at construction time" in {
      // Per-entry validator on XattrEntry rejects values > MaxValueLen
      // without ever reaching pack().
      an[IllegalArgumentException] should be thrownBy
        XattrEntry("x", new Array[Byte](XattrBlock.MaxValueLen + 1))
    }

    "refuses zero-length names" in {
      an[IllegalArgumentException] should be thrownBy XattrEntry("", Array[Byte](0))
    }

    "refuses names longer than 255 bytes" in {
      val long = "a" * 256
      an[IllegalArgumentException] should be thrownBy XattrEntry(long, Array[Byte](0))
    }

    "accepts UTF-8 names with multi-byte characters" in {
      val b = buf
      val e = XattrEntry("user.résumé", "café".getBytes("UTF-8"))
      XattrBlock.pack(Seq(e), 1, b)
      val out = XattrBlock.unpack(b, 1)
      out(0).name shouldBe "user.résumé"
      new String(out(0).value, "UTF-8") shouldBe "café"
    }
  }
