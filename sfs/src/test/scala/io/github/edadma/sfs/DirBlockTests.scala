package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class DirBlockTests extends AnyFreeSpec with Matchers:

  "DirEntry — variable-length record" - {

    "computes minRecLen correctly" in {
      DirEntry.minRecLen(1) shouldBe 12 // 8 + 1 + 3 padding
      DirEntry.minRecLen(2) shouldBe 12 // 8 + 2 + 2 padding
      DirEntry.minRecLen(3) shouldBe 12 // 8 + 3 + 1 padding
      DirEntry.minRecLen(4) shouldBe 12 // 8 + 4 + 0
      DirEntry.minRecLen(5) shouldBe 16 // 8 + 5 + 3 padding
      DirEntry.minRecLen(255) shouldBe 264 // 8 + 255 + 1 padding
    }

    "round-trips a regular-file entry" in {
      val e = DirEntry(inode = 42, fileType = DirEntry.TypeRegular, name = "hello.txt")
      val buf = new Array[Byte](e.recLen)
      DirEntry.pack(e, buf, 0)
      DirEntry.unpack(buf, 0) shouldBe e
    }

    "writes name bytes at offset 8 without NUL terminator" in {
      val e = DirEntry(inode = 1, fileType = 1, name = "abc")
      val buf = new Array[Byte](e.recLen)
      DirEntry.pack(e, buf, 0)
      buf(8) shouldBe 'a'.toByte
      buf(9) shouldBe 'b'.toByte
      buf(10) shouldBe 'c'.toByte
      buf(11) shouldBe 0.toByte // padding (not a NUL terminator semantically)
    }

    "honors explicit recLen larger than minimum (tombstone donation)" in {
      val e = DirEntry(inode = 5, fileType = 1, name = "x", recLen = 32)
      val buf = new Array[Byte](32)
      DirEntry.pack(e, buf, 0)
      val got = DirEntry.unpack(buf, 0)
      got.recLen shouldBe 32
      got.name shouldBe "x"
    }

    "rejects names longer than NameMax bytes" in {
      an[IllegalArgumentException] should be thrownBy DirEntry(1, 1, "x" * (NameMax + 1))
    }

    "rejects misaligned recLen" in {
      an[IllegalArgumentException] should be thrownBy DirEntry(1, 1, "x", recLen = 13)
    }

    "rejects recLen smaller than the minimum" in {
      an[IllegalArgumentException] should be thrownBy DirEntry(1, 1, "x", recLen = 8)
    }
  }

  "DirTail — magic + inode + CRC at end of block" - {

    "stamps then verifies" in {
      val buf = Array.fill[Byte](BlockSize)(0xa5.toByte)
      DirTail.pack(buf, ownerInode = 7)
      DirTail.verify(buf, 7) // no exception
    }

    "rejects wrong magic" in {
      val buf = new Array[Byte](BlockSize)
      DirTail.pack(buf, 7)
      Le.putU32(buf, BlockSize - 12, 0)
      a[SfsCorruptError] should be thrownBy DirTail.verify(buf, 7)
    }

    "rejects wrong owning inode" in {
      val buf = new Array[Byte](BlockSize)
      DirTail.pack(buf, 7)
      a[SfsCorruptError] should be thrownBy DirTail.verify(buf, 8)
    }

    "rejects mid-block tampering" in {
      val buf = new Array[Byte](BlockSize)
      DirTail.pack(buf, 7)
      buf(2000) = (buf(2000) ^ 0xff).toByte
      a[SfsCorruptError] should be thrownBy DirTail.verify(buf, 7)
    }
  }

  "DirLeafBlock — entries packed end-to-end with tail" - {

    /** Build a sequence of entries whose recLens sum to UsableSize by
      * extending the last entry's recLen to absorb the remainder. */
    def fillEntries(es: Seq[DirEntry]): Seq[DirEntry] =
      val used = es.dropRight(1).foldLeft(0)(_ + _.recLen)
      val last = es.last
      val absorbed = DirLeafBlock.UsableSize - used
      es.dropRight(1) :+ last.copy(recLen = absorbed)

    "round-trips a small leaf with the trailing-tombstone trick" in {
      val raw = Seq(
        DirEntry(10, DirEntry.TypeRegular, "alpha"),
        DirEntry(11, DirEntry.TypeRegular, "beta"),
        DirEntry(12, DirEntry.TypeRegular, "gamma"),
      )
      val es = fillEntries(raw)
      val buf = new Array[Byte](BlockSize)
      DirLeafBlock.pack(es, ownerInode = 99, buf)
      val got = DirLeafBlock.unpack(buf, 99)
      got.length shouldBe 3
      got(0).name shouldBe "alpha"
      got(1).name shouldBe "beta"
      got(2).name shouldBe "gamma"
      got.last.recLen + got.dropRight(1).foldLeft(0)(_ + _.recLen) shouldBe DirLeafBlock.UsableSize
    }

    "rejects entries that don't sum to UsableSize" in {
      val es = Seq(DirEntry(1, 1, "a"))
      val buf = new Array[Byte](BlockSize)
      an[IllegalArgumentException] should be thrownBy DirLeafBlock.pack(es, 1, buf)
    }

    "rejects on tail tamper" in {
      val es = fillEntries(Seq(DirEntry(1, 1, "a")))
      val buf = new Array[Byte](BlockSize)
      DirLeafBlock.pack(es, 1, buf)
      Le.putU32(buf, BlockSize - 4, 0)
      a[SfsCorruptError] should be thrownBy DirLeafBlock.unpack(buf, 1)
    }
  }

  "DirIndexBlock — interior HTree node" - {

    "Capacity is 510" in {
      DirIndexBlock.Capacity shouldBe 510
    }

    "round-trips a few entries" in {
      val entries = Seq((0x10000000, 5), (0x40000000, 9), (0x80000000, 13))
      val buf = new Array[Byte](BlockSize)
      DirIndexBlock.pack(entries, ownerInode = 4, buf)
      val got = DirIndexBlock.unpack(buf, 4)
      got.take(3) shouldBe entries
      got.drop(3).forall(_ == ((0, 0))) shouldBe true
    }

    "rejects on tail tamper" in {
      val buf = new Array[Byte](BlockSize)
      DirIndexBlock.pack(Seq((1, 2)), 4, buf)
      buf(8) = (buf(8) ^ 0xff).toByte
      a[SfsCorruptError] should be thrownBy DirIndexBlock.unpack(buf, 4)
    }
  }

  "DirRootBlock — directory file's block 0" - {

    "InfoLength is 8 and IndexEntries start at offset 32" in {
      DirRootBlock.InfoLength shouldBe 8
      DirRootBlock.IndexEntriesOff shouldBe 32
    }

    "round-trips at tree_depth=0 with no index entries" in {
      val root = DirRootBlock(
        dot = DirEntry(2, DirEntry.TypeDirectory, "."),
        dotdot = DirEntry(2, DirEntry.TypeDirectory, ".."),
        hashVersion = HashFnv1a,
        treeDepth = 0,
        flags = 0,
        indexEntries = IndexedSeq.empty,
      )
      val buf = new Array[Byte](BlockSize)
      DirRootBlock.pack(root, ownerInode = 2, buf)
      val got = DirRootBlock.unpack(buf, 2)
      got.dot.name shouldBe "."
      got.dotdot.name shouldBe ".."
      got.hashVersion shouldBe HashFnv1a
      got.treeDepth shouldBe 0
      got.flags shouldBe 0
      got.indexEntries.length shouldBe DirRootBlock.MaxIndexEntries
      got.indexEntries.forall(_ == ((0, 0))) shouldBe true
    }

    "round-trips with several index entries at tree_depth=1" in {
      val entries = IndexedSeq((0x00000000, 1), (0x40000000, 2), (0x80000000, 3))
      val root = DirRootBlock(
        dot = DirEntry(2, DirEntry.TypeDirectory, "."),
        dotdot = DirEntry(2, DirEntry.TypeDirectory, ".."),
        hashVersion = HashFnv1a,
        treeDepth = 1,
        flags = 0,
        indexEntries = entries,
      )
      val buf = new Array[Byte](BlockSize)
      DirRootBlock.pack(root, ownerInode = 2, buf)
      val got = DirRootBlock.unpack(buf, 2)
      got.indexEntries.take(3) shouldBe entries
      got.treeDepth shouldBe 1
    }

    "rejects info_length != 8 on unpack" in {
      val root = DirRootBlock(
        dot = DirEntry(2, DirEntry.TypeDirectory, "."),
        dotdot = DirEntry(2, DirEntry.TypeDirectory, ".."),
        hashVersion = HashFnv1a,
        treeDepth = 0,
        flags = 0,
        indexEntries = IndexedSeq.empty,
      )
      val buf = new Array[Byte](BlockSize)
      DirRootBlock.pack(root, 2, buf)
      Le.putU8(buf, 29, 7) // tamper info_length
      DirTail.pack(buf, 2) // re-stamp tail so we get past tail check
      a[SfsCorruptError] should be thrownBy DirRootBlock.unpack(buf, 2)
    }
  }
