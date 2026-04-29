package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Tests for the leaf-block ops layer. Operates on a single 4 KiB
  * `Array[Byte]` so we can exercise insert / lookup / delete logic
  * without constructing an inode or block device. */
class DirLeafTests extends AnyFreeSpec with Matchers:

  private val Owner = 17

  private def freshLeaf(): Array[Byte] =
    val buf = new Array[Byte](BlockSize)
    DirLeaf.initEmpty(buf, Owner)
    buf

  /** Total bytes accounted for by all records (tombstones + live). Should
    * always equal `DirLeaf.UsableSize` for a well-formed leaf. */
  private def recLenSum(buf: Array[Byte]): Int =
    DirLeaf.entries(buf).map(_._2.recLen).sum

  "initEmpty" - {

    "round-trips through entries() as one tombstone covering the whole region" in {
      val buf = freshLeaf()
      val xs = DirLeaf.entries(buf)
      xs.length shouldBe 1
      val (off, e) = xs.head
      off shouldBe 0
      e.inode shouldBe 0
      e.recLen shouldBe DirLeaf.UsableSize
    }

    "tail verifies against owner inode" in {
      val buf = freshLeaf()
      noException should be thrownBy DirTail.verify(buf, Owner)
    }
  }

  "tryInsert" - {

    "inserts a single entry and findByName resolves it" in {
      val buf = freshLeaf()
      val e = DirEntry(inode = 100, fileType = DirEntry.TypeRegular, name = "hello")
      DirLeaf.tryInsert(buf, e, Owner) shouldBe true
      val found = DirLeaf.findByName(buf, "hello")
      found shouldBe defined
      found.get._2.inode shouldBe 100
      recLenSum(buf) shouldBe DirLeaf.UsableSize
    }

    "inserts ten short names; entries() returns them in insertion order" in {
      val buf = freshLeaf()
      val names = (0 until 10).map(i => f"f$i%02d")
      var inode = 100
      for n <- names do
        DirLeaf.tryInsert(
          buf,
          DirEntry(inode, DirEntry.TypeRegular, n),
          Owner,
        ) shouldBe true
        inode += 1

      val live = DirLeaf.entries(buf).map(_._2).filter(_.inode != 0)
      live.map(_.name) shouldBe names
      recLenSum(buf) shouldBe DirLeaf.UsableSize
    }

    "findByName returns None for missing names" in {
      val buf = freshLeaf()
      DirLeaf.tryInsert(
        buf,
        DirEntry(100, DirEntry.TypeRegular, "alpha"),
        Owner,
      ) shouldBe true
      DirLeaf.findByName(buf, "beta") shouldBe None
    }

    "fills the block, then a clearly-too-large insert returns false" in {
      val buf = freshLeaf()
      // Pack until the residual tombstone is < minRecLen(NameMax) = 264.
      // Use 64-byte names → recLen 72.
      val baseName = "n" * 60
      var i = 0
      while
        DirLeaf.tryInsert(
          buf,
          DirEntry(100 + i, DirEntry.TypeRegular, baseName + f"$i%04d"),
          Owner,
        )
      do i += 1

      // Now try to insert a max-length name. Should fail since residual
      // tombstone is <= 71 bytes (< 264).
      val huge = "X" * NameMax
      DirLeaf.tryInsert(
        buf,
        DirEntry(99999, DirEntry.TypeRegular, huge),
        Owner,
      ) shouldBe false
      recLenSum(buf) shouldBe DirLeaf.UsableSize
    }
  }

  "delete" - {

    "tombstones an entry; lookup misses it but recLen sum is preserved" in {
      val buf = freshLeaf()
      DirLeaf.tryInsert(buf, DirEntry(100, DirEntry.TypeRegular, "alpha"), Owner) shouldBe true
      DirLeaf.tryInsert(buf, DirEntry(101, DirEntry.TypeRegular, "beta"), Owner) shouldBe true

      DirLeaf.delete(buf, "alpha", Owner) shouldBe true
      DirLeaf.findByName(buf, "alpha") shouldBe None
      DirLeaf.findByName(buf, "beta") shouldBe defined
      recLenSum(buf) shouldBe DirLeaf.UsableSize
    }

    "returns false when name absent" in {
      val buf = freshLeaf()
      DirLeaf.delete(buf, "nope", Owner) shouldBe false
    }

    "tombstone slack can be reused by a later insert with a longer name" in {
      val buf = freshLeaf()
      DirLeaf.tryInsert(buf, DirEntry(100, DirEntry.TypeRegular, "ab"), Owner)
      DirLeaf.tryInsert(buf, DirEntry(101, DirEntry.TypeRegular, "cd"), Owner)
      DirLeaf.delete(buf, "ab", Owner)

      // Insert a name that needs more bytes than "ab"'s minRecLen — it should
      // land in the trailing tombstone (which has lots of slack).
      val long = "x" * 30
      DirLeaf.tryInsert(buf, DirEntry(200, DirEntry.TypeRegular, long), Owner) shouldBe true
      DirLeaf.findByName(buf, long) shouldBe defined
      DirLeaf.findByName(buf, "cd") shouldBe defined
      recLenSum(buf) shouldBe DirLeaf.UsableSize
    }
  }

  "corruption" - {

    "entries() throws on a non-4-aligned recLen" in {
      val buf = freshLeaf()
      // Corrupt the only entry's recLen to 13 (not 4-aligned).
      Le.putU16(buf, 4, 13)
      a[SfsCorruptError] should be thrownBy DirLeaf.entries(buf)
    }

    "entries() throws on a recLen overrunning the usable region" in {
      val buf = freshLeaf()
      DirLeaf.tryInsert(buf, DirEntry(100, DirEntry.TypeRegular, "x"), Owner)
      // Inflate the first record's recLen past UsableSize.
      Le.putU16(buf, 4, DirLeaf.UsableSize + 4)
      a[SfsCorruptError] should be thrownBy DirLeaf.entries(buf)
    }
  }
