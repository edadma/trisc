package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class LayoutTests extends AnyFreeSpec with Matchers:

  "compute" - {

    "places regions in spec order with no gaps" in {
      val l = Layout.compute(totalBlocks = 8192, totalInodes = 1024, journalBlocks = 64)
      l.blockBitmapStart shouldBe 2
      l.inodeBitmapStart shouldBe (l.blockBitmapStart + l.blockBitmapLen)
      l.inodeTableStart shouldBe (l.inodeBitmapStart + l.inodeBitmapLen)
      l.journalStart shouldBe (l.inodeTableStart + l.inodeTableLen)
      l.dataStart shouldBe (l.journalStart + l.journalLen)
    }

    "block_bitmap_len = ceil(totalBlocks / (8 * BlockSize))" in {
      Layout.compute(8192, 1024, 16).blockBitmapLen shouldBe 1
      Layout.compute(8 * BlockSize, 1024, 16).blockBitmapLen shouldBe 1
      Layout.compute(8 * BlockSize + 1, 1024, 16).blockBitmapLen shouldBe 2
    }

    "inode_bitmap_len = ceil(totalInodes / (8 * BlockSize))" in {
      Layout.compute(8192, 8 * BlockSize, 16).inodeBitmapLen shouldBe 1
      Layout.compute(8192, 8 * BlockSize + 1, 16).inodeBitmapLen shouldBe 2
    }

    "inode_table_len = ceil(totalInodes / 16)" in {
      Layout.compute(8192, 16, 16).inodeTableLen shouldBe 1
      Layout.compute(8192, 17, 16).inodeTableLen shouldBe 2
      Layout.compute(8192, 1024, 16).inodeTableLen shouldBe 64
    }

    "rejects totalBlocks <= 0" in {
      an[IllegalArgumentException] should be thrownBy Layout.compute(0, 16, 16)
      an[IllegalArgumentException] should be thrownBy Layout.compute(-1, 16, 16)
    }

    "rejects totalInodes < 3" in {
      an[IllegalArgumentException] should be thrownBy Layout.compute(8192, 2, 16)
    }

    "rejects journalBlocks < 1" in {
      an[IllegalArgumentException] should be thrownBy Layout.compute(8192, 16, 0)
    }

    "rejects a device too small to hold metadata" in {
      // 2 MiB device, 1 Mi inodes, default journal: vastly too big.
      an[IllegalArgumentException] should be thrownBy
        Layout.compute(512, DefaultInodeCount, DefaultJournalBlocks)
    }
  }

  "inodeLocation" - {

    "maps inode N to (table_start + N/16, (N%16) * 256)" in {
      val l = Layout.compute(8192, 1024, 16)
      l.inodeLocation(0) shouldBe (l.inodeTableStart.toLong, 0)
      l.inodeLocation(1) shouldBe (l.inodeTableStart.toLong, 256)
      l.inodeLocation(15) shouldBe (l.inodeTableStart.toLong, 15 * 256)
      l.inodeLocation(16) shouldBe ((l.inodeTableStart + 1).toLong, 0)
      l.inodeLocation(17) shouldBe ((l.inodeTableStart + 1).toLong, 256)
    }

    "rejects out-of-range inode numbers" in {
      val l = Layout.compute(8192, 1024, 16)
      an[IllegalArgumentException] should be thrownBy l.inodeLocation(-1)
      an[IllegalArgumentException] should be thrownBy l.inodeLocation(1024)
    }
  }
