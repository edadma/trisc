package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

/** Sanity checks that the spec's derived numbers actually hold. If anyone
  * tweaks BlockSize or InodeSize, these will catch the fallout. */
class ConstantsTests extends AnyFreeSpec with Matchers:

  "block geometry" - {

    "block size is 4 KiB" in {
      BlockSize shouldBe 4096
    }

    "total volume is 2 TB" in {
      TotalBlocks * BlockSize.toLong shouldBe (2L * 1024 * 1024 * 1024 * 1024)
    }

    "extents per indirect = BlockSize / ExtentSize" in {
      ExtentsPerIndirect shouldBe (BlockSize / ExtentSize)
      ExtentsPerIndirect shouldBe 512
    }

    "block-pointers per indirect = BlockSize / 4" in {
      PtrsPerIndirect2 shouldBe (BlockSize / 4)
      PtrsPerIndirect2 shouldBe 1024
    }

    "triple-indirect addresses every block on disk under max fragmentation" in {
      // 1024 * 1024 * 512 = 2^29, exactly equal to TotalBlocks — so even with
      // every extent covering only one 4 KiB block, file size is disk-bound.
      val tripleExtents =
        PtrsPerIndirect2.toLong * PtrsPerIndirect2 * ExtentsPerIndirect
      tripleExtents shouldBe TotalBlocks
    }
  }

  "magic numbers decode to expected ASCII" in {
    def packAscii(a: Int, b: Int, c: Int, d: Int): Int =
      (a << 24) | (b << 16) | (c << 8) | d

    val S = 0x53
    val F = 0x46
    val NUL = 0x00

    MagicSuperblock shouldBe packAscii(S, F, S, NUL)
    MagicJournalSuperblock shouldBe packAscii(S, F, S, 'J'.toInt)
    MagicTxnDescriptor shouldBe packAscii(S, F, S, 'T'.toInt)
    MagicCommit shouldBe packAscii(S, F, S, 'C'.toInt)
    MagicDirTail shouldBe packAscii(S, F, S, 'D'.toInt)
  }

  "extent flags" - {

    "occupy the top bits and don't overlap the start_block field" in {
      (ExtentFlagUninitialized & ExtentStartMask) shouldBe 0
      (ExtentFlagSparse & ExtentStartMask) shouldBe 0
      (ExtentFlagUninitialized & ExtentFlagSparse) shouldBe 0
    }

    "ExtentStartMask covers exactly 29 bits" in {
      ExtentStartMask shouldBe ((1 << 29) - 1)
    }
  }

  "default journal size is 128 MiB" in {
    DefaultJournalBlocks.toLong * BlockSize shouldBe (128L * 1024 * 1024)
  }
