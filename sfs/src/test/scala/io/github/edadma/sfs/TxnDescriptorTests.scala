package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import Constants.*

class TxnDescriptorTests extends AnyFreeSpec with Matchers:

  "MaxEntriesPerBlock is (BlockSize - 16) / 8 = 510" in {
    TxnDescriptor.MaxEntriesPerBlock shouldBe 510
  }

  "round-trips a small descriptor" in {
    val entries = IndexedSeq(
      TxnEntry(fsBlock = 100, flags = 0),
      TxnEntry(fsBlock = 101, flags = TxnEntry.FlagEscaped),
      TxnEntry(fsBlock = 102, flags = 0),
    )
    val d = TxnDescriptor(sequence = 7, blockCount = 3, flags = 0, entries = entries)
    val buf = new Array[Byte](BlockSize)
    TxnDescriptor.pack(d, buf, 0)
    TxnDescriptor.unpack(buf, 0, 3) shouldBe d
  }

  "writes magic 'SFST' at offset 0" in {
    val d = TxnDescriptor(sequence = 1, blockCount = 0, flags = 0, entries = IndexedSeq.empty)
    val buf = new Array[Byte](BlockSize)
    TxnDescriptor.pack(d, buf, 0)
    Le.u32(buf, 0) shouldBe MagicTxnDescriptor
  }

  "rejects bad magic" in {
    val d = TxnDescriptor(sequence = 1, blockCount = 0, flags = 0, entries = IndexedSeq.empty)
    val buf = new Array[Byte](BlockSize)
    TxnDescriptor.pack(d, buf, 0)
    Le.putU32(buf, 0, 0)
    a[SfsCorruptError] should be thrownBy TxnDescriptor.unpack(buf, 0, 0)
  }

  "fills exactly MaxEntriesPerBlock entries" in {
    val entries = (0 until TxnDescriptor.MaxEntriesPerBlock).map(i => TxnEntry(i + 1, 0))
    val d = TxnDescriptor(
      sequence = 99,
      blockCount = TxnDescriptor.MaxEntriesPerBlock,
      flags = 0,
      entries = entries.toIndexedSeq,
    )
    val buf = new Array[Byte](BlockSize)
    TxnDescriptor.pack(d, buf, 0)
    val got = TxnDescriptor.unpack(buf, 0, TxnDescriptor.MaxEntriesPerBlock)
    got.entries.length shouldBe TxnDescriptor.MaxEntriesPerBlock
    got.entries shouldBe entries
  }

  "rejects more than MaxEntriesPerBlock entries" in {
    val entries = (0 to TxnDescriptor.MaxEntriesPerBlock).map(i => TxnEntry(i, 0))
    an[IllegalArgumentException] should be thrownBy
      TxnDescriptor(sequence = 0, blockCount = entries.length, flags = 0, entries = entries.toIndexedSeq)
  }

  "zero-fills bytes after the last entry" in {
    val entries = IndexedSeq(TxnEntry(1, 0))
    val d = TxnDescriptor(sequence = 0, blockCount = 1, flags = 0, entries = entries)
    val buf = Array.fill[Byte](BlockSize)(0xa5.toByte)
    TxnDescriptor.pack(d, buf, 0)
    // first entry occupies bytes 16..23; everything from 24..end should be zero
    for i <- 24 until BlockSize do buf(i) shouldBe 0.toByte
  }
