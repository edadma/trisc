package io.github.edadma.sfs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LeTests extends AnyFreeSpec with Matchers:

  "u8" - {

    "reads bytes as unsigned" in {
      val buf = Array(0x00, 0x7f, 0x80, 0xff).map(_.toByte)
      Le.u8(buf, 0) shouldBe 0x00
      Le.u8(buf, 1) shouldBe 0x7f
      Le.u8(buf, 2) shouldBe 0x80
      Le.u8(buf, 3) shouldBe 0xff
    }
  }

  "u16" - {

    "reads little-endian" in {
      val buf = Array(0x34, 0x12, 0xff, 0xff).map(_.toByte)
      Le.u16(buf, 0) shouldBe 0x1234
      Le.u16(buf, 2) shouldBe 0xffff
    }
  }

  "u32" - {

    "reads little-endian" in {
      val buf = Array(0x78, 0x56, 0x34, 0x12).map(_.toByte)
      Le.u32(buf, 0) shouldBe 0x12345678
    }

    "u32AsLong widens without sign extension" in {
      val buf = Array(0xff, 0xff, 0xff, 0xff).map(_.toByte)
      Le.u32(buf, 0) shouldBe -1
      Le.u32AsLong(buf, 0) shouldBe 0xffffffffL
    }
  }

  "u64" - {

    "reads little-endian" in {
      val buf = Array(0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef).map(_.toByte)
      Le.u64(buf, 0) shouldBe 0xefcdab8967452301L
    }
  }

  "round-trip" - {

    "u8/u16/u32/u64 each round-trip every interesting value" in {
      val buf = new Array[Byte](32)

      for v <- Seq(0, 1, 0x7f, 0x80, 0xff) do
        Le.putU8(buf, 0, v)
        Le.u8(buf, 0) shouldBe v

      for v <- Seq(0, 1, 0x7fff, 0x8000, 0xffff) do
        Le.putU16(buf, 0, v)
        Le.u16(buf, 0) shouldBe v

      for v <- Seq(0, 1, 0x7fffffff, 0x80000000, 0xffffffff) do
        Le.putU32(buf, 0, v)
        Le.u32(buf, 0) shouldBe v

      for v <- Seq(0L, 1L, Long.MaxValue, Long.MinValue, -1L) do
        Le.putU64(buf, 0, v)
        Le.u64(buf, 0) shouldBe v
    }

    "writes do not bleed past their declared size" in {
      val buf = Array.fill[Byte](8)(0xaa.toByte)
      Le.putU16(buf, 2, 0x1234)
      buf(0) shouldBe 0xaa.toByte
      buf(1) shouldBe 0xaa.toByte
      buf(2) shouldBe 0x34.toByte
      buf(3) shouldBe 0x12.toByte
      buf(4) shouldBe 0xaa.toByte
    }
  }

  "bytes / putBytes / zero" - {

    "bytes returns a copy" in {
      val buf = Array[Byte](1, 2, 3, 4, 5)
      val out = Le.bytes(buf, 1, 3)
      out shouldBe Array[Byte](2, 3, 4)
      out(0) = 99
      buf(1) shouldBe 2.toByte
    }

    "putBytes copies into the buffer at the given offset" in {
      val buf = new Array[Byte](8)
      Le.putBytes(buf, 2, Array[Byte](0x11, 0x22, 0x33))
      buf shouldBe Array[Byte](0, 0, 0x11, 0x22, 0x33, 0, 0, 0)
    }

    "zero clears the requested span only" in {
      val buf = Array.fill[Byte](8)(0xff.toByte)
      Le.zero(buf, 2, 4)
      buf shouldBe Array[Byte](0xff.toByte, 0xff.toByte, 0, 0, 0, 0, 0xff.toByte, 0xff.toByte)
    }
  }
