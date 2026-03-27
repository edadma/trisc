package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MemoryTests extends AnyFreeSpec with Matchers {

  "RAM read/write byte" in {
    val ram = new RAM(0, 256)
    ram.writeByte(0, 0x42)
    ram.readByte(0) shouldBe 0x42
  }

  "RAM read/write at different addresses" in {
    val ram = new RAM(0, 256)
    ram.writeByte(0, 0x11)
    ram.writeByte(100, 0x22)
    ram.writeByte(255, 0x33)
    ram.readByte(0) shouldBe 0x11
    ram.readByteUnsigned(100) shouldBe 0x22
    ram.readByteUnsigned(255) shouldBe 0x33
  }

  "RAM with nonzero base" in {
    val ram = new RAM(0x1000, 256)
    ram.writeByte(0x1000, 0x42)
    ram.readByte(0x1000) shouldBe 0x42
  }

  "ROM is not writable" in {
    val rom = new ROM(0, 256)
    rom.loadByte(0, 0x42)
    rom.readByte(0) shouldBe 0x42
    an[Exception] should be thrownBy {
      rom.writeByte(0, 0x43)
    }
  }

  "ROM loadByte works" in {
    val rom = new ROM(0, 256)
    rom.loadByte(0, 0xAB.toByte)
    rom.readByte(0) shouldBe 0xAB.toByte
  }

  "Memory rejects overlapping blocks" in {
    an[Exception] should be thrownBy {
      new Memory("test", new RAM(0, 256), new RAM(128, 256))
    }
  }

  "Memory routes to correct block" in {
    val ram1 = new RAM(0, 256)
    val ram2 = new RAM(0x1000, 256)
    val mem = new Memory("test", ram1, ram2)
    mem.writeByte(0, 0x11)
    mem.writeByte(0x1000, 0x22)
    mem.readByte(0) shouldBe 0x11
    mem.readByteUnsigned(0x1000) shouldBe 0x22
  }

  "Memory rejects unmapped address" in {
    val mem = new Memory("test", new RAM(0, 256))
    an[Exception] should be thrownBy {
      mem.readByte(0x1000)
    }
  }

  "Addressable short read/write" in {
    val ram = new RAM(0, 256)
    ram.writeShort(0, 0x1234)
    ram.readShortUnsigned(0) shouldBe 0x1234
    ram.readShort(0) shouldBe 0x1234
  }

  "Addressable int read/write" in {
    val ram = new RAM(0, 256)
    ram.writeInt(0, 0x12345678)
    ram.readInt(0) shouldBe 0x12345678
  }

  "Addressable long read/write" in {
    val ram = new RAM(0, 256)
    ram.writeLong(0, 0x123456789ABCDEF0L)
    ram.readLong(0) shouldBe 0x123456789ABCDEF0L
  }

  "Addressable big-endian byte order" in {
    val ram = new RAM(0, 256)
    ram.writeShort(0, 0x1234)
    ram.readByteUnsigned(0) shouldBe 0x12
    ram.readByteUnsigned(1) shouldBe 0x34
  }
}
