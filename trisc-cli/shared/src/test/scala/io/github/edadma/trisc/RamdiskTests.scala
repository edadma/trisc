package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RamdiskTests extends AnyFreeSpec with Matchers {

  private def mkRamdisk(
      sectors: Int = 64,
      sectorSize: Int = 512,
      prefill: String = "",
  ): (Ramdisk, RAM, InterruptController) =
    val ram = new RAM(0, 0x10000)
    val intc = new InterruptController(0x20000)
    val ramdisk = new Ramdisk(0x10000, ram, sectors, sectorSize, intc, irq = 3, prefill)
    (ramdisk, ram, intc)

  private val BASE = 0x10000L

  // ===== Register reads =====

  "status is ready on init" in {
    val (rd, _, _) = mkRamdisk()
    rd.readByte(BASE) shouldBe 0x01 // STATUS_READY
  }

  "capacity registers return sector count" in {
    val (rd, _, _) = mkRamdisk(sectors = 2048)
    val cap = (rd.readByte(BASE + 12) << 24) | (rd.readByte(BASE + 13) << 16) |
      (rd.readByte(BASE + 14) << 8) | rd.readByte(BASE + 15)
    cap shouldBe 2048
  }

  "capacity registers for small disk" in {
    val (rd, _, _) = mkRamdisk(sectors = 1)
    val cap = (rd.readByte(BASE + 12) << 24) | (rd.readByte(BASE + 13) << 16) |
      (rd.readByte(BASE + 14) << 8) | rd.readByte(BASE + 15)
    cap shouldBe 1
  }

  "LBA registers read back written value" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeByte(BASE + 2, 0x00)
    rd.writeByte(BASE + 3, 0x00)
    rd.writeByte(BASE + 4, 0x00)
    rd.writeByte(BASE + 5, 0x07)
    rd.readByte(BASE + 5) shouldBe 0x07
  }

  "ADDR registers read back written value" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeByte(BASE + 6, 0x00)
    rd.writeByte(BASE + 7, 0x00)
    rd.writeByte(BASE + 8, 0x10)
    rd.writeByte(BASE + 9, 0x00)
    val addr = (rd.readByte(BASE + 6) << 24) | (rd.readByte(BASE + 7) << 16) |
      (rd.readByte(BASE + 8) << 8) | rd.readByte(BASE + 9)
    addr shouldBe 0x1000
  }

  "COUNT registers read back written value" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeByte(BASE + 10, 0x00)
    rd.writeByte(BASE + 11, 0x03)
    val count = (rd.readByte(BASE + 10) << 8) | rd.readByte(BASE + 11)
    count shouldBe 3
  }

  // ===== DMA write then read =====

  "write sector then read it back" in {
    val (rd, ram, _) = mkRamdisk()

    // Write pattern into RAM at address 0x1000
    for i <- 0 until 512 do
      ram.writeByte(0x1000 + i, (i & 0xff).toByte)

    // Set LBA = 0, ADDR = 0x1000, COUNT = 1
    rd.writeByte(BASE + 2, 0)
    rd.writeByte(BASE + 3, 0)
    rd.writeByte(BASE + 4, 0)
    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 6, 0)
    rd.writeByte(BASE + 7, 0)
    rd.writeByte(BASE + 8, 0x10)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 10, 0)
    rd.writeByte(BASE + 11, 1)

    // Write command
    rd.writeByte(BASE + 1, 0x02)
    rd.readByte(BASE) shouldBe 0x01 // ready

    // Clear RAM destination
    for i <- 0 until 512 do
      ram.writeByte(0x2000 + i, 0)

    // Set ADDR = 0x2000, read command
    rd.writeByte(BASE + 6, 0)
    rd.writeByte(BASE + 7, 0)
    rd.writeByte(BASE + 8, 0x20)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 1, 0x01)

    // Verify data round-tripped
    for i <- 0 until 512 do
      ram.readByte(0x2000 + i).toByte shouldBe (i & 0xff).toByte
  }

  "write multiple sectors" in {
    val (rd, ram, _) = mkRamdisk()

    // Fill 2 sectors in RAM
    for i <- 0 until 1024 do
      ram.writeByte(0x1000 + i, ((i * 3) & 0xff).toByte)

    // LBA = 2, ADDR = 0x1000, COUNT = 2
    rd.writeByte(BASE + 5, 2)
    rd.writeByte(BASE + 8, 0x10)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 10, 0)
    rd.writeByte(BASE + 11, 2)
    rd.writeByte(BASE + 1, 0x02) // write

    // Read back to different location
    rd.writeByte(BASE + 8, 0x30)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 1, 0x01) // read

    for i <- 0 until 1024 do
      ram.readByte(0x3000 + i).toByte shouldBe ((i * 3) & 0xff).toByte
  }

  // ===== Error conditions =====

  "error on LBA out of range" in {
    val (rd, _, _) = mkRamdisk(sectors = 4)
    rd.writeByte(BASE + 5, 4) // LBA = 4, but only 4 sectors (0-3)
    rd.writeByte(BASE + 11, 1) // count = 1
    rd.writeByte(BASE + 1, 0x01) // read
    rd.readByte(BASE) shouldBe 0x02 // STATUS_ERROR
  }

  "error on LBA + count exceeding capacity" in {
    val (rd, _, _) = mkRamdisk(sectors = 4)
    rd.writeByte(BASE + 5, 3) // LBA = 3
    rd.writeByte(BASE + 11, 2) // count = 2, total = 5 > 4
    rd.writeByte(BASE + 1, 0x01)
    rd.readByte(BASE) shouldBe 0x02
  }

  "error on zero count" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 11, 0) // count = 0
    rd.writeByte(BASE + 1, 0x01)
    rd.readByte(BASE) shouldBe 0x02
  }

  "error on unknown command" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0xFF) // unknown command
    rd.readByte(BASE) shouldBe 0x02
  }

  "write to status clears error" in {
    val (rd, _, _) = mkRamdisk(sectors = 4)
    rd.writeByte(BASE + 5, 99) // bad LBA
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x01)
    rd.readByte(BASE) shouldBe 0x02 // error
    rd.writeByte(BASE, 0) // clear
    rd.readByte(BASE) shouldBe 0x01 // ready
  }

  // ===== Interrupt =====

  "successful read raises IRQ" in {
    val (rd, ram, intc) = mkRamdisk()
    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 8, 0x10)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x01)
    // Check IRQ 3 is pending
    intc.readByte(0x20000) shouldBe (1 << 3) // PENDING register
  }

  "successful write raises IRQ" in {
    val (rd, ram, intc) = mkRamdisk()
    for i <- 0 until 512 do ram.writeByte(0x1000 + i, 0)
    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 8, 0x10)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x02)
    intc.readByte(0x20000) shouldBe (1 << 3)
  }

  "error does not raise IRQ" in {
    val (rd, _, intc) = mkRamdisk(sectors = 4)
    rd.writeByte(BASE + 5, 99)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x01)
    intc.readByte(0x20000) shouldBe 0 // no pending IRQs
  }

  // ===== Prefill / NFS integration =====

  "prefill formats disk with TFS" in {
    val (rd, ram, _) = mkRamdisk(prefill = "/dev/tty0 char 0 0")

    // Read sector 1 (TFS superblock) into RAM
    rd.writeByte(BASE + 5, 1) // LBA = 1
    rd.writeByte(BASE + 8, 0x00)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x01)

    // Check TFS magic at RAM address 0
    val magic = (ram.readByteUnsigned(0) << 24) | (ram.readByteUnsigned(1) << 16) |
      (ram.readByteUnsigned(2) << 8) | ram.readByteUnsigned(3)
    magic shouldBe 0x54465300
  }

  "empty prefill creates blank disk" in {
    val (rd, ram, _) = mkRamdisk(prefill = "")

    // Read sector 0 into RAM
    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 8, 0x00)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x01)

    // Should be all zeros (no TFS)
    val magic = (ram.readByteUnsigned(0) << 24) | (ram.readByteUnsigned(1) << 16) |
      (ram.readByteUnsigned(2) << 8) | ram.readByteUnsigned(3)
    magic shouldBe 0
  }

  // ===== Custom sector size =====

  "works with non-default sector size" in {
    val (rd, ram, _) = mkRamdisk(sectorSize = 256)

    // Write 256-byte pattern
    for i <- 0 until 256 do
      ram.writeByte(0x1000 + i, (i & 0xff).toByte)

    rd.writeByte(BASE + 5, 0)
    rd.writeByte(BASE + 8, 0x10)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 11, 1)
    rd.writeByte(BASE + 1, 0x02) // write

    // Read back
    rd.writeByte(BASE + 8, 0x20)
    rd.writeByte(BASE + 9, 0x00)
    rd.writeByte(BASE + 1, 0x01) // read

    for i <- 0 until 256 do
      ram.readByte(0x2000 + i).toByte shouldBe (i & 0xff).toByte

    // Verify only 256 bytes transferred (not 512)
    ram.readByte(0x2100) shouldBe 0
  }

  // ===== Device metadata =====

  "has correct name and size" in {
    val (rd, _, _) = mkRamdisk()
    rd.name shouldBe "Ramdisk"
    rd.size shouldBe 16
  }
}
