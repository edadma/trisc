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

  // New register offsets:
  //   +0  LBA      (4B)
  //   +4  ADDR     (4B)
  //   +8  CAPACITY (4B, R)
  //   +12 COUNT    (2B)
  //   +14 STATUS   (1B, R)
  //   +15 COMMAND  (1B, W — triggers operation)

  private val LBA = BASE
  private val ADDR = BASE + 4
  private val CAPACITY = BASE + 8
  private val COUNT = BASE + 12
  private val STATUS = BASE + 14
  private val COMMAND = BASE + 15

  // ===== Register reads =====

  "status is ready on init" in {
    val (rd, _, _) = mkRamdisk()
    rd.readByte(STATUS) shouldBe 0x01
  }

  "capacity registers return sector count" in {
    val (rd, _, _) = mkRamdisk(sectors = 2048)
    rd.readInt(CAPACITY) shouldBe 2048
  }

  "capacity registers for small disk" in {
    val (rd, _, _) = mkRamdisk(sectors = 1)
    rd.readInt(CAPACITY) shouldBe 1
  }

  "LBA registers read back written value" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeInt(LBA, 7)
    rd.readInt(LBA) shouldBe 7
  }

  "ADDR registers read back written value" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeInt(ADDR, 0x1000)
    rd.readInt(ADDR) shouldBe 0x1000
  }

  "COUNT registers read back written value" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeShort(COUNT, 3)
    rd.readShort(COUNT) shouldBe 3
  }

  // ===== DMA write then read =====

  "write sector then read it back" in {
    val (rd, ram, _) = mkRamdisk()

    // Write pattern into RAM at address 0x1000
    for i <- 0 until 512 do
      ram.writeByte(0x1000 + i, (i & 0xff).toByte)

    // Set LBA = 0, ADDR = 0x1000, COUNT = 1, COMMAND = write
    rd.writeInt(LBA, 0)
    rd.writeInt(ADDR, 0x1000)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x02)
    rd.readByte(STATUS) shouldBe 0x01 // ready

    // Clear RAM destination
    for i <- 0 until 512 do
      ram.writeByte(0x2000 + i, 0)

    // Read back to different address
    rd.writeInt(ADDR, 0x2000)
    rd.writeByte(COMMAND, 0x01)

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
    rd.writeInt(LBA, 2)
    rd.writeInt(ADDR, 0x1000)
    rd.writeShort(COUNT, 2)
    rd.writeByte(COMMAND, 0x02) // write

    // Read back to different location
    rd.writeInt(ADDR, 0x3000)
    rd.writeByte(COMMAND, 0x01) // read

    for i <- 0 until 1024 do
      ram.readByte(0x3000 + i).toByte shouldBe ((i * 3) & 0xff).toByte
  }

  // ===== Error conditions =====

  "error on LBA out of range" in {
    val (rd, _, _) = mkRamdisk(sectors = 4)
    rd.writeInt(LBA, 4)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x01)
    rd.readByte(STATUS) shouldBe 0x02
  }

  "error on LBA + count exceeding capacity" in {
    val (rd, _, _) = mkRamdisk(sectors = 4)
    rd.writeInt(LBA, 3)
    rd.writeShort(COUNT, 2) // 3 + 2 = 5 > 4
    rd.writeByte(COMMAND, 0x01)
    rd.readByte(STATUS) shouldBe 0x02
  }

  "error on zero count" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeInt(LBA, 0)
    rd.writeShort(COUNT, 0)
    rd.writeByte(COMMAND, 0x01)
    rd.readByte(STATUS) shouldBe 0x02
  }

  "error on unknown command" in {
    val (rd, _, _) = mkRamdisk()
    rd.writeInt(LBA, 0)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0xFF)
    rd.readByte(STATUS) shouldBe 0x02
  }

  "write to status clears error" in {
    val (rd, _, _) = mkRamdisk(sectors = 4)
    rd.writeInt(LBA, 99)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x01)
    rd.readByte(STATUS) shouldBe 0x02 // error
    rd.writeByte(STATUS, 0) // clear
    rd.readByte(STATUS) shouldBe 0x01 // ready
  }

  // ===== Interrupt =====

  "successful read raises IRQ" in {
    val (rd, ram, intc) = mkRamdisk()
    rd.writeInt(LBA, 0)
    rd.writeInt(ADDR, 0x1000)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x01)
    intc.readByte(0x20000) shouldBe (1 << 3)
  }

  "successful write raises IRQ" in {
    val (rd, ram, intc) = mkRamdisk()
    for i <- 0 until 512 do ram.writeByte(0x1000 + i, 0)
    rd.writeInt(LBA, 0)
    rd.writeInt(ADDR, 0x1000)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x02)
    intc.readByte(0x20000) shouldBe (1 << 3)
  }

  "error does not raise IRQ" in {
    val (rd, _, intc) = mkRamdisk(sectors = 4)
    rd.writeInt(LBA, 99)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x01)
    intc.readByte(0x20000) shouldBe 0
  }

  // ===== Prefill / TFS integration =====

  "prefill formats disk with TFS" in {
    val (rd, ram, _) = mkRamdisk(prefill = "/dev/tty0 char 0 0")

    // Read sector 1 (TFS superblock) into RAM
    rd.writeInt(LBA, 1)
    rd.writeInt(ADDR, 0)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x01)

    // Check TFS magic at RAM address 0
    val magic = (ram.readByteUnsigned(0) << 24) | (ram.readByteUnsigned(1) << 16) |
      (ram.readByteUnsigned(2) << 8) | ram.readByteUnsigned(3)
    magic shouldBe 0x54465300
  }

  "empty prefill creates blank disk" in {
    val (rd, ram, _) = mkRamdisk(prefill = "")

    rd.writeInt(LBA, 0)
    rd.writeInt(ADDR, 0)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x01)

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

    rd.writeInt(LBA, 0)
    rd.writeInt(ADDR, 0x1000)
    rd.writeShort(COUNT, 1)
    rd.writeByte(COMMAND, 0x02) // write

    // Read back
    rd.writeInt(ADDR, 0x2000)
    rd.writeByte(COMMAND, 0x01) // read

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
