package io.github.edadma.trisc

import io.github.edadma.cross_platform.createTempFile

class HardDiskTests extends TestHelpers {

  val SECTOR_SIZE = 512
  val BASE = 0x100L

  // New register offsets:
  //   +0  LBA      (4B)
  //   +4  ADDR     (4B)
  //   +8  CAPACITY (4B, R)
  //   +12 COUNT    (2B)
  //   +14 STATUS   (1B, R)
  //   +15 COMMAND  (1B, W)

  val LBA = BASE
  val ADDR = BASE + 4
  val CAPACITY = BASE + 8
  val COUNT = BASE + 12
  val STATUS = BASE + 14
  val COMMAND = BASE + 15

  def mkDisk(sectors: Int = 16): (HardDisk, InterruptController, Memory, String) =
    val intc = new InterruptController(0x200)
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val path = createTempFile("trisc-hd-", ".img")
    val raf = io.github.edadma.cross_platform.openRandomAccessFile(path, "rw")
    raf.setLength(sectors.toLong * SECTOR_SIZE)
    raf.close()
    val disk = new HardDisk(BASE, mem, path, SECTOR_SIZE, intc, irq = 4)
    (disk, intc, mem, path)

  // ===== Basic properties =====

  "HardDisk has correct size" in {
    val (disk, _, _, _) = mkDisk()
    disk.size shouldBe 16
  }

  "HardDisk status is ready initially" in {
    val (disk, _, _, _) = mkDisk()
    disk.readByte(STATUS) shouldBe 0x01
  }

  "HardDisk capacity reflects file size" in {
    val (disk, _, _, _) = mkDisk(32)
    disk.readInt(CAPACITY) shouldBe 32
  }

  // ===== Write then read =====

  "write and read back single sector" in {
    val (disk, _, mem, _) = mkDisk()
    for i <- 0 until SECTOR_SIZE do
      mem.writeByte(0x1000 + i, (i & 0xFF).toByte)
    disk.writeInt(LBA, 0)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x02) // write

    for i <- 0 until SECTOR_SIZE do
      mem.writeByte(0x1000 + i, 0)

    disk.writeInt(LBA, 0)
    disk.writeInt(ADDR, 0x2000)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x01) // read

    for i <- 0 until SECTOR_SIZE do
      (mem.readByte(0x2000 + i) & 0xFF) shouldBe (i & 0xFF)
  }

  "write and read multiple sectors" in {
    val (disk, _, mem, _) = mkDisk()
    for i <- 0 until SECTOR_SIZE * 2 do
      mem.writeByte(0x1000 + i, ((i * 3) & 0xFF).toByte)
    disk.writeInt(LBA, 2)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 2)
    disk.writeByte(COMMAND, 0x02)

    disk.writeInt(LBA, 2)
    disk.writeInt(ADDR, 0x4000)
    disk.writeShort(COUNT, 2)
    disk.writeByte(COMMAND, 0x01)

    for i <- 0 until SECTOR_SIZE * 2 do
      (mem.readByte(0x4000 + i) & 0xFF) shouldBe ((i * 3) & 0xFF)
  }

  // ===== Interrupt =====

  "read raises completion interrupt" in {
    val (disk, intc, _, _) = mkDisk()
    disk.writeInt(LBA, 0)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x01)
    (intc.readByte(0x200) & (1 << 4)) should not be 0
  }

  "write raises completion interrupt" in {
    val (disk, intc, _, _) = mkDisk()
    disk.writeInt(LBA, 0)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x02)
    (intc.readByte(0x200) & (1 << 4)) should not be 0
  }

  // ===== Error handling =====

  "read past capacity sets error" in {
    val (disk, _, _, _) = mkDisk(4)
    disk.writeInt(LBA, 3)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 2)
    disk.writeByte(COMMAND, 0x01)
    disk.readByte(STATUS) shouldBe 0x02
  }

  "write past capacity sets error" in {
    val (disk, _, _, _) = mkDisk(4)
    disk.writeInt(LBA, 4)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x02)
    disk.readByte(STATUS) shouldBe 0x02
  }

  "error clears on status write" in {
    val (disk, _, _, _) = mkDisk(4)
    disk.writeInt(LBA, 10)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x01)
    disk.readByte(STATUS) shouldBe 0x02
    disk.writeByte(STATUS, 0)
    disk.readByte(STATUS) shouldBe 0x01
  }

  // ===== Persistence =====

  "data persists across close and reopen" in {
    val (disk, intc, mem, path) = mkDisk()
    for i <- 0 until 16 do
      mem.writeByte(0x1000 + i, (0xA0 + i).toByte)
    disk.writeInt(LBA, 0)
    disk.writeInt(ADDR, 0x1000)
    disk.writeShort(COUNT, 1)
    disk.writeByte(COMMAND, 0x02)
    disk.close()

    val disk2 = new HardDisk(BASE, mem, path, SECTOR_SIZE, intc, irq = 4)
    disk2.writeInt(LBA, 0)
    disk2.writeInt(ADDR, 0x3000)
    disk2.writeShort(COUNT, 1)
    disk2.writeByte(COMMAND, 0x01)
    disk2.close()

    for i <- 0 until 16 do
      (mem.readByte(0x3000 + i) & 0xFF) shouldBe ((0xA0 + i) & 0xFF)
  }

  // ===== Register readback =====

  "LBA register readback" in {
    val (disk, _, _, _) = mkDisk()
    disk.writeInt(LBA, 0x00010002)
    disk.readInt(LBA) shouldBe 0x00010002
  }

  "ADDR register readback" in {
    val (disk, _, _, _) = mkDisk()
    disk.writeInt(ADDR, 0x00005000)
    disk.readInt(ADDR) shouldBe 0x00005000
  }
}
