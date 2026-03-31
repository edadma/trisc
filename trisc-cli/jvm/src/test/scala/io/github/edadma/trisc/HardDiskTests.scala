package io.github.edadma.trisc

import io.github.edadma.cross_platform.createTempFile

class HardDiskTests extends TestHelpers {

  val SECTOR_SIZE = 512

  def mkDisk(sectors: Int = 16): (HardDisk, InterruptController, Memory, String) =
    val intc = new InterruptController(0x200)
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val path = createTempFile("trisc-hd-", ".img")
    // Create disk image file with the right size
    val raf = io.github.edadma.cross_platform.openRandomAccessFile(path, "rw")
    raf.setLength(sectors.toLong * SECTOR_SIZE)
    raf.close()
    val disk = new HardDisk(0x100, mem, path, SECTOR_SIZE, intc, irq = 4)
    (disk, intc, mem, path)

  /** Write LBA register */
  def setLBA(d: HardDisk, v: Int): Unit =
    d.writeByte(0x102, (v >> 24) & 0xFF)
    d.writeByte(0x103, (v >> 16) & 0xFF)
    d.writeByte(0x104, (v >> 8) & 0xFF)
    d.writeByte(0x105, v & 0xFF)

  /** Write ADDR register */
  def setADDR(d: HardDisk, v: Int): Unit =
    d.writeByte(0x106, (v >> 24) & 0xFF)
    d.writeByte(0x107, (v >> 16) & 0xFF)
    d.writeByte(0x108, (v >> 8) & 0xFF)
    d.writeByte(0x109, v & 0xFF)

  /** Write COUNT register */
  def setCOUNT(d: HardDisk, v: Int): Unit =
    d.writeByte(0x10A, (v >> 8) & 0xFF)
    d.writeByte(0x10B, v & 0xFF)

  // ===== Basic properties =====

  "HardDisk has correct size" in {
    val (disk, _, _, _) = mkDisk()
    disk.size shouldBe 16
  }

  "HardDisk status is ready initially" in {
    val (disk, _, _, _) = mkDisk()
    disk.readByte(0x100) shouldBe 0x01
  }

  "HardDisk capacity reflects file size" in {
    val (disk, _, _, _) = mkDisk(32)
    val cap = ((disk.readByte(0x10C) & 0xFF) << 24) |
              ((disk.readByte(0x10D) & 0xFF) << 16) |
              ((disk.readByte(0x10E) & 0xFF) << 8) |
              (disk.readByte(0x10F) & 0xFF)
    cap shouldBe 32
  }

  // ===== Write then read =====

  "write and read back single sector" in {
    val (disk, _, mem, _) = mkDisk()
    // Write pattern to RAM at 0x1000
    for i <- 0 until SECTOR_SIZE do
      mem.writeByte(0x1000 + i, (i & 0xFF).toByte)
    // DMA write: RAM 0x1000 → disk sector 0
    setLBA(disk, 0)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x02) // CMD_WRITE

    // Clear RAM
    for i <- 0 until SECTOR_SIZE do
      mem.writeByte(0x1000 + i, 0)

    // DMA read: disk sector 0 → RAM 0x2000
    setLBA(disk, 0)
    setADDR(disk, 0x2000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x01) // CMD_READ

    // Verify
    for i <- 0 until SECTOR_SIZE do
      (mem.readByte(0x2000 + i) & 0xFF) shouldBe (i & 0xFF)
  }

  "write and read multiple sectors" in {
    val (disk, _, mem, _) = mkDisk()
    // Fill 2 sectors of RAM
    for i <- 0 until SECTOR_SIZE * 2 do
      mem.writeByte(0x1000 + i, ((i * 3) & 0xFF).toByte)
    // Write 2 sectors starting at LBA 2
    setLBA(disk, 2)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 2)
    disk.writeByte(0x101, 0x02)

    // Read back to different address
    setLBA(disk, 2)
    setADDR(disk, 0x4000)
    setCOUNT(disk, 2)
    disk.writeByte(0x101, 0x01)

    for i <- 0 until SECTOR_SIZE * 2 do
      (mem.readByte(0x4000 + i) & 0xFF) shouldBe ((i * 3) & 0xFF)
  }

  // ===== Interrupt =====

  "read raises completion interrupt" in {
    val (disk, intc, _, _) = mkDisk()
    setLBA(disk, 0)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x01)
    (intc.readByte(0x200) & (1 << 4)) should not be 0 // IRQ 4
  }

  "write raises completion interrupt" in {
    val (disk, intc, _, _) = mkDisk()
    setLBA(disk, 0)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x02)
    (intc.readByte(0x200) & (1 << 4)) should not be 0
  }

  // ===== Error handling =====

  "read past capacity sets error" in {
    val (disk, _, _, _) = mkDisk(4)
    setLBA(disk, 3)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 2) // LBA 3 + 2 = 5, but only 4 sectors
    disk.writeByte(0x101, 0x01)
    disk.readByte(0x100) shouldBe 0x02 // STATUS_ERROR
  }

  "write past capacity sets error" in {
    val (disk, _, _, _) = mkDisk(4)
    setLBA(disk, 4)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x02)
    disk.readByte(0x100) shouldBe 0x02
  }

  "error clears on status write" in {
    val (disk, _, _, _) = mkDisk(4)
    setLBA(disk, 10)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x01) // will error
    disk.readByte(0x100) shouldBe 0x02
    disk.writeByte(0x100, 0) // clear error
    disk.readByte(0x100) shouldBe 0x01 // ready again
  }

  // ===== Persistence =====

  "data persists across close and reopen" in {
    val (disk, intc, mem, path) = mkDisk()
    // Write data
    for i <- 0 until 16 do
      mem.writeByte(0x1000 + i, (0xA0 + i).toByte)
    setLBA(disk, 0)
    setADDR(disk, 0x1000)
    setCOUNT(disk, 1)
    disk.writeByte(0x101, 0x02)
    disk.close()

    // Reopen same file
    val disk2 = new HardDisk(0x100, mem, path, SECTOR_SIZE, intc, irq = 4)
    setLBA(disk2, 0)
    setADDR(disk2, 0x3000)
    setCOUNT(disk2, 1)
    disk2.writeByte(0x101, 0x01)
    disk2.close()

    for i <- 0 until 16 do
      (mem.readByte(0x3000 + i) & 0xFF) shouldBe ((0xA0 + i) & 0xFF)
  }

  // ===== Register readback =====

  "LBA register readback" in {
    val (disk, _, _, _) = mkDisk()
    setLBA(disk, 0x00010002)
    disk.readByte(0x102) shouldBe 0x00
    disk.readByte(0x103) shouldBe 0x01
    disk.readByte(0x104) shouldBe 0x00
    disk.readByte(0x105) shouldBe 0x02
  }

  "ADDR register readback" in {
    val (disk, _, _, _) = mkDisk()
    setADDR(disk, 0x00005000)
    disk.readByte(0x106) shouldBe 0x00
    disk.readByte(0x107) shouldBe 0x00
    disk.readByte(0x108) shouldBe 0x50
    disk.readByte(0x109) shouldBe 0x00
  }
}
