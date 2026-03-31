package io.github.edadma.trisc

import io.github.edadma.cross_platform.{RandomAccessFile, openRandomAccessFile}

/**
 * Hard disk device backed by a host file via RandomAccessFile.
 * Same register interface as Ramdisk — drop-in replacement for file-backed storage.
 *
 * Register map (16 bytes):
 *   0:    STATUS   (R)   — bit 0: ready, bit 1: error
 *   1:    COMMAND  (W)   — 0x01 = read, 0x02 = write
 *   2-5:  LBA      (R/W) — 32-bit sector number, big-endian
 *   6-9:  ADDR     (R/W) — 32-bit RAM address for DMA, big-endian
 *   10-11: COUNT   (R/W) — 16-bit sector count, big-endian
 *   12-15: CAPACITY (R)  — 32-bit total sector count, big-endian
 *
 * Transfers use DMA: data is copied between the file and main memory
 * via the mem reference, without CPU involvement.
 *
 * @param base       Base address in memory map
 * @param mem        Main memory (for DMA transfers)
 * @param path       Host file path for disk image
 * @param sectorSize Bytes per sector (default 512)
 * @param intc       Interrupt controller
 * @param irq        IRQ line for completion interrupt
 */
class HardDisk(
    val base: Long,
    mem: Addressable,
    path: String,
    sectorSize: Int,
    intc: InterruptController,
    irq: Int,
) extends Device:
  val name = "HardDisk"
  val size = 16

  private val file: RandomAccessFile = openRandomAccessFile(path, "rw")
  private val sectors: Int = (file.length / sectorSize).toInt
  private val buffer: Array[Byte] = new Array[Byte](sectorSize)

  // Register offsets
  private val STATUS = 0
  private val COMMAND = 1
  private val LBA = 2
  private val ADDR = 6
  private val COUNT = 10
  private val CAPACITY = 12

  // Commands
  private val CMD_READ = 0x01
  private val CMD_WRITE = 0x02

  // Status bits
  private val STATUS_READY = 0x01
  private val STATUS_ERROR = 0x02

  // Register state
  private var status: Int = STATUS_READY
  private var lba: Int = 0
  private var addr: Int = 0
  private var count: Int = 0

  def readByte(address: Long): Int =
    (address - base).toInt match
      case STATUS => status
      case 2      => (lba >> 24) & 0xff
      case 3      => (lba >> 16) & 0xff
      case 4      => (lba >> 8) & 0xff
      case 5      => lba & 0xff
      case 6      => (addr >> 24) & 0xff
      case 7      => (addr >> 16) & 0xff
      case 8      => (addr >> 8) & 0xff
      case 9      => addr & 0xff
      case 10     => (count >> 8) & 0xff
      case 11     => count & 0xff
      case 12     => (sectors >> 24) & 0xff
      case 13     => (sectors >> 16) & 0xff
      case 14     => (sectors >> 8) & 0xff
      case 15     => sectors & 0xff
      case _      => 0

  def writeByte(address: Long, data: Long): Unit =
    val b = (data & 0xff).toInt
    (address - base).toInt match
      case COMMAND => execute(b)
      case 2       => lba = (lba & 0x00ffffff) | (b << 24)
      case 3       => lba = (lba & 0xff00ffff) | (b << 16)
      case 4       => lba = (lba & 0xffff00ff) | (b << 8)
      case 5       => lba = (lba & 0xffffff00) | b
      case 6       => addr = (addr & 0x00ffffff) | (b << 24)
      case 7       => addr = (addr & 0xff00ffff) | (b << 16)
      case 8       => addr = (addr & 0xffff00ff) | (b << 8)
      case 9       => addr = (addr & 0xffffff00) | b
      case 10      => count = (count & 0x00ff) | (b << 8)
      case 11      => count = (count & 0xff00) | b
      case STATUS  => status = STATUS_READY // write to clear error
      case _       =>

  private def execute(cmd: Int): Unit =
    if count <= 0 || lba < 0 || lba + count > sectors then
      status = STATUS_ERROR
      return

    cmd match
      case CMD_READ =>
        var sector = lba
        var memAddr = addr.toLong
        var remaining = count
        while remaining > 0 do
          file.seek(sector.toLong * sectorSize)
          file.readFully(buffer)
          var i = 0
          while i < sectorSize do
            mem.writeByte(memAddr + i, buffer(i))
            i += 1
          sector += 1
          memAddr += sectorSize
          remaining -= 1
        status = STATUS_READY
        intc.raise(irq)
      case CMD_WRITE =>
        var sector = lba
        var memAddr = addr.toLong
        var remaining = count
        while remaining > 0 do
          var i = 0
          while i < sectorSize do
            buffer(i) = mem.readByte(memAddr + i).toByte
            i += 1
          file.seek(sector.toLong * sectorSize)
          file.write(buffer)
          sector += 1
          memAddr += sectorSize
          remaining -= 1
        file.fsync()
        status = STATUS_READY
        intc.raise(irq)
      case _ =>
        status = STATUS_ERROR

  /** Close the backing file. Call when the emulator shuts down. */
  def close(): Unit = file.close()
