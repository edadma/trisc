package io.github.edadma.trisc

class Ramdisk(
    val base: Long,
    mem: Addressable,
    sectors: Int,
    sectorSize: Int,
    intc: InterruptController,
    irq: Int,
    prefill: String = "",
    maxInodes: Int = 128,
    files: Map[String, Array[Byte]] = Map.empty,
) extends Device:
  val name = "Ramdisk"
  val size = 16

  private val disk: Array[Byte] =
    if prefill.nonEmpty || files.nonEmpty then
      val layout = if prefill.nonEmpty then prefill else ""
      // Add file entries for any files not already in the prefill layout
      val extraLines = files.keys.filterNot(path =>
        layout.linesIterator.map(_.trim).exists(l => l.startsWith(path + " "))
      ).map(path => s"$path file").mkString("\n")
      val fullLayout = (layout + "\n" + extraLines).trim
      TFS.format(sectorSize, sectors, maxInodes, fullLayout, files = files)
    else new Array[Byte](sectors * sectorSize)

  // Register offsets
  private val STATUS = 0 // 1 byte, R
  private val COMMAND = 1 // 1 byte, W
  private val LBA = 2 // 4 bytes, R/W
  private val ADDR = 6 // 4 bytes, R/W
  private val COUNT = 10 // 2 bytes, R/W
  private val CAPACITY = 12 // 4 bytes, R

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

    val diskOffset = lba * sectorSize
    val byteCount = count * sectorSize

    cmd match
      case CMD_READ =>
        var i = 0
        while i < byteCount do
          mem.writeByte(addr.toLong + i, disk(diskOffset + i))
          i += 1
        status = STATUS_READY
        intc.raise(irq)
      case CMD_WRITE =>
        var i = 0
        while i < byteCount do
          disk(diskOffset + i) = mem.readByte(addr.toLong + i).toByte
          i += 1
        status = STATUS_READY
        intc.raise(irq)
      case _ =>
        status = STATUS_ERROR
