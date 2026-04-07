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

  // Register offsets (word-aligned layout for struct access)
  //   +0  LBA      (4B, R/W) — logical block address
  //   +4  ADDR     (4B, R/W) — memory address for DMA
  //   +8  CAPACITY (4B, R)   — total sectors
  //   +12 COUNT    (2B, R/W) — sector count
  //   +14 STATUS   (1B, R)   — bit0=ready, bit1=error
  //   +15 COMMAND  (1B, W)   — 1=read, 2=write (triggers operation)
  private val LBA = 0
  private val ADDR = 4
  private val CAPACITY = 8
  private val COUNT = 12
  private val STATUS = 14
  private val COMMAND = 15

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
      case 0  => (lba >> 24) & 0xff
      case 1  => (lba >> 16) & 0xff
      case 2  => (lba >> 8) & 0xff
      case 3  => lba & 0xff
      case 4  => (addr >> 24) & 0xff
      case 5  => (addr >> 16) & 0xff
      case 6  => (addr >> 8) & 0xff
      case 7  => addr & 0xff
      case 8  => (sectors >> 24) & 0xff
      case 9  => (sectors >> 16) & 0xff
      case 10 => (sectors >> 8) & 0xff
      case 11 => sectors & 0xff
      case 12 => (count >> 8) & 0xff
      case 13 => count & 0xff
      case 14 => status
      case _  => 0

  def writeByte(address: Long, data: Long): Unit =
    val b = (data & 0xff).toInt
    (address - base).toInt match
      case 0  => lba = (lba & 0x00ffffff) | (b << 24)
      case 1  => lba = (lba & 0xff00ffff) | (b << 16)
      case 2  => lba = (lba & 0xffff00ff) | (b << 8)
      case 3  => lba = (lba & 0xffffff00) | b
      case 4  => addr = (addr & 0x00ffffff) | (b << 24)
      case 5  => addr = (addr & 0xff00ffff) | (b << 16)
      case 6  => addr = (addr & 0xffff00ff) | (b << 8)
      case 7  => addr = (addr & 0xffffff00) | b
      case 12 => count = (count & 0x00ff) | (b << 8)
      case 13 => count = (count & 0xff00) | b
      case 14 => status = STATUS_READY // write to clear error
      case 15 => execute(b) // COMMAND — triggers operation
      case _  =>

  // Word-level access for struct pointer writes (stw/ldw)
  override def writeInt(address: Long, data: Long): Unit =
    val off = (address - base).toInt
    val v = data.toInt
    off match
      case LBA  => lba = v
      case ADDR => addr = v
      case _ => super.writeInt(address, data) // fall back to byte-by-byte

  override def readInt(address: Long): Int =
    val off = (address - base).toInt
    off match
      case LBA      => lba
      case ADDR     => addr
      case CAPACITY => sectors
      case _ => super.readInt(address)

  override def writeShort(address: Long, data: Long): Unit =
    val off = (address - base).toInt
    off match
      case COUNT => count = data.toInt & 0xffff
      case _ => super.writeShort(address, data)

  override def readShort(address: Long): Int =
    val off = (address - base).toInt
    off match
      case COUNT => count
      case _ => super.readShort(address)

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
