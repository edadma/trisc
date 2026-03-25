package io.github.edadma.trisc

trait Device extends Addressable:
  def loadByte(addr: Long, data: Long): Unit = sys.error("attempting to load a byte into memory-mapped device")

class CallbackDevice(
    val name: String,
    val base: Long,
    val size: Long,
    onWrite: (Long, Long) => Unit = (_, _) => (),
    onRead: Long => Int = _ => 0,
) extends Device:
  def readByte(addr: Long): Int = onRead(addr - base)
  def writeByte(addr: Long, data: Long): Unit = onWrite(addr - base, data)

class BufferedDevice(
    name: String,
    base: Long,
    size: Long,
    onWrite: (Long, Long) => Unit = (_, _) => (),
) extends CallbackDevice(name, base, size, onWrite):
  val buffer: Array[Byte] = new Array[Byte](size.toInt)

  override def readByte(addr: Long): Int = buffer((addr - base).toInt) & 0xff

  override def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    buffer(off) = data.toByte
    onWrite(off, data)

class Stdout(val base: Long, output: String => Unit = s => print(s)) extends Device with WriteOnlyAddressable:
  val name = "stdout"
  val size = 4

  private var utf8Buf = 0
  private var utf8Remaining = 0
  private var highSurrogate: Int = 0

  override def writeShort(addr: Long, data: Long): Unit =
    val u = (data & 0xffff).toInt
    if highSurrogate != 0 then
      if u >= 0xdc00 && u <= 0xdfff then
        val cp = 0x10000 + ((highSurrogate - 0xd800) << 10) + (u - 0xdc00)
        highSurrogate = 0
        output(new String(Character.toChars(cp)))
      else
        highSurrogate = 0
        writeShort(addr, data)
    else if u >= 0xd800 && u <= 0xdbff then
      highSurrogate = u
    else
      output(new String(Character.toChars(u)))

  override def writeInt(addr: Long, data: Long): Unit =
    val cp = (data & 0x1fffff).toInt
    output(new String(Character.toChars(cp)))

  def writeByte(addr: Long, data: Long): Unit =
    val b = (data & 0xff).toInt

    if utf8Remaining > 0 then
      if (b & 0xc0) == 0x80 then
        utf8Buf = (utf8Buf << 6) | (b & 0x3f)
        utf8Remaining -= 1
        if utf8Remaining == 0 then
          output(new String(Character.toChars(utf8Buf)))
      else
        // invalid continuation — reset and treat as new byte
        utf8Remaining = 0
        writeByte(addr, data)
    else if (b & 0x80) == 0 then
      output(b.toChar.toString)
    else if (b & 0xe0) == 0xc0 then
      utf8Buf = b & 0x1f
      utf8Remaining = 1
    else if (b & 0xf0) == 0xe0 then
      utf8Buf = b & 0x0f
      utf8Remaining = 2
    else if (b & 0xf8) == 0xf0 then
      utf8Buf = b & 0x07
      utf8Remaining = 3

class Timer(val base: Long, clock: () => Long = () => System.currentTimeMillis()) extends Device with (CPU => Unit):
  val name = "timer"
  val size = 6

  private val PERIOD = 0 // 4 bytes, W
  private val CONTROL = 4 // 1 byte, W
  private val STATUS = 5 // 1 byte, R/W

  var period: Long = 0
  var running: Boolean = false
  var fired: Boolean = false
  private var last: Long = 0

  def readByte(addr: Long): Int =
    addr - base match
      case STATUS => if fired then 1 else 0
      case _      => 0

  def writeByte(addr: Long, data: Long): Unit =
    addr - base match
      case 0 => period = (period & 0x00ffffffL) | ((data & 0xff) << 24)
      case 1 => period = (period & 0xff00ffffL) | ((data & 0xff) << 16)
      case 2 => period = (period & 0xffff00ffL) | ((data & 0xff) << 8)
      case 3 => period = (period & 0xffffff00L) | (data & 0xff)
      case CONTROL =>
        running = data != 0
        if running then
          last = clock()
          fired = false
      case STATUS =>
        fired = false // acknowledge
      case _ =>

  def apply(cpu: CPU): Unit =
    if running && clock() - last >= period then
      last += period
      fired = true
      cpu.interrupt()

class RNG(val base: Long, seed: Option[Long] = None) extends Device with ReadOnlyAddressable:
  val name = "RNG"
  val size = 1

  private val random = seed match
    case Some(s) => new java.util.Random(s)
    case None    => new java.util.Random()

  def readByte(addr: Long): Int = random.nextInt(256)

case class TimeFields(second: Int, minute: Int, hour: Int, day: Int, month: Int, dow: Int, year: Int)

class RTC(val base: Long, timeSource: () => TimeFields) extends Device with ReadOnlyAddressable:
  val name = "RTC"
  val size = 7

  private var lastRead: Long = System.currentTimeMillis
  private var cached: TimeFields = timeSource()

  def readByte(addr: Long): Int =
    val now = System.currentTimeMillis

    if now - lastRead > 50 then
      lastRead = now
      cached = timeSource()

    addr - base match
      case 0 => toBCD(cached.second)
      case 1 => toBCD(cached.minute)
      case 2 => toBCD(cached.hour)
      case 3 => toBCD(cached.day)
      case 4 => toBCD(cached.month)
      case 5 => cached.dow
      case 6 => toBCD(cached.year % 100)
