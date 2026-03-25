package io.github.edadma.trisc

import java.time.{LocalDateTime, ZoneId}

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

class Stdout(val base: Long) extends Device with WriteOnlyAddressable:
  val name = "stdout"
  val size = 1

  def writeByte(addr: Long, data: Long): Unit = print(data.toChar.toString)

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

class RTC(val base: Long) extends Device with ReadOnlyAddressable:
  val name = "RTC"
  val size = 7

  var lastread: Long = 0
  var time: LocalDateTime = LocalDateTime.now(ZoneId.systemDefault())

  val SECOND = 0
  val MINUTE = 1
  val HOUR = 2
  val DAY = 3
  val MONTH = 4
  val DOW = 5
  val YEAR = 6

  def readByte(addr: Long): Int =
    val now = System.currentTimeMillis

    if (now - lastread > 50)
      lastread = now
      time = LocalDateTime.now(ZoneId.systemDefault())

    addr - base match
      case SECOND => toBCD(time.getSecond)
      case MINUTE => toBCD(time.getMinute)
      case HOUR   => toBCD(time.getHour)
      case DAY    => toBCD(time.getDayOfMonth)
      case MONTH  => toBCD(time.getMonthValue)
      case DOW    => time.getDayOfWeek.getValue
      case YEAR   => toBCD(time.getYear % 100)
