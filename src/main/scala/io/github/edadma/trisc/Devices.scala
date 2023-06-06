package io.github.edadma.trisc

import java.time.{LocalDateTime, ZoneId}

trait Device extends Addressable:
  def loadByte(addr: Long, data: Long): Unit = sys.error("attempting to load a byte into memory-mapped device")

class Stdout(val base: Long) extends Device with WriteOnlyAddressable:
  val name = "stdout"
  val size = 1

  def writeByte(addr: Long, data: Long): Unit = print(data.toChar.toString)

class Timer(val base: Long) extends Device with WriteOnlyAddressable with (CPU => Unit):
  val name = "timer"
  val size = 3

  val DELAY_HI = 0
  val DELAY_LO = 1
  val START = 2

  var delay: Long = 0
  var start: Boolean = false
  var last: Long = 0

  def writeByte(addr: Long, data: Long): Unit =
    addr - base match
      case DELAY_HI => delay = (delay & 0xff) | (data << 8)
      case DELAY_LO => delay = (delay & 0xff00) | (data & 0xff)
      case START =>
        start = data != 0

        if start then last = System.currentTimeMillis()

  def apply(cpu: CPU): Unit =
    if start && System.currentTimeMillis() - last >= delay then
      last += delay
      cpu.interrupt()

class RTC(val base: Long) extends Device with ReadOnlyAddressable:
  val name = "RTC"
  val size = 6

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
      case YEAR   => toBCD(time.getYear)
