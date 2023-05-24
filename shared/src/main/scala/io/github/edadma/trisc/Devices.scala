package io.github.edadma.trisc

import java.time.{LocalDateTime, ZoneId}

class Stdout(val base: Long) extends WriteOnlyAddressable:
  val name = "stdout"
  val size = 1

  def writeByte(addr: Long, data: Long): Unit = print(data.toChar.toString)

class RTC(val base: Long) extends ReadOnlyAddressable:
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
