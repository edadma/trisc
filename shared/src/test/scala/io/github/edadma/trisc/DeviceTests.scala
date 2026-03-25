package io.github.edadma.trisc

import java.time.LocalDateTime

class DeviceTests extends TestHelpers {

  // ===== Stdout =====

  "stdout outputs character" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r1, STDOUT
        |sti r1, 'A'
        |sti r1, 'B'
        |sti r1, 'C'
        |halt
        |""".stripMargin)
    output shouldBe "ABC"
  }

  // ===== Timer =====

  "timer device has correct size" in {
    val timer = new Timer(0x100)
    timer.size shouldBe 3
  }

  "timer delay registers accept writes" in {
    val timer = new Timer(0x100)
    timer.writeByte(0x100, 0x01) // DELAY_HI
    timer.writeByte(0x101, 0xF4) // DELAY_LO = 0x01F4 = 500ms
    timer.delay shouldBe 0x01F4
  }

  "timer does not fire before started" in {
    val timer = new Timer(0x100)
    timer.writeByte(0x100, 0x00)
    timer.writeByte(0x101, 0x01)
    val cpu = new CPU(new RAM(0, 256), Nil)
    timer(cpu) // should not interrupt
    cpu.state should not be State.Interrupt
  }

  // ===== RTC =====

  "RTC has size 7" in {
    val rtc = new RTC(0x200)
    rtc.size shouldBe 7
  }

  "RTC second is valid BCD" in {
    val rtc = new RTC(0x200)
    val sec = rtc.readByte(0x200) // SECOND
    // BCD byte: each nibble 0-9, value 0-59
    (sec & 0x0F) should be <= 9
    ((sec >> 4) & 0x0F) should be <= 5
  }

  "RTC minute is valid BCD" in {
    val rtc = new RTC(0x200)
    val min = rtc.readByte(0x201) // MINUTE
    (min & 0x0F) should be <= 9
    ((min >> 4) & 0x0F) should be <= 5
  }

  "RTC hour is valid BCD" in {
    val rtc = new RTC(0x200)
    val hour = rtc.readByte(0x202) // HOUR
    (hour & 0x0F) should be <= 9
    ((hour >> 4) & 0x0F) should be <= 2
  }

  "RTC day is valid BCD (1-31)" in {
    val rtc = new RTC(0x200)
    val day = rtc.readByte(0x203) // DAY
    val dayVal = (day & 0x0F) + ((day >> 4) & 0x0F) * 10
    dayVal should be >= 1
    dayVal should be <= 31
  }

  "RTC month is valid BCD (1-12)" in {
    val rtc = new RTC(0x200)
    val month = rtc.readByte(0x204) // MONTH
    val monthVal = (month & 0x0F) + ((month >> 4) & 0x0F) * 10
    monthVal should be >= 1
    monthVal should be <= 12
  }

  "RTC day-of-week is 1-7" in {
    val rtc = new RTC(0x200)
    val dow = rtc.readByte(0x205) // DOW
    dow should be >= 1
    dow should be <= 7
  }

  "RTC year is 2-digit BCD (0-99)" in {
    val rtc = new RTC(0x200)
    val year = rtc.readByte(0x206) // YEAR
    (year & 0x0F) should be <= 9
    ((year >> 4) & 0x0F) should be <= 9
    val yearVal = (year & 0x0F) + ((year >> 4) & 0x0F) * 10
    yearVal should be >= 0
    yearVal should be <= 99
  }

  "RTC year matches current year mod 100" in {
    val rtc = new RTC(0x200)
    val year = rtc.readByte(0x206)
    val yearVal = (year & 0x0F) + ((year >> 4) & 0x0F) * 10
    yearVal shouldBe (LocalDateTime.now().getYear % 100)
  }

  "RTC caches reads within 50ms" in {
    val rtc = new RTC(0x200)
    val sec1 = rtc.readByte(0x200)
    val sec2 = rtc.readByte(0x200) // should return same cached value
    sec1 shouldBe sec2
  }

  // ===== BCD helper =====

  "toBCD converts 0" in {
    toBCD(0) shouldBe 0x00
  }

  "toBCD converts single digit" in {
    toBCD(5) shouldBe 0x05
  }

  "toBCD converts 59" in {
    toBCD(59) shouldBe 0x59
  }

  "toBCD converts 23" in {
    toBCD(23) shouldBe 0x23
  }

  "toBCD converts 99" in {
    toBCD(99) shouldBe 0x99
  }

  "toBCD converts 12" in {
    toBCD(12) shouldBe 0x12
  }
}
