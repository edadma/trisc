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

  // ===== CallbackDevice =====

  "CallbackDevice fires onWrite with offset and data" in {
    var written = List.empty[(Long, Long)]
    val dev = new CallbackDevice("test", 0x400, 4, onWrite = (off, data) => written = written :+ (off, data))
    dev.writeByte(0x400, 0x11)
    dev.writeByte(0x401, 0x22)
    dev.writeByte(0x403, 0xFF)
    written shouldBe List((0L, 0x11L), (1L, 0x22L), (3L, 0xFFL))
  }

  "CallbackDevice fires onRead with offset" in {
    val dev = new CallbackDevice("test", 0x400, 4, onRead = off => (off * 10).toInt)
    dev.readByte(0x400) shouldBe 0
    dev.readByte(0x401) shouldBe 10
    dev.readByte(0x403) shouldBe 30
  }

  "CallbackDevice defaults to no-op write and zero read" in {
    val dev = new CallbackDevice("test", 0x400, 2)
    dev.writeByte(0x400, 0xFF) // should not throw
    dev.readByte(0x400) shouldBe 0
  }

  "CallbackDevice as write-only (like display command register)" in {
    var lastCmd = 0L
    val dev = new CallbackDevice("display", 0x500, 8,
      onWrite = (off, data) => if off == 0 then lastCmd = data)
    dev.writeByte(0x500, 42)
    lastCmd shouldBe 42
    dev.writeByte(0x501, 99) // different offset, no effect on lastCmd
    lastCmd shouldBe 42
  }

  "CallbackDevice as read-write (like framebuffer status)" in {
    var ready = false
    val dev = new CallbackDevice("fb", 0x600, 2,
      onWrite = (off, data) => if off == 0 then ready = (data != 0),
      onRead = off => if off == 1 then (if ready then 1 else 0) else 0)
    dev.readByte(0x601) shouldBe 0
    dev.writeByte(0x600, 1)
    dev.readByte(0x601) shouldBe 1
    dev.writeByte(0x600, 0)
    dev.readByte(0x601) shouldBe 0
  }

  "CallbackDevice works with CPU" in {
    var captured = List.empty[Byte]
    val dev = new CallbackDevice("out", 0xFF0, 1,
      onWrite = (_, data) => captured = captured :+ data.toByte)
    val mem = new Memory("Memory", new RAM(0, 0xFF0), dev)
    val tof = assemble(
      """dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r3, 0xFF0
        |ldi r1, 'X'
        |stb r1, r3, r0
        |ldi r1, 'Y'
        |stb r1, r3, r0
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    captured shouldBe List('X'.toByte, 'Y'.toByte)
  }

  "CallbackDevice with buffer pattern (multi-byte write)" in {
    val buf = new Array[Byte](8)
    val dev = new CallbackDevice("buf", 0x700, 8,
      onWrite = (off, data) => buf(off.toInt) = data.toByte,
      onRead = off => buf(off.toInt))
    dev.writeByte(0x700, 0x12)
    dev.writeByte(0x701, 0x34)
    dev.readByte(0x700) shouldBe 0x12
    dev.readByte(0x701) shouldBe 0x34
  }

  // ===== BufferedDevice =====

  "BufferedDevice stores written bytes" in {
    val dev = new BufferedDevice("fb", 0x800, 16)
    dev.writeByte(0x800, 0xAA)
    dev.writeByte(0x80F, 0xBB)
    dev.readByte(0x800) shouldBe 0xAA
    dev.readByte(0x80F) shouldBe 0xBB
  }

  "BufferedDevice starts zeroed" in {
    val dev = new BufferedDevice("fb", 0x800, 8)
    for i <- 0 until 8 do
      dev.readByte(0x800 + i) shouldBe 0
  }

  "BufferedDevice fires callback with offset and data" in {
    var lastOff = -1L
    var lastData = -1L
    val dev = new BufferedDevice("fb", 0x800, 4,
      onWrite = (off, data) =>
        lastOff = off
        lastData = data)
    dev.writeByte(0x802, 0x42)
    lastOff shouldBe 2
    lastData shouldBe 0x42
    dev.buffer(2) shouldBe 0x42
  }

  "BufferedDevice buffer is updated before callback fires" in {
    var cbValues = List.empty[Long]
    val dev = new BufferedDevice("fb", 0x800, 4,
      onWrite = (_, data) => cbValues = cbValues :+ data)
    dev.writeByte(0x800, 0x11)
    dev.writeByte(0x801, 0x22)
    cbValues shouldBe List(0x11L, 0x22L)
    // verify buffer reflects the writes
    dev.buffer(0) shouldBe 0x11.toByte
    dev.buffer(1) shouldBe 0x22.toByte
  }

  "BufferedDevice reads back without callback" in {
    val dev = new BufferedDevice("fb", 0x800, 4)
    dev.writeByte(0x800, 0x12)
    dev.writeByte(0x801, 0x34)
    dev.writeByte(0x802, 0x56)
    dev.writeByte(0x803, 0x78)
    dev.readByte(0x800) shouldBe 0x12
    dev.readByte(0x801) shouldBe 0x34
    dev.readByte(0x802) shouldBe 0x56
    dev.readByte(0x803) shouldBe 0x78
  }

  "BufferedDevice buffer is directly accessible" in {
    val dev = new BufferedDevice("fb", 0x800, 4)
    dev.writeByte(0x800, 0xAA)
    dev.buffer(0) shouldBe 0xAA.toByte
    dev.buffer.length shouldBe 4
  }

  "BufferedDevice works with CPU (display pattern)" in {
    var dirtyPixels = List.empty[(Int, Int)]
    val dev = new BufferedDevice("display", 0xE00, 8,
      onWrite = (off, data) => dirtyPixels = dirtyPixels :+ (off.toInt, data.toInt))
    val mem = new Memory("Memory", new RAM(0, 0xE00), dev)
    val tof = assemble(
      """DISPLAY = 0xE00
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r3, DISPLAY
        |ldi r1, 0xFF
        |stb r1, r3, r0
        |ldi r4, 3
        |ldi r1, 0x80
        |stb r1, r3, r4
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    dirtyPixels shouldBe List((0, 0xFF), (3, 0x80))
    dev.readByte(0xE00) shouldBe 0xFF
    dev.readByte(0xE03) shouldBe 0x80
  }

  // ===== RNG =====

  "RNG has size 1" in {
    val rng = new RNG(0x300)
    rng.size shouldBe 1
  }

  "RNG is read-only" in {
    val rng = new RNG(0x300)
    an[Exception] should be thrownBy {
      rng.writeByte(0x300, 0x42)
    }
  }

  "RNG returns values in 0-255 range" in {
    val rng = new RNG(0x300, Some(42))
    for _ <- 0 until 100 do
      val v = rng.readByte(0x300)
      v should be >= 0
      v should be <= 255
  }

  "RNG with seed produces deterministic sequence" in {
    val rng1 = new RNG(0x300, Some(12345))
    val rng2 = new RNG(0x300, Some(12345))
    for _ <- 0 until 20 do
      rng1.readByte(0x300) shouldBe rng2.readByte(0x300)
  }

  "RNG without seed produces values (non-deterministic)" in {
    val rng = new RNG(0x300)
    // Just verify it doesn't throw and returns bytes
    val values = (0 until 10).map(_ => rng.readByte(0x300))
    values.foreach { v =>
      v should be >= 0
      v should be <= 255
    }
  }

  "RNG returns varying values" in {
    val rng = new RNG(0x300, Some(42))
    val values = (0 until 20).map(_ => rng.readByte(0x300)).toSet
    values.size should be > 1 // not all the same
  }

  "RNG works as memory-mapped device in CPU" in {
    val rng = new RNG(0xFF0, Some(42))
    val expected = new java.util.Random(42)
    val mem = new Memory("Memory", new RAM(0, 0xFF0), rng)
    val tof = assemble(
      """RNG = 0xFF0
        |dw 8
        |dw 0
        |dw 0
        |dw 0
        |movi r3, RNG
        |ldb r1, r3, r0
        |ldb r2, r3, r0
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem, Nil) { limit = 10000 }
    cpu.reset()
    cpu.run()
    cpu.r(1).read shouldBe expected.nextInt(256)
    cpu.r(2).read shouldBe expected.nextInt(256)
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
