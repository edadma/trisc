package io.github.edadma.trisc

class DeviceTests extends TestHelpers {

  val fixedTime: TimeFields = TimeFields(second = 45, minute = 30, hour = 14, day = 25, month = 3, dow = 2, year = 2026)

  // ===== Stdout =====

  "stdout outputs ASCII character" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dd 0xFF0
        |dd 160
        |rb 144
        |movi r1, STDOUT
        |sti r1, 'A'
        |sti r1, 'B'
        |sti r1, 'C'
        |halt
        |""".stripMargin)
    output shouldBe "ABC"
  }

  "stdout handles UTF-8 2-byte sequence" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // é = U+00E9 = 0xC3 0xA9
    dev.writeByte(0x100, 0xC3)
    dev.writeByte(0x100, 0xA9)
    buf.toString shouldBe "é"
  }

  "stdout handles UTF-8 3-byte sequence" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // € = U+20AC = 0xE2 0x82 0xAC
    dev.writeByte(0x100, 0xE2)
    dev.writeByte(0x100, 0x82)
    dev.writeByte(0x100, 0xAC)
    buf.toString shouldBe "€"
  }

  "stdout handles UTF-8 4-byte sequence" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // 😀 = U+1F600 = 0xF0 0x9F 0x98 0x80
    dev.writeByte(0x100, 0xF0)
    dev.writeByte(0x100, 0x9F)
    dev.writeByte(0x100, 0x98)
    dev.writeByte(0x100, 0x80)
    buf.toString shouldBe "\uD83D\uDE00"
  }

  "stdout handles mixed ASCII and UTF-8" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // "Hé!" = H, é (2-byte), !
    dev.writeByte(0x100, 'H')
    dev.writeByte(0x100, 0xC3)
    dev.writeByte(0x100, 0xA9)
    dev.writeByte(0x100, '!')
    buf.toString shouldBe "Hé!"
  }

  "stdout handles consecutive multi-byte sequences" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // "éà" = two 2-byte sequences
    // é = 0xC3 0xA9, à = 0xC3 0xA0
    dev.writeByte(0x100, 0xC3)
    dev.writeByte(0x100, 0xA9)
    dev.writeByte(0x100, 0xC3)
    dev.writeByte(0x100, 0xA0)
    buf.toString shouldBe "éà"
  }

  // ===== Stdout UTF-16 (writeShort) =====

  "stdout handles UTF-16 BMP character" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeShort(0x100, 0x00E9) // é
    buf.toString shouldBe "é"
  }

  "stdout handles UTF-16 ASCII via writeShort" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeShort(0x100, 'A')
    dev.writeShort(0x100, 'B')
    buf.toString shouldBe "AB"
  }

  "stdout handles UTF-16 surrogate pair" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // 😀 = U+1F600 = surrogate pair D83D DE00
    dev.writeShort(0x100, 0xD83D)
    dev.writeShort(0x100, 0xDE00)
    buf.toString shouldBe "\uD83D\uDE00"
  }

  "stdout handles UTF-16 mixed BMP and surrogate" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeShort(0x100, 'H')
    dev.writeShort(0x100, 0xD83D)
    dev.writeShort(0x100, 0xDE00)
    dev.writeShort(0x100, '!')
    buf.toString shouldBe "H\uD83D\uDE00!"
  }

  "stdout recovers from orphaned high surrogate" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeShort(0x100, 0xD83D) // high surrogate
    dev.writeShort(0x100, 'X')    // not a low surrogate — reset and output X
    buf.toString shouldBe "X"
  }

  // ===== Stdout UTF-32 (writeInt) =====

  "stdout handles UTF-32 ASCII" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeInt(0x100, 'A')
    buf.toString shouldBe "A"
  }

  "stdout handles UTF-32 BMP codepoint" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeInt(0x100, 0x00E9) // é
    buf.toString shouldBe "é"
  }

  "stdout handles UTF-32 supplementary codepoint" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeInt(0x100, 0x1F600) // 😀
    buf.toString shouldBe "\uD83D\uDE00"
  }

  "stdout handles UTF-32 CJK character" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    dev.writeInt(0x100, 0x4E16) // 世
    buf.toString shouldBe "世"
  }

  // ===== Stdout UTF-8 error recovery =====

  "stdout recovers from invalid continuation byte" in {
    val buf = new StringBuilder
    val dev = new Stdout(0x100, s => buf ++= s)
    // Start 2-byte sequence then send ASCII instead of continuation
    dev.writeByte(0x100, 0xC3) // start of 2-byte
    dev.writeByte(0x100, 'X')  // not a continuation byte — should reset and output X
    buf.toString shouldBe "X"
  }

  // ===== Timer =====
  // Register map: ARR(0-3), CNT(4-7), PSC(8-9), CR(10), SR(11), IER(12), pad(13-15), channels(16+)

  /** Helper: set ARR (period) via word write at offset 0 */
  def setARR(timer: Timer, base: Long, value: Int): Unit =
    timer.writeInt(base, value)

  /** Helper: enable timer with overflow interrupt */
  def startTimer(timer: Timer, base: Long): Unit =
    timer.writeByte(base + 12, 0x01) // IER: overflow interrupt enable
    timer.writeByte(base + 10, 0x01) // CR: enable

  "timer has correct size" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    timer.size shouldBe 48
  }

  "timer ARR registers accept 32-bit write" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 0x00010000)
    timer.period shouldBe 0x00010000L
  }

  "timer ARR small value" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    timer.period shouldBe 10
  }

  "timer does not fire before started" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    for _ <- 1 to 100 do timer.tick()
    timer.fired shouldBe false
  }

  "timer fires after period elapses" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)
    for _ <- 1 to 10 do timer.tick()
    timer.fired shouldBe true
  }

  "timer does not fire before period elapses" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)
    for _ <- 1 to 5 do timer.tick()
    timer.fired shouldBe false
  }

  "timer auto-reloads for periodic interrupts" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)

    for _ <- 1 to 10 do timer.tick()
    timer.fired shouldBe true
    timer.writeByte(0x10B, 0x01) // SR: write-1-to-clear overflow

    for _ <- 1 to 10 do timer.tick()
    timer.fired shouldBe true
  }

  "timer status reads overflow flag" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    timer.readByte(0x10B) shouldBe 0 // SR: no flags
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)
    for _ <- 1 to 10 do timer.tick()
    (timer.readByte(0x10B) & 0x01) shouldBe 1 // overflow flag
  }

  "timer acknowledge clears overflow flag" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)
    for _ <- 1 to 10 do timer.tick()
    (timer.readByte(0x10B) & 0x01) shouldBe 1
    timer.writeByte(0x10B, 0x01) // write-1-to-clear
    (timer.readByte(0x10B) & 0x01) shouldBe 0
  }

  "timer stop halts firing" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)
    timer.writeByte(0x10A, 0x00) // CR: disable
    for _ <- 1 to 100 do timer.tick()
    timer.fired shouldBe false
  }

  "timer counter resets each period" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)

    for _ <- 1 to 10 do timer.tick()
    timer.fired shouldBe true
    timer.writeByte(0x10B, 0x01) // acknowledge

    for _ <- 1 to 9 do timer.tick()
    timer.fired shouldBe false

    timer.tick()
    timer.fired shouldBe true
  }

  "timer start resets counter and clears fired" in {
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0)
    setARR(timer, 0x100, 10)
    startTimer(timer, 0x100)
    for _ <- 1 to 10 do timer.tick()
    timer.fired shouldBe true
    timer.writeByte(0x10A, 0x01) // CR: re-enable (resets counter)
    timer.fired shouldBe false
    timer.running shouldBe true
  }

  // ===== RTC =====

  "RTC has size 7" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.size shouldBe 7
  }

  "RTC second returns BCD" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x200) shouldBe toBCD(45) // 0x45
  }

  "RTC minute returns BCD" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x201) shouldBe toBCD(30) // 0x30
  }

  "RTC hour returns BCD" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x202) shouldBe toBCD(14) // 0x14
  }

  "RTC day returns BCD" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x203) shouldBe toBCD(25) // 0x25
  }

  "RTC month returns BCD" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x204) shouldBe toBCD(3) // 0x03
  }

  "RTC day-of-week returns raw value" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x205) shouldBe 2
  }

  "RTC year returns 2-digit BCD" in {
    val rtc = new RTC(0x200, () => fixedTime)
    rtc.readByte(0x206) shouldBe toBCD(26) // 2026 % 100 = 26 → 0x26
  }

  "RTC caches reads within 50ms" in {
    var calls = 0
    val rtc = new RTC(0x200, () => { calls += 1; fixedTime })
    rtc.readByte(0x200)
    rtc.readByte(0x201)
    rtc.readByte(0x202)
    // Initial read + all within 50ms cache window = only 1 call to timeSource
    calls shouldBe 1
  }

  "RTC updates time source after cache expires" in {
    var second = 10
    val rtc = new RTC(0x200, () => fixedTime.copy(second = second))
    rtc.readByte(0x200) shouldBe toBCD(10)
    second = 11
    // Force cache expiry by advancing past 50ms
    Thread.sleep(60)
    rtc.readByte(0x200) shouldBe toBCD(11)
  }

  "RTC is read-only" in {
    val rtc = new RTC(0x200, () => fixedTime)
    an[Exception] should be thrownBy {
      rtc.writeByte(0x200, 0)
    }
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
      """dd 0xFE0
        |dd 160
        |rb 144
        |movi r3, 0xFF0
        |ldi r1, 'X'
        |stb r1, r3, r0
        |ldi r1, 'Y'
        |stb r1, r3, r0
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000; quiet = true }
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
        |dd 0xDF0
        |dd 160
        |rb 144
        |movi r3, DISPLAY
        |ldi r1, 0xFF
        |stb r1, r3, r0
        |ldi r4, 3
        |ldi r1, 0x80
        |stb r1, r3, r4
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000; quiet = true }
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
        |dd 0xFE0
        |dd 160
        |rb 144
        |movi r3, RNG
        |ldb r1, r3, r0
        |ldb r2, r3, r0
        |halt
        |""".stripMargin)
    tof.load(mem)
    val cpu = new CPU(mem) { limit = 10000; quiet = true }
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
