package io.github.edadma.trisc

class TimerTests extends TestHelpers {

  def mkTimer(channels: Seq[TimerChannel] = Nil): (Timer, InterruptController) =
    val intc = new InterruptController(0x200)
    val timer = new Timer(0x100, intc, irq = 0, channels = channels)
    (timer, intc)

  /** Set ARR (period) via word write at base+0 */
  def setARR(t: Timer, v: Long): Unit =
    t.writeInt(0x100, v)

  /** Set PSC (prescaler) via short write at base+8 */
  def setPSC(t: Timer, v: Int): Unit =
    t.writeShort(0x108, v)

  /** Set CCR for channel ch via word write at base+16+ch*8 */
  def setCCR(t: Timer, ch: Int, v: Long): Unit =
    t.writeInt(0x110 + ch * 8, v)

  /** Read CCR for channel ch via word read at base+16+ch*8 */
  def readCCR(t: Timer, ch: Int): Long =
    t.readInt(0x110 + ch * 8).toLong & 0xFFFFFFFFL

  /** Set CCMR for channel ch at base+16+ch*8+4 */
  def setCCMR(t: Timer, ch: Int, mode: Int): Unit =
    t.writeByte(0x110 + ch * 8 + 4, mode)

  /** Enable timer with overflow interrupt */
  def enable(t: Timer): Unit =
    t.writeByte(0x10C, 0x01) // IER: overflow
    t.writeByte(0x10A, 0x01) // CR: enable

  /** Enable timer with overflow + channel interrupts */
  def enableWithChannels(t: Timer, chMask: Int): Unit =
    t.writeByte(0x10C, 0x01 | (chMask << 1)) // IER: overflow + channels
    t.writeByte(0x10A, 0x01) // CR: enable

  // ===== Prescaler =====

  "prescaler=0 increments counter every tick" in {
    val (t, _) = mkTimer()
    setARR(t, 100)
    setPSC(t, 0)
    enable(t)
    for _ <- 1 to 5 do t.tick()
    // Counter should be 5 (increments every tick)
    val cnt = t.readInt(0x104).toLong & 0xFFFFFFFFL
    cnt shouldBe 5
  }

  "prescaler=1 increments counter every 2 ticks" in {
    val (t, _) = mkTimer()
    setARR(t, 100)
    setPSC(t, 1)
    enable(t)
    for _ <- 1 to 10 do t.tick()
    val cnt = t.readInt(0x104).toLong & 0xFFFFFFFFL
    cnt shouldBe 5
  }

  "prescaler=4 increments counter every 5 ticks" in {
    val (t, _) = mkTimer()
    setARR(t, 100)
    setPSC(t, 4)
    enable(t)
    for _ <- 1 to 25 do t.tick()
    val cnt = t.readInt(0x104).toLong & 0xFFFFFFFFL
    cnt shouldBe 5
  }

  "prescaler affects overflow timing" in {
    val (t, _) = mkTimer()
    setARR(t, 10)
    setPSC(t, 1) // counter increments every 2 ticks
    enable(t)
    for _ <- 1 to 19 do t.tick()
    t.fired shouldBe false
    t.tick() // tick 20 = counter 10 = overflow
    t.fired shouldBe true
  }

  // ===== One-shot mode =====

  "one-shot mode stops after overflow" in {
    val (t, _) = mkTimer()
    setARR(t, 5)
    t.writeByte(0x10C, 0x01) // IER
    t.writeByte(0x10A, 0x03) // CR: enable + one-shot
    for _ <- 1 to 5 do t.tick()
    t.fired shouldBe true
    t.writeByte(0x10B, 0x01) // clear overflow
    for _ <- 1 to 10 do t.tick()
    t.fired shouldBe false // should not fire again
  }

  // ===== CNT register =====

  "CNT is readable" in {
    val (t, _) = mkTimer()
    setARR(t, 100)
    enable(t)
    for _ <- 1 to 7 do t.tick()
    val cnt = t.readInt(0x104).toLong & 0xFFFFFFFFL
    cnt shouldBe 7
  }

  "CNT is writable" in {
    val (t, _) = mkTimer()
    setARR(t, 100)
    enable(t)
    // Set counter to 95
    t.writeInt(0x104, 95)
    for _ <- 1 to 5 do t.tick()
    t.fired shouldBe true // 95 + 5 = 100 = overflow
  }

  // ===== Input capture =====

  "input capture rising edge latches counter" in {
    var pin = false
    val ch = new TimerChannel(pinRead = () => pin)
    val (t, intc) = mkTimer(Seq(ch))
    setARR(t, 1000)
    setCCMR(t, 0, 1) // capture rising edge
    enableWithChannels(t, 0x01) // ch0 interrupt

    for _ <- 1 to 50 do t.tick()
    pin = true
    t.tick() // rising edge at counter=51
    // CCR should have the counter value at capture
    readCCR(t, 0) shouldBe 50 // captured before increment
    // SR channel 0 bit should be set
    (t.readByte(0x10B) & 0x02) should not be 0
  }

  "input capture falling edge latches counter" in {
    var pin = true
    val ch = new TimerChannel(pinRead = () => pin)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 1000)
    setCCMR(t, 0, 2) // capture falling edge
    enable(t)

    t.tick() // establish baseline with pin high
    for _ <- 1 to 30 do t.tick()
    pin = false
    t.tick() // falling edge
    readCCR(t, 0) shouldBe 31 // captured before increment
  }

  "input capture both edges" in {
    var pin = false
    val ch = new TimerChannel(pinRead = () => pin)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 1000)
    setCCMR(t, 0, 3) // capture both edges
    enable(t)

    for _ <- 1 to 20 do t.tick()
    pin = true
    t.tick() // rising edge
    val firstCapture = readCCR(t, 0)
    firstCapture shouldBe 20

    for _ <- 1 to 10 do t.tick()
    pin = false
    t.tick() // falling edge
    readCCR(t, 0) shouldBe 31
  }

  "input capture does not fire when mode is off" in {
    var pin = false
    val ch = new TimerChannel(pinRead = () => pin)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 1000)
    setCCMR(t, 0, 0) // disabled
    enable(t)

    for _ <- 1 to 10 do t.tick()
    pin = true
    t.tick()
    (t.readByte(0x10B) & 0x02) shouldBe 0 // no channel event
  }

  // ===== Output compare =====

  "output compare toggle" in {
    var pinState = false
    val ch = new TimerChannel(
      pinRead = () => pinState,
      pinWrite = v => pinState = v
    )
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 100)
    setCCR(t, 0, 10) // compare at 10
    setCCMR(t, 0, 4) // toggle
    enable(t)

    for _ <- 1 to 10 do t.tick()
    pinState shouldBe true // toggled from false to true
    // SR channel bit should be set
    (t.readByte(0x10B) & 0x02) should not be 0
  }

  "output compare set" in {
    var pinState = false
    val ch = new TimerChannel(pinWrite = v => pinState = v)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 100)
    setCCR(t, 0, 5)
    setCCMR(t, 0, 5) // set high
    enable(t)

    for _ <- 1 to 5 do t.tick()
    pinState shouldBe true
  }

  "output compare clear" in {
    var pinState = true
    val ch = new TimerChannel(pinWrite = v => pinState = v)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 100)
    setCCR(t, 0, 5)
    setCCMR(t, 0, 6) // clear low
    enable(t)

    for _ <- 1 to 5 do t.tick()
    pinState shouldBe false
  }

  "output compare does not fire before match" in {
    var pinState = false
    val ch = new TimerChannel(pinWrite = v => pinState = v)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 100)
    setCCR(t, 0, 10)
    setCCMR(t, 0, 5) // set
    enable(t)

    for _ <- 1 to 9 do t.tick()
    pinState shouldBe false
  }

  // ===== PWM =====

  "PWM sets pin at overflow, clears at match" in {
    var pinState = false
    val ch = new TimerChannel(pinWrite = v => pinState = v)
    val (t, _) = mkTimer(Seq(ch))
    setARR(t, 10)
    setCCR(t, 0, 3) // 30% duty cycle
    setCCMR(t, 0, 7) // PWM
    enable(t)

    // Ticks 1-3: pin high (set at overflow/start)
    // At tick 3: compare match → pin cleared
    // Ticks 4-10: pin low
    // At tick 10: overflow → pin set again
    for _ <- 1 to 2 do t.tick()
    pinState shouldBe false // PWM pin starts low, set at first overflow

    // First overflow at tick 10
    for _ <- 3 to 10 do t.tick()
    pinState shouldBe true // set at overflow

    // Ticks into second period
    for _ <- 1 to 3 do t.tick()
    pinState shouldBe false // cleared at match (cnt=3)

    for _ <- 4 to 10 do t.tick()
    pinState shouldBe true // overflow again
  }

  // ===== Multiple channels =====

  "two channels operate independently" in {
    var pin0 = false
    var pin1 = false
    val ch0 = new TimerChannel(pinWrite = v => pin0 = v)
    val ch1 = new TimerChannel(pinWrite = v => pin1 = v)
    val (t, _) = mkTimer(Seq(ch0, ch1))
    setARR(t, 100)
    setCCR(t, 0, 10)
    setCCR(t, 1, 20)
    setCCMR(t, 0, 5) // ch0: set at 10
    setCCMR(t, 1, 5) // ch1: set at 20
    enable(t)

    for _ <- 1 to 10 do t.tick()
    pin0 shouldBe true
    pin1 shouldBe false

    for _ <- 11 to 20 do t.tick()
    pin1 shouldBe true
  }

  // ===== Channel interrupt =====

  "channel compare raises interrupt when enabled in IER" in {
    val ch = new TimerChannel()
    val (t, intc) = mkTimer(Seq(ch))
    setARR(t, 100)
    setCCR(t, 0, 5)
    setCCMR(t, 0, 5) // set
    enableWithChannels(t, 0x01)

    for _ <- 1 to 5 do t.tick()
    (intc.readByte(0x200) & 0x01) should not be 0 // IRQ 0 pending
  }

  "channel compare does not raise interrupt when not in IER" in {
    val ch = new TimerChannel()
    val (t, intc) = mkTimer(Seq(ch))
    setARR(t, 100)
    setCCR(t, 0, 5)
    setCCMR(t, 0, 5) // set
    enable(t) // only overflow interrupt, not channel

    for _ <- 1 to 5 do t.tick()
    // Overflow hasn't happened, and channel interrupt not in IER
    (intc.readByte(0x200) & 0x01) shouldBe 0
  }

  // ===== Timer + GPIO composition =====

  "timer captures GPIO pin transitions" in {
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x300, 8, intc, irq = 1)
    gpio.writeByte(0x300, 0x00) // all input

    // Wire GPIO pin 0 to timer channel 0
    val ch = new TimerChannel(pinRead = () => (gpio.readByte(0x302) & 0x01) != 0)
    val timer = new Timer(0x100, intc, irq = 0, channels = Seq(ch))
    setARR(timer, 10000)
    setCCMR(timer, 0, 1) // capture rising edge
    enableWithChannels(timer, 0x01)

    for _ <- 1 to 100 do timer.tick()
    gpio.setInput(0x01) // pin goes high
    timer.tick()
    readCCR(timer, 0) shouldBe 100
  }

  "timer drives GPIO pin via output compare" in {
    val intc = new InterruptController(0x200)
    val gpio = new GPIO(0x300, 8, intc, irq = 1)
    gpio.writeByte(0x300, 0x01) // pin 0 = output

    // Wire timer channel 0 to GPIO pin 0
    val ch = new TimerChannel(
      pinRead = () => (gpio.readByte(0x302) & 0x01) != 0,
      pinWrite = v => if v then gpio.writeByte(0x303, 0x01) else gpio.writeByte(0x304, 0x01)
    )
    val timer = new Timer(0x100, intc, irq = 0, channels = Seq(ch))
    setARR(timer, 100)
    setCCR(timer, 0, 10)
    setCCMR(timer, 0, 5) // set high at match
    timer.writeByte(0x10C, 0x01)
    timer.writeByte(0x10A, 0x01)

    (gpio.readByte(0x302) & 0x01) shouldBe 0 // starts low
    for _ <- 1 to 10 do timer.tick()
    (gpio.readByte(0x302) & 0x01) shouldBe 1 // set high at match
  }
}
