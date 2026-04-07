package io.github.edadma.trisc

/**
 * MCU-style timer/counter with prescaler, auto-reload, and up to 4 capture/compare channels.
 * Inspired by STM32 general-purpose timers and RP2040 timer channels.
 *
 * The counter counts up from 0 to ARR (auto-reload register), then wraps to 0
 * and raises an overflow event. In one-shot mode, the timer stops instead of wrapping.
 * A prescaler divides the input clock: the counter increments every (PSC+1) ticks.
 *
 * Each channel can independently operate as:
 *   - Input capture: on a pin edge, the current counter value is latched into CCR
 *   - Output compare: when counter matches CCR, a pin action is taken (toggle/set/clear)
 *   - PWM: output compare with auto-clear at overflow (duty cycle = CCR/ARR)
 *
 * Channels are wired to GPIO pins via callbacks passed at construction time.
 *
 * Register map (word-aligned for struct access):
 *   +0   ARR      (4B, R/W) — auto-reload value (period)
 *   +4   CNT      (4B, R/W) — counter value
 *   +8   PSC      (2B, R/W) — prescaler (counter increments every PSC+1 ticks)
 *   +10  CR       (1B, R/W) — control: bit0=enable, bit1=one-shot, bit2=down
 *   +11  SR       (1B, R/W) — status: bit0=overflow, bits1-4=channel events (write-1-to-clear)
 *   +12  IER      (1B, R/W) — interrupt enable (bits match SR)
 *   +13-15 pad
 *
 * Per channel (4 channels × 8 bytes, starting at offset 16):
 *   +0-3  CCR     (4B, R/W) — capture/compare register
 *   +4    CCMR    (1B, R/W) — channel mode (0=off..7=PWM)
 *   +5-7  pad
 *
 * Total size: 16 + 4×8 = 48 bytes
 *
 * @param base      Base address in memory map
 * @param intc      Interrupt controller
 * @param irq       IRQ line for all timer events
 * @param channels  Up to 4 channel I/O bindings (pinRead for capture, pinWrite for compare)
 */
class Timer(val base: Long, intc: InterruptController, irq: Int, channels: Seq[TimerChannel] = Nil)
    extends Device with (CPU => Unit):
  val name = "timer"
  val size = 48

  private val numChannels = channels.size.min(4)

  // Global register offsets (word-aligned)
  private val ARR = 0   // 4 bytes
  private val CNT = 4   // 4 bytes
  private val PSC = 8   // 2 bytes
  private val CR = 10   // 1 byte
  private val SR = 11   // 1 byte
  private val IER = 12  // 1 byte
  // 13-15: padding
  private val CH_BASE = 16
  private val CH_SIZE = 8 // CCR(4) + CCMR(1) + pad(3)

  // CR bits
  private val CR_EN = 0x01
  private val CR_ONESHOT = 0x02
  private val CR_DOWN = 0x04

  // Channel modes
  private val MODE_OFF = 0
  private val MODE_CAP_RISE = 1
  private val MODE_CAP_FALL = 2
  private val MODE_CAP_BOTH = 3
  private val MODE_CMP_TOGGLE = 4
  private val MODE_CMP_SET = 5
  private val MODE_CMP_CLEAR = 6
  private val MODE_PWM = 7

  // State
  private var psc: Int = 0
  private var arr: Long = 0
  private var cnt: Long = 0
  private var cr: Int = 0
  private var sr: Int = 0
  private var ier: Int = 0
  private var prescaleCounter: Int = 0

  // Per-channel state
  private val ccr = new Array[Long](4)
  private val ccmr = new Array[Int](4)
  private val prevPin = new Array[Boolean](4)

  // ===== Backward compatibility =====

  var running: Boolean = false
  var fired: Boolean = false
  def period: Long = arr
  def period_=(v: Long): Unit = arr = v
  def tick(): Unit = apply(null)

  // ===== Register access =====

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    off match
      case 0  => ((arr >> 24) & 0xFF).toInt
      case 1  => ((arr >> 16) & 0xFF).toInt
      case 2  => ((arr >> 8) & 0xFF).toInt
      case 3  => (arr & 0xFF).toInt
      case 4  => ((cnt >> 24) & 0xFF).toInt
      case 5  => ((cnt >> 16) & 0xFF).toInt
      case 6  => ((cnt >> 8) & 0xFF).toInt
      case 7  => (cnt & 0xFF).toInt
      case 8  => (psc >> 8) & 0xFF
      case 9  => psc & 0xFF
      case 10 => cr
      case 11 => sr
      case 12 => ier
      case n if n >= CH_BASE && n < CH_BASE + 4 * CH_SIZE =>
        val ch = (n - CH_BASE) / CH_SIZE
        val field = (n - CH_BASE) % CH_SIZE
        field match
          case 0 => ((ccr(ch) >> 24) & 0xFF).toInt
          case 1 => ((ccr(ch) >> 16) & 0xFF).toInt
          case 2 => ((ccr(ch) >> 8) & 0xFF).toInt
          case 3 => (ccr(ch) & 0xFF).toInt
          case 4 => ccmr(ch)
          case _ => 0
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    val d = (data & 0xFF).toInt
    val off = (addr - base).toInt
    off match
      case 0  => arr = (arr & 0x00FFFFFFL) | ((d.toLong & 0xFF) << 24)
      case 1  => arr = (arr & 0xFF00FFFFL) | ((d.toLong & 0xFF) << 16)
      case 2  => arr = (arr & 0xFFFF00FFL) | ((d.toLong & 0xFF) << 8)
      case 3  => arr = (arr & 0xFFFFFF00L) | (d.toLong & 0xFF)
      case 4  => cnt = (cnt & 0x00FFFFFFL) | ((d.toLong & 0xFF) << 24)
      case 5  => cnt = (cnt & 0xFF00FFFFL) | ((d.toLong & 0xFF) << 16)
      case 6  => cnt = (cnt & 0xFFFF00FFL) | ((d.toLong & 0xFF) << 8)
      case 7  => cnt = (cnt & 0xFFFFFF00L) | (d.toLong & 0xFF)
      case 8  => psc = (psc & 0x00FF) | (d << 8)
      case 9  => psc = (psc & 0xFF00) | d
      case CR =>
        cr = d
        val nowEnabled = (cr & CR_EN) != 0
        running = nowEnabled
        if nowEnabled then
          cnt = 0
          prescaleCounter = 0
          fired = false
          sr = 0
      case SR =>
        sr &= ~d // write-1-to-clear
        if (sr & 0x01) == 0 then
          fired = false
          intc.lower(irq)
      case IER => ier = d
      case n if n >= CH_BASE && n < CH_BASE + 4 * CH_SIZE =>
        val ch = (n - CH_BASE) / CH_SIZE
        val field = (n - CH_BASE) % CH_SIZE
        field match
          case 0 => ccr(ch) = (ccr(ch) & 0x00FFFFFFL) | ((d.toLong & 0xFF) << 24)
          case 1 => ccr(ch) = (ccr(ch) & 0xFF00FFFFL) | ((d.toLong & 0xFF) << 16)
          case 2 => ccr(ch) = (ccr(ch) & 0xFFFF00FFL) | ((d.toLong & 0xFF) << 8)
          case 3 => ccr(ch) = (ccr(ch) & 0xFFFFFF00L) | (d.toLong & 0xFF)
          case 4 => ccmr(ch) = d
          case _ =>
      case _ =>

  // Word-level access for struct pointer writes
  override def writeInt(address: Long, data: Long): Unit =
    val off = (address - base).toInt
    val v = data.toInt
    off match
      case ARR => arr = v & 0xFFFFFFFFL
      case CNT => cnt = v & 0xFFFFFFFFL
      case n if n >= CH_BASE && (n - CH_BASE) % CH_SIZE == 0 =>
        val ch = (n - CH_BASE) / CH_SIZE
        if ch < 4 then ccr(ch) = v & 0xFFFFFFFFL
      case _ => super.writeInt(address, data)

  override def readInt(address: Long): Int =
    val off = (address - base).toInt
    off match
      case ARR => arr.toInt
      case CNT => cnt.toInt
      case n if n >= CH_BASE && (n - CH_BASE) % CH_SIZE == 0 =>
        val ch = (n - CH_BASE) / CH_SIZE
        if ch < 4 then ccr(ch).toInt else 0
      case _ => super.readInt(address)

  override def writeShort(address: Long, data: Long): Unit =
    val off = (address - base).toInt
    off match
      case PSC => psc = data.toInt & 0xFFFF
      case _ => super.writeShort(address, data)

  override def readShort(address: Long): Int =
    val off = (address - base).toInt
    off match
      case PSC => psc
      case _ => super.readShort(address)

  // ===== Tick =====

  def apply(cpu: CPU): Unit =
    if (cr & CR_EN) == 0 then return

    // Prescaler
    prescaleCounter += 1
    if prescaleCounter <= psc then return
    prescaleCounter = 0

    // Check capture/compare channels BEFORE incrementing counter
    processChannels()

    // Increment counter
    cnt += 1

    // Check compare matches AFTER incrementing
    for ch <- 0 until numChannels do
      val mode = ccmr(ch)
      if mode >= MODE_CMP_TOGGLE && mode <= MODE_PWM then
        if cnt == ccr(ch) then
          sr |= (1 << (ch + 1))
          if ch < channels.size then
            mode match
              case MODE_CMP_TOGGLE => channels(ch).pinWrite(!channels(ch).pinRead())
              case MODE_CMP_SET    => channels(ch).pinWrite(true)
              case MODE_CMP_CLEAR  => channels(ch).pinWrite(false)
              case MODE_PWM        => channels(ch).pinWrite(false) // clear on match
              case _               =>
          if (ier & (1 << (ch + 1))) != 0 then intc.raise(irq)

    // Overflow check
    if arr > 0 && cnt >= arr then
      cnt = 0
      sr |= 0x01
      fired = true
      // PWM channels: set pin at overflow (start of period)
      for ch <- 0 until numChannels do
        if ccmr(ch) == MODE_PWM && ch < channels.size then
          channels(ch).pinWrite(true)
      if (cr & CR_ONESHOT) != 0 then
        cr &= ~CR_EN
        running = false
      if (ier & 0x01) != 0 then intc.raise(irq)

  private def processChannels(): Unit =
    for ch <- 0 until numChannels do
      val mode = ccmr(ch)
      if mode >= MODE_CAP_RISE && mode <= MODE_CAP_BOTH && ch < channels.size then
        val pin = channels(ch).pinRead()
        val prev = prevPin(ch)
        val rising = pin && !prev
        val falling = !pin && prev
        val capture = mode match
          case MODE_CAP_RISE => rising
          case MODE_CAP_FALL => falling
          case MODE_CAP_BOTH => rising || falling
          case _             => false
        if capture then
          ccr(ch) = cnt
          sr |= (1 << (ch + 1))
          if (ier & (1 << (ch + 1))) != 0 then intc.raise(irq)
        prevPin(ch) = pin

/**
 * Channel I/O binding for connecting timer channels to GPIO pins or other signals.
 * pinRead returns current pin state; pinWrite drives the pin.
 */
class TimerChannel(val pinRead: () => Boolean = () => false, val pinWrite: Boolean => Unit = _ => ())
