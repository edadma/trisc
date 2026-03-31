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
 * Register map:
 *   0-1:   PSC      (R/W) 16-bit prescaler value (counter increments every PSC+1 ticks)
 *   2-5:   ARR      (R/W) 32-bit auto-reload value (period)
 *   6-9:   CNT      (R/W) 32-bit counter value
 *   10:    CR       (R/W) control — bit 0: enable, bit 1: one-shot, bit 2: count direction (0=up)
 *   11:    SR       (R/W) status — bit 0: overflow, bits 1-4: channel 0-3 event (write-1-to-clear)
 *   12:    IER      (R/W) interrupt enable — bits match SR
 *
 * Per channel (4 channels × 5 bytes, starting at offset 13):
 *   +0 to +3: CCR   (R/W) 32-bit capture/compare register
 *   +4:       CCMR  (R/W) channel mode:
 *     0 = disabled
 *     1 = input capture, rising edge
 *     2 = input capture, falling edge
 *     3 = input capture, both edges
 *     4 = output compare, toggle pin
 *     5 = output compare, set pin high
 *     6 = output compare, clear pin low
 *     7 = PWM (set at start, clear on match, reset on overflow)
 *
 * Total size: 13 + 4×5 = 33 bytes
 *
 * @param base      Base address in memory map
 * @param intc      Interrupt controller
 * @param irq       IRQ line for all timer events
 * @param channels  Up to 4 channel I/O bindings (pinRead for capture, pinWrite for compare)
 */
class Timer(val base: Long, intc: InterruptController, irq: Int, channels: Seq[TimerChannel] = Nil)
    extends Device with (CPU => Unit):
  val name = "timer"
  val size = 33

  private val numChannels = channels.size.min(4)

  // Global register offsets
  private val PSC = 0  // 2 bytes
  private val ARR = 2  // 4 bytes
  private val CNT = 6  // 4 bytes
  private val CR = 10  // 1 byte
  private val SR = 11  // 1 byte
  private val IER = 12 // 1 byte
  private val CH_BASE = 13
  private val CH_SIZE = 5

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

  /** Expose running state for existing tests. */
  var running: Boolean = false
  /** Expose fired state for existing tests. */
  var fired: Boolean = false
  /** Expose period for existing tests. */
  def period: Long = arr
  def period_=(v: Long): Unit = arr = v

  /** Direct tick for existing tests that call timer.tick() manually. */
  def tick(): Unit = apply(null)

  // ===== Register access =====

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    off match
      case 0  => (psc >> 8) & 0xFF
      case 1  => psc & 0xFF
      case 2  => ((arr >> 24) & 0xFF).toInt
      case 3  => ((arr >> 16) & 0xFF).toInt
      case 4  => ((arr >> 8) & 0xFF).toInt
      case 5  => (arr & 0xFF).toInt
      case 6  => ((cnt >> 24) & 0xFF).toInt
      case 7  => ((cnt >> 16) & 0xFF).toInt
      case 8  => ((cnt >> 8) & 0xFF).toInt
      case 9  => (cnt & 0xFF).toInt
      case CR => cr
      case SR => sr
      case IER => ier
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
      case 0  => psc = (psc & 0x00FF) | (d << 8)
      case 1  => psc = (psc & 0xFF00) | d
      case 2  => arr = (arr & 0x00FFFFFFL) | ((d.toLong & 0xFF) << 24)
      case 3  => arr = (arr & 0xFF00FFFFL) | ((d.toLong & 0xFF) << 16)
      case 4  => arr = (arr & 0xFFFF00FFL) | ((d.toLong & 0xFF) << 8)
      case 5  => arr = (arr & 0xFFFFFF00L) | (d.toLong & 0xFF)
      case 6  => cnt = (cnt & 0x00FFFFFFL) | ((d.toLong & 0xFF) << 24)
      case 7  => cnt = (cnt & 0xFF00FFFFL) | ((d.toLong & 0xFF) << 16)
      case 8  => cnt = (cnt & 0xFFFF00FFL) | ((d.toLong & 0xFF) << 8)
      case 9  => cnt = (cnt & 0xFFFFFF00L) | (d.toLong & 0xFF)
      case CR =>
        cr = d
        val nowEnabled = (cr & CR_EN) != 0
        running = nowEnabled
        if nowEnabled then
          // Enable/re-enable: reset counter and prescaler
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
