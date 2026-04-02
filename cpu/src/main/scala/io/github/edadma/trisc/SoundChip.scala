package io.github.edadma.trisc

/**
 * Additive synthesis sound chip with 256 sine wave channels and ADSR envelopes.
 *
 * Each channel is an independent sine wave oscillator with its own frequency,
 * amplitude, and envelope. The host mixes all active channels. Multiple channels
 * at harmonic frequencies create complex timbres (piano, guitar, bell, etc.).
 *
 * Envelope behavior:
 *   - Trigger ON: Attack (ramp to peak) → Decay (fall to sustain level)
 *   - If SUSTAIN > 0: holds at sustain level until trigger OFF → Release (fade to 0)
 *   - If SUSTAIN = 0: one-shot — Decay continues to 0, no hold (plucked string)
 *
 * Register map per channel (8 bytes × 256 channels = 2048 bytes):
 *   +0-1: FREQ     (R/W) — frequency in Hz, 16-bit big-endian
 *   +2:   AMP      (R/W) — peak amplitude 0-255 (0 = channel inactive)
 *   +3:   ATTACK   (R/W) — attack time in ms (0-255, 0 = instant)
 *   +4:   DECAY    (R/W) — decay time in ms (0-255)
 *   +5:   SUSTAIN  (R/W) — sustain level 0-255 (0 = one-shot, no hold)
 *   +6:   RELEASE  (R/W) — release time in ms (0-255)
 *   +7:   reserved
 *
 * Global registers (offset 2048):
 *   2048:    MASTER    (R/W) — master volume 0-255
 *   2049:    TRIGGER   (W)   — write 1: start all channels with AMP > 0
 *                               write 2: release all triggered channels
 *   2050-2081: CH_TRIG (R/W) — 256-bit trigger state (32 bytes, 1 bit per channel)
 *                               Read: current trigger state
 *                               Write: set bits = trigger ON, cleared by writing to RELEASE_MASK
 *   2082-2113: CH_REL  (W)   — 256-bit release mask (32 bytes, 1 bit per channel)
 *                               Write: set bits = begin release for those channels
 *
 * Total size: 2114 bytes
 *
 * Usage pattern for a piano chord:
 *   1. Set up channels 0-7 for note C (fundamental + harmonics)
 *   2. Set up channels 8-15 for note E
 *   3. Set up channels 16-23 for note G
 *   4. Write 1 to TRIGGER — all start simultaneously
 *   5. Later, write release mask to release specific notes
 *
 * @param base     Base address in memory map
 * @param onUpdate Called on any register write; host audio thread polls getChannels()
 */
class SoundChip(val base: Long, onUpdate: () => Unit = () => ()) extends Device:
  val name = "SoundChip"
  val size = 2114

  private val NUM_CHANNELS = 256
  private val CH_SIZE = 8
  private val CH_AREA = NUM_CHANNELS * CH_SIZE // 2048
  private val MASTER_OFF = CH_AREA            // 2048
  private val TRIGGER_OFF = CH_AREA + 1       // 2049
  private val CH_TRIG_OFF = CH_AREA + 2       // 2050-2081
  private val CH_REL_OFF = CH_AREA + 34       // 2082-2113

  // Per-channel registers
  private val freq = new Array[Int](NUM_CHANNELS)
  private val amp = new Array[Int](NUM_CHANNELS)
  private val attack = new Array[Int](NUM_CHANNELS)
  private val decay = new Array[Int](NUM_CHANNELS)
  private val sustain = new Array[Int](NUM_CHANNELS)
  private val release = new Array[Int](NUM_CHANNELS)

  // Trigger state: 256 bits = 32 bytes
  private val trig = new Array[Byte](32)

  private var master: Int = 255

  private def isTrig(ch: Int): Boolean =
    (trig(ch / 8) & (1 << (ch % 8))) != 0

  private def setTrig(ch: Int): Unit =
    trig(ch / 8) = (trig(ch / 8) | (1 << (ch % 8))).toByte

  private def clearTrig(ch: Int): Unit =
    trig(ch / 8) = (trig(ch / 8) & ~(1 << (ch % 8))).toByte

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    if off >= CH_TRIG_OFF && off < CH_TRIG_OFF + 32 then
      return trig(off - CH_TRIG_OFF) & 0xFF
    if off == MASTER_OFF then return master
    if off >= 0 && off < CH_AREA then
      val ch = off / CH_SIZE
      val field = off % CH_SIZE
      return field match
        case 0 => (freq(ch) >> 8) & 0xFF
        case 1 => freq(ch) & 0xFF
        case 2 => amp(ch)
        case 3 => attack(ch)
        case 4 => decay(ch)
        case 5 => sustain(ch)
        case 6 => release(ch)
        case _ => 0
    0

  def writeByte(addr: Long, data: Long): Unit =
    val d = (data & 0xFF).toInt
    val off = (addr - base).toInt

    if off == MASTER_OFF then
      master = d
      onUpdate()
      return

    if off == TRIGGER_OFF then
      if d == 1 then
        // Trigger all channels with AMP > 0
        for ch <- 0 until NUM_CHANNELS do
          if amp(ch) > 0 then setTrig(ch)
      else if d == 2 then
        // Release all triggered channels
        for ch <- 0 until NUM_CHANNELS do
          clearTrig(ch)
      onUpdate()
      return

    if off >= CH_TRIG_OFF && off < CH_TRIG_OFF + 32 then
      // Set trigger bits directly
      val byteIdx = off - CH_TRIG_OFF
      trig(byteIdx) = (trig(byteIdx) | d).toByte
      onUpdate()
      return

    if off >= CH_REL_OFF && off < CH_REL_OFF + 32 then
      // Release: clear trigger bits for set bits in data
      val byteIdx = off - CH_REL_OFF
      trig(byteIdx) = (trig(byteIdx) & ~d).toByte
      onUpdate()
      return

    if off >= 0 && off < CH_AREA then
      val ch = off / CH_SIZE
      val field = off % CH_SIZE
      field match
        case 0 => freq(ch) = (freq(ch) & 0x00FF) | (d << 8)
        case 1 => freq(ch) = (freq(ch) & 0xFF00) | d
        case 2 => amp(ch) = d
        case 3 => attack(ch) = d
        case 4 => decay(ch) = d
        case 5 => sustain(ch) = d
        case 6 => release(ch) = d
        case _ => return
      onUpdate()

  /** Get current state of a channel. Thread-safe snapshot. */
  def getChannel(ch: Int): SoundChannel =
    if ch < 0 || ch >= NUM_CHANNELS then SoundChannel(0, 0, 0, 0, 0, 0, false)
    else SoundChannel(freq(ch), amp(ch), attack(ch), decay(ch), sustain(ch), release(ch), isTrig(ch))

  /** Get all active channels (for host audio mixer). */
  def getActiveChannels: Seq[(Int, SoundChannel)] =
    (0 until NUM_CHANNELS).flatMap { ch =>
      if isTrig(ch) && amp(ch) > 0 && freq(ch) > 0 then
        Some((ch, SoundChannel(freq(ch), amp(ch), attack(ch), decay(ch), sustain(ch), release(ch), true)))
      else None
    }

  /** Master volume 0-255. */
  def masterVolume: Int = master

  /** Number of channels. */
  def numChannels: Int = NUM_CHANNELS

/**
 * Snapshot of a single sound channel's state.
 */
case class SoundChannel(
    freq: Int,        // Hz
    amp: Int,         // 0-255 peak amplitude
    attack: Int,      // ms to reach peak
    decay: Int,       // ms from peak to sustain (or to 0 if one-shot)
    sustain: Int,     // 0-255 sustain level (0 = one-shot)
    release: Int,     // ms to fade after release
    triggered: Boolean,
):
  def isActive: Boolean = triggered && amp > 0 && freq > 0
  def isOneShot: Boolean = sustain == 0
