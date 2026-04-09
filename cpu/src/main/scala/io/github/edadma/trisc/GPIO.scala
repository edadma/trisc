package io.github.edadma.trisc

class GPIO(val base: Long, width: Int, intc: InterruptController, irq: Int, onChange: Int => Unit = _ => ())
    extends Device with (Processor => Unit):
  require(width >= 1 && width <= 8, "GPIO width must be 1-8 pins")
  val name = "GPIO"
  val size = 10

  private val mask = (1 << width) - 1

  // Register offsets
  private val DDR = 0        // 1 byte, R/W — direction: 0=input, 1=output
  private val OUT = 1        // 1 byte, R/W — output latch
  private val IN = 2         // 1 byte, R   — (output pins from OUT) | (input pins from host)
  private val SET = 3        // 1 byte, W   — atomic set: OUT |= data
  private val CLEAR = 4      // 1 byte, W   — atomic clear: OUT &= ~data
  private val XOR = 5        // 1 byte, W   — atomic toggle: OUT ^= data
  private val INT_MASK = 6   // 1 byte, R/W — which pins can generate interrupts
  private val INT_STATUS = 7 // 1 byte, R/W — pending interrupts (write-1-to-clear)
  private val INT_MODE = 8   // 1 byte, R/W — 0=level, 1=edge per pin
  private val INT_POL = 9    // 1 byte, R/W — 0=low/falling, 1=high/rising per pin

  var ddr: Int = 0
  var out: Int = 0
  private var input: Int = 0
  var intMask: Int = 0
  var intStatus: Int = 0
  var intMode: Int = 0
  var intPolarity: Int = 0
  private var previousInput: Int = 0

  /** Host calls this to drive input pins. Only bits where DDR=0 (input) are visible. */
  def setInput(pins: Int): Unit =
    input = pins & mask

  /** Current pin state: output pins from OUT, input pins from host. */
  private def pinState: Int =
    ((out & ddr) | (input & ~ddr)) & mask

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case DDR        => ddr
      case OUT        => out
      case IN         => pinState
      case INT_MASK   => intMask
      case INT_STATUS => intStatus
      case INT_MODE   => intMode
      case INT_POL    => intPolarity
      case _          => 0

  def writeByte(addr: Long, data: Long): Unit =
    val d = data.toInt & mask
    (addr - base).toInt match
      case DDR => ddr = d
      case OUT =>
        out = d
        onChange(pinState)
      case SET =>
        out = (out | d) & mask
        onChange(pinState)
      case CLEAR =>
        out = (out & ~d) & mask
        onChange(pinState)
      case XOR =>
        out = (out ^ d) & mask
        onChange(pinState)
      case INT_MASK   => intMask = d
      case INT_STATUS => intStatus &= ~d // write-1-to-clear
      case INT_MODE   => intMode = d
      case INT_POL    => intPolarity = d
      case _          =>

  def apply(cpu: Processor): Unit =
    val current = pinState
    val changed = current ^ previousInput

    // Edge-triggered: detect transitions matching polarity
    val rising = changed & current           // was 0, now 1
    val falling = changed & ~current & mask  // was 1, now 0
    val edgeHits = (rising & intPolarity) | (falling & ~intPolarity & mask)
    val edgeActive = edgeHits & intMode & intMask

    // Level-triggered: pin matches polarity right now
    val levelHigh = current & intPolarity
    val levelLow = ~current & ~intPolarity & mask
    val levelHits = levelHigh | levelLow
    val levelActive = levelHits & ~intMode & intMask

    val newInterrupts = (edgeActive | levelActive) & mask
    if newInterrupts != 0 then
      intStatus = (intStatus | newInterrupts) & mask
      intc.raise(irq)

    previousInput = current
