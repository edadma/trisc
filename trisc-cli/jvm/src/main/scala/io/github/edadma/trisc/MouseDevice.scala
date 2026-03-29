package io.github.edadma.trisc

class MouseDevice(val base: Long, intc: InterruptController, irq: Int) extends Device:
  val name = "Mouse"
  val size = 6

  private val STATUS = 0
  private val BUTTONS = 1
  private val X_HI = 2
  private val X_LO = 3
  private val Y_HI = 4
  private val Y_LO = 5

  @volatile private var ready: Boolean = false
  @volatile private var buttons: Int = 0
  @volatile private var x: Int = 0
  @volatile private var y: Int = 0

  def update(newX: Int, newY: Int, newButtons: Int): Unit =
    x = newX
    y = newY
    buttons = newButtons
    ready = true
    intc.raise(irq)

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case STATUS => if ready then 1 else 0
      case BUTTONS =>
        val b = buttons
        ready = false
        intc.lower(irq)
        b
      case X_HI => (x >> 8) & 0xff
      case X_LO => x & 0xff
      case Y_HI => (y >> 8) & 0xff
      case Y_LO => y & 0xff
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit = () // read-only device
