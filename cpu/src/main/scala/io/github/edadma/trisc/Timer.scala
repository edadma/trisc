package io.github.edadma.trisc

class Timer(val base: Long, intc: InterruptController, irq: Int)
    extends Device with (CPU => Unit):
  val name = "timer"
  val size = 6

  private val PERIOD = 0 // 4 bytes, W
  private val CONTROL = 4 // 1 byte, W
  private val STATUS = 5 // 1 byte, R/W

  var period: Long = 0
  var running: Boolean = false
  var fired: Boolean = false
  private var counter: Long = 0

  def readByte(addr: Long): Int =
    addr - base match
      case STATUS => if fired then 1 else 0
      case _      => 0

  def writeByte(addr: Long, data: Long): Unit =
    addr - base match
      case 0 => period = (period & 0x00ffffffL) | ((data & 0xff) << 24)
      case 1 => period = (period & 0xff00ffffL) | ((data & 0xff) << 16)
      case 2 => period = (period & 0xffff00ffL) | ((data & 0xff) << 8)
      case 3 => period = (period & 0xffffff00L) | (data & 0xff)
      case CONTROL =>
        running = data != 0
        if running then
          counter = 0
          fired = false
      case STATUS =>
        fired = false // acknowledge
        intc.lower(irq)
      case _ =>

  def tick(): Unit =
    if running && period > 0 then
      counter += 1
      if counter >= period then
        counter = 0
        fired = true
        intc.raise(irq)

  def apply(cpu: CPU): Unit = tick()
