package io.github.edadma.trisc

class InterruptController(val base: Long) extends Device with (CPU => Unit):
  val name = "InterruptController"
  val size = 4

  private val PENDING = 0
  private val ENABLED = 1
  private val CLAIM = 2
  private val ACK = 3

  private var pending: Int = 0
  private var enabled: Int = 0xff // all sources enabled by default
  private var tickables: List[() => Unit] = Nil

  def raise(irq: Int): Unit = pending |= (1 << irq)

  def lower(irq: Int): Unit = pending &= ~(1 << irq)

  def addTickable(tick: () => Unit): Unit = tickables = tick :: tickables

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case PENDING => pending & 0xff
      case ENABLED => enabled & 0xff
      case CLAIM =>
        val active = pending & enabled
        if active == 0 then 0xff
        else
          val irq = Integer.numberOfTrailingZeros(active)
          pending &= ~(1 << irq) // auto-clear on claim (like RISC-V PLIC)
          irq
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    (addr - base).toInt match
      case ENABLED => enabled = data.toInt & 0xff
      case ACK =>
        val irq = data.toInt & 7
        pending &= ~(1 << irq)
      case _ =>

  def apply(cpu: CPU): Unit =
    tickables.foreach(_())
    val active = pending & enabled
    if active != 0 then
      pending &= ~active // edge-triggered: clear on delivery
      cpu.interrupt()
