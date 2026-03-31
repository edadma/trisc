package io.github.edadma.trisc

import io.github.edadma.logger._

class InterruptController(val base: Long) extends Device with (CPU => Unit):
  val name = "InterruptController"
  val size = 4

  private val PENDING = 0
  private val ENABLED = 1
  private val CLAIM = 2
  private val ACK = 3

  private var pending: Int = 0
  private var enabled: Int = 0xff // all sources enabled by default
  private var delivered: Int = 0 // IRQs signaled to CPU but not yet claimed
  val log: Logger = {
    val l = new Logger(new ConsoleHandler, new DefaultLogFormatter(includeTimestamp = false))
    l.setLogLevel(LogLevel.OFF)
    l
  }

  def raise(irq: Int): Unit =
    pending |= (1 << irq)
    delivered &= ~(1 << irq)
    log.trace(f"raise IRQ $irq — pending=$pending%02x delivered=$delivered%02x", category = "INTC")

  def lower(irq: Int): Unit =
    pending &= ~(1 << irq)
    log.trace(f"lower IRQ $irq — pending=$pending%02x", category = "INTC")

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case PENDING => pending & 0xff
      case ENABLED => enabled & 0xff
      case CLAIM =>
        val active = pending & enabled
        if active == 0 then 0xff
        else
          val irq = Integer.numberOfTrailingZeros(active)
          pending &= ~(1 << irq)
          delivered &= ~(1 << irq)
          irq
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    (addr - base).toInt match
      case ENABLED => enabled = data.toInt & 0xff
      case ACK =>
        val irq = data.toInt & 7
        pending &= ~(1 << irq)
        delivered &= ~(1 << irq)
      case _ =>

  def apply(cpu: CPU): Unit =
    val active = (pending & enabled) & ~delivered
    if active != 0 then
      delivered |= active
      cpu.interrupt()
