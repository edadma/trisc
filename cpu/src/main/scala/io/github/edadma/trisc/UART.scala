package io.github.edadma.trisc

import scala.collection.mutable

/**
 * UART device with TX/RX FIFOs, baud rate divider, and interrupt support.
 *
 * Register map (8 bytes):
 *   0: TX_DATA  (W)  — write byte to TX FIFO
 *   1: RX_DATA  (R)  — read byte from RX FIFO
 *   2: STATUS   (R)  — bit 0: TX empty, bit 1: RX ready, bit 2: TX full, bit 3: overrun
 *   3: CONTROL  (R/W) — bit 0: TX interrupt enable, bit 1: RX interrupt enable, bit 2: enable
 *   4-7: DIVISOR (R/W) — 32-bit baud rate divisor (ticks per byte), big-endian
 *
 * TX: CPU writes to TX_DATA → byte enters TX FIFO. Each tick decrements a counter;
 * when it reaches zero, the next byte is shifted out (onTx callback) and the counter reloads.
 * TX-empty interrupt fires when FIFO becomes empty after a transmission.
 *
 * RX: Host calls receive() → byte enters RX FIFO. RX-ready interrupt fires when FIFO
 * goes from empty to non-empty. Overrun flag set if FIFO is full on receive.
 *
 * @param base     Base address in memory map
 * @param intc     Interrupt controller
 * @param txIrq    IRQ line for TX events
 * @param rxIrq    IRQ line for RX events
 * @param onTx     Callback when a byte is transmitted
 * @param fifoDepth Maximum FIFO depth (default 16)
 */
class UART(val base: Long, intc: InterruptController, txIrq: Int, rxIrq: Int, onTx: Int => Unit = _ => (), fifoDepth: Int = 16)
    extends Device with (CPU => Unit):
  val name = "UART"
  val size = 8

  // Register offsets
  private val TX_DATA = 0
  private val RX_DATA = 1
  private val STATUS = 2
  private val CONTROL = 3
  private val DIVISOR = 4 // 4 bytes, big-endian

  // Status bits
  private val ST_TX_EMPTY = 0x01
  private val ST_RX_READY = 0x02
  private val ST_TX_FULL = 0x04
  private val ST_OVERRUN = 0x08

  // Control bits
  private val CTL_TX_IE = 0x01
  private val CTL_RX_IE = 0x02
  private val CTL_ENABLE = 0x04

  // State
  private val txFifo = new mutable.Queue[Int]()
  private val rxFifo = new mutable.Queue[Int]()
  private var control: Int = 0
  private var divisor: Long = 1
  private var txCounter: Long = 0
  private var overrun: Boolean = false

  /** Host calls this to push a byte into the RX FIFO. */
  def receive(byte: Int): Unit =
    if rxFifo.size >= fifoDepth then
      overrun = true
    else
      val wasEmpty = rxFifo.isEmpty
      rxFifo.enqueue(byte & 0xFF)
      if wasEmpty && (control & CTL_RX_IE) != 0 && (control & CTL_ENABLE) != 0 then
        intc.raise(rxIrq)

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case RX_DATA =>
        if rxFifo.isEmpty then 0
        else
          val b = rxFifo.dequeue()
          if rxFifo.isEmpty then intc.lower(rxIrq)
          b
      case STATUS =>
        var s = 0
        if txFifo.isEmpty then s |= ST_TX_EMPTY
        if rxFifo.nonEmpty then s |= ST_RX_READY
        if txFifo.size >= fifoDepth then s |= ST_TX_FULL
        if overrun then s |= ST_OVERRUN
        s
      case CONTROL => control
      case 4 => ((divisor >> 24) & 0xFF).toInt
      case 5 => ((divisor >> 16) & 0xFF).toInt
      case 6 => ((divisor >> 8) & 0xFF).toInt
      case 7 => (divisor & 0xFF).toInt
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    val d = (data & 0xFF).toInt
    (addr - base).toInt match
      case TX_DATA =>
        if txFifo.size < fifoDepth then
          txFifo.enqueue(d)
          // If this is the first byte and counter is idle, start it
          if txFifo.size == 1 && txCounter == 0 then
            txCounter = divisor
      case STATUS =>
        overrun = false // write to clear overrun
      case CONTROL =>
        control = d
      case 4 => divisor = (divisor & 0x00FFFFFFL) | ((d.toLong & 0xFF) << 24)
      case 5 => divisor = (divisor & 0xFF00FFFFL) | ((d.toLong & 0xFF) << 16)
      case 6 => divisor = (divisor & 0xFFFF00FFL) | ((d.toLong & 0xFF) << 8)
      case 7 => divisor = (divisor & 0xFFFFFF00L) | (d.toLong & 0xFF)
      case _ =>

  def apply(cpu: CPU): Unit =
    if (control & CTL_ENABLE) == 0 then return

    // TX: count down and shift out
    if txFifo.nonEmpty then
      txCounter -= 1
      if txCounter <= 0 then
        val b = txFifo.dequeue()
        onTx(b)
        if txFifo.nonEmpty then
          txCounter = divisor
        else
          txCounter = 0
          if (control & CTL_TX_IE) != 0 then
            intc.raise(txIrq)
