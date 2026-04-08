package io.github.edadma.trisc

import scala.collection.mutable

/**
 * ARM PL011 UART — register-compatible with QEMU's virt machine UART.
 *
 * This device can be used both in the TRISC emulator (for development)
 * and eventually replaced by real PL011 hardware when running under QEMU.
 * Software written against this register interface will work on both.
 *
 * Register map (4KB region, standard PL011 offsets):
 *   0x00  UARTDR     (R/W) — Data register (TX write / RX read)
 *   0x04  UARTRSR    (R/W) — Receive status / error clear (write any to clear)
 *   0x18  UARTFR     (R)   — Flag register
 *   0x24  UARTIBRD   (R/W) — Integer baud rate divisor (16-bit)
 *   0x28  UARTFBRD   (R/W) — Fractional baud rate divisor (6-bit)
 *   0x2C  UARTLCR_H  (R/W) — Line control register
 *   0x30  UARTCR     (R/W) — Control register
 *   0x34  UARTIFLS   (R/W) — Interrupt FIFO level select
 *   0x38  UARTIMSC   (R/W) — Interrupt mask set/clear
 *   0x3C  UARTRIS    (R)   — Raw interrupt status
 *   0x40  UARTMIS    (R)   — Masked interrupt status (RIS & IMSC)
 *   0x44  UARTICR    (W)   — Interrupt clear (write-1-to-clear)
 *
 * Flag register (UARTFR) bits:
 *   bit 7: TXFE (TX FIFO empty)
 *   bit 5: TXFF (TX FIFO full)
 *   bit 4: RXFE (RX FIFO empty)
 *   bit 6: RXFF (RX FIFO full)
 *   bit 3: BUSY (transmitter busy)
 *
 * Control register (UARTCR) bits:
 *   bit 0: UARTEN (UART enable)
 *   bit 8: TXE (transmit enable)
 *   bit 9: RXE (receive enable)
 *
 * Interrupt bits (shared across IMSC/RIS/MIS/ICR):
 *   bit 4: RXIM (receive interrupt)
 *   bit 5: TXIM (transmit interrupt)
 *   bit 6: RTIM (receive timeout interrupt)
 *
 * @param base      Base address (standard: 0x09000000 for QEMU virt)
 * @param intc      Interrupt controller
 * @param irq       IRQ line
 * @param onTx      Callback when a byte is transmitted
 * @param fifoDepth FIFO depth (standard PL011 = 16)
 */
class PL011(val base: Long, intc: InterruptController, irq: Int, onTx: Int => Unit = _ => (), fifoDepth: Int = 16)
    extends Device with (Processor => Unit):
  val name = "PL011"
  val size = 0x1000 // 4KB region as per ARM spec

  private val txFifo = new mutable.Queue[Int]()
  private val rxFifo = new mutable.Queue[Int]()

  // Registers
  private var rsr: Int = 0        // receive status register
  private var ibrd: Int = 0       // integer baud rate divisor
  private var fbrd: Int = 0       // fractional baud rate divisor
  private var lcr_h: Int = 0      // line control
  private var cr: Int = 0x0300    // control: TXE and RXE enabled by default
  private var ifls: Int = 0x12    // interrupt FIFO level: 1/2 full
  private var imsc: Int = 0       // interrupt mask
  private var ris: Int = 0        // raw interrupt status

  // Flag bits
  private val TXFE = 0x80  // TX FIFO empty
  private val RXFF = 0x40  // RX FIFO full
  private val TXFF = 0x20  // TX FIFO full
  private val RXFE = 0x10  // RX FIFO empty
  private val BUSY = 0x08  // transmitter busy

  // Control bits
  private val UARTEN = 0x01
  private val TXE = 0x100
  private val RXE = 0x200

  // Interrupt bits
  private val RXIM = 0x10
  private val TXIM = 0x20

  /** Host calls this to push a byte into the RX FIFO. */
  def receive(byte: Int): Unit =
    if rxFifo.size < fifoDepth then
      rxFifo.enqueue(byte & 0xFF)
      ris |= RXIM // RX interrupt raw status
      updateInterrupt()

  private def flags: Int =
    var f = 0
    if txFifo.isEmpty then f |= TXFE
    if txFifo.size >= fifoDepth then f |= TXFF
    if rxFifo.isEmpty then f |= RXFE
    if rxFifo.size >= fifoDepth then f |= RXFF
    f

  private def updateInterrupt(): Unit =
    val mis = ris & imsc
    if mis != 0 then intc.raise(irq)
    else intc.lower(irq)

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    // PL011 registers are 32-bit aligned; handle byte reads within each register
    val regOff = off & ~3 // align to 4-byte boundary
    val byteInReg = off & 3
    val regVal = readReg(regOff)
    (regVal >> (byteInReg * 8)) & 0xFF

  override def readInt(addr: Long): Int =
    readReg((addr - base).toInt & ~3)

  private def readReg(off: Int): Int =
    off match
      case 0x00 => // UARTDR — read from RX FIFO
        if rxFifo.isEmpty then 0
        else
          val b = rxFifo.dequeue()
          if rxFifo.isEmpty then
            ris &= ~RXIM // clear RX interrupt if FIFO empty
            updateInterrupt()
          b
      case 0x04 => rsr
      case 0x18 => flags
      case 0x24 => ibrd
      case 0x28 => fbrd
      case 0x2C => lcr_h
      case 0x30 => cr
      case 0x34 => ifls
      case 0x38 => imsc
      case 0x3C => ris
      case 0x40 => ris & imsc // MIS = RIS & IMSC
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    val regOff = off & ~3
    val d = (data & 0xFF).toInt
    // For simplicity, byte writes to register offset 0 write the full byte value
    writeReg(regOff, d)

  override def writeInt(addr: Long, data: Long): Unit =
    writeReg((addr - base).toInt & ~3, data.toInt)

  private def writeReg(off: Int, data: Int): Unit =
    off match
      case 0x00 => // UARTDR — write to TX FIFO
        if (cr & UARTEN) != 0 && (cr & TXE) != 0 then
          if txFifo.size < fifoDepth then
            txFifo.enqueue(data & 0xFF)
      case 0x04 => rsr = 0 // any write clears error flags
      case 0x24 => ibrd = data & 0xFFFF
      case 0x28 => fbrd = data & 0x3F
      case 0x2C => lcr_h = data & 0xFF
      case 0x30 => cr = data & 0xFFFF
      case 0x34 => ifls = data & 0x3F
      case 0x38 =>
        imsc = data & 0x7FF
        updateInterrupt()
      case 0x44 => // UARTICR — write-1-to-clear
        ris &= ~(data & 0x7FF)
        updateInterrupt()
      case _ =>

  def apply(cpu: Processor): Unit =
    if (cr & UARTEN) == 0 then return
    // Transmit: drain TX FIFO one byte per tick
    if txFifo.nonEmpty && (cr & TXE) != 0 then
      val b = txFifo.dequeue()
      onTx(b)
      if txFifo.isEmpty then
        ris |= TXIM // TX interrupt when FIFO becomes empty
        updateInterrupt()
