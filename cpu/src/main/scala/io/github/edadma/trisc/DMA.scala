package io.github.edadma.trisc

/**
 * RP2040-style DMA controller with 12 channels.
 *
 * Each channel independently transfers data between memory addresses.
 * Transfers happen one element per tick when a channel is active,
 * allowing the CPU to do other work. Supports byte, short, word, and
 * long transfer widths, with independent source/dest address increment.
 *
 * Per-channel register map (16 bytes × 12 channels = 192 bytes):
 *   +0   READ_ADDR   (4B, R/W) — source address
 *   +4   WRITE_ADDR  (4B, R/W) — destination address
 *   +8   TRANS_COUNT  (4B, R/W) — number of transfers remaining
 *   +12  CTRL_TRIG    (4B, R/W) — control register; write triggers transfer
 *
 * CTRL_TRIG bits:
 *   bits 1:0   — DATA_SIZE: 0=byte, 1=short(2B), 2=word(4B), 3=long(8B)
 *   bit 2      — INCR_READ: auto-increment read address
 *   bit 3      — INCR_WRITE: auto-increment write address
 *   bits 7:4   — CHAIN_TO: channel to trigger on completion (0xF = none)
 *   bit 8      — IRQ_QUIET: if 0, raise interrupt on completion
 *   bit 9      — ENABLE: channel is active (set on trigger, cleared on completion)
 *   bit 10     — BUSY: transfer in progress (read-only)
 *
 * Global registers (offset 192):
 *   +192  INTS     (2B, R)   — interrupt status (bit per channel, read-only)
 *   +194  INTE     (2B, R/W) — interrupt enable (bit per channel)
 *   +196  INTF     (2B, R/W) — interrupt force (write-1-to-set raw status)
 *   +198  INTC     (2B, W)   — interrupt clear (write-1-to-clear)
 *   +200  ABORT    (2B, W)   — write bit per channel to abort
 *
 * Total size: 202 bytes
 *
 * Usage:
 *   1. Write READ_ADDR, WRITE_ADDR, TRANS_COUNT
 *   2. Write CTRL_TRIG with ENABLE=0 to configure, then write again with ENABLE=1 to start
 *      OR write CTRL_TRIG once with ENABLE=1 to configure and start atomically
 *   3. DMA transfers one element per tick until TRANS_COUNT reaches 0
 *   4. On completion: ENABLE cleared, CHAIN_TO triggered (if != 0xF), interrupt raised (if enabled)
 *
 * @param base  Base address in memory map
 * @param mem   Main memory for DMA transfers
 * @param intc  Interrupt controller
 * @param irq   IRQ line for DMA completion
 */
class DMA(val base: Long, var mem: Addressable, intc: InterruptController, irq: Int, var mmu: Option[MMU] = None)
    extends Device with (CPU => Unit):
  val name = "DMA"
  val size = 202

  private val NUM_CHANNELS = 12
  private val CH_SIZE = 16
  private val CH_AREA = NUM_CHANNELS * CH_SIZE // 192

  // Per-channel state
  private val readAddr = new Array[Long](NUM_CHANNELS)
  private val writeAddr = new Array[Long](NUM_CHANNELS)
  private val transCount = new Array[Int](NUM_CHANNELS)
  private val ctrl = new Array[Int](NUM_CHANNELS)

  // Global interrupt state
  private var intStatus: Int = 0  // raw interrupt status (bit per channel)
  private var intEnable: Int = 0  // interrupt enable mask

  // CTRL_TRIG bit masks
  private val DATA_SIZE_MASK = 0x03
  private val INCR_READ = 0x04
  private val INCR_WRITE = 0x08
  private val CHAIN_TO_MASK = 0xF0
  private val CHAIN_TO_SHIFT = 4
  private val IRQ_QUIET = 0x100
  private val ENABLE = 0x200
  private val BUSY = 0x400

  private def dataSize(ch: Int): Int = 1 << (ctrl(ch) & DATA_SIZE_MASK) // 1, 2, 4, or 8
  private def chainTo(ch: Int): Int = (ctrl(ch) & CHAIN_TO_MASK) >> CHAIN_TO_SHIFT
  private def isEnabled(ch: Int): Boolean = (ctrl(ch) & ENABLE) != 0

  /** Translate a virtual address through the MMU (if present and enabled).
    * DMA uses supervisor mode since it's initiated by kernel code. */
  private def xlat(addr: Long, access: Access): Long =
    mmu match
      case Some(m) if m.enabled =>
        m.translate(addr, access, supervisor = true) match
          case Right(phys) => phys
          case Left(_)     => addr // fallback on fault (shouldn't happen with correct OS)
      case _ => addr

  // ===== Register access =====

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    val regOff = off & ~3
    val byteInReg = 3 - (off & 3) // big-endian: byte 0 is high byte
    val regVal = readReg(regOff)
    (regVal >> (byteInReg * 8)) & 0xFF

  override def readInt(addr: Long): Int =
    readReg((addr - base).toInt & ~3)

  override def readShort(addr: Long): Int =
    val off = (addr - base).toInt & ~1
    if off >= CH_AREA then
      off match
        case 192 => intStatus & intEnable  // INTS — masked status
        case 194 => intEnable              // INTE
        case 196 => intStatus              // INTF — raw status
        case _   => 0
    else
      val regVal = readReg(off & ~3)
      if (off & 2) == 0 then regVal & 0xFFFF
      else (regVal >> 16) & 0xFFFF

  private def readReg(off: Int): Int =
    if off < CH_AREA then
      val ch = off / CH_SIZE
      val field = off % CH_SIZE
      if ch < NUM_CHANNELS then
        field match
          case 0  => readAddr(ch).toInt
          case 4  => writeAddr(ch).toInt
          case 8  => transCount(ch)
          case 12 => ctrl(ch) | (if transCount(ch) > 0 && isEnabled(ch) then BUSY else 0)
          case _  => 0
      else 0
    else 0 // global registers handled by readShort/readByte directly

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    val regOff = off & ~3
    val byteInReg = 3 - (off & 3) // big-endian: byte 0 is high byte
    val current = readReg(regOff)
    val mask = 0xFF << (byteInReg * 8)
    val newVal = (current & ~mask) | (((data.toInt & 0xFF) << (byteInReg * 8)) & mask)
    writeRegByteMode(regOff, newVal)

  override def writeInt(addr: Long, data: Long): Unit =
    writeReg((addr - base).toInt & ~3, data.toInt)

  override def writeShort(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt & ~1
    val v = data.toInt & 0xFFF
    if off >= CH_AREA then
      off match
        case 194 => intEnable = v; updateInterrupt()       // INTE
        case 196 => intStatus |= v; updateInterrupt()      // INTF — write-1-to-set
        case 198 => intStatus &= ~v; updateInterrupt()     // INTC — write-1-to-clear
        case 200 =>                                          // ABORT
          for ch <- 0 until NUM_CHANNELS do
            if (v & (1 << ch)) != 0 then
              ctrl(ch) &= ~ENABLE
              transCount(ch) = 0
        case _ =>
    else
      val regBase = off & ~3
      val current = readReg(regBase)
      val sv = data.toInt & 0xFFFF
      val newVal = if (off & 2) == 0 then (current & 0xFFFF0000) | sv
                   else (current & 0x0000FFFF) | (sv << 16)
      writeReg(regBase, newVal)

  /** Write register from byte-at-a-time access. For CTRL_TRIG, triggers
    * when ENABLE transitions from 0 to 1 (i.e., the byte that sets ENABLE). */
  private def writeRegByteMode(off: Int, data: Int): Unit =
    if off < CH_AREA then
      val ch = off / CH_SIZE
      val field = off % CH_SIZE
      if ch < NUM_CHANNELS then
        field match
          case 0  => readAddr(ch) = data.toLong & 0xFFFFFFFFL
          case 4  => writeAddr(ch) = data.toLong & 0xFFFFFFFFL
          case 8  => transCount(ch) = data
          case 12 =>
            val wasEnabled = (ctrl(ch) & ENABLE) != 0
            ctrl(ch) = data & ~BUSY
            if !wasEnabled && (data & ENABLE) != 0 && transCount(ch) > 0 then
              completeTransfer(ch)
          case _ =>
    else ()

  private def writeReg(off: Int, data: Int): Unit =
    if off < CH_AREA then
      val ch = off / CH_SIZE
      val field = off % CH_SIZE
      if ch < NUM_CHANNELS then
        field match
          case 0  => readAddr(ch) = data.toLong & 0xFFFFFFFFL
          case 4  => writeAddr(ch) = data.toLong & 0xFFFFFFFFL
          case 8  => transCount(ch) = data
          case 12 => // CTRL_TRIG
            ctrl(ch) = data & ~BUSY // BUSY is read-only
            // Instant transfer: when ENABLE is set, complete all transfers immediately.
            // This makes DMA copies effectively free from the CPU's perspective.
            if (data & ENABLE) != 0 && transCount(ch) > 0 then
              completeTransfer(ch)
          case _ =>
    else () // global registers handled by writeShort directly

  /** Complete all remaining transfers for a channel immediately.
    * This runs in the JVM, not emulated TRISC, so it's effectively instant
    * compared to emulated byte-copy loops (~10 TRISC instructions per byte). */
  private def completeTransfer(ch: Int): Unit =
    val sz = dataSize(ch)
    val count = transCount(ch)
    val incrRead = (ctrl(ch) & INCR_READ) != 0
    val incrWrite = (ctrl(ch) & INCR_WRITE) != 0

    var i = 0
    while i < count do
      val rAddr = xlat(readAddr(ch), Access.Read)
      val wAddr = xlat(writeAddr(ch), Access.Write)
      sz match
        case 1 => mem.writeByte(wAddr, mem.readByte(rAddr))
        case 2 => mem.writeShort(wAddr, mem.readShort(rAddr))
        case 4 => mem.writeInt(wAddr, mem.readInt(rAddr))
        case 8 => mem.writeLong(wAddr, mem.readLong(rAddr))
        case _ =>
      if incrRead then readAddr(ch) += sz
      if incrWrite then writeAddr(ch) += sz
      i += 1

    // Complete
    transCount(ch) = 0
    ctrl(ch) &= ~ENABLE

    // Raise interrupt
    if (ctrl(ch) & IRQ_QUIET) == 0 then
      intStatus |= (1 << ch)
      updateInterrupt()

    // Chain to next channel
    val next = chainTo(ch)
    if next < NUM_CHANNELS && next != ch then
      ctrl(next) |= ENABLE
      if transCount(next) > 0 then completeTransfer(next)

  private def updateInterrupt(): Unit =
    if (intStatus & intEnable) != 0 then intc.raise(irq)
    else intc.lower(irq)

  // ===== Tick — transfer one element per active channel =====

  def apply(cpu: CPU): Unit =
    var ch = 0
    while ch < NUM_CHANNELS do
      if isEnabled(ch) && transCount(ch) > 0 then
        val sz = dataSize(ch)

        // Transfer one element
        val rAddr = xlat(readAddr(ch), Access.Read)
        val wAddr = xlat(writeAddr(ch), Access.Write)
        sz match
          case 1 =>
            val b = mem.readByte(rAddr)
            mem.writeByte(wAddr, b)
          case 2 =>
            val s = mem.readShort(rAddr)
            mem.writeShort(wAddr, s)
          case 4 =>
            val w = mem.readInt(rAddr)
            mem.writeInt(wAddr, w)
          case 8 =>
            val l = mem.readLong(rAddr)
            mem.writeLong(wAddr, l)
          case _ =>

        // Advance addresses
        if (ctrl(ch) & INCR_READ) != 0 then readAddr(ch) += sz
        if (ctrl(ch) & INCR_WRITE) != 0 then writeAddr(ch) += sz

        // Decrement count
        transCount(ch) -= 1

        // Check completion
        if transCount(ch) == 0 then
          ctrl(ch) &= ~ENABLE

          // Raise interrupt
          if (ctrl(ch) & IRQ_QUIET) == 0 then
            intStatus |= (1 << ch)
            updateInterrupt()

          // Chain to next channel
          val next = chainTo(ch)
          if next < NUM_CHANNELS && next != ch then
            ctrl(next) |= ENABLE

      ch += 1
