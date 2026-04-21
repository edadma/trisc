package io.github.edadma.trisc

/**
 * Inter-Processor Interrupt (IPI) device for multi-core TRISC.
 * Inspired by the x86 LAPIC ICR — allows any core to send an interrupt
 * to any other core (or itself) by writing to memory-mapped registers.
 *
 * Each core in a multi-core system gets its own IPI device instance
 * (like each x86 core has its own LAPIC), but they all share the same
 * array of per-core interrupt controllers so any core can target any other.
 *
 * Register map (word-aligned):
 *   +0   TARGET    (4B, R/W) — destination core ID
 *   +4   VECTOR    (4B, R/W) — interrupt vector (reserved for future use)
 *   +8   COMMAND   (1B, W)   — write 1 to send IPI to TARGET core
 *   +9   STATUS    (1B, R)   — bit0=last IPI sent ok, bit1=invalid target
 *   +10  pad       (2B)
 *   +12  SELF_ID   (4B, R)   — this core's ID (read-only core ID register)
 *
 * Size: 16 bytes
 *
 * To send an IPI:
 *   1. Write destination core ID to TARGET
 *   2. Optionally write vector to VECTOR
 *   3. Write 1 to COMMAND — triggers interrupt on target core's INTC
 *
 * @param base          Base address of this IPI device instance
 * @param selfCoreId    The core ID that owns this device instance
 * @param intcs         Array of per-core interrupt controllers (indexed by core ID)
 * @param ipiIrq        IRQ number to raise on the target core's INTC (default 7)
 */
class IPI(
    val base: Long,
    val selfCoreId: Int,
    intcs: Array[InterruptController],
    ipiIrq: Int = 7,
) extends Device:
  val name = "IPI"
  val size = 16

  private var target: Int = 0
  private var vector: Int = 0
  private var status: Int = 0

  private val numCores: Int = intcs.length

  override def readInt(addr: Long): Int =
    (addr - base).toInt match
      case 0  => target
      case 4  => vector
      case 12 => selfCoreId
      case _  => 0

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case 8  => 0 // COMMAND is write-only
      case 9  => status & 0xff
      case _  =>
        // Fall back to int reads for 4-byte registers
        val off = (addr - base).toInt
        if off >= 0 && off < 4 then (target >> ((off & 3) * 8)) & 0xff
        else if off >= 4 && off < 8 then (vector >> (((off - 4) & 3) * 8)) & 0xff
        else if off >= 12 && off < 16 then (selfCoreId >> (((off - 12) & 3) * 8)) & 0xff
        else 0

  override def writeInt(addr: Long, data: Long): Unit =
    (addr - base).toInt match
      case 0 => target = data.toInt
      case 4 => vector = data.toInt
      case _ =>

  def writeByte(addr: Long, data: Long): Unit =
    (addr - base).toInt match
      case 8 => // COMMAND
        if (data.toInt & 1) != 0 then sendIPI()
      case 9 => // STATUS is read-only
      case _ =>
        // Fall back to int writes for 4-byte registers
        val off = (addr - base).toInt
        if off >= 0 && off < 4 then
          val shift = (off & 3) * 8
          target = (target & ~(0xff << shift)) | ((data.toInt & 0xff) << shift)
        else if off >= 4 && off < 8 then
          val shift = ((off - 4) & 3) * 8
          vector = (vector & ~(0xff << shift)) | ((data.toInt & 0xff) << shift)

  private def sendIPI(): Unit =
    if target >= 0 && target < numCores then
      intcs(target).raise(ipiIrq)
      status = 1 // success
    else
      status = 2 // invalid target
