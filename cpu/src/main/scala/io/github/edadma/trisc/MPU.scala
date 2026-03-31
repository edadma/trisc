package io.github.edadma.trisc

enum Access:
  case Read, Write, Execute

/** Abstract memory protection unit. Implementations define how regions are configured and checked. */
trait MPU:
  /** Number of bytes in the MPU's register space. */
  def registerSize: Int

  /** Read an MPU register. Called by CPU for supervisor-mode reads to the MPU address range. */
  def readRegister(offset: Int): Int

  /** Write an MPU register. Called by CPU for supervisor-mode writes to the MPU address range. */
  def writeRegister(offset: Int, data: Int): Unit

  /** Check whether an access is permitted. Returns true if the access is allowed. */
  def check(addr: Long, access: Access, supervisor: Boolean): Boolean

/**
 * Simple MPU with 8 configurable regions, inspired by ARMv7-M.
 *
 * Each region has:
 *   - BASE (4 bytes): start address (must be aligned to SIZE)
 *   - SIZE (4 bytes): region size in bytes
 *   - ATTR (1 byte): permission bits
 *       bit 0: supervisor read
 *       bit 1: supervisor write
 *       bit 2: supervisor execute
 *       bit 3: user read
 *       bit 4: user write
 *       bit 5: user execute
 *       bit 6: enable
 *
 * Register layout (per region, 9 bytes × 8 regions = 72 bytes):
 *   offset 0-3: BASE (big-endian)
 *   offset 4-7: SIZE (big-endian)
 *   offset 8:   ATTR
 *
 * Global control register at offset 72:
 *   bit 0: MPU enable (when 0, all accesses allowed)
 */
class SimpleMPU(val regions: Int = 8) extends MPU:
  require(regions >= 1 && regions <= 8, "region count must be 1-8")

  private val REGION_SIZE = 9 // bytes per region: 4 (base) + 4 (size) + 1 (attr)
  val registerSize: Int = regions * REGION_SIZE + 1 // +1 for global control

  // Permission bits in ATTR
  private val SR = 0x01 // supervisor read
  private val SW = 0x02 // supervisor write
  private val SX = 0x04 // supervisor execute
  private val UR = 0x08 // user read
  private val UW = 0x10 // user write
  private val UX = 0x20 // user execute
  private val EN = 0x40 // region enable

  private val base = new Array[Long](regions)
  private val size = new Array[Long](regions)
  private val attr = new Array[Int](regions)
  private var enabled: Boolean = false

  def readRegister(offset: Int): Int =
    val controlOffset = regions * REGION_SIZE
    if offset == controlOffset then
      if enabled then 1 else 0
    else
      val region = offset / REGION_SIZE
      val field = offset % REGION_SIZE
      if region >= regions then 0
      else
        field match
          case 0 => ((base(region) >> 24) & 0xff).toInt
          case 1 => ((base(region) >> 16) & 0xff).toInt
          case 2 => ((base(region) >> 8) & 0xff).toInt
          case 3 => (base(region) & 0xff).toInt
          case 4 => ((size(region) >> 24) & 0xff).toInt
          case 5 => ((size(region) >> 16) & 0xff).toInt
          case 6 => ((size(region) >> 8) & 0xff).toInt
          case 7 => (size(region) & 0xff).toInt
          case 8 => attr(region)
          case _ => 0

  def writeRegister(offset: Int, data: Int): Unit =
    val d = data & 0xff
    val controlOffset = regions * REGION_SIZE
    if offset == controlOffset then
      enabled = (d & 1) != 0
    else
      val region = offset / REGION_SIZE
      val field = offset % REGION_SIZE
      if region < regions then
        field match
          case 0 => base(region) = (base(region) & 0x00ffffffL) | ((d.toLong & 0xff) << 24)
          case 1 => base(region) = (base(region) & 0xff00ffffL) | ((d.toLong & 0xff) << 16)
          case 2 => base(region) = (base(region) & 0xffff00ffL) | ((d.toLong & 0xff) << 8)
          case 3 => base(region) = (base(region) & 0xffffff00L) | (d.toLong & 0xff)
          case 4 => size(region) = (size(region) & 0x00ffffffL) | ((d.toLong & 0xff) << 24)
          case 5 => size(region) = (size(region) & 0xff00ffffL) | ((d.toLong & 0xff) << 16)
          case 6 => size(region) = (size(region) & 0xffff00ffL) | ((d.toLong & 0xff) << 8)
          case 7 => size(region) = (size(region) & 0xffffff00L) | (d.toLong & 0xff)
          case 8 => attr(region) = d
          case _ =>

  def check(addr: Long, access: Access, supervisor: Boolean): Boolean =
    if !enabled then return true

    // Find highest-numbered matching region (higher regions override lower)
    var i = regions - 1
    while i >= 0 do
      if (attr(i) & EN) != 0 && addr >= base(i) && addr < base(i) + size(i) then
        val a = attr(i)
        val permitted = (supervisor, access) match
          case (true, Access.Read)    => (a & SR) != 0
          case (true, Access.Write)   => (a & SW) != 0
          case (true, Access.Execute) => (a & SX) != 0
          case (false, Access.Read)   => (a & UR) != 0
          case (false, Access.Write)  => (a & UW) != 0
          case (false, Access.Execute) => (a & UX) != 0
        return permitted
      i -= 1

    // No matching region — default deny when MPU is enabled
    false
