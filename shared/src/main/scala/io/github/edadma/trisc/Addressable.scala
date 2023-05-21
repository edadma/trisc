package io.github.edadma.trisc

import scala.collection.{immutable, mutable}

enum Endian:
  case Little, Big

trait Addressable:
  def name: String
  def base: Long
  def size: Long
  def readByte(addr: Long): Int
  def writeByte(addr: Long, data: Int): Unit

  def readShort(addr: Long, endian: Endian): Int =
    endian match
      case Endian.Big    => readByte(addr) << 8 | readByte(addr + 1)
      case Endian.Little => readByte(addr + 1) << 8 | readByte(addr)

  def writeShort(addr: Long, data: Short, endian: Endian): Unit =
    endian match
      case Endian.Big =>
        writeByte(addr, data >> 8)
        writeByte(addr + 1, data)
      case Endian.Little =>
        writeByte(addr + 1, data >> 8)
        writeByte(addr, data)

  def readInt(addr: Long, endian: Endian): Int =
    endian match
      case Endian.Big    => readShort(addr, endian) << 16 | readShort(addr + 2, endian)
      case Endian.Little => readShort(addr + 2, endian) << 16 | readShort(addr, endian)

  def readLong(addr: Long, endian: Endian): Long =
    endian match
      case Endian.Big    => readInt(addr, endian).toLong << 32 | readInt(addr + 4, endian) & 0xffffffff
      case Endian.Little => readInt(addr + 4, endian).toLong << 32 | readInt(addr, endian) & 0xffffffff

  def baseAddress: String = f"$base%08x".toUpperCase

class Memory(val name: String, blocks: Addressable*) extends Addressable:
  require(blocks.nonEmpty, "memory must contain at least one Addressable block")

  val mem = blocks.sortBy(_.base).to(immutable.ArraySeq)
  val base = mem.head.base
  val size = mem.last.base + mem.last.size - base

  for i <- mem.indices do
    if i < mem.length - 1 && mem(i).base + mem(i).size > mem(i + 1).base then
      sys.error(
        s"overlapping Addressable blocks: '${mem(i).name}' (${mem(i).baseAddress}) and '${mem(i + 1).name}' (${mem(i + 1).baseAddress})",
      )

  def block(addr: Long): Option[Addressable] =
    search(mem, addr, _ < _.base, _ == _.base) match
      case Left(idx) if idx == 0 => None
      case Left(idx) =>
        val prec = mem(idx - 1)

        if addr < prec.base + prec.size then Some(prec) else None
      case Right(idx) => Some(mem(idx))

  private def badAddress: Nothing = sys.error("address not found")

  def readByte(addr: Long): Int = block(addr) getOrElse badAddress readByte addr

  def writeByte(addr: Long, data: Int): Unit = block(addr) getOrElse badAddress writeByte (addr, data)

class RAM(val base: Long, val size: Long) extends Addressable:
  val name = "RAM"
  require(base >= 0, "base is negative")
  require(0 <= size && size <= Int.MaxValue, "size out of range")

  val seq = mutable.ArraySeq.fill(size.toInt)(0.asInstanceOf[Byte])

  def readByte(addr: Long): Int =
    require(base <= addr && addr < base + size, "address out of range")
    seq((addr - base).toInt) & 0xff

  def writeByte(addr: Long, data: Int): Unit =
    require(base <= addr && addr < base + size, "address out of range")
    seq((addr - base).toInt) = data.toByte

class ROM(seq: immutable.IndexedSeq[Byte], val base: Long) extends Addressable:
  val name = "ROM"
  require(base >= 0, "base is negative")
  require(seq.nonEmpty, "Addressable is empty")

  val size = seq.length

  def readByte(addr: Long): Int =
    require(base <= addr && addr < base + size, "address out of range")
    seq((addr - base).toInt) & 0xff

  def writeByte(addr: Long, data: Int): Unit = sys.error("not writable")
