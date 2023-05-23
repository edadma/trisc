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

  def readShort(addr: Long): Int = readByte(addr) << 8 | readByte(addr + 1)

  def writeShort(addr: Long, data: Int): Unit =
    writeByte(addr, data >> 8)
    writeByte(addr + 1, data)

  def readInt(addr: Long): Int = readShort(addr) << 16 | readShort(addr + 2)

  def writeInt(addr: Long, data: Int): Unit =
    writeShort(addr, data >> 16)
    writeShort(addr + 2, data)

  def readLong(addr: Long): Long = readInt(addr).toLong << 32 | readInt(addr + 4) & 0xffffffff

  def writeLong(addr: Long, data: Long): Unit =
    writeInt(addr, (data >> 32).toInt)
    writeInt(addr + 4, data.toInt)

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

trait ReadOnlyAddressable extends Addressable:
  def writeByte(addr: Long, data: Int): Unit = sys.error(s"$name not writable at address ${addr.toHexString}")

trait WriteOnlyAddressable extends Addressable:
  def readByte(addr: Long): Int = sys.error(s"$name not readable at address ${addr.toHexString}")

class ROM(seq: immutable.IndexedSeq[Byte], val base: Long) extends ReadOnlyAddressable:
  val name = "ROM"
  require(base >= 0, "base is negative")
  require(seq.nonEmpty, "Addressable is empty")

  val size = seq.length

  def readByte(addr: Long): Int =
    require(base <= addr && addr < base + size, "address out of range")
    seq((addr - base).toInt) & 0xff

def mkrom(insts: IndexedSeq[String]): ROM =
  def literal(n: String): Iterator[Int] =
    val s = n.replace(" ", "")

    if s.length == 4 || s.length == 8 then s.grouped(2) map (d => Integer.parseInt(d, 16))
    else if s.length == 16 then s.grouped(8) map (d => Integer.parseInt(d, 2))
    else sys.error(s"bad literal '$n'")

  new ROM(insts.flatMap(inst => literal(inst).map(_.toByte).toIndexedSeq), 0)

abstract class OutputDevice(addr: Long) extends WriteOnlyAddressable:
  def emulation(data: Int): Unit

  def writeByte(addr: Long, data: Int): Unit = emulation(data)
