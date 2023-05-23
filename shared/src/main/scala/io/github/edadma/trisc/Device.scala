package io.github.edadma.trisc

trait Device:
  def emulation(data: Long): Unit

abstract class ByteOutputDevice(addr: Long) extends WriteOnlyAddressable with Device:
  def writeByte(addr: Long, data: Long): Unit = emulation(data)

class Stdout(val base: Long) extends ByteOutputDevice(base):
  val name = "stdout"
  val size = 1

  def emulation(data: Long): Unit = print(data.toChar.toString)
