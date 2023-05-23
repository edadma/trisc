package io.github.edadma.trisc

class Stdout(val base: Long) extends ByteOutputDevice(base):
  val name = "stdout"
  val size = 1

  def emulation(data: Long): Unit = print(data.toChar.toString)
