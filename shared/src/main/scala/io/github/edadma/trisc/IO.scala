package io.github.edadma.trisc

class Stdout(val base: Long) extends OutputDevice(base):
  val name = "stdout"
  val size = 1

  def emulation(data: Int): Unit = print(data.toChar.toString)
