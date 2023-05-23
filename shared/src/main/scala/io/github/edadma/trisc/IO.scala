package io.github.edadma.trisc

class Stdout(addr: Long) extends OutputDevice(addr):
  def emulation(data: Int): Unit = print(data.toChar.toString)
