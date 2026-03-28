package io.github.edadma.trisc

class KeyboardDevice(val base: Long, terminal: TerminalWidget) extends Device:
  val name = "Keyboard"
  val size = 2

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case 0 => if terminal.hasKey then 1 else 0 // STATUS
      case 1 => terminal.readKey().getOrElse(0)   // DATA
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit = () // writes ignored
