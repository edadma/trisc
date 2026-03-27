package io.github.edadma.trisc

case class TOFReadError(line: Int, msg: String) extends RuntimeException(s"line $line: $msg")
