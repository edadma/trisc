package io.github.edadma.trisc

@main def run(): Unit =
  val rom = mkrom(IndexedSeq("1100 0100 0110 0010 1110 0100 0111 0010", "1234", "12345678"))

  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)
