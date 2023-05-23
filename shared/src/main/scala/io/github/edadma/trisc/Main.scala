package io.github.edadma.trisc

@main def run(): Unit =
  val rom = mkrom(
    IndexedSeq(
      "00000004", // reset vector
      "111 001 00 1111 1110", // LDI r1, 0xFE
      "110 000 000 11 00000", // BRK
    ),
  )

//  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)

  val cpu = new CPU(rom)

  cpu.reset()
  cpu.run()
  println(cpu.r(1).read)
