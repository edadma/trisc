package io.github.edadma.trisc

@main def run(): Unit =
  val rom = mkrom(
    IndexedSeq(
      "00000004", // reset vector
      "111 001 01 0000 0001", // LDI r1, 1
      "101 001 001 0000101", // ADDI r1, r1, 1
      "111 002 01 0000 0000", // LDI r1, 6
      "100 001 002 1111 1010" // BLS r1, r2, -6
      "110 000 000 11 00000", // BRK
    ),
  )

//  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)

  val cpu = new CPU(rom)

  cpu.reset()
  cpu.run()
  println(cpu.r(1).read)
  println(cpu.r(2).read)
