package io.github.edadma.trisc

@main def run(): Unit =
  val mem = new Memory(
    "Memory",
    mkrom(
      IndexedSeq(
        "00000004", // reset vector
        "111 001 01 0000 0001", // LDI r1, 1
        "111 101 00 1111 0000", // LDI r3, 0xF0 (stdout)
        "111 101 11 0100 0001", // STI r3, 'A'
        "111 101 11 0000 1010", // STI r3, '\n'
        "101 001 001 0000101", // ADDI r1, r1, 1
        "111 010 01 0000 0010", // LDI r2, 2
        "100 010 001 000 0010", // BLS r2, r1, 2
        "010 000 000 111 0100", // BEQ r0, r0, 0x74
        "110 000 000 11 00000", // BRK
      ),
    ),
    new Stdout(0xf0),
  )

//  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)

  val cpu = new CPU(mem)

  cpu.reset()
  cpu.run()
  println(cpu.r(1).read)
  println(cpu.r(2).read)
