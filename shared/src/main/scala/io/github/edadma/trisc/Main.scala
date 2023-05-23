package io.github.edadma.trisc

@main def run(): Unit =
  val mem = new Memory(
    "Memory",
    mkrom(
      IndexedSeq(
        "00000004", // reset vector
        "111 001 01 0000 0001", // LDI r1, 1
        "111 011 00 1111 0000", // LDI r3, 0xF0 (stdout)
        "101 100 001 011 0000", // ADDI r4, r1, '0'
        "00 0 011 000 100 0001", // STB r3, r0, r4
        "111 011 11 0000 1010", // STI r3, '\n'
        "101 001 001 0000001", // ADDI r1, r1, 1
        "111 010 01 0000 0101", // LDI r2, 5
        "100 010 001 000 0010", // BLS r2, r1, 2
        "010 000 000 111 0010", // BEQ r0, r0, 0x72
        "110 000 000 11 00000", // BRK
      ),
    ),
    new Stdout(0xf0),
  )

//  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)

  val cpu = new CPU(mem) // { trace = true }

  cpu.reset()
  cpu.run()
