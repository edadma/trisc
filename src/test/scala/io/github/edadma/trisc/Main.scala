package io.github.edadma.trisc

import pprint.pprintln

@main def run(): Unit =
//  val segs = new Assembler(stacked = true).assemble(
//    """
//      |STDOUT = 0x78
//      |
//      |segment code
//      |dw reset
//      |
//      |reset
//      |  ldi r1, 1          // start counter at 1
//      |  ldi r3, STDOUT     // r3 contains stdout device address
//      |loop
//      |  addi r4, r1, '0'   // convert counter to ASCII character
//      |  stb r3, r0, r4     // output counter character
//      |  sti r3, '\n'       // output linefeed character
//      |  addi r1, r1, 1     // increment counter
//      |  ldi r2, 5          // we so that we can compare counter to 5
//      |  bls r2, r1, end    // is 5 < counter? if so, jump to end of program
//      |  beq r0, r0, loop   // jump back to loop start for next iteration
//      |end
//      |  brk                // end program
//      |  """.stripMargin,
//  )

//  val segs = new Assembler(stacked = true).assemble(
//    """
//      |STDOUT = 0x78
//      |TIMER_DELAY = 0x7A
//      |TIMER_START = 0x7C
//      |
//      |segment code
//      |dw reset
//      |dw timer
//      |dw 0
//      |dw trap0
//      |
//      |reset
//      |  ldi r1, TIMER_DELAY
//      |  ldi r2, 1
//      |  sli r2, 0xf4
//      |  sts r1, r0, r2
//      |  ldi r1, TIMER_START
//      |  sti r1, 1
//      |  cli
//      |  ldi r1, 1
//      |  ldi r2, startMessage
//      |  trap 0
//      |loop
//      |  beq r0, r0, loop
//      |timer
//      |  ldi r1, STDOUT
//      |  sti r1, 'A'
//      |  sti r1, '\n'
//      |  rte
//      |trap0
//      |  beq r1, r0, characterOutput
//      |  addi r1, r1, -1
//      |  beq r1, r0, stringOutput
//      |  brk
//      |characterOutput
//      |  ldi r3, STDOUT
//      |  stb r3, r0, r2
//      |  sti r3, '\n'
//      |  rte
//      |stringOutput
//      |  ldi r3, STDOUT
//      |.char
//      |  ldb r4, r2, r0
//      |  beq r4, r0, .done
//      |  stb r3, r0, r4
//      |  addi r2, r2, 1
//      |  beq r0, r0, .char
//      |.done
//      |  sti r3, '\n'
//      |  rte
//      |
//      |startMessage
//      |  db "start", 0
//      |  """.stripMargin,
//  )

val segs = new Assembler(stacked = true).assemble(
  """
    |STDOUT = 0x78
    |TIMER_DELAY = 0x7A
    |TIMER_START = 0x7C
    |
    |segment code
    |dw reset
    |dw timer
    |dw 0
    |dw trap0
    |
    |reset
    |  ldi r1, 1
    |  ldi r2, startMessage
    |  trap 0
    |  brk
    |trap0
    |  ldi r3, STDOUT
    |.char
    |  ldb r4, r2, r0
    |  beq r4, r0, .done
    |  stb r3, r0, r4
    |  addi r2, r2, 1
    |  beq r0, r0, .char
    |.done
    |  rte
    |
    |startMessage
    |  db "start\n", 0
    |  """.stripMargin,
)

//  pprintln(segs)

  val chunks = segs flatMap (_.chunks)
  val code = chunks flatMap { case DataChunk(data) => data } to IndexedSeq
  val timer = new Timer(0x7a)
  val mem = new Memory(
    "Memory",
    new ROM(code, 0),
    new Stdout(0x78),
    timer,
  )

//  val mem = new Memory(
//    "Memory",
//    mkROM(
//      IndexedSeq(
//        "00000004", // DL 4 // reset vector
//        "111 001 01 0000 0001", // LDI r1, 1 // start counter at 1
//        "111 011 00 1111 0000", // LDI r3, 0xF0 // r3 contains stdout device address
//        // loop:
//        "101 100 001 0110000",  // ADDI r4, r1, '0' // convert counter to ASCII character
//        "000 011 000 100 0001", // STB r3, r0, r4 // output counter character
//        "111 011 11 0000 1010", // STI r3, '\n' // output linefeed character
//        "101 001 001 0000001",  // ADDI r1, r1, 1 // increment counter
//        "111 010 01 0000 0101", // LDI r2, 5 // 5 -> r2 so that we can compare counter to 5
//        "100 010 001 000 0010", // BLS r2, r1, 2 // is 5 < counter? if so, jump to end program
//        "010 000 000 111 0010", // BEQ r0, r0, 0x72 // jump back to loop start
//        "110 000 000 10 00000", // BRK // end program
//      ),
//    ),
//    new Stdout(0xf0),
//  )
//
//  //  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)

  val cpu = new CPU(mem, List(timer)) { /*trace = true;*/
    limit = 30000
  }

  cpu.reset()
  val start = System.currentTimeMillis()
  cpu.run()
  println(System.currentTimeMillis() - start)
