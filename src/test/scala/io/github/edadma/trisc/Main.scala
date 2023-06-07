package io.github.edadma.trisc

import pprint.pprintln

@main def run(): Unit =
//  val segs = new Assembler(stacked = true).assemble(
//    """
//      |STDOUT = 0xF8
//      |
//      |segment code
//      |dw reset
//      |dw 0
//      |dw 0
//      |dw 0
//      |
//      |reset
//      |  ldi r1, 1          // start counter at 1
//      |  ldi r3, STDOUT     // r3 contains stdout device address
//      |loop
//      |  addi r4, r1, '0'   // convert counter to ASCII character
//      |  stb r4, r3, r0     // output counter character
//      |  sti r3, '\n'       // output linefeed character
//      |  addi r1, r1, 1     // increment counter
//      |  ldi r2, 5          // we so that we can compare counter to 5
//      |  bls r2, r1, end    // is 5 < counter? if so, jump to end of program
//      |  bra loop           // jump back to loop start for next iteration
//      |end
//      |  halt
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
//      |  ldi r1, 0
//      |  spsr r1
//      |  ldi r1, 1
//      |  ldi r2, startMessage
//      |  trap 0
//      |loop
//      |  bra loop
//      |timer
//      |  ldi r1, STDOUT
//      |  sti r1, 'A'
//      |  sti r1, '\n'
//      |  rte
//      |trap0
//      |  beq r1, r0, characterOutput
//      |  addi r1, r1, -1
//      |  beq r1, r0, stringOutput
//      |  halt
//      |characterOutput
//      |  ldi r3, STDOUT
//      |  stb r2, r3, r0
//      |  sti r3, '\n'
//      |  rte
//      |stringOutput
//      |  ldi r3, STDOUT
//      |.char
//      |  ldb r4, r2, r0
//      |  beq r4, r0, .done
//      |  stb r4, r3, r0
//      |  addi r2, r2, 1
//      |  bra .char
//      |.done
//      |  rte
//      |
//      |startMessage
//      |  db "start\n", 0
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
//      |dw 0
//      |dw 0
//      |dw 0
//      |
//      |reset
//      |  ldi r2, 'A'
//      |  ldi r1, characterOutput
//      |  jalr r3, r1
//      |  ldi r2, 'B'
//      |  ldi r1, characterOutput
//      |  jalr r3, r1
//      |  halt
//      |characterOutput
//      |  ldi r4, STDOUT
//      |  stb r4, r0, r2
//      |  sti r4, '\n'
//      |  jalr r0, r3
//      |  """.stripMargin,
//  )

  val tof = assemble(
    """
      |STDOUT = 0xFFF8
      |TIMER_DELAY = 0xFFFA
      |TIMER_START = 0xFFFC
      |
      |dw _reset_
      |dw 0
      |dw 0
      |dw _trap0_
      |
      |segment code
      |
      |_reset_
      |  ldi r1, 0
      |  spsr r1
      |
      |  ldi r1, 2
      |  movi r2, 45678
      |  ldi r3, 10
      |  trap 0
      |  halt
      |
      |_trap0_
      |  beq r1, r0, characterOutput
      |  ldi r4, 1
      |  beq r1, r4, stringOutput
      |  ldi r4, 2
      |  beq r1, r4, numberOutput
      |  ldi r2, trap0error
      |stringOutput
      |  movi r3, STDOUT
      |.char
      |  ldb r4, r2, r0
      |  beq r4, r0, .done
      |  stb r4, r3, r0
      |  addi r2, r2, 1
      |  bra .char
      |.done
      |  sti r3, '\n'
      |  rte
      |characterOutput
      |  movi r3, STDOUT
      |  stb r2, r3, r0
      |  sti r3, '\n'
      |  rte
      |numberOutput
      |  // r2: n
      |  // r3: radix
      |  movi r4, buf
      |  addi r4, r4, 19
      |.digit
      |  addi r4, r4, -1
      |  rem r5, r2, r3
      |  addi r5, r5, '0'
      |  stb r5, r4, r0
      |  div r2, r2, r3
      |  beq r2, r0, .done
      |  bra .digit
      |.done
      |  mov r2, r4
      |  bra stringOutput
      |trap0error db "unknown operation",0
      |
      |segment bss
      |buf resb 20
      |  """.stripMargin,
    orgs = Map("bss" -> 0x1000),
  )

//  val segs = assemble(
//    """
//      |STDOUT = 0xFFF8
//      |TIMER_DELAY = 0xFFFA
//      |TIMER_START = 0xFFFC
//      |
//      |dw _reset_
//      |dw 0
//      |dw 0
//      |dw _trap0_
//      |
//      |segment code
//      |
//      |_reset_
//      |  ldi r1, 0
//      |  spsr r1
//      |
//      |  ldi r1, 1
//      |
//      |  movi r3, table
//      |
//      |  ld r2, r3, 0
//      |  trap 0
//      |  ld r2, r3, 4
//      |  trap 0
//      |  ld r2, r3, 8
//      |  trap 0
//      |  movi r2, buf
//      |  sti r2, 'A'
//      |  ldb r3, r2, r0
//      |
//      |  ldi r1, 2
//      |  ldi r2, 123
//      |  trap 0
//      |  halt
//      |
//      |table
//      |  dw firstMessage
//      |  dw secondMessage
//      |  dw thirdMessage
//      |
//      |firstMessage db "first",0
//      |secondMessage db "second",0
//      |thirdMessage db "third",0
//      |
//      |_trap0_
//      |  beq r1, r0, characterOutput
//      |  ldi r3, 1
//      |  beq r1, r3, stringOutput
//      |  ldi r3, 2
//      |  beq r1, r3, numberOutput
//      |  ldi r2, trap0error
//      |stringOutput
//      |  movi r3, STDOUT
//      |.char
//      |  ldb r4, r2, r0
//      |  beq r4, r0, .done
//      |  stb r4, r3, r0
//      |  addi r2, r2, 1
//      |  bra .char
//      |.done
//      |  sti r3, '\n'
//      |  rte
//      |characterOutput
//      |  movi r3, STDOUT
//      |  stb r2, r3, r0
//      |  sti r3, '\n'
//      |  rte
//      |numberOutput
//      |  // r2: n
//      |  // r3: radix
//      |  movi r4, buf
//      |  addi r4, r4, 20
//      |.digit
//      |  addi r4, r4, -1
//      |  rem r5, r2, r3
//      |  addi r5, r5, '0'
//      |  stb r5, r4, r0
//      |  div r5, r2, r3
//      |  beq r5, r0, .done
//      |  bra .digit
//      |.done
//      |  addi r2, r5, 0
//      |  bra stringOutput
//      |trap0error db "unknown operation",0
//      |
//      |segment bss
//      |buf resb 20
//      |  """.stripMargin,
//    orgs = Map("bss" -> 0x1000),
//  )

//  pprintln(segs)

  pprintln(tof)
  val s = tof.serialize

  println(s)

//  val chunks = segs flatMap (_.chunks)
//  val code = chunks flatMap { case DataChunk(data) => data } to IndexedSeq
  val timer = new Timer(0xfffa)
  val mem = new Memory(
    "Memory",
//    new ROM(code, 0),
    new RAM(0x1000, 0x1000),
    new Stdout(0xfff8),
    timer,
  )

//  val cpu = new CPU(mem, List(timer)) {
//    //    trace = true
//    //    clump = 1
//    limit = 30000
//  }
//
//  cpu.reset()
//  val start = System.currentTimeMillis()
//  cpu.run()
//  println(System.currentTimeMillis() - start)

//  val mem = new Memory(
//    "Memory",
//    mkROM(
//      IndexedSeq(
//        "00000004", // DL 4 // reset vector
//        "111 001 01 0000 0001", // LDI r1, 1 // start counter at 1
//        "111 011 00 1111 1000", // LDI r3, 0xF8 // r3 contains stdout device address
//        // loop:
//        "101 100 001 0110000",  // ADDI r4, r1, '0' // convert counter to ASCII character
//        "000 100 011 000 0001", // STB r4, r3, r0 // output counter character
//        "111 011 11 0000 1010", // STI r3, '\n' // output linefeed character
//        "101 001 001 0000001",  // ADDI r1, r1, 1 // increment counter
//        "111 010 01 0000 0101", // LDI r2, 5 // 5 -> r2 so that we can compare counter to 5
//        "100 010 001 000 0001", // BLS r2, r1, 2 // is 5 < counter? if so, jump to end program
//        "010 000 000 011 1001", // BEQ r0, r0, 0x72 // jump back to loop start
//        "110 000 000 00 00000", // HALT // end program
//      ),
//    ),
//    new Stdout(0xf0),
//  )
//
//  //  for i <- 0L until rom.size do println(rom.readByte(i).toHexString)
