package io.github.edadma.trisc

class IntegrationTests extends TestHelpers {

  "loop counting to 3 with output" in {
    val output = runProgram(
      """STDOUT = 0xFF8
        |dd 0xFF0
        |dd 160
        |rb 144
        |ldi r1, 1
        |movi r3, STDOUT
        |loop
        |  addi r4, r1, '0'
        |  stb r4, r3, r0
        |  addi r1, r1, 1
        |  ldi r2, 3
        |  bls r2, r1, done
        |  bra loop
        |done
        |  halt
        |""".stripMargin)
    output shouldBe "123"
  }

  "fibonacci sequence" in {
    val cpu = runCPU(VECTORS +
      """ldi r1, 0
        |ldi r2, 1
        |ldi r3, 10
        |ldi r4, 0
        |loop
        |  add r5, r1, r2
        |  mov r1, r2
        |  mov r2, r5
        |  addi r4, r4, 1
        |  bls r4, r3, loop
        |halt
        |""".stripMargin)
    cpu.r(1).read shouldBe 55
    cpu.r(2).read shouldBe 89
  }

  "number to string conversion" in {
    val output = runProgram(
      """STDOUT = 0x1FF8
        |dd 0x1FF0
        |dd reset
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd 0
        |dd trap0
        |rb 80
        |reset
        |  ldi r1, 2
        |  spsr r1
        |  movi r2, 123
        |  ldi r3, 10
        |  trap 0
        |  halt
        |trap0
        |  movi r4, buf
        |  addi r4, r4, 19
        |.digit
        |  addi r4, r4, -1
        |  div r5, r2, r3
        |  ; r5 = quotient, r6 = remainder
        |  addi r6, r6, '0'
        |  stb r6, r4, r0
        |  mov r2, r5
        |  beq r2, r0, .print
        |  bra .digit
        |.print
        |  movi r6, STDOUT
        |.char
        |  ldb r5, r4, r0
        |  beq r5, r0, .done
        |  stb r5, r6, r0
        |  addi r4, r4, 1
        |  bra .char
        |.done
        |  rte
        |
        |segment bss
        |buf rb 20
        |""".stripMargin,
      memSize = 0x2000)
    output shouldBe "123"
  }
}
