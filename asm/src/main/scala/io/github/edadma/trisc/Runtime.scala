package io.github.edadma.trisc

// Minimal TRISC runtime: vector table + I/O stubs.
// Linked with user code when emitting TOF.
object Runtime:
  val stdoutAddress = 0x100000L // 1MB — devices start here, RAM below
  val keyboardAddress = 0x100004L
  val displayCtrlAddress = 0x100006L
  val blitterAddress = 0x10000CL
  val timerAddress = 0x100020L
  val framebufferAddress = 0x200000L // 2MB — framebuffer pixel data
  val framebufferMaxSize: Long = 1920 * 1080 * 4
  val initialSSP: Long = stdoutAddress - 8 // stack grows down, below devices

  // Boot module — must be linked first so vector table is at address 0.
  // Like the 68000: vector[0] = initial SSP, vector[1] = initial PC.
  // Full 20-slot vector table matching CPU State enum ordering.
  val bootSource: String =
    s"""extern main
       |; vector table (20 slots x 8 bytes = 160 bytes)
       |  dl $initialSSP
       |  dl _start
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |  dl _fault
       |; boot: call main, then halt
       |_start
       |  movi r4, main
       |  jalr r6, r4
       |  halt
       |_fault
       |  halt
       |  align 8
       |""".stripMargin

  val ioSource: String =
    s"""; TRISC runtime I/O library
       |; putchar: write low byte of r1 to stdout device, return r1
       |putchar
       |  movi r2, ${stdoutAddress}
       |  stb r1, r2, r0
       |  jalr r0, r6
       |
       |; print: write integer in r1 as decimal to stdout
       |; (stub — just writes the low digit for now)
       |print
       |  movi r2, ${stdoutAddress}
       |  stb r1, r2, r0
       |  jalr r0, r6
       |
       |; println: same as print but appends newline
       |println
       |  movi r2, ${stdoutAddress}
       |  stb r1, r2, r0
       |  ldi r1, 10
       |  stb r1, r2, r0
       |  jalr r0, r6
       |
       |; kbhit: return 1 in r1 if keyboard has data, 0 otherwise
       |kbhit
       |  movi r2, ${keyboardAddress}
       |  ldb r1, r2, r0
       |  jalr r0, r6
       |
       |; getchar: block until keyboard has data, return byte in r1
       |getchar
       |  movi r2, ${keyboardAddress}
       |_getchar_wait
       |  ldb r1, r2, r0
       |  beq r1, r0, _getchar_wait
       |  addi r2, r2, 1
       |  ldb r1, r2, r0
       |  jalr r0, r6
       |""".stripMargin

  def bootTof: TOF = assemble(bootSource, relocatable = true)
  def ioTof: TOF = assemble(ioSource, relocatable = true)
