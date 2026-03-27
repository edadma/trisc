package io.github.edadma.trisc

// Minimal TRISC runtime: vector table + I/O stubs.
// Linked with user code when emitting TOF.
object Runtime:
  val stdoutAddress = 0xFF00L // near top of default 64KB address space
  val initialSSP: Long = stdoutAddress - 8 // stack grows down, below stdout device

  // Boot module — must be linked first so vector table is at address 0.
  // Like the 68000: vector[0] = initial SSP, vector[1] = initial PC.
  val bootSource: String =
    s"""extern main
       |; vector table (address 0)
       |  dl $initialSSP
       |  dl main
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
       |""".stripMargin

  def bootTof: TOF = assemble(bootSource, relocatable = true)
  def ioTof: TOF = assemble(ioSource, relocatable = true)
