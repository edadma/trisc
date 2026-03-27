package io.github.edadma.trisc

// Minimal TRISC runtime stubs for I/O builtins.
// Linked with user code when emitting TOF.
object Runtime:
  val stdoutAddress = 0xFF00L // near top of default 64KB address space

  val source: String =
    s"""; TRISC runtime library
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

  def tof: TOF = assemble(source, relocatable = true)
