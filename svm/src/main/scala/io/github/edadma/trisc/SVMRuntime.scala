package io.github.edadma.trisc

object SVMRuntime:
  val stdoutAddress = 0x100000L

  // Boot module — vector table + entry point.
  // SVM vector table: slot 0 = initial IP, slot 1 = interrupt handler.
  val bootSource: String =
    s"""extern main
       |
       |; vector table (2 slots x 8 bytes = 16 bytes)
       |  dl _start
       |  dl _fault
       |
       |entry _start
       |_start:
       |  call main
       |  halt
       |
       |_fault:
       |  halt
       |""".stripMargin

  // IO stubs using STDOUT device (memory-mapped, same as TRISC)
  val ioSource: String =
    s"""STDOUT = ${stdoutAddress}
       |
       |; putchar: TOS = char, write to stdout device
       |putchar:
       |  push_i32 STDOUT
       |  store8
       |  push_0
       |  ret
       |
       |; puts: TOS = address of {ptr, len} string struct
       |; writes each byte to stdout
       |puts:
       |  frame 3
       |  local_set 0       ; local[0] = string struct addr
       |  local_get 0
       |  load64             ; ptr
       |  local_set 1       ; local[1] = ptr
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64             ; len
       |  local_set 2       ; local[2] = remaining
       |.puts_loop:
       |  local_get 2
       |  eqz
       |  jumpnz .puts_done
       |  local_get 1
       |  load8              ; read byte at ptr
       |  push_i32 STDOUT
       |  store8
       |  local_get 1
       |  inc
       |  local_set 1
       |  local_get 2
       |  dec
       |  local_set 2
       |  jump .puts_loop
       |.puts_done:
       |  push_0
       |  ret
       |""".stripMargin

  def bootTof: TOF = svmAssemble(bootSource, relocatable = true)
  def ioTof: TOF = svmAssemble(ioSource, relocatable = true)
