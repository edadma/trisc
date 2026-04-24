package io.github.edadma.trisc

object SVMRuntime:
  val stdoutAddress = 0x100000L
  val initialSP = stdoutAddress - 8 // memory stack grows down, below devices

  // Boot module — vector table + entry point.
  // SVM vector table: slot 0 = initial IP, slot 1 = interrupt handler.
  val bootSource: String =
    s"""extern main
       |
       |global _start, func
       |global _fault, func
       |
       |; vector table (2 slots x 8 bytes = 16 bytes)
       |  dl _start
       |  dl _fault
       |
       |entry _start
       |_start:
       |  ; Initialize memory stack pointer
       |  push_i64 $initialSP
       |  push_i64 __sp
       |  store64
       |  call main
       |  halt
       |
       |_fault:
       |  halt
       |
       |segment data
       |  align 8
       |global __sp, data, 8
       |__sp:
       |  dl 0
       |""".stripMargin

  // IO stubs using STDOUT device (memory-mapped, same as TRISC)
  val ioSource: String =
    s"""STDOUT = ${stdoutAddress}
       |
       |global putchar, func
       |global puts, func
       |global panic, func
       |global assert, func
       |global unreachable, func
       |global todo, func
       |
       |; putchar: TOS = char, write to stdout device
       |putchar:
       |  push_i32 STDOUT
       |  store8
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
       |  ret
       |
       |; panic: TOS = address of {ptr, len} message string struct
       |; writes "panic: <msg>\\n" to stdout then halts the VM.
       |panic:
       |  frame 3
       |  local_set 0       ; local[0] = msg struct addr
       |  ; emit "panic: " prefix (7 bytes)
       |  push_i8 112       ; 'p'
       |  push_i32 STDOUT
       |  store8
       |  push_i8 97        ; 'a'
       |  push_i32 STDOUT
       |  store8
       |  push_i8 110       ; 'n'
       |  push_i32 STDOUT
       |  store8
       |  push_i8 105       ; 'i'
       |  push_i32 STDOUT
       |  store8
       |  push_i8 99        ; 'c'
       |  push_i32 STDOUT
       |  store8
       |  push_i8 58        ; ':'
       |  push_i32 STDOUT
       |  store8
       |  push_i8 32        ; ' '
       |  push_i32 STDOUT
       |  store8
       |  ; emit message bytes
       |  local_get 0
       |  load64             ; ptr
       |  local_set 1
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64             ; len
       |  local_set 2
       |.panic_loop:
       |  local_get 2
       |  eqz
       |  jumpnz .panic_done
       |  local_get 1
       |  load8
       |  push_i32 STDOUT
       |  store8
       |  local_get 1
       |  inc
       |  local_set 1
       |  local_get 2
       |  dec
       |  local_set 2
       |  jump .panic_loop
       |.panic_done:
       |  push_i8 10        ; '\\n'
       |  push_i32 STDOUT
       |  store8
       |  halt
       |
       |; assert(cond: bool, msg: string) — no-op if cond is non-zero, else panic
       |assert:
       |  frame 2
       |  local_set 1       ; msg (top of stack = 2nd arg)
       |  local_set 0       ; cond
       |  local_get 0
       |  jumpnz .assert_ok
       |  local_get 1
       |  call panic
       |.assert_ok:
       |  ret
       |
       |; unreachable(msg: string) — always panics
       |unreachable:
       |  ; just forward to panic (no prefix here since std.debug usually adds "unreachable: ")
       |  call panic
       |  ret
       |
       |; todo(msg: string) — always panics
       |todo:
       |  call panic
       |  ret
       |""".stripMargin

  def bootTof: TOF = svmAssemble(bootSource, relocatable = true)
  def ioTof: TOF = svmAssemble(ioSource, relocatable = true)
