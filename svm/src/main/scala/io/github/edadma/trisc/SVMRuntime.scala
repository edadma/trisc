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
       |global write_str, func
       |global exit, func
       |
       |global std_io__STDIN, data, 8
       |global std_io__STDOUT, data, 8
       |global std_io__STDERR, data, 8
       |global std_io__O_RDONLY, data, 8
       |global std_io__O_WRONLY, data, 8
       |global std_io__O_RDWR, data, 8
       |global std_io__O_CREATE, data, 8
       |global std_io__O_TRUNC, data, 8
       |global std_io__O_APPEND, data, 8
       |global std_io__SEEK_SET, data, 8
       |global std_io__SEEK_CUR, data, 8
       |global std_io__SEEK_END, data, 8
       |
       |segment data
       |  align 8
       |std_io__STDIN:     dl 0
       |  align 8
       |std_io__STDOUT:    dl 1
       |  align 8
       |std_io__STDERR:    dl 2
       |  align 8
       |std_io__O_RDONLY:  dl 0
       |  align 8
       |std_io__O_WRONLY:  dl 1
       |  align 8
       |std_io__O_RDWR:    dl 2
       |  align 8
       |std_io__O_CREATE:  dl 4
       |  align 8
       |std_io__O_TRUNC:   dl 8
       |  align 8
       |std_io__O_APPEND:  dl 16
       |  align 8
       |std_io__SEEK_SET:  dl 0
       |  align 8
       |std_io__SEEK_CUR:  dl 1
       |  align 8
       |std_io__SEEK_END:  dl 2
       |
       |segment code
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
       |; write_str(s: string) — same as puts: write all bytes to stdout.
       |write_str:
       |  frame 0
       |  call puts
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
       |
       |; exit(code: int) — halt the VM. The code is ignored for the test runner,
       |; since halt leaves whatever is on TOS as svm.result; tests don't call
       |; exit directly, but modules like std.log import it from std.process.
       |exit:
       |  halt
       |
       |; __svm_str_concat(l: *string, r: *string) -> *string
       |; Allocates a new string buffer + fat-pointer struct on the memory stack
       |; and copies the bytes of both inputs into it.
       |;
       |; Args on entry (call pushes left-to-right, so TOS = r):
       |;   local 0 = l, local 1 = r
       |; Locals: 2 = totalLen, 3 = new struct addr, 4 = dst ptr, 5 = src ptr, 6 = remaining
       |; Returns new struct address on TOS.
       |global __svm_str_concat, func
       |__svm_str_concat:
       |  frame 7
       |  local_set 1        ; r (top of stack)
       |  local_set 0        ; l
       |  ; totalLen = l.len + r.len
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64
       |  local_get 1
       |  push_i8 8
       |  add
       |  load64
       |  add
       |  local_set 2
       |  ; Allocate (aligned totalLen) + 16 on memory stack via __sp
       |  push_i64 __sp
       |  dup
       |  load64              ; (&__sp old_sp)
       |  local_get 2
       |  push_i8 7
       |  add
       |  push_i8 -8
       |  and                 ; alignedLen
       |  push_i8 16
       |  add
       |  sub                 ; new_sp = old_sp - (alignedLen + 16)
       |  dup
       |  rot
       |  store64             ; write new_sp; TOS = new_sp = struct addr
       |  local_set 3
       |  ; struct.ptr = struct_addr + 16 (byte buffer starts after struct)
       |  local_get 3
       |  push_i8 16
       |  add
       |  local_get 3
       |  store64
       |  ; struct.len = totalLen
       |  local_get 2
       |  local_get 3
       |  push_i8 8
       |  add
       |  store64
       |  ; dst = buffer = struct_addr + 16
       |  local_get 3
       |  push_i8 16
       |  add
       |  local_set 4
       |  ; copy left: src = l.ptr, remaining = l.len
       |  local_get 0
       |  load64
       |  local_set 5
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64
       |  local_set 6
       |.concat_L:
       |  local_get 6
       |  eqz
       |  jumpnz .concat_R_init
       |  local_get 5
       |  load8
       |  local_get 4
       |  store8
       |  local_get 5
       |  inc
       |  local_set 5
       |  local_get 4
       |  inc
       |  local_set 4
       |  local_get 6
       |  dec
       |  local_set 6
       |  jump .concat_L
       |.concat_R_init:
       |  local_get 1
       |  load64
       |  local_set 5
       |  local_get 1
       |  push_i8 8
       |  add
       |  load64
       |  local_set 6
       |.concat_R:
       |  local_get 6
       |  eqz
       |  jumpnz .concat_done
       |  local_get 5
       |  load8
       |  local_get 4
       |  store8
       |  local_get 5
       |  inc
       |  local_set 5
       |  local_get 4
       |  inc
       |  local_set 4
       |  local_get 6
       |  dec
       |  local_set 6
       |  jump .concat_R
       |.concat_done:
       |  local_get 3
       |  ret
       |
       |; __svm_str_eq(l: *string, r: *string) -> bool
       |; Returns 1 if strings have equal length and contents, else 0.
       |global __svm_str_eq, func
       |__svm_str_eq:
       |  frame 5
       |  local_set 1        ; r
       |  local_set 0        ; l
       |  ; compare lens
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64
       |  local_get 1
       |  push_i8 8
       |  add
       |  load64
       |  over
       |  over
       |  neq
       |  jumpnz .streq_no
       |  ; lens equal; TOS has both lens still — drop one
       |  drop
       |  local_set 2        ; remaining
       |  ; lp = l.ptr, rp = r.ptr
       |  local_get 0
       |  load64
       |  local_set 3
       |  local_get 1
       |  load64
       |  local_set 4
       |.streq_loop:
       |  local_get 2
       |  eqz
       |  jumpnz .streq_yes
       |  local_get 3
       |  load8
       |  local_get 4
       |  load8
       |  neq
       |  jumpnz .streq_no_clean
       |  local_get 3
       |  inc
       |  local_set 3
       |  local_get 4
       |  inc
       |  local_set 4
       |  local_get 2
       |  dec
       |  local_set 2
       |  jump .streq_loop
       |.streq_yes:
       |  push_1
       |  ret
       |.streq_no_clean:
       |  push_0
       |  ret
       |.streq_no:
       |  drop
       |  drop
       |  push_0
       |  ret
       |
       |; __svm_new_slice(byteSize: i64, elemCount: i64) -> *slice_struct
       |; Allocates a 24-byte slice struct + `byteSize` bytes of data on
       |; the memory stack, zero-fills the data region, and fills the
       |; struct with { ptr=data, len=elemCount, cap=elemCount, backref=0 }.
       |global __svm_new_slice, func
       |__svm_new_slice:
       |  frame 4
       |  local_set 1        ; elemCount (TOS)
       |  local_set 0        ; byteSize
       |  ; total = 24 + ((byteSize + 7) & ~7)
       |  push_i64 __sp
       |  dup
       |  load64             ; (&__sp old_sp)
       |  local_get 0
       |  push_i8 7
       |  add
       |  push_i8 -8
       |  and                ; aligned
       |  push_i8 24
       |  add
       |  sub                ; new_sp
       |  dup
       |  rot
       |  store64            ; write new_sp; TOS = new_sp = struct addr
       |  local_set 2        ; struct addr
       |  ; struct.ptr = struct + 24
       |  local_get 2
       |  push_i8 24
       |  add
       |  local_get 2
       |  store64
       |  ; struct.len = elemCount (i32)
       |  local_get 1
       |  local_get 2
       |  push_i8 8
       |  add
       |  store32
       |  ; struct.cap = elemCount (i32)
       |  local_get 1
       |  local_get 2
       |  push_i8 12
       |  add
       |  store32
       |  ; struct.backref = 0
       |  push_0
       |  local_get 2
       |  push_i8 16
       |  add
       |  store64
       |  ; zero the data
       |  local_get 2
       |  push_i8 24
       |  add
       |  local_set 3        ; dp
       |.nslice_zloop:
       |  local_get 0
       |  eqz
       |  jumpnz .nslice_done
       |  push_0
       |  local_get 3
       |  store8
       |  local_get 3
       |  inc
       |  local_set 3
       |  local_get 0
       |  dec
       |  local_set 0
       |  jump .nslice_zloop
       |.nslice_done:
       |  local_get 2
       |  ret
       |""".stripMargin

  def bootTof: TOF = svmAssemble(bootSource, relocatable = true)
  def ioTof: TOF = svmAssemble(ioSource, relocatable = true)
