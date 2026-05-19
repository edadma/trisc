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
       |; __svm_str_cmp(l: *string, r: *string) -> i64
       |; Three-way lexicographic compare. Returns negative if l < r,
       |; zero if l == r, positive if l > r. Bytes compare unsigned;
       |; if one operand is a prefix of the other, the shorter is less
       |; (final tiebreak is l.len - r.len).
       |global __svm_str_cmp, func
       |__svm_str_cmp:
       |  frame 5
       |  local_set 1        ; r (string desc pointer)
       |  local_set 0        ; l
       |  ; load lens
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64
       |  local_set 2        ; llen
       |  local_get 1
       |  push_i8 8
       |  add
       |  load64
       |  local_set 3        ; rlen
       |  ; minlen = min(llen, rlen)
       |  local_get 2
       |  local_get 3
       |  lt                 ; llen < rlen ?
       |  jumpz .strcmp_use_rlen
       |  local_get 2
       |  jump .strcmp_minlen_done
       |.strcmp_use_rlen:
       |  local_get 3
       |.strcmp_minlen_done:
       |  local_set 4        ; minlen (remaining)
       |  ; lp = l.ptr, rp = r.ptr
       |  local_get 0
       |  load64
       |  local_set 0        ; reuse slot 0 as lp
       |  local_get 1
       |  load64
       |  local_set 1        ; reuse slot 1 as rp
       |.strcmp_loop:
       |  local_get 4
       |  eqz
       |  jumpnz .strcmp_lens
       |  local_get 0
       |  load8              ; unsigned byte
       |  local_get 1
       |  load8
       |  sub                ; lb - rb on 64-bit signed (bytes are u8 0..255)
       |  dup
       |  jumpnz .strcmp_ret ; mismatch — return diff
       |  drop
       |  local_get 0
       |  inc
       |  local_set 0
       |  local_get 1
       |  inc
       |  local_set 1
       |  local_get 4
       |  dec
       |  local_set 4
       |  jump .strcmp_loop
       |.strcmp_lens:
       |  local_get 2
       |  local_get 3
       |  sub                ; llen - rlen
       |.strcmp_ret:
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
       |
       |; __svm_str_from_i64(n: i64) -> *string
       |; Convert a signed 64-bit integer to its decimal string representation.
       |; Allocates a 16-byte string struct + 24-byte digit buffer on the memory
       |; stack and returns the struct address. Handles LONG_MIN correctly via
       |; unsigned digit extraction on the absolute value (negation is computed
       |; with `0 - n` which wraps for LONG_MIN, but we then mask via two's
       |; complement: the magnitude as a u64 is what we extract digits from).
       |;
       |; Locals: 0=n, 1=u (u64 magnitude), 2=is_neg, 3=digit_count,
       |;         4=write_ptr (descends), 5=struct_addr.
       |global __svm_str_from_i64, func
       |__svm_str_from_i64:
       |  frame 6
       |  local_set 0          ; n
       |  ; Allocate 40 bytes (16 struct + 24 digit buffer)
       |  push_i64 __sp
       |  dup
       |  load64
       |  push_i8 40
       |  sub
       |  dup
       |  rot
       |  store64
       |  local_set 5          ; struct_addr
       |  ; is_neg = (n < 0)
       |  local_get 0
       |  push_0
       |  lt
       |  local_set 2
       |  ; u = is_neg ? -n : n  (using `0 - n` for LONG_MIN safety)
       |  local_get 2
       |  jumpz .strn_pos
       |  push_0
       |  local_get 0
       |  sub
       |  jump .strn_have_u
       |.strn_pos:
       |  local_get 0
       |.strn_have_u:
       |  local_set 1          ; u
       |  ; write_ptr = struct_addr + 40 (one past end)
       |  local_get 5
       |  push_i8 40
       |  add
       |  local_set 4
       |  push_0
       |  local_set 3          ; digit_count
       |  ; do { write_ptr--; *write_ptr = '0' + (u % 10); u = u / 10; digit_count++ } while u != 0
       |.strn_loop:
       |  local_get 4
       |  dec
       |  local_set 4
       |  local_get 1
       |  push_i8 10
       |  modu
       |  push_i8 48           ; '0'
       |  add
       |  local_get 4
       |  store8
       |  local_get 1
       |  push_i8 10
       |  divu
       |  local_set 1
       |  local_get 3
       |  inc
       |  local_set 3
       |  local_get 1
       |  jumpnz .strn_loop
       |  ; Prepend '-' if negative
       |  local_get 2
       |  jumpz .strn_no_neg
       |  local_get 4
       |  dec
       |  local_set 4
       |  push_i8 45           ; '-'
       |  local_get 4
       |  store8
       |  local_get 3
       |  inc
       |  local_set 3
       |.strn_no_neg:
       |  ; struct.ptr = write_ptr; struct.len = digit_count
       |  local_get 4
       |  local_get 5
       |  store64
       |  local_get 3
       |  local_get 5
       |  push_i8 8
       |  add
       |  store64
       |  local_get 5
       |  ret
       |
       |; __svm_str_from_bool(b: bool) -> *string
       |; Returns "true" or "false". Allocates a 16-byte string struct on the
       |; memory stack pointing at the appropriate static rodata buffer.
       |global __svm_str_from_bool, func
       |__svm_str_from_bool:
       |  frame 2
       |  local_set 0
       |  push_i64 __sp
       |  dup
       |  load64
       |  push_i8 16
       |  sub
       |  dup
       |  rot
       |  store64
       |  local_set 1
       |  local_get 0
       |  jumpz .strb_false
       |  ; "true"
       |  push_i64 __svm_str_true_data
       |  local_get 1
       |  store64
       |  push_i8 4
       |  local_get 1
       |  push_i8 8
       |  add
       |  store64
       |  local_get 1
       |  ret
       |.strb_false:
       |  push_i64 __svm_str_false_data
       |  local_get 1
       |  store64
       |  push_i8 5
       |  local_get 1
       |  push_i8 8
       |  add
       |  store64
       |  local_get 1
       |  ret
       |
       |; __svm_str_fmt_i64(n: i64, base: i32, width: i32, flags: i32) -> *string
       |; Format a signed 64-bit integer with the given base (2/8/10/16) and
       |; padding width.
       |;   flags bit 0x1: zero-pad (right-aligned with '0' fill; ignored if
       |;                  leftAlign is set)
       |;   flags bit 0x2: left-align (with space fill)
       |;   flags bit 0x4: show-sign (always emit '+' for non-negative)
       |;   flags bit 0x8: upper-case hex digits
       |; Allocates a 16-byte string struct + 64-byte buffer on the memory
       |; stack (enough for u64 in binary plus sign and padding).
       |;
       |; Locals: 0=n, 1=base, 2=width, 3=flags, 4=u (magnitude), 5=is_neg,
       |;         6=digit_count, 7=write_ptr, 8=struct_addr, 9=sign_char,
       |;         10=total_len, 11=pad_count, 12=tmp.
       |global __svm_str_fmt_i64, func
       |__svm_str_fmt_i64:
       |  frame 13
       |  local_set 3          ; flags
       |  local_set 2          ; width
       |  local_set 1          ; base
       |  local_set 0          ; n
       |  ; Allocate 16 + 64 = 80 bytes
       |  push_i64 __sp
       |  dup
       |  load64
       |  push_i8 80
       |  sub
       |  dup
       |  rot
       |  store64
       |  local_set 8
       |  ; is_neg = (base == 10) && (n < 0)
       |  ; For non-decimal bases we treat n as unsigned (no sign char).
       |  push_0
       |  local_set 5
       |  local_get 1
       |  push_i8 10
       |  eq
       |  jumpz .fmt_skip_sign_check
       |  local_get 0
       |  push_0
       |  lt
       |  local_set 5
       |.fmt_skip_sign_check:
       |  ; u = is_neg ? -n : n
       |  local_get 5
       |  jumpz .fmt_pos
       |  push_0
       |  local_get 0
       |  sub
       |  jump .fmt_have_u
       |.fmt_pos:
       |  local_get 0
       |.fmt_have_u:
       |  local_set 4
       |  ; write_ptr = struct_addr + 80 (one past end)
       |  local_get 8
       |  push_i8 80
       |  add
       |  local_set 7
       |  push_0
       |  local_set 6          ; digit_count
       |.fmt_loop:
       |  local_get 7
       |  dec
       |  local_set 7
       |  ; digit = u % base
       |  local_get 4
       |  local_get 1
       |  modu
       |  ; if digit < 10: char = '0' + digit; else char = (uppercase ? 'A' : 'a') + digit - 10
       |  dup
       |  push_i8 10
       |  ltu
       |  jumpz .fmt_letter
       |  push_i8 48           ; '0'
       |  add
       |  jump .fmt_store_d
       |.fmt_letter:
       |  push_i8 10
       |  sub
       |  ; uppercase if flags & 0x8
       |  local_get 3
       |  push_i8 8
       |  and
       |  jumpz .fmt_lower
       |  push_i8 65           ; 'A'
       |  add
       |  jump .fmt_store_d
       |.fmt_lower:
       |  push_i8 97           ; 'a'
       |  add
       |.fmt_store_d:
       |  local_get 7
       |  store8
       |  local_get 4
       |  local_get 1
       |  divu
       |  local_set 4
       |  local_get 6
       |  inc
       |  local_set 6
       |  local_get 4
       |  jumpnz .fmt_loop
       |  ; sign_char: '-' if is_neg; '+' if (flags & 0x4) && !is_neg && base==10; else 0
       |  push_0
       |  local_set 9
       |  local_get 5
       |  jumpz .fmt_sign_check_plus
       |  push_i8 45           ; '-'
       |  local_set 9
       |  jump .fmt_sign_done
       |.fmt_sign_check_plus:
       |  local_get 3
       |  push_i8 4
       |  and
       |  jumpz .fmt_sign_done
       |  local_get 1
       |  push_i8 10
       |  eq
       |  jumpz .fmt_sign_done
       |  push_i8 43           ; '+'
       |  local_set 9
       |.fmt_sign_done:
       |  ; total_len_no_pad = digit_count + (sign_char ? 1 : 0)
       |  local_get 6
       |  local_get 9
       |  push_0
       |  neq
       |  add
       |  local_set 10
       |  ; pad_count = max(0, width - total_len_no_pad)
       |  local_get 2
       |  local_get 10
       |  sub
       |  dup
       |  push_0
       |  lt
       |  jumpz .fmt_pad_ok
       |  drop
       |  push_0
       |.fmt_pad_ok:
       |  local_set 11
       |  ; Determine fill char and order:
       |  ;   leftAlign (flag 0x2): space fill, append after digits
       |  ;   else if zeroPad (flag 0x1): '0' fill, prepend BEFORE digits but AFTER sign
       |  ;   else: space fill, prepend BEFORE sign
       |  local_get 3
       |  push_i8 2
       |  and
       |  jumpz .fmt_check_zeropad
       |  ; leftAlign: build result at the START of the buffer (struct+16),
       |  ; sign first if any, then digits forward-copied from their tail
       |  ; location, then pad_count spaces. The original code wrote spaces
       |  ; AFTER the digits at the tail of the buffer, which overflowed the
       |  ; 64-byte buffer by pad_count bytes — surfaced as "address not
       |  ; found" crashes whenever leftAlign was used with width > digits.
       |  local_get 8
       |  push_i8 16
       |  add
       |  local_set 12         ; dst = struct+16 (buffer start)
       |  ; If sign_char != 0, write it at dst and advance dst.
       |  local_get 9
       |  jumpz .fmt_la_no_sign
       |  local_get 9
       |  local_get 12
       |  store8
       |  local_get 12
       |  inc
       |  local_set 12
       |.fmt_la_no_sign:
       |  ; Forward-copy digit_count bytes from write_ptr to dst.
       |  ; Loop uses a TOS i counter; pop on exit.
       |  push_0
       |.fmt_la_cpy:
       |  dup
       |  local_get 6
       |  ltu
       |  jumpz .fmt_la_cpy_done
       |  dup
       |  local_get 7
       |  add
       |  load8                ; ( i byte )
       |  over                 ; ( i byte i )
       |  local_get 12
       |  add                  ; ( i byte dst+i )
       |  store8               ; ( i )
       |  inc
       |  jump .fmt_la_cpy
       |.fmt_la_cpy_done:
       |  drop                 ; drop i
       |  ; dst += digit_count
       |  local_get 12
       |  local_get 6
       |  add
       |  local_set 12
       |  ; Fill pad_count spaces at dst, dst+1, ...
       |.fmt_la_pad:
       |  local_get 11
       |  eqz
       |  jumpnz .fmt_la_done
       |  push_i8 32           ; ' '
       |  local_get 12
       |  store8
       |  local_get 12
       |  inc
       |  local_set 12
       |  local_get 11
       |  dec
       |  local_set 11
       |  jump .fmt_la_pad
       |.fmt_la_done:
       |  ; final length = dst - (struct+16)
       |  local_get 12
       |  local_get 8
       |  sub
       |  push_i8 16
       |  sub
       |  local_set 6
       |  ; struct.ptr will be struct+16 — reset write_ptr (local 7) so
       |  ; fmt_finish writes the right address into the descriptor.
       |  local_get 8
       |  push_i8 16
       |  add
       |  local_set 7
       |  jump .fmt_finish
       |.fmt_check_zeropad:
       |  local_get 3
       |  push_i8 1
       |  and
       |  jumpz .fmt_space_pad
       |  ; zero-pad: emit '0' pad_count times BEFORE digits, sign emitted FIRST
       |  ; Emit padding zeros before the digits (lower address):
       |.fmt_zp_loop:
       |  local_get 11
       |  eqz
       |  jumpnz .fmt_zp_done
       |  local_get 7
       |  dec
       |  local_set 7
       |  push_i8 48           ; '0'
       |  local_get 7
       |  store8
       |  local_get 11
       |  dec
       |  local_set 11
       |  jump .fmt_zp_loop
       |.fmt_zp_done:
       |  ; Then emit sign at the very front
       |  local_get 9
       |  jumpz .fmt_zp_no_sign
       |  local_get 7
       |  dec
       |  local_set 7
       |  local_get 9
       |  local_get 7
       |  store8
       |.fmt_zp_no_sign:
       |  ; final length = total_len + initial_pad_count = original (10) + (width-10) = width-ish.
       |  ; recompute as (struct_addr + 80) - write_ptr
       |  local_get 8
       |  push_i8 80
       |  add
       |  local_get 7
       |  sub
       |  local_set 6
       |  jump .fmt_finish
       |.fmt_space_pad:
       |  ; Right-align with space fill: emit sign first, then space-pad before digits
       |  ; Order in memory (low to high): [spaces][sign][digits]
       |  ; We have digits at [write_ptr, write_ptr+digit_count). We need to prepend
       |  ; sign (if any), then pad_count spaces before that.
       |  local_get 9
       |  jumpz .fmt_sp_no_sign
       |  local_get 7
       |  dec
       |  local_set 7
       |  local_get 9
       |  local_get 7
       |  store8
       |.fmt_sp_no_sign:
       |.fmt_sp_loop:
       |  local_get 11
       |  eqz
       |  jumpnz .fmt_sp_done
       |  local_get 7
       |  dec
       |  local_set 7
       |  push_i8 32           ; ' '
       |  local_get 7
       |  store8
       |  local_get 11
       |  dec
       |  local_set 11
       |  jump .fmt_sp_loop
       |.fmt_sp_done:
       |  local_get 8
       |  push_i8 80
       |  add
       |  local_get 7
       |  sub
       |  local_set 6
       |.fmt_finish:
       |  ; struct.ptr = write_ptr; struct.len = digit_count
       |  local_get 7
       |  local_get 8
       |  store64
       |  local_get 6
       |  local_get 8
       |  push_i8 8
       |  add
       |  store64
       |  local_get 8
       |  ret
       |
       |; __svm_str_fmt_str(s: *string, width: i64, leftAlign: bool) -> *string
       |; Pad a string with spaces to at least `width` characters. If `s.len >=
       |; width` the source descriptor is returned unchanged (no copy). Otherwise
       |; a fresh `width`-byte buffer + 16-byte descriptor is allocated on the
       |; memory stack; the buffer is filled with either "s_bytes + spaces"
       |; (leftAlign == 1) or "spaces + s_bytes" (right-align, leftAlign == 0)
       |; and the new descriptor points at it.
       |;
       |; Locals: 0=s, 1=width, 2=leftAlign, 3=s_ptr, 4=s_len, 5=pad_count,
       |;         6=buf_ptr, 7=struct_addr, 8=i (loop cursor), 9=aligned_buf_size.
       |global __svm_str_fmt_str, func
       |__svm_str_fmt_str:
       |  frame 10
       |  local_set 2            ; leftAlign
       |  local_set 1            ; width
       |  local_set 0            ; s
       |  ; s_ptr = s[0]; s_len = s[8]
       |  local_get 0
       |  load64
       |  local_set 3
       |  local_get 0
       |  push_i8 8
       |  add
       |  load64
       |  local_set 4
       |  ; if s_len >= width: return s (no padding needed)
       |  local_get 4
       |  local_get 1
       |  geu
       |  jumpz .fmtss_pad
       |  local_get 0
       |  ret
       |.fmtss_pad:
       |  ; pad_count = width - s_len
       |  local_get 1
       |  local_get 4
       |  sub
       |  local_set 5
       |  ; aligned_buf_size = (width + 7) & ~7
       |  local_get 1
       |  push_i8 7
       |  add
       |  push_i64 -8
       |  and
       |  local_set 9
       |  ; Allocate aligned_buf_size + 16 bytes (struct directly above buffer)
       |  push_i64 __sp
       |  dup
       |  load64
       |  local_get 9
       |  push_i8 16
       |  add
       |  sub
       |  dup
       |  rot
       |  store64
       |  local_set 7            ; struct_addr (lowest address)
       |  local_get 7
       |  push_i8 16
       |  add
       |  local_set 6            ; buf_ptr = struct_addr + 16
       |  ; Decide layout: leftAlign → s_bytes then spaces; else spaces then s_bytes.
       |  push_0
       |  local_set 8            ; i = 0
       |  local_get 2
       |  jumpz .fmtss_right
       |  ; left-align: copy s_bytes to buf[0..s_len), then pad spaces to width
       |.fmtss_la_copy:
       |  local_get 8
       |  local_get 4
       |  ltu
       |  jumpz .fmtss_la_pad
       |  local_get 3
       |  local_get 8
       |  add
       |  load8
       |  local_get 6
       |  local_get 8
       |  add
       |  store8
       |  local_get 8
       |  inc
       |  local_set 8
       |  jump .fmtss_la_copy
       |.fmtss_la_pad:
       |  ; spaces from buf[s_len .. width)
       |.fmtss_la_pad_loop:
       |  local_get 8
       |  local_get 1
       |  ltu
       |  jumpz .fmtss_finish
       |  push_i8 32             ; ' '
       |  local_get 6
       |  local_get 8
       |  add
       |  store8
       |  local_get 8
       |  inc
       |  local_set 8
       |  jump .fmtss_la_pad_loop
       |.fmtss_right:
       |  ; right-align: pad_count spaces at buf[0..pad_count), then s_bytes
       |.fmtss_ra_pad:
       |  local_get 8
       |  local_get 5
       |  ltu
       |  jumpz .fmtss_ra_copy
       |  push_i8 32             ; ' '
       |  local_get 6
       |  local_get 8
       |  add
       |  store8
       |  local_get 8
       |  inc
       |  local_set 8
       |  jump .fmtss_ra_pad
       |.fmtss_ra_copy:
       |  ; copy s_bytes to buf[pad_count .. width)
       |.fmtss_ra_copy_loop:
       |  local_get 8
       |  local_get 1
       |  ltu
       |  jumpz .fmtss_finish
       |  local_get 3
       |  local_get 8
       |  add
       |  local_get 5
       |  sub
       |  load8
       |  local_get 6
       |  local_get 8
       |  add
       |  store8
       |  local_get 8
       |  inc
       |  local_set 8
       |  jump .fmtss_ra_copy_loop
       |.fmtss_finish:
       |  ; struct.ptr = buf_ptr; struct.len = width
       |  local_get 6
       |  local_get 7
       |  store64
       |  local_get 1
       |  local_get 7
       |  push_i8 8
       |  add
       |  store64
       |  local_get 7
       |  ret
       |
       |; __svm_str_from_f64(x: f64) -> *string
       |; Format a finite double as decimal with up to 6 fractional digits.
       |; Trailing zeros (and a trailing '.') are trimmed so `3.14` looks like
       |; the interpreter / LLVM output rather than the TRISC "3.140000" style.
       |; NaN and infinities are not handled (no current call site exercises them).
       |;
       |; Memory layout, low → high:
       |;   [base+0..15]   string descriptor {ptr, len}
       |;   [base+16..47]  main buffer (32 bytes — sign + ≤17 int digits + '.' + 6 frac)
       |;   [base+48..63]  digit scratch (16 bytes — LSB-first integer digits)
       |;
       |; Locals:
       |;   0 = x           (possibly negated)
       |;   1 = struct_addr
       |;   2 = main_buf
       |;   3 = is_neg
       |;   4 = int_part_f
       |;   5 = int_int     (u64 integer part)
       |;   6 = frac_int    (u64 0..999999)
       |;   7 = write_ptr   (cursor in main_buf)
       |;   8 = digit_buf
       |;   9 = scratch (digit_count / loop counter)
       |;  10 = scratch (loop index)
       |;  11 = scratch (probe pointer for trim loop)
       |global __svm_str_from_f64, func
       |__svm_str_from_f64:
       |  frame 12
       |  local_set 0
       |  ; Allocate 64 bytes (struct + main + scratch).
       |  push_i64 __sp
       |  dup
       |  load64
       |  push_i8 64
       |  sub
       |  dup
       |  rot
       |  store64
       |  local_set 1
       |  local_get 1
       |  push_i8 16
       |  add
       |  local_set 2
       |  local_get 1
       |  push_i8 48
       |  add
       |  local_set 8
       |  ; is_neg = (x < 0.0)
       |  local_get 0
       |  push_f0
       |  flt
       |  local_set 3
       |  ; if neg: x = -x
       |  local_get 3
       |  jumpz .ffd_have_x
       |  local_get 0
       |  fneg
       |  local_set 0
       |.ffd_have_x:
       |  ; int_part_f = ftrunc(x)
       |  local_get 0
       |  ftrunc
       |  local_set 4
       |  ; int_int = f2u(int_part_f)
       |  local_get 4
       |  f2u
       |  local_set 5
       |  ; frac_int = f2u(fround((x - int_part_f) * 1000000.0))
       |  local_get 0
       |  local_get 4
       |  fsub
       |  push_i32 1000000
       |  i2f
       |  fmul
       |  fround
       |  f2u
       |  local_set 6
       |  ; Carry: if frac_int >= 1000000 then frac_int -= 1000000; int_int += 1
       |  local_get 6
       |  push_i32 1000000
       |  geu
       |  jumpz .ffd_no_carry
       |  local_get 6
       |  push_i32 1000000
       |  sub
       |  local_set 6
       |  local_get 5
       |  inc
       |  local_set 5
       |.ffd_no_carry:
       |  ; Extract integer-part digits LSB-first into digit_buf.
       |  push_0
       |  local_set 9                  ; digit_count
       |  ; Special-case int_int == 0 → one '0' digit.
       |  local_get 5
       |  jumpnz .ffd_int_loop
       |  push_i8 48
       |  local_get 8
       |  store8
       |  push_1
       |  local_set 9
       |  jump .ffd_int_done
       |.ffd_int_loop:
       |  local_get 5
       |  eqz
       |  jumpnz .ffd_int_done
       |  local_get 5
       |  push_i8 10
       |  modu
       |  push_i8 48
       |  add
       |  local_get 8
       |  local_get 9
       |  add
       |  store8
       |  local_get 5
       |  push_i8 10
       |  divu
       |  local_set 5
       |  local_get 9
       |  inc
       |  local_set 9
       |  jump .ffd_int_loop
       |.ffd_int_done:
       |  ; Main buffer writes start here.
       |  local_get 2
       |  local_set 7
       |  ; Write '-' if negative.
       |  local_get 3
       |  jumpz .ffd_no_sign
       |  push_i8 45
       |  local_get 7
       |  store8
       |  local_get 7
       |  inc
       |  local_set 7
       |.ffd_no_sign:
       |  ; Copy int digits MSB-first from digit_buf[count-1..0].
       |.ffd_int_copy:
       |  local_get 9
       |  eqz
       |  jumpnz .ffd_int_copied
       |  local_get 9
       |  dec
       |  local_set 9
       |  local_get 8
       |  local_get 9
       |  add
       |  load8
       |  local_get 7
       |  store8
       |  local_get 7
       |  inc
       |  local_set 7
       |  jump .ffd_int_copy
       |.ffd_int_copied:
       |  ; Write '.'
       |  push_i8 46
       |  local_get 7
       |  store8
       |  local_get 7
       |  inc
       |  local_set 7
       |  ; Reserve 6 frac slots and fill in reverse from LSB.
       |  local_get 7
       |  push_i8 6
       |  add
       |  local_set 7
       |  push_i8 6
       |  local_set 9
       |.ffd_frac_loop:
       |  local_get 9
       |  eqz
       |  jumpnz .ffd_frac_done
       |  local_get 7
       |  dec
       |  local_set 7
       |  local_get 6
       |  push_i8 10
       |  modu
       |  push_i8 48
       |  add
       |  local_get 7
       |  store8
       |  local_get 6
       |  push_i8 10
       |  divu
       |  local_set 6
       |  local_get 9
       |  dec
       |  local_set 9
       |  jump .ffd_frac_loop
       |.ffd_frac_done:
       |  ; Restore write_ptr to end of the frac block.
       |  local_get 7
       |  push_i8 6
       |  add
       |  local_set 7
       |  ; Trim trailing zeros.
       |.ffd_trim:
       |  local_get 7
       |  dec
       |  local_set 11
       |  local_get 11
       |  load8
       |  push_i8 48
       |  eq
       |  jumpz .ffd_trim_dot_check
       |  local_get 11
       |  local_set 7
       |  jump .ffd_trim
       |.ffd_trim_dot_check:
       |  ; If the new last char is '.', drop it too.
       |  local_get 7
       |  dec
       |  local_set 11
       |  local_get 11
       |  load8
       |  push_i8 46
       |  eq
       |  jumpz .ffd_finish
       |  local_get 11
       |  local_set 7
       |.ffd_finish:
       |  local_get 2
       |  local_get 1
       |  store64
       |  local_get 7
       |  local_get 2
       |  sub
       |  local_get 1
       |  push_i8 8
       |  add
       |  store64
       |  local_get 1
       |  ret
       |
       |segment rodata
       |global __svm_str_true_data, data, 5
       |global __svm_str_false_data, data, 6
       |  align 8
       |  dl -1                ; rc=-1 (immortal)
       |__svm_str_true_data:
       |  db 116                ; 't'
       |  db 114                ; 'r'
       |  db 117                ; 'u'
       |  db 101                ; 'e'
       |  db 0
       |  align 8
       |  dl -1
       |__svm_str_false_data:
       |  db 102                ; 'f'
       |  db 97                 ; 'a'
       |  db 108                ; 'l'
       |  db 115                ; 's'
       |  db 101                ; 'e'
       |  db 0
       |""".stripMargin

  def bootTof: TOF = svmAssemble(bootSource, relocatable = true)
  def ioTof: TOF = svmAssemble(ioSource, relocatable = true)
