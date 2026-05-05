# SVM — Sysl Virtual Machine Specification

The Sysl Virtual Machine (SVM) is a 64-bit stack-based bytecode virtual
machine and one of four backends for the Sysl compiler (alongside the
tree-walking interpreter, the LLVM backend, and the TRISC native backend).
It is designed to be small, fast to interpret, and easy to target from
codegen.

This document specifies the SVM execution model, memory model, instruction
encoding, instruction semantics, and the assembly syntax accepted by
`svmAssemble`.

The reference implementation is in this repository:

| Component | Path |
|---|---|
| Interpreter | `cpu/src/main/scala/io/github/edadma/trisc/SVM.scala` |
| Assembler | `svm/src/main/scala/io/github/edadma/trisc/svmAssemble.scala` |
| Asm parser | `svm/src/main/scala/io/github/edadma/trisc/SVMAssemblerParser.scala` |
| Runtime | `svm/src/main/scala/io/github/edadma/trisc/SVMRuntime.scala` |

The interpreter is byte-for-byte authoritative: anything this document
says about an opcode's behaviour is whatever `SVM.execute()` does for that
opcode.

---

## 1. Architectural Overview

SVM is a stack machine with these resources:

- A 64-bit byte-addressable memory shared with the host (the same
  `Addressable` abstraction used by the TRISC CPU). All loads and stores
  go through this memory.
- A **data stack** of 64-bit words (default depth 4096). All computation
  happens here. The top of the stack (TOS) is cached in a register
  (`tos`) for performance; the rest lives in `stack[0..sp]`.
- A separate **return stack** of 64-bit words (default depth 1024) that
  holds return addresses for `call`/`ret` and is also user-accessible
  via `r_push` / `r_pop` / `r_peek`.
- A **frame array** holding `CallFrame` records (default depth 1024).
  Each frame remembers the caller's IP and the base index of its
  function's locals.
- A **locals array** of 64-bit slots (default `256 * maxCallDepth`)
  carved into per-frame regions by the `frame` instruction.
- An **instruction pointer** `ip`, a byte address in the shared memory
  space.
- A processor `state` (Run / Halt / Wfi / Interrupt / IllegalDivide /
  UnimplementedOpcode).

There are **no general-purpose integer or floating-point registers**.
All values flow through the data stack.

### 1.1 Word size and types

Every data-stack slot is 64 bits. SVM does not type-tag values; the same
slot can hold a signed integer, an unsigned integer, an IEEE-754 double,
a memory address, or a packed bit-field, and the active interpretation is
determined by which instruction reads it.

Floating-point values are stored as their `doubleToLongBits` encoding,
so a 64-bit `push_i64` of the bit pattern of `1.0` is indistinguishable
from a `push_f1`.

### 1.2 Endianness

The bytecode stream is **big-endian** for all multi-byte immediates
(`push_i16`, `push_i32`, `push_i64`, branch offsets). Memory loads and
stores delegate to the underlying `Addressable`; in the standard
configuration this is **little-endian** to match TRISC and host x86_64.
Values pushed via `push_i64` are assembled big-endian into the bytecode
but loaded back into a 64-bit register with their numeric value
preserved either way — endianness only matters when a program reads its
own bytecode as data.

### 1.3 Why a stack VM

Stack VMs have no register-allocation problem: codegen is essentially
"emit the value, then emit the operator." This makes the SVM backend
the simplest of the four, both to implement and to extend with new
language features. The cost is denser bytecode (roughly 1.5× the
instruction count of an equivalent register VM), which the
superinstructions (§7) partially recover.

---

## 2. Memory Model

### 2.1 Address space

The host supplies a single `Addressable` object covering the entire
address space. This object is queried by:

- Instruction fetch (`mem.readByte(ip)`, `mem.readShort`, etc.)
- Data loads (`load8`..`load64`)
- Data stores (`store8`..`store64`)

Memory-mapped I/O works the same way it does on TRISC: writing to a
device-bound address (e.g. `0x100000` for stdout in the standard
runtime) triggers a side effect on the host.

### 2.2 The memory stack

SVM has no built-in heap allocator. The Sysl runtime convention reserves
a downward-growing **memory stack** in the shared address space for
allocation of:

- Slice descriptors (24 bytes: `{ ptr, len, cap, backref }`)
- String descriptors (16 bytes: `{ ptr, len }`)
- Heap-promoted local arrays / structs / closures
- Concatenated string buffers, formatted-integer buffers, etc.

The stack pointer lives at the data label `__sp` (a single 64-bit slot).
On boot, `_start` initializes it to `stdoutAddress - 8 = 0x0FFFF8`. The
runtime's allocation idiom is:

```
push_i64 __sp
dup
load64                ; (&__sp old_sp)
push_i8 N             ; size in bytes
push_i8 7
add
push_i8 -8
and                   ; aligned to 8
sub                   ; new_sp = old_sp - aligned_size
dup
rot
store64               ; commit new_sp; TOS = new_sp = allocated address
```

Allocations from this stack have **function-scope lifetime** in the
codegen contract: a function that allocates does not free, but the
memory naturally becomes reusable when later allocations from a deeper
frame fall above it. (Refcounted heap is layered on top by the Sysl
runtime; SVM itself knows nothing about ARC.)

### 2.3 Vector table

The first 16 bytes of memory are the vector table:

| Offset | Slot | Used by |
|---|---|---|
| `0x00` | initial IP | loaded by `reset()` into `ip` |
| `0x08` | interrupt handler | jumped to on `interrupt()` |

The boot module (§9.1) writes the symbols `_start` and `_fault` into
these two slots.

### 2.4 Code, data, rodata

The assembler supports three named segments by convention: `code` (the
default), `data`, and `rodata`. Segments are concatenated in declaration
order unless explicit `org`s are passed to `svmAssemble`. There is no
hardware memory protection: code, data, and rodata sit in the same flat
address space and the VM does not enforce read-only or no-execute
properties on rodata.

---

## 3. Execution Model

### 3.1 Instruction cycle

`SVM.execute()` is one fetch-decode-dispatch step:

1. If `state == Interrupt`, run `handleInterrupt()` and return.
2. Fetch one byte at `ip` as the opcode; advance `ip`.
3. Increment `cycles`.
4. Dispatch through a `match` on the opcode.
5. The handler may consume additional immediate bytes from the bytecode
   stream (advancing `ip`) and/or pop and push data-stack values.
6. Unknown opcodes set `state = UnimplementedOpcode`.

`run()` loops `execute()` while `state` is `Run` or `Interrupt`. The
limit field, if positive, decrements each cycle and stops the loop at
zero — used by tests to bound execution.

### 3.2 Stack operations and TOS caching

Conceptually, every instruction operates on a single 64-bit-word data
stack. The implementation caches the topmost element in a register
(`tos`) and stores `stack[0..sp]` in memory. The depth of the stack is
`sp + 1` when `sp >= 0`; `sp == -1` denotes empty.

This caching is invisible at the instruction level — operators that
mutate "TOS in place" (`inc`, `dec`, `not`, `neg`, `eqz`, …) simply
update the register, while binary operators that consume two values
pop one and combine it with `tos`. Stack-effect notation in this
document uses traditional Forth `( before -- after )` form, with the
right end being TOS.

### 3.3 Halt and traps

- `halt` (0x6B) — stops the VM by setting `state = Halt`. The value
  on TOS is the program's "result" (`SVM.result`); the test harness
  reads it to determine pass/fail.
- `trap u8` (0x6A) — calls the protected `handleTrap(num)` method.
  The default implementation halts on any number except `0`, which
  enters `Wfi` state to wait for an interrupt. Subclasses (e.g. test
  harnesses, OS kernels) override `handleTrap` to implement syscalls.
- `breakpoint` (0xFF) — also halts. Reserved for debugger use.

### 3.4 Interrupts

Devices on the shared bus may call `interrupt()` to request servicing.
The current instruction completes; on the next cycle, if the VM is in
`Run` or `Wfi`, the handler at vector slot 1 (`VECTOR_INTERRUPT = 0x08`)
is invoked: the current `ip` is pushed onto the return stack, `ip` is
loaded from the vector slot, and `state` becomes `Run`. The handler
returns with `ret` like any other call.

If the vector slot contains `0`, the interrupt is silently consumed.

### 3.5 States

| State | Meaning |
|---|---|
| `Run` | Executing instructions normally |
| `Halt` | Stopped — by `halt`, `breakpoint`, or unknown trap |
| `Wfi` | Waiting for interrupt (entered by `trap 0`) |
| `Interrupt` | Pending interrupt; handled at start of next cycle |
| `IllegalDivide` | Divide by zero on `div`/`mod`/`divu`/`modu`/`divmod` |
| `UnimplementedOpcode` | Unknown opcode at `ip - 1` |

---

## 4. Calling Convention

### 4.1 Frames and locals

A function prologue allocates `N` 64-bit local slots with a single
instruction:

```
frame N           ; opcode 0x70, N is u8 → at most 255 locals/function
```

`frame` does three things:

1. Pushes a new `CallFrame { returnIP = ip, localsBase = localsTop, localsCount = n }` onto the frame stack.
2. Zero-initializes locals `locals[localsTop .. localsTop + n - 1]`.
3. Advances `localsTop` by `n`.

`local_get idx`, `local_set idx`, and `local_tee idx` index the current
frame's locals using `localsBase + idx`. The byte-sized index is
unsigned (0–255) — functions needing more than 256 locals must spill
manually to the memory stack.

### 4.2 Argument passing

SVM has no separate argument area: arguments are pushed left-to-right
onto the data stack before the `call`. The first thing a callee does
after `frame N` is pop them in reverse order with `local_set k` until
all arguments are bound. Conventional Sysl codegen layout is:

```
push arg0
push arg1
push arg2
call f               ; pushes return IP onto return stack
                     ; data stack: ( arg0 arg1 arg2 ) on entry to f

f:
  frame 3
  local_set 2        ; arg2 (TOS first)
  local_set 1        ; arg1
  local_set 0        ; arg0
  ...
  ret                ; pops return IP, restores frame
```

Return values are left on the data stack at `ret` — typically a single
scalar, but may be multiple slots for tuple returns or struct-by-value
returns.

### 4.3 Call and return

| Instr | Encoding | Semantics |
|---|---|---|
| `call rel32` | `0x63 + i32` | `rstack[++rsp] = ip; ip += rel32` |
| `tail rel32` | `0x65 + i32` | `ip += rel32` (no frame change) |
| `callr` | `0x66` | `addr = pop(); rstack[++rsp] = ip; ip = addr` |
| `tailr` | `0x67` | `addr = pop(); ip = addr` |
| `call_abs abs64` | `0x69 + i64` | `rstack[++rsp] = ip; ip = abs64` |
| `ret` | `0x64` | restore frame; `ip = rstack[rsp--]` |

`call_abs` exists for relocatable bytecode — when the callee's address
is unknown until link time, the assembler promotes a `call` of an
extern symbol to `call_abs` with an `ABS64` relocation entry. Likewise
`push_i32 sym` of an extern reference is promoted to `push_i64 sym +
ABS64`. (See §8.5.)

`tail` and `tailr` jump without saving the return IP and without
allocating a fresh frame; the callee reuses the caller's frame
(including its locals). This is sound only if the caller will not
read its locals after the tail-call — the standard way to exploit it is
self-tail-recursion.

### 4.4 Frame restoration on `ret`

`ret` does:

```
if fp >= 0:
  frame = frames[fp]
  localsTop = frame.localsBase
  fp -= 1
ip = rstack[rsp--]
```

Note that **`localsTop` is rolled back unconditionally** — even if the
callee allocated frames more deeply and they have all already returned.
This relies on the invariant that frames are nested LIFO, which the
standard codegen guarantees.

The return IP comes from the return stack, *not* from the frame's
`returnIP` field. The `returnIP` field exists for debugging and
introspection; the canonical return mechanism is the return stack.

### 4.5 Auxiliary return-stack ops

`r_push` ( a -- ), `r_pop` ( -- a ), `r_peek` ( -- a ) move a value
between the data stack and the top of the return stack. Useful for
temporary storage that must survive a `call` (since the call leaves the
data stack alone but pushes onto the return stack). Symmetric usage
required: a function that `r_push`es must `r_pop` before its own `ret`.

---

## 5. Memory Loads and Stores

All loads consume an address from TOS and replace it with the loaded
value. All stores consume `( value addr -- )` (note: addr is on top).

### 5.1 Loads

| Mnemonic | Opcode | Width | Sign-extend? |
|---|---|---|---|
| `load8` | 0x50 | 1 byte | no (zero-extend to 64) |
| `load8s` | 0x51 | 1 byte | yes |
| `load16` | 0x52 | 2 bytes | no |
| `load16s` | 0x53 | 2 bytes | yes |
| `load32` | 0x54 | 4 bytes | no |
| `load32s` | 0x55 | 4 bytes | yes |
| `load64` | 0x56 | 8 bytes | n/a |

There is **no `load64s`** — 64-bit loads are bit-exact, and if the
value is being treated as signed at the language level, no extra
extension is needed.

### 5.2 Stores

| Mnemonic | Opcode | Width |
|---|---|---|
| `store8` | 0x58 | low byte |
| `store16` | 0x59 | low two bytes |
| `store32` | 0x5A | low four bytes |
| `store64` | 0x5B | full 64 bits |

Narrow stores ignore the high bits of the value silently — they neither
trap nor sign-test. The Sysl codegen emits an explicit truncation
(`push_i32 mask; and`) only when subsequent reads can observe the
upper bits.

### 5.3 Address arithmetic

There is no dedicated `add_addr` or pointer-arithmetic instruction.
Pointer math is plain integer math:

```
push_i64 base
push_i32 offset
add
load64                ; *(base + offset)
```

The superinstructions (§7) compress some common patterns of this:
`push_i8_add` is "push a small constant and add it" in one byte.

---

## 6. Instruction Set Reference

This is the complete instruction set, organized by opcode group. For
each instruction we give the mnemonic, opcode, encoded size in bytes,
stack effect, and a one-line semantics summary.

Stack-effect notation: `( a b c -- d )` means before the instruction the
stack ends in `... a b c` (with `c` on top) and after it ends in
`... d`.

### 6.1 0x00–0x0F — Stack manipulation

| Op | Mnemonic | Bytes | Effect | Semantics |
|---|---|---|---|---|
| 0x00 | `nop` | 1 | `( -- )` | No-op |
| 0x01 | `drop` | 1 | `( a -- )` | Discard TOS |
| 0x02 | `dup` | 1 | `( a -- a a )` | Duplicate TOS |
| 0x03 | `swap` | 1 | `( a b -- b a )` | Swap top two |
| 0x04 | `over` | 1 | `( a b -- a b a )` | Copy second over |
| 0x05 | `rot` | 1 | `( a b c -- b c a )` | Rotate top three left |
| 0x06 | `nrot` | 1 | `( a b c -- c a b )` | Rotate top three right (inverse `rot`) |
| 0x07 | `nip` | 1 | `( a b -- b )` | Drop second |
| 0x08 | `tuck` | 1 | `( a b -- b a b )` | Insert TOS below second |
| 0x09 | `drop2` | 1 | `( a b -- )` | Drop top two |
| 0x0A | `dup2` | 1 | `( a b -- a b a b )` | Duplicate top pair |
| 0x0B | `swap2` | 1 | `( a b c d -- c d a b )` | Swap top two pairs |
| 0x0C | `over2` | 1 | `( a b c d -- a b c d a b )` | Copy second pair |
| 0x0D | `depth` | 1 | `( -- n )` | Push current data-stack depth |

(Opcodes `0x0E` and `0x0F` are unallocated.)

### 6.2 0x10–0x1F — Literals

| Op | Mnemonic | Bytes | Effect |
|---|---|---|---|
| 0x10 | `push_0` | 1 | `( -- 0 )` |
| 0x11 | `push_1` | 1 | `( -- 1 )` |
| 0x12 | `push_2` | 1 | `( -- 2 )` |
| 0x13 | `push_m1` | 1 | `( -- -1 )` |
| 0x14 | `push_i8 i8` | 2 | `( -- sext64(i8) )` |
| 0x15 | `push_u8 u8` | 2 | `( -- zext64(u8) )` |
| 0x16 | `push_i16 i16` | 3 | `( -- sext64(i16) )` |
| 0x17 | `push_i32 i32` | 5 | `( -- sext64(i32) )` |
| 0x18 | `push_i64 i64` | 9 | `( -- i64 )` |

The assembler accepts `push_i8 -1`, `push_i8 0xff`, etc.; the immediate
field is interpreted as signed for `push_i8`/`push_i16`/`push_i32` and
unsigned for `push_u8`. `push_i64` takes a literal, a relocation
reference, or a folded constant expression.

The instruction `push_i32 sym` of an unresolved symbol is **promoted by
the assembler** to a `push_i64` with an `ABS64` relocation, because
relocations cannot be 32-bit-signed-extended at link time. The
codegen-side mnemonic is preserved; only the encoded size grows from 5
to 9 bytes.

### 6.3 0x20–0x2F — Integer arithmetic

All operate on 64-bit values; signed/unsigned distinction matters only
for division/comparison.

| Op | Mnemonic | Bytes | Effect | Semantics |
|---|---|---|---|---|
| 0x20 | `add` | 1 | `( a b -- a+b )` | Two's-complement add |
| 0x21 | `sub` | 1 | `( a b -- a-b )` | Two's-complement sub |
| 0x22 | `mul` | 1 | `( a b -- a*b )` | Low 64 bits of product |
| 0x23 | `div` | 1 | `( a b -- a/b )` | Signed; `b == 0` → `IllegalDivide` |
| 0x24 | `mod` | 1 | `( a b -- a%b )` | Signed (Java semantics: sign of dividend) |
| 0x25 | `divmod` | 1 | `( a b -- rem quot )` | Signed; both results pushed |
| 0x26 | `divu` | 1 | `( a b -- a/b )` | Unsigned divide |
| 0x27 | `modu` | 1 | `( a b -- a%b )` | Unsigned remainder |
| 0x28 | `neg` | 1 | `( a -- -a )` | Two's-complement negate |
| 0x29 | `abs` | 1 | `( a -- |a| )` | Java `Math.abs`; `Long.MIN_VALUE` returns itself |
| 0x2A | `inc` | 1 | `( a -- a+1 )` | Wraps on overflow |
| 0x2B | `dec` | 1 | `( a -- a-1 )` | Wraps on underflow |

Division by zero deterministically transitions the VM to
`State.IllegalDivide` and stops execution; it does *not* trap to a
handler. Programs that need to handle division failure must guard
against zero divisors before issuing the instruction.

### 6.4 0x30–0x3F — Bitwise

| Op | Mnemonic | Bytes | Effect | Semantics |
|---|---|---|---|---|
| 0x30 | `and` | 1 | `( a b -- a&b )` | Bitwise AND |
| 0x31 | `or` | 1 | `( a b -- a|b )` | Bitwise OR |
| 0x32 | `xor` | 1 | `( a b -- a^b )` | Bitwise XOR |
| 0x33 | `not` | 1 | `( a -- ~a )` | Bitwise NOT |
| 0x34 | `shl` | 1 | `( v s -- v<<s )` | Logical shift left; `s` is `Int` (low 32 bits of TOS), modulo 64 per Java |
| 0x35 | `shr` | 1 | `( v s -- v>>>s )` | Logical (zero-fill) shift right |
| 0x36 | `sar` | 1 | `( v s -- v>>s )` | Arithmetic (sign-fill) shift right |
| 0x37 | `clz` | 1 | `( a -- )` | TOS replaced with leading-zero count |
| 0x38 | `ctz` | 1 | `( a -- )` | TOS replaced with trailing-zero count |
| 0x39 | `popcnt` | 1 | `( a -- )` | TOS replaced with population count |
| 0x3A | `rotl` | 1 | `( v s -- )` | Rotate left (`s` mod 64) |
| 0x3B | `rotr` | 1 | `( v s -- )` | Rotate right (`s` mod 64) |
| 0x3C | `bswap` | 1 | `( a -- )` | Reverse byte order of TOS |

`shl`/`shr`/`sar` truncate the shift count to `Int` first and then to a
shift amount. Java's `<<`, `>>>`, `>>` mask the count to its low 6 bits;
shifts of 64 or more therefore behave as shifts of `count & 63`, *not*
as zeroing — this is a deliberate alignment with TRISC and most ISAs.

### 6.5 0x40–0x4F — Comparison

All comparison instructions push `1` for true, `0` for false.

| Op | Mnemonic | Bytes | Effect | Semantics |
|---|---|---|---|---|
| 0x40 | `eq` | 1 | `( a b -- bool )` | `a == b` |
| 0x41 | `neq` | 1 | `( a b -- bool )` | `a != b` |
| 0x42 | `lt` | 1 | `( a b -- bool )` | Signed `a < b` |
| 0x43 | `gt` | 1 | `( a b -- bool )` | Signed `a > b` |
| 0x44 | `le` | 1 | `( a b -- bool )` | Signed `a <= b` |
| 0x45 | `ge` | 1 | `( a b -- bool )` | Signed `a >= b` |
| 0x46 | `ltu` | 1 | `( a b -- bool )` | Unsigned `a < b` |
| 0x47 | `gtu` | 1 | `( a b -- bool )` | Unsigned `a > b` |
| 0x48 | `leu` | 1 | `( a b -- bool )` | Unsigned `a <= b` |
| 0x49 | `geu` | 1 | `( a b -- bool )` | Unsigned `a >= b` |
| 0x4A | `eqz` | 1 | `( a -- bool )` | `a == 0` (TOS in place) |
| 0x4B | `nez` | 1 | `( a -- bool )` | `a != 0` |
| 0x4C | `ltz` | 1 | `( a -- bool )` | `a < 0` (signed) |
| 0x4D | `gtz` | 1 | `( a -- bool )` | `a > 0` (signed) |
| 0x4E | `lez` | 1 | `( a -- bool )` | `a <= 0` (signed) |
| 0x4F | `gez` | 1 | `( a -- bool )` | `a >= 0` (signed) |

The unary forms (`eqz`..`gez`) are TOS-in-place — they do not change the
data-stack depth. The binary forms pop two and push one boolean.

### 6.6 0x50–0x5F — Memory

See §5 for full details. Summary:

| Op | Mnemonic | Bytes | Effect |
|---|---|---|---|
| 0x50 | `load8` | 1 | `( addr -- u8 )` |
| 0x51 | `load8s` | 1 | `( addr -- i8 )` |
| 0x52 | `load16` | 1 | `( addr -- u16 )` |
| 0x53 | `load16s` | 1 | `( addr -- i16 )` |
| 0x54 | `load32` | 1 | `( addr -- u32 )` |
| 0x55 | `load32s` | 1 | `( addr -- i32 )` |
| 0x56 | `load64` | 1 | `( addr -- v )` |
| 0x58 | `store8` | 1 | `( v addr -- )` |
| 0x59 | `store16` | 1 | `( v addr -- )` |
| 0x5A | `store32` | 1 | `( v addr -- )` |
| 0x5B | `store64` | 1 | `( v addr -- )` |

Opcodes `0x57` and `0x5C`–`0x5F` are unallocated.

### 6.7 0x60–0x6F — Control flow

| Op | Mnemonic | Bytes | Operand | Semantics |
|---|---|---|---|---|
| 0x60 | `jump` | 3 | rel16 | `ip += rel16` |
| 0x61 | `jumpz` | 3 | rel16 | `if pop() == 0 then ip += rel16` |
| 0x62 | `jumpnz` | 3 | rel16 | `if pop() != 0 then ip += rel16` |
| 0x63 | `call` | 5 | rel32 | `rstack[++rsp] = ip; ip += rel32` |
| 0x64 | `ret` | 1 | — | restore frame; `ip = rstack[rsp--]` |
| 0x65 | `tail` | 5 | rel32 | `ip += rel32` (no frame change) |
| 0x66 | `callr` | 1 | — | `addr = pop(); rstack[++rsp] = ip; ip = addr` |
| 0x67 | `tailr` | 1 | — | `addr = pop(); ip = addr` |
| 0x68 | `jump_wide` | 5 | rel32 | `ip += rel32` |
| 0x69 | `call_abs` | 9 | abs64 | `rstack[++rsp] = ip; ip = abs64` |
| 0x6A | `trap` | 2 | u8 | `handleTrap(u8)` |
| 0x6B | `halt` | 1 | — | `state = Halt` |

Branch offsets are computed from the byte **after** the instruction —
i.e., the offset is added to the post-fetch `ip`. So `jump 0` is a no-op
control-wise (falls through to the next instruction), and `jump -3`
(self-jump for `jump`'s 3-byte size) is an infinite loop.

`jumpz` and `jumpnz` consume their condition from the data stack
unconditionally — even on the not-taken path. Codegen that wants to
preserve the value should `dup` it before the branch.

### 6.8 0x70–0x7F — Locals

| Op | Mnemonic | Bytes | Operand | Effect |
|---|---|---|---|---|
| 0x70 | `frame` | 2 | u8 n | Allocate `n` zeroed locals; push frame |
| 0x71 | `local_get` | 2 | u8 idx | `( -- locals[base+idx] )` |
| 0x72 | `local_set` | 2 | u8 idx | `( v -- )` writes to `locals[base+idx]` |
| 0x73 | `local_tee` | 2 | u8 idx | `( v -- v )` writes without consuming |

Locals are 64-bit slots. Each call site needs `frame N` before any
`local_get`/`local_set` with index < N; the set of valid indices is
`[0, N)`. Re-entering a function (recursion) creates a fresh frame each
time, so locals do not collide.

### 6.9 0x80–0x8F — Return stack

| Op | Mnemonic | Bytes | Effect |
|---|---|---|---|
| 0x80 | `r_push` | 1 | `( a -- )`, `rstack[++rsp] = a` |
| 0x81 | `r_pop` | 1 | `( -- a )`, `a = rstack[rsp--]` |
| 0x82 | `r_peek` | 1 | `( -- a )`, `a = rstack[rsp]` |

These let user code use the return stack as a scratch save/restore area
for values that must outlive a `call` (since the data stack is
otherwise unmolested by `call`/`ret`).

### 6.10 0x90–0xAF — Floating point

All floats are 64-bit IEEE-754 doubles, encoded on the stack via Java's
`doubleToLongBits` / `longBitsToDouble`.

| Op | Mnemonic | Bytes | Effect |
|---|---|---|---|
| 0x90 | `fadd` | 1 | `( a b -- a+b )` |
| 0x91 | `fsub` | 1 | `( a b -- a-b )` |
| 0x92 | `fmul` | 1 | `( a b -- a*b )` |
| 0x93 | `fdiv` | 1 | `( a b -- a/b )` |
| 0x94 | `fmod` | 1 | `( a b -- a%b )` |
| 0x95 | `fneg` | 1 | `( a -- -a )` (TOS in place) |
| 0x96 | `fabs` | 1 | `( a -- |a| )` |
| 0x97 | `fsqrt` | 1 | `( a -- sqrt(a) )` |
| 0x98 | `ffloor` | 1 | `( a -- floor(a) )` |
| 0x99 | `fceil` | 1 | `( a -- ceil(a) )` |
| 0x9A | `fround` | 1 | `( a -- rint(a) )` (banker's rounding) |
| 0x9B | `ftrunc` | 1 | `( a -- trunc(a) )` (toward zero) |
| 0x9C | `fmin` | 1 | `( a b -- min(a,b) )` |
| 0x9D | `fmax` | 1 | `( a b -- max(a,b) )` |
| 0x9E | `feq` | 1 | `( a b -- bool )` IEEE `==` |
| 0x9F | `fneq` | 1 | `( a b -- bool )` |
| 0xA0 | `flt` | 1 | `( a b -- bool )` |
| 0xA1 | `fgt` | 1 | `( a b -- bool )` |
| 0xA2 | `fle` | 1 | `( a b -- bool )` |
| 0xA3 | `fge` | 1 | `( a b -- bool )` |
| 0xA4 | `f2i` | 1 | `( a -- )` — TOS becomes signed-int truncation of double |
| 0xA5 | `i2f` | 1 | `( a -- )` — TOS becomes double of signed long |
| 0xA6 | `f2u` | 1 | `( a -- )` — TOS becomes saturated unsigned-int truncation: `<0 → 0`, `>= 2^64 → -1L`, else `f.toLong` |
| 0xA7 | `u2f` | 1 | `( a -- )` — TOS becomes double from u64 (handles bit 63) |
| 0xA8 | `push_f0` | 1 | `( -- 0.0 )` (stored as 0L) |
| 0xA9 | `push_f1` | 1 | `( -- 1.0 )` (stored as `doubleToLongBits(1.0)`) |

Comparison semantics follow Java's `==`, `!=`, `<`, etc., on `double`,
which means `NaN` is never equal to anything (including itself) and
ordered comparisons against `NaN` return `0`. `feq NaN NaN` returns `0`,
`fneq NaN NaN` returns `1`.

### 6.11 0xB0–0xCF — Superinstructions

These are fused common patterns — codegen may emit them, but a basic
backend can omit them entirely without correctness loss. Each
superinstruction has a multi-instruction equivalent that produces the
same observable result.

| Op | Mnemonic | Bytes | Effect | Equivalent |
|---|---|---|---|---|
| 0xB0 | `dup_add` | 1 | `( a -- 2a )` | `dup; add` |
| 0xB1 | `dup_mul` | 1 | `( a -- a² )` | `dup; mul` |
| 0xB2 | `over_add` | 1 | `( a b -- a a+b )` | `over; add` |
| 0xB3 | `over_sub` | 1 | `( a b -- a a-b )` | `over; sub` |
| 0xB4 | `add_imm8 i8` | 2 | `( a -- a+i8 )` | `push_i8 i8; add` |
| 0xB5 | `sub_imm8 i8` | 2 | `( a -- a-i8 )` | `push_i8 i8; sub` |
| 0xB6 | `mul_imm8 i8` | 2 | `( a -- a*i8 )` | `push_i8 i8; mul` |
| 0xB7 | `eqz_jumpz rel16` | 3 | `( a -- )` jump if `a != 0` | `eqz; jumpz` (note: `eqz` produces 0 for nonzero, then `jumpz` takes it — net effect is "branch if a was nonzero") |
| 0xB8 | `eqz_jumpnz rel16` | 3 | `( a -- )` jump if `a == 0` | `eqz; jumpnz` |
| 0xB9 | `inc_jumpnz rel16` | 3 | `( a -- a+1 )` jump if result nonzero | `inc; …` (note: keeps incremented value on TOS) |
| 0xBA | `dec_jumpnz rel16` | 3 | `( a -- a-1 )` jump if result nonzero | `dec; …` (loop counter idiom) |
| 0xBB | `local_get_add idx` | 2 | `( a -- a + locals[idx] )` | `local_get idx; add` |
| 0xBC | `local_get_sub idx` | 2 | `( a -- a - locals[idx] )` | `local_get idx; sub` |
| 0xBD | `local_get_eqz idx` | 2 | `( -- bool )` | `local_get idx; eqz` |
| 0xBE | `local_get_jumpz idx, rel16` | 4 | jump if `locals[idx] == 0` | `local_get idx; jumpz` |
| 0xBF | `local_get_jumpnz idx, rel16` | 4 | jump if `locals[idx] != 0` | `local_get idx; jumpnz` |
| 0xC0 | `dup_load64` | 1 | `( a -- a *a )` | `dup; load64` |
| 0xC1 | `drop_jump rel16` | 3 | `( a -- )` then jump | `drop; jump` |
| 0xC2 | `push_i8_add i8` | 2 | `( a -- a+i8 )` | `push_i8 i8; add` (alias of `add_imm8`) |
| 0xC3 | `push_i8_load64 i8` | 2 | `( a -- *(a+i8) )` | `push_i8 i8; add; load64` |

Two subtleties:

- `inc_jumpnz` / `dec_jumpnz` **keep the modified counter on the
  stack**. The standard "decrement and loop" idiom is therefore:
  ```
  loop:
    ; ... loop body ...
    dec_jumpnz loop
    drop                ; counter (now 0) sitting on TOS
  ```
- `eqz_jumpz` is read as "do `eqz` then `jumpz`", which net-net branches
  when the **original** value was nonzero. The mnemonic name is the
  fused-pair name, not the semantic name.

(Opcodes `0xC4`–`0xFE` are unallocated.)

### 6.12 0xFF — Debug

| Op | Mnemonic | Bytes | Effect |
|---|---|---|---|
| 0xFF | `breakpoint` | 1 | `state = Halt` |

Reserved for debugger insertion. Behaviour is identical to `halt` in
the reference implementation; debugger-aware harnesses may distinguish.

---

## 7. Encoding Summary

Instruction encoding is a 1-byte opcode followed by an operand of one of
these shapes:

| Shape | Bytes total | Used by |
|---|---|---|
| opcode | 1 | most arithmetic, comparison, stack, memory ops |
| opcode + u8 | 2 | `push_i8`/`push_u8`, `frame`, `local_*`, `trap`, `*_imm8`, `local_get_add`, `local_get_sub`, `local_get_eqz`, `push_i8_add`, `push_i8_load64` |
| opcode + i16 | 3 | `push_i16`, `jump*`, `eqz_jump*`, `inc_jumpnz`, `dec_jumpnz`, `drop_jump` |
| opcode + u8 + i16 | 4 | `local_get_jumpz`, `local_get_jumpnz` |
| opcode + i32 | 5 | `push_i32`, `call`, `tail`, `jump_wide` |
| opcode + i64 | 9 | `push_i64`, `call_abs` |

The `instrSize` function in the assembler is authoritative; see
`svm/src/main/scala/io/github/edadma/trisc/svmAssemble.scala`.

In **relocatable mode**, the assembler conservatively enlarges any
`call`/`tail` of an unresolved (extern) symbol from 5 bytes to 9 bytes
(`call_abs` + `ABS64` reloc), and any `push_i32` of a symbol to a
9-byte `push_i64` + `ABS64`. This sizing decision is made in pass 1 so
that pass 2 produces matching byte offsets for branch targets.

---

## 8. Assembly Syntax

The assembler `svmAssemble(src, stacked, orgs, relocatable)` parses a
text source and emits a TOF (Trisc Object Format) module. The grammar
is implemented by `SVMAssemblerParser`.

### 8.1 Lines, comments, whitespace

- Lines are separated by `\n`. A logical line is one statement
  (instruction, directive, label, etc.) plus optional trailing comment.
- Spaces and tabs separate tokens within a line.
- `;` starts a line-comment that runs to end of line.
- `#text` (full-line) is recorded as a comment line in the TOF (used
  for source-listing aids); `;text` is purely lexical.

### 8.2 Tokens

- **Identifiers**: `[a-zA-Z_][a-zA-Z0-9_]*`
- **Decimal**: `[0-9]+`
- **Hex**: `0x` or `0X` followed by `[0-9a-fA-F]+`. Parsed as **unsigned**
  64-bit (so `0xffffffffffffffff` is a legal `push_i64` operand).
- **Float**: `123.456` or `1e10` (used in `dl`/`dd` and as `push_i64`
  operand, where it's converted to `doubleToLongBits`).
- **String**: `"..."` with C escapes (`\n \r \t \b \f \0 \\ \' \" \xHH \uHHHH`).
- **Char**: `'c'` — a single character with the same escapes; lexes as
  the codepoint as a `Long`.
- **Local label reference**: `.foo` — resolved against the most recent
  non-local label, e.g. `func.foo`.

### 8.3 Directives

| Directive | Meaning |
|---|---|
| `segment NAME` | Switch active segment (default `_default_`; common values: `code`, `data`, `rodata`) |
| `entry SYM` | Mark `SYM` as the program entry; recorded in TOF |
| `extern SYM` | Declare `SYM` as imported from another module |
| `global SYM[, kind[, size[, typeinfo]]]` | Mark `SYM` as exported. `kind` ∈ `{func, data, const}`; `size` is decimal or hex bytes (data only); `typeinfo` is an opaque identifier list |
| `align N` | Pad current segment to a multiple of `N` bytes |
| `equ NAME, EXPR` / `NAME = EXPR` | Define a compile-time symbolic constant |
| `include "PATH"` | Recorded in the AST but not currently expanded by the assembler |

### 8.4 Data and reservation

Data directives emit values in-place:

| Directive | Width | Meaning |
|---|---|---|
| `db v0, v1, ...` | 1 byte | bytes (also accepts string literals: emits UTF-8 + NUL) |
| `ds v0, v1, ...` | 2 bytes | shorts |
| `dw v0, v1, ...` | 4 bytes | words |
| `dl v0, v1, ...` | 8 bytes | longs (also accepts floats and reference symbols) |
| `dd v0, v1, ...` | 8 bytes | "default" — same as `dl` |

Reservation directives advance the segment pointer without emitting
any bytes:

| Directive | Width per element |
|---|---|
| `rb N` | 1 |
| `rs N` | 2 |
| `rw N` | 4 |
| `rl N` | 8 |
| `rd N` | 8 |

`N` must be a positive integer ≤ 10 MiB. Used for BSS-style
zero-initialized regions.

### 8.5 Labels

Two forms of label:

```
foo:           ; explicit colon — always a label, even if the name
               ; matches a mnemonic
foo            ; bare ident on a line by itself — a label only if it's
               ; not a mnemonic and not a directive
.bar           ; local label, resolves to "<last non-local>.bar"
.bar:          ; same with optional colon
```

Label-on-same-line as instruction is also valid:

```
foo: push_0
```

### 8.6 Expressions

Operands are expressions, not just literals:

- Primary: number, char, string, label reference, local label
  reference, or parenthesized expression.
- Unary minus.
- Binary operators are parsed but not currently used in instruction
  operands (the assembler folds only constants and references; complex
  arithmetic in operands is unsupported).
- Compile-time `equ`/`=` symbols are inlined wherever they appear.

All expression evaluation happens in pass 1 for size calculation and
pass 2 for emission. References to defined labels become byte offsets
relative to the post-fetch IP for branches, or absolute addresses
elsewhere. References to extern symbols leave a relocation entry in
the TOF.

### 8.7 Relocations

The assembler emits one relocation kind:

| Kind | Trigger | Effect |
|---|---|---|
| `ABS64` | `call`/`tail`/`callr` of extern, `push_i32`/`push_i64` of extern, `dl SYM` | Linker writes the resolved 64-bit absolute address into the 8-byte slot |

There is no PC-relative relocation type — branches must remain within
their compilation unit.

---

## 9. Runtime Conventions

The Sysl runtime layers conventions on top of the raw VM. These are
implemented as standard assembly modules linked into every program.

### 9.1 Boot module

```
_start:
  push_i64 initialSP
  push_i64 __sp
  store64
  call main
  halt
```

Sets up the memory stack pointer at `0x0FFFF8` (just below the device
region at `0x100000`), calls `main`, and halts when `main` returns.
`_fault` is wired to slot 1 of the vector table and just halts on any
interrupt.

The boot module also exports the `__sp` symbol — a single 64-bit data
slot that holds the live memory-stack pointer. All allocation
primitives read and update it directly.

### 9.2 I/O module

The I/O module exports `STDOUT = 0x100000` and a small library of
runtime helpers backed by memory-mapped writes to that address:

- `putchar` — write one byte
- `puts(s: *string)` — write all bytes of a `{ptr,len}` string
- `panic(s: *string)` — write `"panic: <msg>\n"` then `halt`
- `assert(cond, msg)` — fall through if `cond != 0`, else `panic`
- `unreachable(msg)`, `todo(msg)` — alias `panic`
- `exit(code)` — `halt` (the test harness treats `halt` as success)
- `write_str(s)` — alias of `puts`

It also exports the std.io descriptors (`std_io__STDIN`,
`std_io__STDOUT`, `std_io__STDERR`, the `O_*` flags, the `SEEK_*`
constants) as 8-byte data slots so user code can refer to them by
symbol.

### 9.3 String / slice / format helpers

The runtime defines three internal helpers used by codegen:

- `__svm_str_concat(l, r) -> *string` — allocate a fresh struct +
  buffer on the memory stack, copy both inputs.
- `__svm_str_eq(l, r) -> bool` — compare lengths, then bytes.
- `__svm_new_slice(byteSize, elemCount) -> *slice_struct` — allocate a
  24-byte slice descriptor + zero-filled data buffer on the memory
  stack.
- `__svm_str_from_i64(n) -> *string` — base-10 decimal conversion.
- `__svm_str_from_bool(b) -> *string` — `"true"` / `"false"`.
- `__svm_str_fmt_i64(n, base, width, flags) -> *string` — full integer
  formatter with sign / pad / case flags.

These are linked in only when codegen actually references them.

### 9.4 Memory layout summary

| Region | Address | Source |
|---|---|---|
| Vector table | `0x000000`–`0x00000F` | boot module |
| Code + data + rodata | `0x000010`–`...` | linked TOF segments |
| Static globals (`__sp`, etc.) | data segment | runtime |
| Memory stack | grows down from `0x0FFFF8` | runtime |
| STDOUT device | `0x100000` | host-side memory-mapped device |

Programs needing more memory-stack headroom should reduce the device
base or split the device region; the VM places no semantic constraint
on the layout, only `_start` does.

### 9.5 Test harness

The test harness compiles each Sysl test function with a tiny `main`
shim:

```
extern test_xyz
entry main

main:
  call test_xyz
  push_i64 0x5AFEFADE5AFEFADE     ; sentinel
  halt
```

After the VM halts, the harness reads `svm.result` (the cached TOS
value):

- If `result == sentinel` → the function returned cleanly → **pass**.
- Anything else → the function `halt`ed via `panic`/`assert`/etc., so
  the program never reached the sentinel push → **fail**.

Captured stdout is searched for `"panic: "` / `"assertion failed: "`
prefixes to extract the failure message. `#test(should_panic: "...")`
flips the sense of pass/fail.

---

## 10. Conformance and Compatibility

A conforming SVM implementation must:

1. Implement every defined opcode with the documented stack effect and
   semantics. Superinstructions (§6.11) are conformance-required: they
   are not merely peephole optimizations, codegen emits them directly.
2. Use 64-bit data and return stacks with the specified word size.
3. Honor the call/return discipline in §4 (frame allocation on `frame`,
   zero-init of locals, stack-based return address).
4. Support `ABS64` relocations on call/push targets.
5. Expose a `result` query that returns the post-halt TOS value.
6. Recognize the vector table layout in §2.3 on reset.

Conforming implementations are *not* required to:

- Provide a debugger or breakpoint handler beyond halting on `0xFF`.
- Optimize superinstructions versus their slow-path equivalents (the
  bytecode is identical from the program's view).
- Implement specific trap numbers beyond `trap 0` = WFI; everything
  else is host-policy.

Future revisions may allocate currently-unassigned opcodes (`0x0E`,
`0x0F`, `0x57`, `0x5C`–`0x5F`, `0xC4`–`0xFE`); existing programs are
unaffected because they do not encode those bytes.

---

## Appendix A — Opcode Map

```
        0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
0x00:  nop  drop dup  swap over rot  nrot nip  tuck dr2  dup2 sw2  ov2  dpth ---  ---
0x10:  p_0  p_1  p_2  p_m1 p_i8 p_u8 p_16 p_32 p_64 ---  ---  ---  ---  ---  ---  ---
0x20:  add  sub  mul  div  mod  divm divu modu neg  abs  inc  dec  ---  ---  ---  ---
0x30:  and  or   xor  not  shl  shr  sar  clz  ctz  popc rotl rotr bswp ---  ---  ---
0x40:  eq   neq  lt   gt   le   ge   ltu  gtu  leu  geu  eqz  nez  ltz  gtz  lez  gez
0x50:  l8   l8s  l16  l16s l32  l32s l64  ---  s8   s16  s32  s64  ---  ---  ---  ---
0x60:  jmp  jz   jnz  call ret  tail clrr tlrr jmpw cabs trap halt ---  ---  ---  ---
0x70:  fram lg   ls   lt   ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
0x80:  rps  rpp  rpk  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
0x90:  fadd fsub fmul fdiv fmod fneg fabs fsqr ffl  fcl  frnd ftru fmin fmax feq  fne
0xA0:  flt  fgt  fle  fge  f2i  i2f  f2u  u2f  pf0  pf1  ---  ---  ---  ---  ---  ---
0xB0:  d+   d*   o+   o-   +i8  -i8  *i8  ezjz ezjn icjn dcjn lg+  lg-  lgez lgjz lgjn
0xC0:  dpl6 drjm pi8+ pi8l ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
0xD0:  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
0xE0:  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
0xF0:  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  brk
```

(`---` = unallocated.)

## Appendix B — Worked Example

A function that returns the sum of its three byte arguments:

```
entry main

global sum3, func
sum3:
  frame 3
  local_set 2          ; arg2 (TOS)
  local_set 1          ; arg1
  local_set 0          ; arg0
  local_get 0
  local_get_add 1      ; superinstr: a0 + a1
  local_get_add 2      ; superinstr: + a2
  ret

main:
  push_i8 10
  push_i8 20
  push_i8 12
  call sum3            ; ( -- 42 )
  halt
```

After halt, `svm.result == 42`.
