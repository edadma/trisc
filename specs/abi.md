# TRISC Application Binary Interface (ABI)

## Registers

| Register | Name | Purpose | Saved by |
|----------|------|---------|----------|
| r0 | zero | Hardwired to zero | — |
| r1 | a1/rv | Argument 1 / return value | Caller |
| r2 | t1 | Temporary / scratch | Caller |
| r3 | t2 | Temporary / scratch | Caller |
| r4 | t3 | Call target / temporary | Caller |
| r5 | fp | Frame pointer | Callee |
| r6 | lr | Link register | Callee |
| r7 | sp | Stack pointer | Callee |

## Calling Convention

### Arguments

- **Only r1** is used for argument passing (at most one scalar argument)
- All remaining arguments are pushed onto the stack right-to-left (C order) before the call
- r4 is reserved for the call target address (`movi r4, func; jalr r6, r4`)
- r2–r3 are free for intermediate results within a function
- The caller is responsible for cleaning up stack arguments after the call returns

### Return Value

- Integer/pointer return value in r1
- Struct/string return: caller allocates a return slot on the stack and passes a hidden pointer as the first argument in r1. All user params move to the stack.
- Functions returning void leave r1 undefined

### String Arguments

Strings are 16-byte fat pointers `{ptr, len}` (Go-style). They are always passed on the stack as 16 bytes, even when in the first argument position:

- **String as first arg:** The caller pushes the 16-byte `{ptr, len}` data above the stack args, then passes the address of that data in r1.
- **String as stack arg (2nd+):** The caller pushes 16 bytes `{ptr, len}` directly onto the call stack.
- **Callee:** Copies the 16 bytes into a local slot in the prologue. Register string params are dereferenced through the address in r1; stack string params are read directly.

### Stack

- The stack grows downward (push decrements sp)
- sp (r7) must be 8-byte aligned at all times
- The caller sets up sp before program start (typically in init code)

### Function Prologue/Epilogue

Standard prologue:

```asm
; save register argument (before frame setup)
pshd r1           ; save arg 1 (if present)
; save callee-saved registers
pshd r6           ; save link register
pshd r5           ; save frame pointer
mov r5, r7        ; frame pointer = stack pointer
addi r7, r7, -N   ; allocate N bytes for locals
```

Standard epilogue:

```asm
mov r7, r5        ; deallocate locals
popd r5           ; restore frame pointer
popd r6           ; restore link register
addi r7, r7, 8    ; skip past pre-prologue r1 push
jalr r0, r6       ; return
```

### Stack Frame Layout

```
Higher addresses
  +-----------------+
  | string data     |  (16 bytes, if first arg is a string — pushed by caller)
  +-----------------+
  | arg N (stack)   |  [fp + 24 + ...]  (string args: 16 bytes; scalar args: 8 bytes)
  | arg 2 (stack)   |  [fp + 24]
  +-----------------+
  | saved r1 (arg1) |  [fp + 16]   (register arg, pushed before prologue)
  +-----------------+
  | saved lr (r6)   |  [fp + 8]
  | saved fp (r5)   |  [fp + 0]    ← fp points here after prologue
  +-----------------+
  | local var 1     |  [fp - 8]
  | local var 2     |  [fp - 16]
  | ...             |
  +-----------------+  ← sp
Lower addresses
```

Only r1 is pushed before the prologue (if the function has at least one argument).
Stack args (params 2+) are pushed by the caller right-to-left and sit above the register arg.

### Register Preservation

- **Caller-saved (volatile):** r1–r4 — may be destroyed by any call. The caller must save them if needed after the call. r4 is used as the call target register.
- **Callee-saved (non-volatile):** r5–r7 — a called function must preserve these. If it uses them, it must save and restore them.
- **r0:** Always zero. Writes are ignored.

### Leaf Functions

Functions that don't call other functions (leaf functions) may skip saving lr (r6) since it won't be overwritten. The compiler can optimize this.

### System Calls

Trap instructions (`trap 0`–`trap 7`) are used for system calls. The trap number selects the handler via the exception vector table. The first argument is passed in r1.

## Data Types

| Type | Size | Alignment | Register |
|------|------|-----------|----------|
| byte / i8 | 1 byte | 1 | r (sign-extended) |
| u8 | 1 byte | 1 | r (zero-extended) |
| i16 | 2 bytes | 2 | r (sign-extended) |
| u16 | 2 bytes | 2 | r (zero-extended) |
| int / i32 | 4 bytes | 4 | r (sign-extended) |
| u32 | 4 bytes | 4 | r (zero-extended) |
| i64 | 8 bytes | 8 | r |
| u64 | 8 bytes | 8 | r |
| pointer | 8 bytes | 8 | r |
| string | 16 bytes | 8 | {ptr, len} fat pointer |
| double | 8 bytes | 8 | r (IEEE 754) |

## Strings

Strings are Go-style immutable fat pointers: 16 bytes = `{ptr: *i8, len: i64}`.

### Memory Layout

**Literals (rodata):**
```
  dq -1           ; immortal refcount (never freed)
label:
  db "hello", 0   ; UTF-8 bytes + null terminator (for *i8 decay)
```
String value: `{label, 5}`. Refcount at `[ptr - 8]`.

**Heap strings (from concatenation):**
```
[base + 0] : i64 refcount = 1
[base + 8] : bytes...
```
String value: `{base + 8, len}`. Refcount at `[ptr - 8]`.

### Refcounting

- Refcount header is 8 bytes at `[ptr - 8]`
- Immortal sentinel: `-1` — never incremented or decremented
- Heap strings start at refcount 1; freed when refcount reaches 0
- String→`*i8` decay: returns the ptr field (first 8 bytes of the fat pointer)

## Global Variables

Global variables are placed in the data or bss segment. The compiler emits `global` directives for exported symbols and `extern` for imported ones.
