# TRISC Application Binary Interface (ABI)

## Registers

| Register | Name | Purpose | Saved by |
|----------|------|---------|----------|
| r0 | zero | Hardwired to zero | — |
| r1 | a1/rv | Argument 1 / return value | Caller |
| r2 | a2 | Argument 2 | Caller |
| r3 | a3 | Argument 3 | Caller |
| r4 | a4 | Argument 4 | Caller |
| r5 | fp | Frame pointer | Callee |
| r6 | lr | Link register | Callee |
| r7 | sp | Stack pointer | Callee |

## Calling Convention

### Arguments

- First 4 arguments are passed in r1–r4
- Additional arguments are pushed onto the stack right-to-left (C order) before the call
- The caller is responsible for cleaning up stack arguments after the call returns

### Return Value

- Integer/pointer return value in r1
- Functions returning void leave r1 undefined

### Stack

- The stack grows downward (push decrements sp)
- sp (r7) must be 8-byte aligned at all times
- The caller sets up sp before program start (typically in init code)

### Function Prologue/Epilogue

Standard prologue:

```asm
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
jalr r0, r6       ; return
```

### Stack Frame Layout

```
Higher addresses
  +-----------------+
  | arg 6           |  [fp + 24]   (if > 4 args)
  | arg 5           |  [fp + 16]   (if > 4 args)
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

### Register Preservation

- **Caller-saved (volatile):** r1–r4 — may be destroyed by any call. The caller must save them if needed after the call.
- **Callee-saved (non-volatile):** r5–r7 — a called function must preserve these. If it uses them, it must save and restore them.
- **r0:** Always zero. Writes are ignored.

### Leaf Functions

Functions that don't call other functions (leaf functions) may skip saving lr (r6) since it won't be overwritten. The compiler can optimize this.

### System Calls

Trap instructions (`trap 0`–`trap 4`) are used for system calls. The trap number selects the handler via the exception vector table. Arguments follow the same r1–r4 convention.

## Data Types

| Type | Size | Alignment | Register |
|------|------|-----------|----------|
| char | 1 byte | 1 | r (zero-extended) |
| short | 2 bytes | 2 | r (sign-extended) |
| int | 4 bytes | 4 | r (sign-extended) |
| long | 8 bytes | 8 | r |
| pointer | 2 bytes* | 2 | r |
| double | 8 bytes | 8 | r (IEEE 754) |

*Pointer size depends on address width (2, 3, or 4 bytes). Default is 2 bytes (16-bit address space).

## Global Variables

Global variables are placed in the data or bss segment. The compiler emits `global` directives for exported symbols and `extern` for imported ones.

## String Literals

String literals are placed in the data segment as null-terminated UTF-8 byte sequences, 2-byte aligned.
