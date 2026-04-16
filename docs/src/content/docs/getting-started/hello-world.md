---
title: Hello World
description: Write, assemble, and run your first TRISC program.
---

## The program

Create `hello.asm`:

```asm
; Vector table
  dl 0x7FFFF8        ; initial SSP (stack pointer)
  dl _start           ; initial PC (entry point)
  rb 144              ; remaining 18 exception vectors

_start
  movi r1, message    ; r1 = address of message
  movi r4, _putc      ; r4 = address of putc helper
_loop
  ldb r2, r1, r0      ; r2 = byte at [r1 + 0]
  beq r2, r0, _done   ; if r2 == 0, we're done
  pshb r2             ; push character to stack
  jalr r6, r4         ; call _putc
  addi r1, r1, 1      ; advance pointer
  bra _loop
_done
  halt

; Write one byte to stdout (address 0x800000)
_putc
  popb r2             ; pop character
  movi r3, 0x800000   ; stdout address
  stb r2, r3, r0      ; write byte to stdout
  ret

message
  db "Hello, world!", 10, 0
```

## Key concepts

### Vector table

The first 160 bytes (20 slots x 8 bytes) form the **vector table**, modeled after the Motorola 68000:

- **Slot 0**: Initial supervisor stack pointer (SSP)
- **Slot 1**: Initial program counter (PC) — where execution begins after reset
- **Slots 2-19**: Exception and trap handlers

The CPU reads slot 0 into r7 (stack pointer) and slot 1 into PC on reset, then begins executing in supervisor mode.

### Memory-mapped I/O

There are no special I/O instructions. Devices are accessed by reading and writing to specific memory addresses. The stdout device at `0x800000` accepts one byte per write — `stb r2, r3, r0` writes the byte in r2 to the address in r3.

### Calling convention

Only **r1** is used for passing arguments. Everything else goes on the stack. The `jalr r6, r4` instruction jumps to the address in r4 and saves the return address in r6. The `ret` pseudo-instruction returns via `jalr r0, r6`.

## Assemble and run

```bash
# Assemble
sbt "triscCliJVM/runMain io.github.edadma.trisc.run asm hello.asm"

# Run
sbt "triscCliJVM/runMain io.github.edadma.trisc.run run hello.tof"
```

Output:

```
Hello, world!
```
