---
title: Calling Convention
description: The TRISC application binary interface.
---

## Register usage

| Register | Role | Saved by |
|----------|------|----------|
| r0 | Zero (hardwired) | -- |
| r1 | Argument 1 / return value | Caller |
| r2 | Scratch | Caller |
| r3 | Scratch | Caller |
| r4 | Call target / scratch | Caller |
| r5 | Frame pointer | Callee |
| r6 | Link register | Callee |
| r7 | Stack pointer | Callee |

## Arguments

**Only r1** is used for passing arguments. At most one scalar argument fits in a register — all remaining arguments are pushed onto the stack right-to-left (C order) before the call.

This is a deliberate design choice: it forces students to use the stack for multi-argument functions, which is the pedagogical point.

```asm
; Call add(3, 5) — r1 = first arg, second arg on stack
ldi r2, 5
pshd r2           ; push second arg
ldi r1, 3         ; first arg in r1
movi r4, add
jalr r6, r4       ; call
popd r2           ; clean up stack arg
; result in r1
```

## Return value

- Integer/pointer return value in **r1**
- Struct/string return: caller allocates a return slot on the stack and passes a hidden pointer as the first argument in r1

## Call sequence

1. Caller pushes arguments right-to-left onto the stack (except first arg in r1)
2. Caller loads function address into r4: `movi r4, func`
3. Caller calls: `jalr r6, r4` (saves return address in r6, jumps to r4)
4. Callee saves r5, r6 if it will use them or make further calls
5. Callee sets up frame pointer: `mov r5, r7`
6. Callee does its work
7. Callee restores r5, r6 and returns: `ret` (alias for `jalr r0, r6`)
8. Caller cleans up stack arguments

## Stack frame layout

```
            ┌──────────────────┐ ← caller's r5
            │ ...              │
            │ arg N (pushed first) │
            │ arg 2            │
            ├──────────────────┤ ← r7 at call site
            │ saved r6         │
            │ saved r5         │
            ├──────────────────┤ ← callee's r5
            │ local variables  │
            ├──────────────────┤ ← r7 during callee
            │ outgoing args    │
            └──────────────────┘
```

## Strings

Strings are 16-byte fat pointers `{ptr: *byte, len: i64}` (Go-style). They are always passed on the stack, even as the first argument:

- The caller pushes the 16-byte `{ptr, len}` pair
- For the first argument, the caller passes the address of the pair in r1
- The callee copies the 16 bytes into a local slot
