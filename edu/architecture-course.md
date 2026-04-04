# Computer Organization / Architecture — Course Outline

## Pacing rationale

Binary, number systems, and basic instruction encoding don't need three weeks — students at this level can absorb that in a single session. The course moves fast through fundamentals and spends time where things are actually hard and interesting: stack discipline, I/O, the exception model, and reading/modifying the real toolchain.

## Weekly outline

**Week 1**: The whole machine in one shot. ISA, registers, encoding, assembler, emulator. Students write and run a program by end of week. No slow buildup — just hand them the spec and go.

**Weeks 2–3**: The stack and calling convention. Not because it's conceptually hard, but because getting it *right* in practice takes repetition. Write functions, call functions, nest calls, debug when the stack is wrong. The single-arg-register ABI means every function exercises the stack.

**Weeks 4–5**: Data structures in assembly. Linked lists, arrays, structs laid out in memory. Pointer arithmetic. This is where assembly stops being toy programs and starts being real.

**Weeks 6–7**: Memory-mapped I/O. Timer, keyboard, stdout. Polling first, then interrupts. Students build something interactive — a simple game or a command-line shell that reads keyboard input.

**Weeks 8–9**: The exception model. Vector table, supervisor mode, dual stacks. Students write an exception handler from scratch. This is where the machine stops being a calculator and starts being a *system*.

**Weeks 10–11**: The emulator internals. Read `CPU.scala`, trace execution, understand the fetch-decode-execute loop as *code* not just a diagram. Modify it — add a performance counter, a new trap, instrument memory accesses.

**Weeks 12–13**: The assembler internals. Two-pass assembly, branch relaxation, symbol resolution, the TOF object format. Students read `assemble.scala`. Possibly extend the assembler with a new directive or pseudo-instruction.

**Weeks 14–15**: Capstone project. Design and implement a custom instruction end-to-end: define the encoding, add it to the assembler, implement it in the emulator, write a program that uses it. Or: build a simple device (like a hardware random number generator) and wire it into the memory map.

## What makes this different from a standard architecture course

Students don't just *learn about* these things from slides — they read the actual implementation and modify it. The toolchain being in Scala (not C or Verilog) means the source is readable by anyone who's taken an intro programming course.

## Capstone instruction menu

With a few free RR opcode slots, students can choose from options like:

- An arithmetic instruction (population count, byte swap, min/max)
- A memory instruction (atomic swap, block copy)
- A control instruction (conditional move, loop/decrement-and-branch)

Each one exercises different parts of the emulator. And they'd have to justify the design — why does this deserve a hardware instruction instead of a software sequence? That's a real architecture question.
