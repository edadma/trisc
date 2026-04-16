---
title: Introduction
description: What TRISC is and who it's for.
---

TRISC is a 64-bit RISC CPU architecture designed for teaching computer systems. It comes with a complete toolchain — assembler, linker, emulator, systems language compiler, and a microkernel operating system — all written in Scala 3.

## Design philosophy

TRISC makes one deliberate trade-off: **simplicity over performance**. Every design choice prioritizes making the system understandable to students:

- **8 registers** (r0 hardwired to zero) — the full register file fits in your head
- **One argument register** (r1) — forces students to use the stack, which is the point
- **16-bit fixed-width instructions** — easy to decode by hand
- **64-bit data** — modern enough to teach real addressing concepts
- **Big-endian** — byte order is visible and consistent

## What's included

| Component | Description |
|-----------|-------------|
| **Emulator** | Cycle-counting CPU emulator with GUI (Swing) and headless modes |
| **Assembler** | Two-pass assembler with segments, symbols, macros, and pseudo-instructions |
| **Linker** | Relocatable TOF object format with linker scripts |
| **Sysl** | Systems language (C-level control, Go syntax) compiling to TRISC and x86_64 via LLVM |
| **SLIX** | Minix 3-style microkernel OS with 5 isolated servers, IPC, virtual memory, and a shell |
| **Devices** | Timer, DMA, MMU, GPIO, UART, PL011, interrupt controller, ramdisk, framebuffer, sound, network |

## Who is this for?

TRISC is designed for university-level CS/CE courses that bridge architecture and operating systems:

- **Computer Architecture** — instruction encoding, register files, memory-mapped I/O, interrupts, page tables
- **Operating Systems** — scheduling, IPC, process isolation, virtual memory, device drivers, filesystem
- **Systems Programming** — assembly, calling conventions, the ABI, linker scripts, writing `cat` and `ls` in Sysl

Students can read every line of code from instruction decode to login prompt. The entire stack — ISA, assembler, compiler, kernel, servers, shell — is one repository.

## Multi-target

The emulator and toolchain run on three platforms via Scala's cross-compilation:

- **JVM** — primary development target, GUI emulator via Swing
- **JavaScript** (Scala.js) — embed in browser-based lab environments
- **Native** (Scala Native) — standalone executables, no JVM required

SLIX also runs on **real x86_64 hardware** via QEMU, using the Sysl compiler's LLVM backend.
