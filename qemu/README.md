# QEMU x86_64 Bare-Metal Target

Runs Sysl programs as bare-metal x86_64 kernels under QEMU. The pipeline:

```
sysl → LLVM IR → x86_64 object → link with C shim → multiboot ELF → QEMU
```

## Quick Start

```bash
# Hello world
bash qemu/build.sh && bash qemu/run.sh

# SHA-256 benchmark
bash qemu/bench.sh && bash qemu/run-bench.sh
```

## Requirements

- `sbt` (builds the Sysl compiler)
- `clang` (compiles LLVM IR to x86_64 object)
- `x86_64-elf-binutils` (assembler, linker, objcopy)
- `qemu-system-x86_64`

On macOS: `brew install x86_64-elf-binutils qemu`

## Files

| File | Purpose |
|------|---------|
| `hello.sysl` | Minimal hello world |
| `bench.sysl` | SHA-256 benchmark (std.crypto.sha256) |
| `main.c` | C runtime shim: UART, bump allocator, libc stubs |
| `startup.s` | Multiboot → long mode trampoline, page tables, SSE init |
| `link.ld` | Linker script (load address 0x100000) |
| `build.sh` | Build hello.sysl |
| `bench.sh` | Build bench.sysl with std modules |
| `run.sh` | Run hello under QEMU |
| `run-bench.sh` | Run benchmark under QEMU |

## x86_64 Bare-Metal Requirements

Three things are critical for LLVM-generated code on bare metal:

### 1. SSE Must Be Enabled

LLVM uses SSE instructions (`movaps`, `xorps`) for memset/memcpy even at `-O0`.
Without enabling SSE via CR0/CR4, these fault silently (triple fault → hang with
no error output). The startup code must:

- Clear `CR0.EM` (bit 2) and set `CR0.MP` (bit 1)
- Set `CR4.OSFXSR` (bit 9) and `CR4.OSXMMEXCPT` (bit 10)

### 2. Identity-Map Enough Memory

The C shim includes a 1 MB bump-allocator heap in BSS. With the kernel loaded
at 0x100000, the BSS can extend past 2 MB. The page directory must map enough
physical memory (currently 8 MB via four 2 MB pages).

### 3. Argument Type Widening

When Sysl iterates a string (`for c in s`), each character is a byte (`i8`).
If passed to a function expecting `char` (`i32`), the codegen must emit a
`zext` to widen. Without this, the LLVM IR is invalid — arm64 clang may
tolerate it, but x86_64 freestanding clang may generate incorrect code.

## C Runtime Shim (`main.c`)

Provides the minimal runtime that Sysl's LLVM preamble expects:

- **`uart_putc(int c)`** — COM1 serial output (called from Sysl via `extern`)
- **`malloc(size)`** — 1 MB bump allocator (no free)
- **`free(p)`** — no-op
- **`memcpy`**, **`memset`** — byte-loop implementations
- **`write(fd, buf, len)`** — redirects to UART (used by panic/assert)
- **`abort()`** — halts the CPU

## Architecture

```
┌─────────────┐
│  startup.s  │  Multiboot header, 32→64 bit switch, page tables, SSE
│             │  Calls boot_entry()
├─────────────┤
│   main.c    │  uart_init(), boot_entry() → main(), libc stubs
├─────────────┤
│  *.sysl     │  Sysl program compiled to LLVM IR
│  (std/*)    │  Standard library modules (sha256, binary, etc.)
└─────────────┘
     ↓ QEMU boots via -kernel (multiboot)
     ↓ Serial output on COM1 → host stdout
     ↓ Exit via isa-debug-exit port 0xf4
```
