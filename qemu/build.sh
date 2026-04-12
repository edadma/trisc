#!/bin/bash
# Build a bare-metal x86_64 kernel from:
#   - startup.s   (multiboot → long mode trampoline)
#   - main.c      (C runtime shim: UART, libc stubs, entry point)
#   - hello.sysl  (sysl program, compiled to LLVM IR)
#
# Produces qemu/hello.elf — an ELF32 multiboot image QEMU can boot with -kernel.

set -e
cd "$(dirname "$0")"

CROSS=x86_64-elf-
SBT="sbt --client"

# 1. Compile sysl → LLVM IR via the sysl CLI.
#    The SBT daemon makes repeat invocations fast; fall back to plain sbt
#    if the client isn't available.
if ! command -v sbt >/dev/null; then
    echo "error: sbt not found in PATH" >&2
    exit 1
fi
(cd .. && sbt "syslCliJVM/run compile --emit llvm qemu/hello.sysl -o qemu/hello.ll")

# 2. Compile LLVM IR → x86_64 freestanding object file via clang.
clang \
    -target x86_64-unknown-none-elf \
    -ffreestanding \
    -nostdlib \
    -mcmodel=kernel \
    -mno-red-zone \
    -fno-pic \
    -fno-pie \
    -c -o hello.o hello.ll

# 3. Assemble startup.s (contains .code32 + .code64 sections).
${CROSS}as --64 -o startup.o startup.s

# 4. Compile the C shim.
${CROSS}gcc -m64 -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone -c -o main.o main.c

# 5. Link everything as a 64-bit ELF, then objcopy to 32-bit ELF for multiboot.
${CROSS}ld -T link.ld -o hello64.elf startup.o main.o hello.o
${CROSS}objcopy -O elf32-i386 hello64.elf hello.elf

echo "Built hello.elf (sysl → LLVM → x86_64 → multiboot)"
