#!/bin/bash
set -e
cd "$(dirname "$0")"

CROSS=x86_64-elf-

# Assemble startup as 64-bit ELF (contains .code32 trampoline + .code64 entry)
${CROSS}as --64 -o startup.o startup.s

# Compile main as 64-bit
${CROSS}gcc -m64 -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone -c -o main.o main.c

# Link as 64-bit ELF
${CROSS}ld -T link.ld -o hello64.elf startup.o main.o

# Convert to 32-bit ELF for QEMU multiboot (-kernel requires ELF32)
${CROSS}objcopy -O elf32-i386 hello64.elf hello.elf

echo "Built hello.elf (multiboot → long mode, 64-bit kernel)"
