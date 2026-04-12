#!/bin/bash
# Build the SHA-256 benchmark for x86_64 bare-metal QEMU.
# Produces qemu/bench.elf — run with: time bash qemu/run-bench.sh

set -e
cd "$(dirname "$0")"

CROSS=x86_64-elf-

# 1. Compile sysl modules → LLVM IR
#    Need: bench.sysl + sha256 + binary + debug (assert)
(cd .. && sbt "syslCliJVM/run compile --emit llvm \
    qemu/bench.sysl \
    std/crypto/sha256/sha256.lsysl \
    std/encoding/binary/binary.lsysl \
    std/debug/debug.lsysl \
    -o qemu/bench.ll")

# 2. Compile LLVM IR → x86_64 object
clang \
    -target x86_64-unknown-none-elf \
    -ffreestanding \
    -nostdlib \
    -mcmodel=kernel \
    -mno-red-zone \
    -fno-pic \
    -fno-pie \
    -w \
    -c -o bench.o bench.ll

# 3. Assemble startup.s
${CROSS}as --64 -o startup.o startup.s

# 4. Compile C shim
${CROSS}gcc -m64 -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone -c -o main.o main.c

# 5. Link and convert
${CROSS}ld -T link.ld -o bench64.elf startup.o main.o bench.o
${CROSS}objcopy -O elf32-i386 bench64.elf bench.elf

echo "Built bench.elf (SHA-256 benchmark)"
