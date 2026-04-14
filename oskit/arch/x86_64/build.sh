#!/bin/bash
# Build SLIX x86_64 kernel — multiboot ELF for QEMU
#
# Usage: ./build.sh [test_kernel.sysl]
#   Default builds with test_kernel.sysl (minimal boot test).
#   Pass a different .sysl to build the real kernel.
#
# Produces: kernel.elf (ELF32 multiboot image)
# Run:      qemu-system-x86_64 -kernel kernel.elf -serial stdio -no-reboot -display none

set -e
cd "$(dirname "$0")"

CROSS=x86_64-elf-
SYSL_SRC="${1:-test_kernel.sysl}"
REPO_ROOT="../../../"

echo "=== SLIX x86_64 build ==="
echo "  Sysl source: $SYSL_SRC"

# 1. Compile Sysl -> LLVM IR
echo "[1/5] sysl -> LLVM IR"
(cd "$REPO_ROOT" && sbt --client "syslCliJVM/run compile --emit llvm oskit/arch/x86_64/$SYSL_SRC -o oskit/arch/x86_64/kernel.ll")

# 2. Compile LLVM IR -> x86_64 object
echo "[2/5] LLVM IR -> x86_64 object"
clang \
    -target x86_64-unknown-none-elf \
    -ffreestanding \
    -nostdlib \
    -mcmodel=kernel \
    -mno-red-zone \
    -fno-pic \
    -fno-pie \
    -c -o kernel.o kernel.ll

# 3. Assemble boot.s
echo "[3/5] boot.s -> object"
${CROSS}as --64 -o boot.o boot.s

# 4. Compile runtime.c
echo "[4/5] runtime.c -> object"
${CROSS}gcc -m64 -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone -fno-pic -c -o runtime.o runtime.c

# 5. Link and produce multiboot ELF
echo "[5/5] link -> kernel.elf"
${CROSS}ld -T link.ld -o kernel64.elf boot.o runtime.o kernel.o
${CROSS}objcopy -O elf32-i386 kernel64.elf kernel.elf

echo ""
echo "Built kernel.elf"
echo "Run: qemu-system-x86_64 -kernel kernel.elf -serial stdio -no-reboot -display none"
