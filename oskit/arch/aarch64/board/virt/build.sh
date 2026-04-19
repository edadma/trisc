#!/bin/bash
# Build SLIX aarch64 kernel for QEMU virt machine.
# Milestone 1: boot + UART "Hello, aarch64!" and halt.
#
# Usage:
#   ./build.sh           # build
#   ./build.sh run       # build and run under QEMU

set -e

BOARD_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$BOARD_DIR/../../../../.." && pwd)"
OUT=/tmp/slix-aarch64
mkdir -p "$OUT"

RUN=""
for arg in "$@"; do
    case "$arg" in
        run) RUN=run ;;
    esac
done

echo "=== Assemble boot.s ==="
aarch64-elf-as -o "$OUT/boot.o" "$BOARD_DIR/boot.s"

echo "=== Compile hello.c ==="
aarch64-elf-gcc -ffreestanding -nostdlib -mcmodel=large \
    -fno-pic -fno-pie -Wall -Wextra -Werror -O1 \
    -c -o "$OUT/hello.o" "$BOARD_DIR/hello.c"

echo "=== Link ==="
aarch64-elf-ld -T "$BOARD_DIR/link.ld" \
    -o "$OUT/kernel.elf" \
    "$OUT/boot.o" "$OUT/hello.o"

echo "=== Built: $OUT/kernel.elf ==="

if [ "$RUN" = "run" ]; then
    echo "=== QEMU (Ctrl-A X to quit) ==="
    exec qemu-system-aarch64 \
        -machine virt \
        -cpu cortex-a72 \
        -nographic \
        -no-reboot \
        -kernel "$OUT/kernel.elf"
fi
