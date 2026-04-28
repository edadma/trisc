#!/bin/bash
# Build a standalone C program for SLIX aarch64 as an ELF binary.
#
# Usage:
#   ./build_prog_c.sh test_c   # builds oskit/bin_c/test_c.c -> /tmp/slix-aarch64/bin/test_c
#
# Uses crt0.c (POSIX->Sysl crt0 bridge) + prog_stubs.c (sbrk, exit,
# putchar, etc.) + prog_start.s (arch _start). Output ELF has the same
# layout as the sysl-compiled programs and is loadable via the normal
# PM spawn path. Mirror of oskit/arch/x86_64/build_prog_c.sh.

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-aarch64
BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

if [ -z "$1" ]; then
    echo "Usage: $0 <name>" >&2
    exit 1
fi

NAME="$1"
SRC="$REPO_ROOT/oskit/bin_c/${NAME}.c"

if [ ! -f "$SRC" ]; then
    echo "ERROR: $SRC not found" >&2
    exit 1
fi

echo "=== Building C /bin/$NAME ==="

cd "$REPO_ROOT"

clang -target aarch64-unknown-none-elf -ffreestanding -nostdlib \
    -fno-pic -fno-pie -w -Os \
    -c -o "$OUT/prog_${NAME}.o" "$SRC"
echo "  C compile ok"

aarch64-elf-as -o "$OUT/prog_start.o" "$ARCH_DIR/prog_start.s"

aarch64-elf-gcc -ffreestanding -nostdlib -fno-pic -fno-pie \
    -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

aarch64-elf-gcc -ffreestanding -nostdlib -fno-pic -fno-pie \
    -c -o "$OUT/crt0.o" "$ARCH_DIR/crt0.c"

# -z max-page-size=0x1000 mirrors build_prog.sh — without it
# aarch64-elf-ld pads to 64 KB pages and the ELF blows past
# LOAD_CROSS_BUF_SIZE.
aarch64-elf-ld -T "$ARCH_DIR/prog.ld" -z max-page-size=0x1000 \
    -o "$BIN_OUT/${NAME}" \
    "$OUT/prog_start.o" "$OUT/crt0.o" "$OUT/prog_stubs.o" "$OUT/prog_${NAME}.o" 2>&1 \
    | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
echo "  Link ok ($(wc -c < "$BIN_OUT/${NAME}" | tr -d ' ') bytes)"

# aarch64 ramdisk packer also wants a flat .bin per binary.
aarch64-elf-objcopy -O binary \
    -j .text -j .rodata -j .data -j .got -j .got.plt \
    "$BIN_OUT/${NAME}" "$BIN_OUT/${NAME}.bin"
echo "  objcopy -> raw ok ($(wc -c < "$BIN_OUT/${NAME}.bin" | tr -d ' ') bytes)"
