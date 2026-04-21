#!/bin/bash
# Build a standalone C program for SLIX x86_64 as an ELF binary.
#
# Usage:
#   ./build_prog_c.sh test_c   # builds oskit/bin_c/test_c.c -> /tmp/slix-x86_64/bin/test_c
#
# Uses crt0.c (POSIX->Sysl crt0 bridge) + prog_stubs.c (sbrk, exit,
# putchar, etc.) + prog_start.s (arch _start). Output ELF has the same
# layout as the sysl-compiled programs and is loadable via the normal
# PM spawn path.

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-x86_64
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

clang -target x86_64-unknown-none-elf -ffreestanding -nostdlib \
    -mcmodel=kernel -mno-red-zone -fno-pic -fno-pie -w -Os \
    -c -o "$OUT/prog_${NAME}.o" "$SRC"
echo "  C compile ok"

x86_64-elf-as --64 -o "$OUT/prog_start.o" "$ARCH_DIR/prog_start.s"

x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
    -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
    -fno-pic -fno-pie -c -o "$OUT/crt0.o" "$ARCH_DIR/crt0.c"

x86_64-elf-ld -T "$ARCH_DIR/prog.ld" \
    -o "$BIN_OUT/${NAME}" \
    "$OUT/prog_start.o" "$OUT/crt0.o" "$OUT/prog_stubs.o" "$OUT/prog_${NAME}.o" 2>&1 \
    | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
echo "  Link ok ($(wc -c < "$BIN_OUT/${NAME}" | tr -d ' ') bytes)"
