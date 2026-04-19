#!/bin/bash
# Build SLIX aarch64 kernel for QEMU virt machine.
# Pipeline: sysl -> LLVM IR -> aarch64 object -> link with boot.s + vectors.s.
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

SYSL_FILES=(
    oskit/arch/aarch64/board/virt/hello.lsysl
)

echo "=== Sysl -> LLVM IR ==="
cd "$REPO_ROOT"
sbt "syslCliJVM/run compile --emit llvm --target=aarch64-elf ${SYSL_FILES[*]} -o $OUT/kernel.ll" > "$OUT/sbt-kernel.log" 2>&1
if ! grep -q "success" "$OUT/sbt-kernel.log"; then
    echo "  Sysl compile failed:" >&2
    tail -10 "$OUT/sbt-kernel.log" >&2
    exit 1
fi

echo "=== LLVM IR -> object ==="
clang -target aarch64-unknown-none-elf -ffreestanding -nostdlib \
    -mcmodel=large -fno-pic -fno-pie -w \
    -c -o "$OUT/kernel.o" "$OUT/kernel.ll"

echo "=== Assemble boot.s ==="
aarch64-elf-as -o "$OUT/boot.o" "$BOARD_DIR/boot.s"

echo "=== Assemble vectors.s ==="
aarch64-elf-as -o "$OUT/vectors.o" "$BOARD_DIR/vectors.s"

echo "=== Compile stubs.c ==="
aarch64-elf-gcc -ffreestanding -nostdlib -mcmodel=large \
    -fno-pic -fno-pie -c -o "$OUT/stubs.o" "$BOARD_DIR/stubs.c"

echo "=== Link ==="
aarch64-elf-ld -T "$BOARD_DIR/link.ld" \
    -o "$OUT/kernel.elf" \
    "$OUT/boot.o" "$OUT/vectors.o" "$OUT/stubs.o" "$OUT/kernel.o" 2>&1 \
    | grep -v "has a LOAD segment with RWX permissions" || true

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
