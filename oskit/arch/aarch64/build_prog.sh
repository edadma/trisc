#!/bin/bash
# Build a standalone aarch64 SLIX program as an ELF binary.
#
# Usage:
#   ./build_prog.sh test_putc   # builds oskit/bin/test_putc.lsysl
#   ./build_prog.sh all         # builds all programs in oskit/bin/
#
# Output: /tmp/slix-aarch64/bin/<name>       (ELF64)
#         /tmp/slix-aarch64/bin/<name>.bin   (flat raw binary, .text+.rodata+.data)

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-aarch64
BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

build_one() {
    local NAME="$1"
    local SRC="oskit/bin/${NAME}.lsysl"

    if [ ! -f "$REPO_ROOT/$SRC" ]; then
        echo "ERROR: $SRC not found" >&2
        return 1
    fi

    echo "=== Building /bin/$NAME ==="

    local SYSL_FILES=(
        "$SRC"
        oskit/ulib/ulib.lsysl
        oskit/ulib/srt0.lsysl
        oskit/ds/client.lsysl
        std/alloc/alloc.lsysl
    )

    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm --target=aarch64-elf ${SYSL_FILES[*]} -o $OUT/prog_${NAME}.ll" > /tmp/sbt-aa-prog-${NAME}.log 2>&1
    if ! grep -q "success" "/tmp/sbt-aa-prog-${NAME}.log"; then
        echo "  Sysl compile failed:" >&2
        tail -10 "/tmp/sbt-aa-prog-${NAME}.log" >&2
        return 1
    fi
    echo "  Sysl -> LLVM IR ok"

    clang -target aarch64-unknown-none-elf -ffreestanding -nostdlib \
        -fno-pic -fno-pie -w \
        -c -o "$OUT/prog_${NAME}.o" "$OUT/prog_${NAME}.ll"
    echo "  LLVM IR -> object ok"

    aarch64-elf-as -o "$OUT/prog_start.o" "$ARCH_DIR/prog_start.s"

    aarch64-elf-gcc -ffreestanding -nostdlib \
        -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

    aarch64-elf-ld -T "$ARCH_DIR/prog.ld" \
        -o "$BIN_OUT/${NAME}" \
        "$OUT/prog_start.o" "$OUT/prog_stubs.o" "$OUT/prog_${NAME}.o" 2>&1 \
        | grep -v "missing .note.GNU-stack" \
        | grep -v "deprecated" \
        | grep -v "RWX permissions" || true
    echo "  Link -> ELF ok ($(wc -c < "$BIN_OUT/${NAME}" | tr -d ' ') bytes)"

    aarch64-elf-objcopy -O binary \
        -j .text -j .rodata -j .data -j .got -j .got.plt \
        "$BIN_OUT/${NAME}" "$BIN_OUT/${NAME}.bin"
    echo "  objcopy -> raw ok ($(wc -c < "$BIN_OUT/${NAME}.bin" | tr -d ' ') bytes)"
}

if [ "$1" = "all" ]; then
    for src in "$REPO_ROOT"/oskit/bin/*.lsysl; do
        name=$(basename "$src" .lsysl)
        build_one "$name" || echo "  FAILED: $name" >&2
    done
elif [ -n "$1" ]; then
    build_one "$1"
else
    echo "Usage: $0 <program|all>" >&2
    exit 1
fi
