#!/bin/bash
# Build a standalone x86_64 SLIX program as a TRB binary.
#
# Usage:
#   ./build_prog.sh echo        # builds oskit/bin/echo.lsysl → /tmp/slix-x86_64/bin/echo.trb
#   ./build_prog.sh hello       # builds oskit/bin/hello.lsysl → /tmp/slix-x86_64/bin/hello.trb
#   ./build_prog.sh all         # builds all programs in oskit/bin/
#
# Output: /tmp/slix-x86_64/bin/<name>.trb

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-x86_64
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

    # Sysl source files for standalone programs
    local SYSL_FILES=(
        "$SRC"
        oskit/ulib/ulib.lsysl
        std/alloc/alloc.lsysl
    )

    # Compile Sysl → LLVM IR
    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm ${SYSL_FILES[*]} -o $OUT/prog_${NAME}.ll" > /tmp/sbt-prog-${NAME}.txt 2>&1
    if [ $? -ne 0 ]; then
        echo "  Sysl compile failed:" >&2
        tail -5 /tmp/sbt-prog-${NAME}.txt >&2
        return 1
    fi
    echo "  Sysl → LLVM IR ok"

    # LLVM IR → object
    clang -target x86_64-unknown-none-elf -ffreestanding -nostdlib \
        -mcmodel=kernel -mno-red-zone -fno-pic -fno-pie -w \
        -c -o "$OUT/prog_${NAME}.o" "$OUT/prog_${NAME}.ll"
    echo "  LLVM IR → object ok"

    # Assemble startup
    x86_64-elf-as --64 -o "$OUT/prog_start.o" "$ARCH_DIR/prog_start.s"

    # Compile C stubs
    x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
        -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

    # Link
    x86_64-elf-ld -T "$ARCH_DIR/prog.ld" \
        -o "$OUT/prog_${NAME}.elf" \
        "$OUT/prog_start.o" "$OUT/prog_stubs.o" "$OUT/prog_${NAME}.o" 2>&1 \
        | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
    echo "  Link ok"

    # ELF → flat binary
    x86_64-elf-objcopy -O binary "$OUT/prog_${NAME}.elf" "$OUT/prog_${NAME}.bin"

    # Flat binary → TRB
    python3 "$ARCH_DIR/mktrb.py" "$OUT/prog_${NAME}.bin" 0xD0000 "$BIN_OUT/${NAME}.trb"
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
