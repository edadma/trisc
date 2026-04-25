#!/bin/bash
# Cross-compile a musl-linked C program for SLIX aarch64.
# Usage: build-c.sh <source.c> <output-binary-name>
# Output: /tmp/slix-aarch64/bin/<output-binary-name>
#
# The binary lands where MakeAarch64RamdiskMain picks up ELF files
# from, so the next ramdisk repack (oskit/arch/aarch64/board/virt/
# build.sh) will include it automatically.
#
# Prereqs: slix/build-musl/ (run slix/build-musl.sh first),
#          Homebrew clang + lld + llvm.
set -e

if [ $# -lt 2 ]; then
    echo "Usage: $0 <source.c> <output-name>" >&2
    exit 2
fi

SRC="$1"
OUT_NAME="$2"

TEST_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$TEST_DIR/../.." && pwd)"

if [ ! -f "$SRC" ]; then
    # Allow a bare basename resolved relative to this script.
    if [ -f "$TEST_DIR/$SRC" ]; then
        SRC="$TEST_DIR/$SRC"
    else
        echo "Source not found: $SRC" >&2
        exit 3
    fi
fi

OUT=/tmp/slix-aarch64
BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

MUSL_LIB="$REPO_ROOT/slix/build-musl/lib"
MUSL_INC="$REPO_ROOT/slix/musl/include"
MUSL_GEN_INC="$REPO_ROOT/slix/build-musl/obj/include"
MUSL_ARCH_INC="$REPO_ROOT/slix/musl/arch/aarch64-slix"
MUSL_GENERIC_INC="$REPO_ROOT/slix/musl/arch/generic"

if [ ! -f "$MUSL_LIB/libc.a" ]; then
    echo "Missing $MUSL_LIB/libc.a — run slix/build-musl.sh first" >&2
    exit 4
fi

CLANG=/opt/homebrew/opt/llvm/bin/clang
LD=/opt/homebrew/opt/lld/bin/ld.lld
OBJ_BASE="$(basename "${SRC%.c}")"

echo "=== Compile $SRC ==="
"$CLANG" \
    --target=aarch64-slix-linux-musl \
    -ffreestanding \
    -nostdinc \
    -isystem "$MUSL_ARCH_INC" \
    -isystem "$MUSL_GEN_INC" \
    -isystem "$MUSL_GENERIC_INC" \
    -isystem "$MUSL_INC" \
    -c -o "$OUT/$OBJ_BASE.o" \
    "$SRC"

echo "=== Link ==="
"$LD" \
    -T "$TEST_DIR/slix-prog.ld" \
    -z max-page-size=0x1000 \
    -static \
    -o "$BIN_OUT/$OUT_NAME" \
    "$MUSL_LIB/crt1.o" \
    "$MUSL_LIB/crti.o" \
    "$OUT/$OBJ_BASE.o" \
    "$MUSL_LIB/libc.a" \
    "$MUSL_LIB/crtn.o"

echo "=== Built: $BIN_OUT/$OUT_NAME ($(wc -c < "$BIN_OUT/$OUT_NAME" | tr -d ' ') bytes) ==="
