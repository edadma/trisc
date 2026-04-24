#!/bin/bash
# Cross-compile a musl-linked "hello from musl" ELF for SLIX aarch64.
# Output: /tmp/slix-aarch64/bin/mhello (drop-in for the ramdisk packer).

set -e

TEST_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$TEST_DIR/../.." && pwd)"

OUT=/tmp/slix-aarch64
BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

MUSL_LIB="$REPO_ROOT/slix/build-musl/lib"
MUSL_INC="$REPO_ROOT/slix/musl/include"
MUSL_GEN_INC="$REPO_ROOT/slix/build-musl/obj/include"
MUSL_ARCH_INC="$REPO_ROOT/slix/musl/arch/aarch64-slix"
MUSL_GENERIC_INC="$REPO_ROOT/slix/musl/arch/generic"

CLANG=/opt/homebrew/opt/llvm/bin/clang
LD=/opt/homebrew/opt/lld/bin/ld.lld

echo "=== Compile hello.c ==="
"$CLANG" \
    --target=aarch64-slix-linux-musl \
    -ffreestanding \
    -nostdinc \
    -isystem "$MUSL_ARCH_INC" \
    -isystem "$MUSL_GEN_INC" \
    -isystem "$MUSL_GENERIC_INC" \
    -isystem "$MUSL_INC" \
    -c -o "$OUT/hello.o" \
    "$TEST_DIR/hello.c"

echo "=== Link ==="
"$LD" \
    -T "$TEST_DIR/slix-prog.ld" \
    -z max-page-size=0x1000 \
    -static \
    -o "$BIN_OUT/mhello" \
    "$MUSL_LIB/crt1.o" \
    "$MUSL_LIB/crti.o" \
    "$OUT/hello.o" \
    "$MUSL_LIB/libc.a" \
    "$MUSL_LIB/crtn.o"

echo "=== Built: $BIN_OUT/mhello ($(wc -c < "$BIN_OUT/mhello" | tr -d ' ') bytes) ==="
