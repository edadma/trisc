#!/bin/bash
# Cross-compile a *dynamically* musl-linked C program for SLIX.
# Mirror of build-c.sh but drops -static and links against libc.so
# with -dynamic-linker /lib/ld-musl-<arch>.so.1.
#
# Usage: build-c-dyn.sh [--arch=aarch64|x86_64] <source.c> <output-binary-name>
#                                       (default: aarch64)
# Output: /tmp/slix-<arch>/bin/<output-binary-name>
#
# The resulting ELF has a PT_INTERP segment pointing at
# /lib/ld-musl-<arch>.so.1 — at exec() time PM (chunk 9) loads
# that interpreter at INTERP_BASE and jumps to its entry point;
# the interp finishes its bootstrap and calls _start in the main exe.
#
# Prereqs: slix/build-musl[-x86]/{libc.so,Scrt1.o,crti.o,crtn.o}
# produced by slix/build-musl[-x86].sh with --enable-shared,
# Homebrew clang + lld.
set -e

ARCH=aarch64
ARGS=()
for arg in "$@"; do
    case "$arg" in
        --arch=aarch64) ARCH=aarch64 ;;
        --arch=x86_64)  ARCH=x86_64 ;;
        --arch=*) echo "Unknown --arch: $arg" >&2; exit 2 ;;
        *) ARGS+=("$arg") ;;
    esac
done

if [ ${#ARGS[@]} -lt 2 ]; then
    echo "Usage: $0 [--arch=aarch64|x86_64] <source.c> <output-name>" >&2
    exit 2
fi

SRC="${ARGS[0]}"
OUT_NAME="${ARGS[1]}"

TEST_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$TEST_DIR/../.." && pwd)"

if [ ! -f "$SRC" ]; then
    if [ -f "$TEST_DIR/$SRC" ]; then
        SRC="$TEST_DIR/$SRC"
    else
        echo "Source not found: $SRC" >&2
        exit 3
    fi
fi

case "$ARCH" in
    aarch64)
        OUT=/tmp/slix-aarch64
        MUSL_BUILD="$REPO_ROOT/slix/build-musl"
        MUSL_ARCH_INC="$REPO_ROOT/slix/musl/arch/aarch64-slix"
        CLANG_TARGET=aarch64-slix-linux-musl
        DYNAMIC_LINKER=/lib/ld-musl-aarch64.so.1
        ;;
    x86_64)
        OUT=/tmp/slix-x86_64
        MUSL_BUILD="$REPO_ROOT/slix/build-musl-x86"
        MUSL_ARCH_INC="$REPO_ROOT/slix/musl/arch/x86_64-slix"
        CLANG_TARGET=x86_64-slix-linux-musl
        DYNAMIC_LINKER=/lib/ld-musl-x86_64.so.1
        ;;
esac

BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

MUSL_LIB="$MUSL_BUILD/lib"
MUSL_INC="$REPO_ROOT/slix/musl/include"
MUSL_GEN_INC="$MUSL_BUILD/obj/include"
MUSL_GENERIC_INC="$REPO_ROOT/slix/musl/arch/generic"

if [ ! -f "$MUSL_LIB/libc.so" ]; then
    echo "Missing $MUSL_LIB/libc.so — run slix/build-musl[-x86].sh first" >&2
    echo "and ensure --enable-shared is in the configure args." >&2
    exit 4
fi

CLANG=/opt/homebrew/opt/llvm/bin/clang
LD=/opt/homebrew/opt/lld/bin/ld.lld
OBJ_BASE="$(basename "${SRC%.c}")"

echo "=== Compile $SRC ($ARCH, dynamic) ==="
"$CLANG" \
    --target=$CLANG_TARGET \
    -ffreestanding \
    -fPIE \
    -nostdinc \
    -isystem "$MUSL_ARCH_INC" \
    -isystem "$MUSL_GEN_INC" \
    -isystem "$MUSL_GENERIC_INC" \
    -isystem "$MUSL_INC" \
    -c -o "$OUT/$OBJ_BASE.dyn.o" \
    "$SRC"

echo "=== Link (dynamic, PIE) ==="
# -pie + Scrt1.o for position-independent executable.
# -dynamic-linker sets the PT_INTERP segment so the kernel/PM
# (chunk 9) loads ld-musl into the process before transferring
# control to it; the linker then bootstraps libc and jumps to _start.
# Note: the slix-prog.ld linker script is bypassed for dynamic
# binaries — lld picks default segment layout for PIE which puts
# .text at a low VA the loader rebases via load_offset=0 (the main
# exe's PT_LOADs are honored at link-time vaddrs since it's not the
# interp). We rely on default page-size 0x1000 to match SLIX.
"$LD" \
    -pie \
    -dynamic-linker "$DYNAMIC_LINKER" \
    -z max-page-size=0x1000 \
    --allow-shlib-undefined \
    -o "$BIN_OUT/$OUT_NAME" \
    "$MUSL_LIB/Scrt1.o" \
    "$MUSL_LIB/crti.o" \
    "$OUT/$OBJ_BASE.dyn.o" \
    "$MUSL_LIB/libc.so" \
    "$MUSL_LIB/crtn.o"

echo "=== Built: $BIN_OUT/$OUT_NAME ($(wc -c < "$BIN_OUT/$OUT_NAME" | tr -d ' ') bytes) ==="
