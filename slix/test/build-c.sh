#!/bin/bash
# Cross-compile a musl-linked C program for SLIX (aarch64 or x86_64).
# Usage: build-c.sh [--arch=aarch64|x86_64] <source.c> <output-binary-name>
#                                 (default: aarch64)
# Output: /tmp/slix-<arch>/bin/<output-binary-name>
#
# Aarch64 binaries land where MakeAarch64RamdiskMain picks up ELF
# files from, so the next ramdisk repack (oskit/arch/aarch64/board/
# virt/build.sh) will include them automatically. x86_64 binaries
# land in /tmp/slix-x86_64/bin/ for symmetry; ramdisk plumbing for
# x86 is still pre-existing-broken — see project_slix_x86_sidelined.
#
# Prereqs: slix/build-musl[-x86]/ produced by slix/build-musl[-x86].sh,
#          Homebrew clang + lld + llvm.
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
        ;;
    x86_64)
        OUT=/tmp/slix-x86_64
        MUSL_BUILD="$REPO_ROOT/slix/build-musl-x86"
        MUSL_ARCH_INC="$REPO_ROOT/slix/musl/arch/x86_64-slix"
        CLANG_TARGET=x86_64-slix-linux-musl
        ;;
esac

BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

MUSL_LIB="$MUSL_BUILD/lib"
MUSL_INC="$REPO_ROOT/slix/musl/include"
MUSL_GEN_INC="$MUSL_BUILD/obj/include"
MUSL_GENERIC_INC="$REPO_ROOT/slix/musl/arch/generic"

if [ ! -f "$MUSL_LIB/libc.a" ]; then
    if [ "$ARCH" = "aarch64" ]; then
        echo "Missing $MUSL_LIB/libc.a — run slix/build-musl.sh first" >&2
    else
        echo "Missing $MUSL_LIB/libc.a — run slix/build-musl-x86.sh first" >&2
    fi
    exit 4
fi

CLANG=/opt/homebrew/opt/llvm/bin/clang
LD=/opt/homebrew/opt/lld/bin/ld.lld
OBJ_BASE="$(basename "${SRC%.c}")"

echo "=== Compile $SRC ($ARCH) ==="
"$CLANG" \
    --target=$CLANG_TARGET \
    -ffreestanding \
    -nostdinc \
    -isystem "$MUSL_ARCH_INC" \
    -isystem "$MUSL_GEN_INC" \
    -isystem "$MUSL_GENERIC_INC" \
    -isystem "$MUSL_INC" \
    -c -o "$OUT/$OBJ_BASE.o" \
    "$SRC"

echo "=== Link ==="
# -u __bin_chunk forces lld to pull in oldmalloc/malloc.lo so its
# strong __libc_malloc_impl wins over lite_malloc's weak alias.
# Without this, archive-order resolution sticks with lite_malloc and
# any malloc that needs to grow falls through to mmap (unimplemented).
"$LD" \
    -T "$TEST_DIR/slix-prog.ld" \
    -z max-page-size=0x1000 \
    -static \
    -u __bin_chunk \
    -o "$BIN_OUT/$OUT_NAME" \
    "$MUSL_LIB/crt1.o" \
    "$MUSL_LIB/crti.o" \
    "$OUT/$OBJ_BASE.o" \
    "$MUSL_LIB/libc.a" \
    "$MUSL_LIB/crtn.o"

echo "=== Built: $BIN_OUT/$OUT_NAME ($(wc -c < "$BIN_OUT/$OUT_NAME" | tr -d ' ') bytes) ==="
