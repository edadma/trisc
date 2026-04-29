#!/bin/bash
# Build the SLIX musl fork for aarch64. Produces:
#   slix/build-musl/lib/{crt1.o,crti.o,crtn.o,libc.a,...}
#   slix/build-musl/obj/include/bits/alltypes.h   (generated)
# These are consumed by slix/test/build-hello.sh to produce mhello.
#
# Run from the repo root (or anywhere — the script resolves its own dir).
# Idempotent: re-runs are cheap once the initial build is done. Full
# wipe + rebuild: `rm -rf slix/build-musl slix/sysroot && slix/build-musl.sh`.
#
# Prereqs (Homebrew): clang, lld, llvm (for llvm-ar / llvm-ranlib).
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$REPO_ROOT/slix/build-musl"

CLANG=/opt/homebrew/opt/llvm/bin/clang
AR=/opt/homebrew/opt/llvm/bin/llvm-ar
RANLIB=/opt/homebrew/opt/llvm/bin/llvm-ranlib
LD=/opt/homebrew/opt/lld/bin/ld.lld

for tool in "$CLANG" "$AR" "$RANLIB" "$LD"; do
    if [ ! -x "$tool" ]; then
        echo "Missing tool: $tool" >&2
        echo "Install with: brew install llvm lld" >&2
        exit 1
    fi
done

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

if [ ! -f config.mak ]; then
    echo "=== Configuring musl for aarch64-slix ==="
    export CC="$CLANG --target=aarch64-slix-linux-musl -ffreestanding"
    export AR
    export RANLIB
    export LDFLAGS="-fuse-ld=$LD"
    "$REPO_ROOT/slix/musl/configure" \
        --target=aarch64-slix-linux-musl \
        --prefix="$REPO_ROOT/slix/sysroot" \
        --disable-shared \
        --enable-static \
        --with-malloc=oldmalloc
fi

echo "=== Building musl libc.a ==="
make -j"$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "=== Built: $BUILD_DIR/lib/libc.a ($(wc -c < "$BUILD_DIR/lib/libc.a" | tr -d ' ') bytes) ==="
