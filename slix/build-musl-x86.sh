#!/bin/bash
# Build the SLIX musl fork for x86_64. Produces:
#   slix/build-musl-x86/lib/{crt1.o,crti.o,crtn.o,libc.a,...}
#   slix/build-musl-x86/obj/include/bits/alltypes.h   (generated)
# These are consumed by slix/test/build-c.sh --arch=x86_64.
#
# Mirrors slix/build-musl.sh which targets aarch64. Both arches use
# the same SLIX-local syscall numbering (see oskit/posix/shim.lsysl)
# but differ in entry instruction (svc 0 vs int $0x80) and ABI.
#
# Prereqs (Homebrew): clang, lld, llvm (for llvm-ar / llvm-ranlib).
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$REPO_ROOT/slix/build-musl-x86"

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
    echo "=== Configuring musl for x86_64-slix ==="
    export CC="$CLANG --target=x86_64-slix-linux-musl -ffreestanding"
    export AR
    export RANLIB
    export LDFLAGS="-fuse-ld=$LD"
    "$REPO_ROOT/slix/musl/configure" \
        --target=x86_64-slix-linux-musl \
        --prefix="$REPO_ROOT/slix/sysroot-x86" \
        --disable-shared \
        --enable-static
fi

echo "=== Building musl libc.a ==="
make -j"$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "=== Built: $BUILD_DIR/lib/libc.a ($(wc -c < "$BUILD_DIR/lib/libc.a" | tr -d ' ') bytes) ==="
