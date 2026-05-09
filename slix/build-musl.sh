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
mkdir -p "$BUILD_DIR/compat"

# Cross-compile our compiler-rt stubs object that musl will link into
# libc.so. Homebrew clang on darwin doesn't ship a libclang_rt.builtins
# for aarch64-linux-musl, so libc.so otherwise has unresolved
# references for ~30 long-double / 128-bit float / complex-multiply
# helpers. Hello-world dynamic doesn't call any of them; the stub
# object satisfies the load-time relocations and aborts (brk #0) if
# any actually fire. See slix/musl-compat/compiler_rt_stubs.c.
STUBS_C="$REPO_ROOT/slix/musl-compat/compiler_rt_stubs.c"
STUBS_O="$BUILD_DIR/compat/stubs.o"
if [ ! -f "$STUBS_O" ] || [ "$STUBS_C" -nt "$STUBS_O" ]; then
    echo "=== Cross-compiling compiler-rt stubs (aarch64) ==="
    "$CLANG" --target=aarch64-slix-linux-musl -ffreestanding \
        -O2 -fPIC -fvisibility=default -nostdinc \
        -c -o "$STUBS_O" "$STUBS_C"
fi

cd "$BUILD_DIR"

if [ ! -f config.mak ]; then
    echo "=== Configuring musl for aarch64-slix ==="
    export CC="$CLANG --target=aarch64-slix-linux-musl -ffreestanding"
    export AR
    export RANLIB
    # --dynamic-list keeps internal-by-default visibility for non-listed
    # symbols, so libc.so's intra-library calls bypass the PLT. Without
    # this, musl's `_dlstart_c` makes PLT-routed calls before its own
    # JUMP_SLOT relocations are applied — the slots still point at
    # PLT0 link-time addresses, the call faults at the link-time PLT
    # base, and ld-musl can't bootstrap. musl's configure normally
    # auto-detects this via tryldflag, but the cross-target setup
    # disables auto-detection, so we force it on the LDFLAGS path.
    export LDFLAGS="-fuse-ld=$LD -Wl,--dynamic-list=$REPO_ROOT/slix/musl/dynamic.list -Wl,--gc-sections -Wl,-soname,libc.so"
    "$REPO_ROOT/slix/musl/configure" \
        --target=aarch64-slix-linux-musl \
        --prefix="$REPO_ROOT/slix/sysroot" \
        --enable-shared \
        --enable-static \
        --with-malloc=oldmalloc \
        LIBCC="$STUBS_O"
fi

echo "=== Building musl libc.a ==="
make -j"$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

echo "=== Built: $BUILD_DIR/lib/libc.a ($(wc -c < "$BUILD_DIR/lib/libc.a" | tr -d ' ') bytes) ==="
