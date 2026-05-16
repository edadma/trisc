#!/bin/bash
# Batch-build every musl test binary referenced by NshTests.
# Usage: build-all.sh --arch=x86_64    (or --arch=aarch64; default aarch64)
#
# /tmp is wiped on Mac reboot, so the produced binaries vanish.
# This script repopulates /tmp/slix-<arch>/bin/ in one shot.
# Each entry is "<src.c> <out_name>" — naming is inconsistent on
# purpose (some predate the m* convention), so we keep the table
# explicit rather than guessing.

set -e

TEST_DIR="$(cd "$(dirname "$0")" && pwd)"
ARCH_ARG="--arch=aarch64"
for arg in "$@"; do
    case "$arg" in
        --arch=*) ARCH_ARG="$arg" ;;
    esac
done

TESTS=(
    "hello.c mhello"
    "socket.c msocket"
    "file.c mfile"
    "stat.c mstat"
    "mpeek.c mpeek"
    "msndto.c msndto"
    "musrto.c musrto"
    "untar.c muntar"
    "fcreat.c mfcreat"
    "fsmod.c mfsmod"
    "epoll.c mepoll"
    "epoll2.c mepoll2"
    "nbacc.c mnbacc"
    "nbcon.c mnbcon"
    "pipe.c mpipe"
    "msghdr.c mmsg"
    "dns.c mdns"
    "lbacklog.c mlbacklog"
    "getaddr.c mgetaddr"
    "epoll_pipe.c epoll_pipe"
    "epoll_multi.c epoll_multi"
    "epoll_stdin.c estdin"
    "nbstdin.c nbstdin"
    "timerfd.c timerfd"
    "eventfd.c eventfd"
)

FAILED=()
for entry in "${TESTS[@]}"; do
    src="${entry% *}"
    out="${entry##* }"
    if bash "$TEST_DIR/build-c.sh" "$ARCH_ARG" "$src" "$out" >/dev/null 2>&1; then
        echo "  ok: $src -> $out"
    else
        echo "  FAILED: $src -> $out" >&2
        FAILED+=("$out")
    fi
done

# Dynamically-linked tests. Each one requires the slix-musl shared
# build (slix/build-musl[-x86]/lib/libc.so produced by the matching
# build-musl*.sh). They're handed off to build-c-dyn.sh which links
# against libc.so + sets PT_INTERP=/lib/ld-musl-<arch>.so.1. Skipped
# silently if libc.so isn't present so static-only setups keep working.
DYN_TESTS=(
    "dhello.c dhello"
    "phello.c phello"
    "sigact.c sigact"
)

case "$ARCH_ARG" in
    --arch=x86_64) MUSL_LIB_DIR="$TEST_DIR/../build-musl-x86/lib" ;;
    *)             MUSL_LIB_DIR="$TEST_DIR/../build-musl/lib" ;;
esac

if [ -f "$MUSL_LIB_DIR/libc.so" ]; then
    for entry in "${DYN_TESTS[@]}"; do
        src="${entry% *}"
        out="${entry##* }"
        if bash "$TEST_DIR/build-c-dyn.sh" "$ARCH_ARG" "$src" "$out" >/dev/null 2>&1; then
            echo "  ok (dyn): $src -> $out"
        else
            echo "  FAILED (dyn): $src -> $out" >&2
            FAILED+=("$out")
        fi
    done
else
    echo "  skipping dynamic tests — $MUSL_LIB_DIR/libc.so not built"
fi

if [ ${#FAILED[@]} -gt 0 ]; then
    echo ""
    echo "Failed: ${FAILED[*]}" >&2
    exit 1
fi
