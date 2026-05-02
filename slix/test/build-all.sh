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

if [ ${#FAILED[@]} -gt 0 ]; then
    echo ""
    echo "Failed: ${FAILED[*]}" >&2
    exit 1
fi
