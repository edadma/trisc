#!/bin/bash
# Build standalone aarch64 SLIX server boot modules.
#
# Usage:
#   ./build_servers.sh          # builds all servers
#   ./build_servers.sh rs       # builds just RS

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"

ARCH_NAME=aarch64
OUT=/tmp/slix-${ARCH_NAME}
SRV_OUT="$OUT/servers"
mkdir -p "$SRV_OUT"

SYSL_TARGET=aarch64-elf
TOOLCHAIN_PREFIX=aarch64-elf-
CLANG_TARGET=aarch64-unknown-none-elf
CLANG_EXTRA_FLAGS=""
GCC_EXTRA_FLAGS=""
AS_EXTRA_FLAGS=""
LD_EXTRA_FLAGS="-z max-page-size=0x1000"
NEEDS_FLAT_BIN=1

SERVER_COMMON_SRCS=(
    oskit/ipc/ipc_client.sysl
    oskit/services/services.lsysl
    oskit/arch/aarch64/prog_config.sysl
    std/alloc/alloc.lsysl
)
SERVER_LINKER_SCRIPT=prog.ld

source "$REPO_ROOT/oskit/arch/common.sh"

dispatch_servers_arg "$1"
