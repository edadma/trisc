#!/bin/bash
# Build standalone x86_64 SLIX server ELFs for boot module isolation.
#
# Usage:
#   ./build_servers.sh          # builds all servers
#   ./build_servers.sh disk     # builds just the disk server

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"

ARCH_NAME=x86_64
OUT=/tmp/slix-${ARCH_NAME}
SRV_OUT="$OUT/servers"
mkdir -p "$SRV_OUT"

SYSL_TARGET=x86_64-elf
TOOLCHAIN_PREFIX=x86_64-elf-
CLANG_TARGET=x86_64-unknown-none-elf
CLANG_EXTRA_FLAGS="-mcmodel=kernel -mno-red-zone"
GCC_EXTRA_FLAGS="-mcmodel=kernel -mno-red-zone"
AS_EXTRA_FLAGS="--64"
LD_EXTRA_FLAGS=""
NEEDS_FLAT_BIN=""

SERVER_COMMON_SRCS=(
    oskit/ipc/ipc_client.sysl
    oskit/services/services.lsysl
    oskit/arch/x86_64/prog_config.sysl
    std/alloc/alloc.lsysl
)
SERVER_LINKER_SCRIPT=server.ld

source "$REPO_ROOT/oskit/arch/common.sh"

# x86 quirk: the non-disk servers use server.ld, but historically the disk
# server has used prog.ld. Override inside build_disk.
orig_server_ld=$SERVER_LINKER_SCRIPT
build_disk() {
    SERVER_LINKER_SCRIPT=prog.ld
    build_disk_server \
        oskit/drivers/disk/disk_x86.lsysl \
        oskit/hal/mem_cpu.lsysl
    SERVER_LINKER_SCRIPT=$orig_server_ld
}

# x86 doesn't run nic as a boot module — virtio-pci BARs live in the
# kernel's identity map and there's no per-BAR grant syscall yet.
# The virtio-net driver stays linked into the kernel (see build.sh's
# app_nsh case), and x86 bootinfo omits nic so RS treats slot 6 as
# [skipped]. Override build_nic to a no-op so `all` succeeds.
build_nic() {
    echo "=== Skipping nic on x86 (kernel-linked virtio-pci; see build.sh) ==="
}

dispatch_servers_arg "$1"
