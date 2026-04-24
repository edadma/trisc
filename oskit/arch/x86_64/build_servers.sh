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

# x86 nic server uses virtio_transport_pci (PCI) + virtio_net + the
# server-context DMA provider (virtio_dma_server). The kernel's
# virtio_bringup_x86 caches the cap addresses at boot; nic's
# srv_nic_attach in prog_config.sysl fetches them via
# svc_virtio_pci_info and calls v_attach_pci.
#
# Also needs pci.lsysl for VIRTIO_PCI_CAP_* constants that
# virtio_transport_pci imports (v_find is unused on the server but
# the transport is the same source file).
build_nic() {
    build_server nic nic_server \
        "import oskit.servers.{nic_server}" \
        oskit/servers/nic.lsysl \
        oskit/arch/x86_64/nic_attach.sysl \
        oskit/arch/x86_64/pci.lsysl \
        oskit/drivers/virtio/virtio_transport_pci.lsysl \
        oskit/drivers/virtio/virtio_net.lsysl \
        oskit/drivers/virtio/virtio_dma_server.lsysl
}

dispatch_servers_arg "$1"
