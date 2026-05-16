#!/bin/bash
# Build and optionally run SLIX on x86_64 QEMU.
#
# Usage:
#   ./build.sh                  # build app_nsh
#   ./build.sh run              # build + run QEMU (interactive, Ctrl-A X to quit)
#   ./build.sh app_hello        # build a different app
#   ./build.sh app_serial run   # build app_serial + run
#
# The APP argument selects oskit/arch/x86_64/APP.sysl.
# Default: app_nsh

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-x86_64
mkdir -p "$OUT"

APP=app_nsh
RUN=""

for arg in "$@"; do
    case "$arg" in
        run) RUN=run ;;
        *) APP="$arg" ;;
    esac
done

# Source files — base kernel + arch
SYSL_FILES=(
    oskit/kernel/kernel.lsysl
    oskit/kernel/kstack.lsysl
    oskit/kernel/vma.lsysl
    oskit/kernel/page_refcnt.lsysl
    oskit/lib/elf.lsysl
    oskit/arch/x86_64/cpu.lsysl
    oskit/arch/x86_64/prog_config.sysl
    oskit/arch/x86_64/vm.lsysl
    oskit/arch/x86_64/runtime.lsysl
    oskit/arch/x86_64/posix.lsysl
    oskit/config/config.sysl
    oskit/hal/mem_cpu.lsysl
    oskit/ipc/ipc.lsysl
    oskit/services/services.lsysl
    std/alloc/alloc.lsysl
    oskit/posix/shim.lsysl
    oskit/posix/unix.lsysl
    oskit/posix/signals.lsysl
)

# App-specific extra modules
case "$APP" in
    app_nsh)
        # All servers are boot modules. Kernel only needs the PCI
        # probe layer — virtio_net + virtio_dma_kernel used to live
        # here when bringup was kernel-linked, but the nic server
        # now owns virtio_net and its own DMA (virtio_dma_server).
        # Kernel just resolves the 4 cap BARs and caches them for
        # svc_virtio_pci_info.
        SYSL_FILES+=(
            oskit/arch/x86_64/pci.lsysl
            oskit/drivers/virtio/virtio_transport_pci.lsysl
            oskit/drivers/virtio/virtio_bringup_x86.lsysl
        )
        ;;
    app_test_spawn)
        # All servers are boot modules — kernel only needs base modules
        ;;
    app_serial)
        SYSL_FILES+=(oskit/drivers/tty/tty.lsysl)
        ;;
esac

SYSL_FILES+=("oskit/arch/x86_64/${APP}.sysl")

echo "=== Sysl → LLVM IR ==="
cd "$REPO_ROOT"
sbt "syslCliJVM/run compile --emit llvm --target=x86_64-elf ${SYSL_FILES[*]} -o $OUT/kernel.ll" 2>&1 | tail -1

echo "=== LLVM IR → object ==="
clang -target x86_64-unknown-none-elf -ffreestanding -nostdlib \
    -mcmodel=kernel -mno-red-zone -fno-pic -fno-pie -w \
    -c -o "$OUT/kernel.o" "$OUT/kernel.ll"

echo "=== Assemble boot.s ==="
x86_64-elf-as --64 -o "$OUT/boot.o" "$ARCH_DIR/boot.s"

echo "=== Compile stubs.c ==="
x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
    -fno-pic -fno-pie -c -o "$OUT/stubs.o" "$ARCH_DIR/stubs.c"

echo "=== Link ==="
x86_64-elf-ld -T "$ARCH_DIR/link.ld" \
    -o "$OUT/kernel64.elf" "$OUT/boot.o" "$OUT/stubs.o" "$OUT/kernel.o" 2>&1 \
    | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
x86_64-elf-objcopy -O elf32-i386 "$OUT/kernel64.elf" "$OUT/kernel.elf"

echo "=== Built: $OUT/kernel.elf ($APP) ==="

if [ "$RUN" = "run" ]; then
    echo "=== QEMU (Ctrl-A X to quit) ==="
    # -cpu max exposes FSGSBASE so musl's __set_thread_area can use
    # WRFSBASE for TLS init. Default qemu64 CPU lacks it and the
    # instruction would #UD in ring 3.
    QEMU_ARGS="-m 512M -cpu max -kernel $OUT/kernel.elf -serial stdio -no-reboot -display none"
    # virtio-net on the default pc machine (i440fx). User-mode
    # networking is enough for bringup — the guest gets a
    # 10.0.2.x address and can ping 10.0.2.2.
    # disable-legacy=on forces non-transitional (modern-only)
    # device ID 0x1041. Default is transitional (ID 0x1000) which
    # presents the virtio device type via the Subsystem ID
    # register instead of device_id — that path isn't implemented
    # here yet (see pci_find_virtio).
    QEMU_ARGS="$QEMU_ARGS -netdev user,id=n0 -device virtio-net-pci,netdev=n0,disable-legacy=on"
    # Load ramdisk (module 0) and boot info (module 1) as multiboot modules
    if [ -f "$OUT/ramdisk.img" ]; then
        if [ -f "$OUT/bootinfo.img" ]; then
            QEMU_ARGS="$QEMU_ARGS -initrd $OUT/ramdisk.img,$OUT/bootinfo.img"
            echo "    (with ramdisk + boot info modules)"
        else
            QEMU_ARGS="$QEMU_ARGS -initrd $OUT/ramdisk.img"
            echo "    (with ramdisk module)"
        fi
    fi
    exec qemu-system-x86_64 $QEMU_ARGS
fi
