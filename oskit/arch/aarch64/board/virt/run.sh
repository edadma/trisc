#!/bin/bash
# Interactive SLIX aarch64 QEMU run. Assumes build.sh has
# already produced kernel.elf, bootinfo.img, and ramdisk.img
# under /tmp/slix-aarch64/. Serial goes to the current terminal
# via -nographic; Ctrl-A X to quit QEMU.

set -e

OUT="${OUT:-/tmp/slix-aarch64}"
KERNEL="$OUT/kernel.elf"
BOOTINFO="$OUT/bootinfo.img"
RAMDISK="$OUT/ramdisk.img"

for f in "$KERNEL" "$BOOTINFO" "$RAMDISK"; do
    if [ ! -f "$f" ]; then
        echo "missing: $f" >&2
        echo "run: bash oskit/arch/aarch64/board/virt/build.sh" >&2
        exit 1
    fi
done

exec qemu-system-aarch64 \
    -machine virt,gic-version=2 \
    -cpu cortex-a72 \
    -m 512M \
    -nographic \
    -no-reboot \
    -kernel "$KERNEL" \
    -device "loader,file=$BOOTINFO,addr=0x44000000" \
    -device "loader,file=$RAMDISK,addr=0x50000000"
