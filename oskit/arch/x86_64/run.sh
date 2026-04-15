#!/bin/bash
# Run SLIX on x86_64 QEMU.
# Ctrl-A X to quit.
#
# Usage: ./run.sh

OUT=/tmp/slix-x86_64

if [ ! -f "$OUT/kernel.elf" ]; then
    echo "ERROR: kernel not built — run: bash oskit/arch/x86_64/build.sh app_nsh" >&2
    exit 1
fi

QEMU_ARGS="-kernel $OUT/kernel.elf -serial stdio -no-reboot -display none"

if [ -f "$OUT/ramdisk.img" ]; then
    if [ -f "$OUT/bootinfo.img" ]; then
        QEMU_ARGS="$QEMU_ARGS -initrd $OUT/ramdisk.img,$OUT/bootinfo.img"
    else
        QEMU_ARGS="$QEMU_ARGS -initrd $OUT/ramdisk.img"
    fi
fi

exec qemu-system-x86_64 $QEMU_ARGS
