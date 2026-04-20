#!/bin/bash
# Run SLIX aarch64 kernel under QEMU for a bounded time and capture output.
# Usage:
#   ./run-test.sh                     # 3s run, output to /tmp/qemu-aa64.log
#   ./run-test.sh <seconds>           # custom duration
#   ./run-test.sh <seconds> <logfile> # custom duration + logfile

set -e

DURATION="${1:-3}"
LOGFILE="${2:-/tmp/qemu-aa64.log}"
KERNEL="${KERNEL:-/tmp/slix-aarch64/kernel.elf}"
BOOTINFO="${BOOTINFO:-/tmp/slix-aarch64/bootinfo.img}"
RAMDISK="${RAMDISK:-/tmp/slix-aarch64/ramdisk.img}"

QEMU_ARGS=(
    -machine virt,gic-version=2
    -cpu cortex-a72
    -m 512M
    -nographic
    -no-reboot
    -kernel "$KERNEL"
)
if [ -f "$BOOTINFO" ]; then
    QEMU_ARGS+=(-device "loader,file=$BOOTINFO,addr=0x44000000")
fi
if [ -f "$RAMDISK" ]; then
    QEMU_ARGS+=(-device "loader,file=$RAMDISK,addr=0x50000000")
fi

qemu-system-aarch64 "${QEMU_ARGS[@]}" > "$LOGFILE" 2>&1 &
QPID=$!

sleep "$DURATION"
kill "$QPID" 2>/dev/null || true
wait "$QPID" 2>/dev/null || true

echo "=== QEMU run complete (${DURATION}s) — log at $LOGFILE ==="
