#!/bin/bash
# Boot the aarch64 QEMU virt kernel with a virtio-net device attached,
# capture a few seconds of output, then kill QEMU so the script can be
# used in automated loops without a human hitting Ctrl-A X.
#
# Usage:   ./run-net.sh [seconds] [logfile]
# Default: 8 seconds, /tmp/vnet-boot.txt

set -u

OUT_DIR=/tmp/slix-aarch64
SECONDS_TO_RUN="${1:-8}"
LOGFILE="${2:-/tmp/vnet-boot.txt}"

qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a72 \
    -m 512M \
    -nographic \
    -no-reboot \
    -global virtio-mmio.force-legacy=false \
    -kernel "$OUT_DIR/kernel.elf" \
    -netdev user,id=n0 \
    -device virtio-net-device,netdev=n0,mac=52:54:00:12:34:56 \
    -device loader,file="$OUT_DIR/bootinfo.img",addr=0x44000000 \
    -device loader,file="$OUT_DIR/ramdisk.img",addr=0x50000000 \
    > "$LOGFILE" 2>&1 &
QEMU_PID=$!

sleep "$SECONDS_TO_RUN"
kill "$QEMU_PID" 2>/dev/null
wait "$QEMU_PID" 2>/dev/null

# Print the captured log to stdout so callers can just exec this script.
cat "$LOGFILE"
exit 0
