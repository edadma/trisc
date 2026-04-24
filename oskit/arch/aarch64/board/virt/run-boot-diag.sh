#!/bin/bash
# Boot the aarch64 SLIX image under QEMU, capture the first N seconds
# of serial output to a file, and exit. Used during bring-up to
# capture boot diagnostics without running the full NshTests suite.
#
# Usage:
#   ./run-boot-diag.sh [seconds]      # default 10s, output -> /tmp/qemu-out.log

set -e

OUT=/tmp/slix-aarch64
LOG=/tmp/qemu-out.log
DUR=${1:-10}

QEMU_PID_FILE=/tmp/qemu-diag.pid

qemu-system-aarch64 \
    -machine virt -cpu cortex-a72 -m 512M \
    -nographic -no-reboot \
    -kernel "$OUT/kernel.elf" \
    -device loader,file="$OUT/bootinfo.img",addr=0x44000000 \
    -device loader,file="$OUT/ramdisk.img",addr=0x50000000 \
    -netdev user,id=n0 \
    -device virtio-net-device,netdev=n0 \
    > "$LOG" 2>&1 &

QEMU_PID=$!
echo "$QEMU_PID" > "$QEMU_PID_FILE"

# Give QEMU $DUR seconds to boot + talk, then kill it.
sleep "$DUR"

kill "$QEMU_PID" 2>/dev/null || true
wait "$QEMU_PID" 2>/dev/null || true
rm -f "$QEMU_PID_FILE"

echo "--- $LOG (first 200 lines) ---"
head -200 "$LOG"
