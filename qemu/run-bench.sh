#!/bin/bash
# Run the SHA-256 benchmark on QEMU x86_64.
# Use: time bash qemu/run-bench.sh
cd "$(dirname "$0")"
qemu-system-x86_64 \
    -kernel bench.elf \
    -nographic \
    -no-reboot \
    -device isa-debug-exit,iobase=0xf4,iosize=0x04 \
    -serial mon:stdio
