#!/bin/bash
cd "$(dirname "$0")"

qemu-system-x86_64 \
    -kernel hello.elf \
    -nographic \
    -serial mon:stdio \
    -device isa-debug-exit,iobase=0xf4,iosize=0x04 \
    -no-reboot
