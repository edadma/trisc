#!/bin/bash
cd "$(dirname "$0")"

qemu-system-aarch64 \
    -machine virt \
    -cpu cortex-a53 \
    -nographic \
    -kernel hello.elf
