#!/bin/bash
set -e
cd "$(dirname "$0")"

CROSS=aarch64-elf-

${CROSS}as -o startup.o startup.s
${CROSS}gcc -ffreestanding -nostdlib -c -o main.o main.c
${CROSS}ld -T link.ld -o hello.elf startup.o main.o
${CROSS}objcopy -O binary hello.elf hello.bin

echo "Built hello.bin"
