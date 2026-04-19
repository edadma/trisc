#!/bin/bash
# Run SLIX on x86_64 QEMU.
#
# Ctrl-C sends SIGINT to the foreground SLIX program.
# Ctrl-A X to quit QEMU.
#
# Usage: ./run.sh

OUT=/tmp/slix-x86_64

if [ ! -f "$OUT/kernel.elf" ]; then
    echo "ERROR: kernel not built — run: bash oskit/arch/x86_64/build.sh app_nsh" >&2
    exit 1
fi

# Put the host terminal in raw-ish mode so Ctrl-C (0x03) and other
# control bytes pass straight through to the guest UART instead of
# being swallowed by the host tty's line discipline.
#   -icanon: byte-at-a-time (no line buffering)
#   -isig:   don't translate Ctrl-C/Ctrl-Z/Ctrl-\ into signals
#   -echo:   SLIX's TTY driver handles echo
#   -ixon:   don't intercept Ctrl-S/Ctrl-Q for flow control
saved_stty=$(stty -g)
restore() { stty "$saved_stty"; }
trap restore EXIT INT TERM
stty -icanon -isig -echo -ixon

QEMU_ARGS=(
    -kernel "$OUT/kernel.elf"
    -chardev stdio,id=char0,signal=off,mux=on
    -serial chardev:char0
    -mon chardev=char0,mode=readline
    -no-reboot
    -display none
)

if [ -f "$OUT/ramdisk.img" ]; then
    if [ -f "$OUT/bootinfo.img" ]; then
        QEMU_ARGS+=( -initrd "$OUT/ramdisk.img,$OUT/bootinfo.img" )
    else
        QEMU_ARGS+=( -initrd "$OUT/ramdisk.img" )
    fi
fi

qemu-system-x86_64 "${QEMU_ARGS[@]}"
