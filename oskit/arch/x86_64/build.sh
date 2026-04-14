#!/bin/bash
# Build and optionally run SLIX on x86_64 QEMU.
#
# Usage:
#   ./build.sh                  # build app_nsh
#   ./build.sh run              # build + run QEMU (interactive, Ctrl-A X to quit)
#   ./build.sh app_hello        # build a different app
#   ./build.sh app_serial run   # build app_serial + run
#
# The APP argument selects oskit/arch/x86_64/APP.sysl.
# Default: app_nsh

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-x86_64
mkdir -p "$OUT"

APP=app_nsh
RUN=""

for arg in "$@"; do
    case "$arg" in
        run) RUN=run ;;
        *) APP="$arg" ;;
    esac
done

# Source files — base kernel + arch
SYSL_FILES=(
    oskit/kernel/kernel.lsysl
    oskit/arch/x86_64/cpu.lsysl
    oskit/arch/x86_64/vm.lsysl
    oskit/arch/x86_64/runtime.lsysl
    oskit/config/config.sysl
    oskit/hal/mem_cpu.lsysl
    oskit/ipc/ipc.lsysl
    oskit/services/services.lsysl
)

# App-specific extra modules
case "$APP" in
    app_nsh)
        SYSL_FILES+=(
            oskit/drivers/tty/tty.lsysl
            oskit/fs/client.lsysl
            oskit/servers/pm_stubs_x86.sysl
            oskit/apps/nsh.lsysl
        )
        ;;
    app_serial)
        SYSL_FILES+=(oskit/drivers/tty/tty.lsysl)
        ;;
esac

SYSL_FILES+=("oskit/arch/x86_64/${APP}.sysl")

echo "=== Sysl → LLVM IR ==="
cd "$REPO_ROOT"
sbt "syslCliJVM/run compile --emit llvm ${SYSL_FILES[*]} -o $OUT/kernel.ll" 2>&1 | tail -1

echo "=== LLVM IR → object ==="
clang -target x86_64-unknown-none-elf -ffreestanding -nostdlib \
    -mcmodel=kernel -mno-red-zone -fno-pic -fno-pie -w \
    -c -o "$OUT/kernel.o" "$OUT/kernel.ll"

echo "=== Assemble boot.s ==="
x86_64-elf-as --64 -o "$OUT/boot.o" "$ARCH_DIR/boot.s"

echo "=== Compile stubs.c ==="
x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
    -fno-pic -fno-pie -c -o "$OUT/stubs.o" "$ARCH_DIR/stubs.c"

echo "=== Link ==="
x86_64-elf-ld -T "$ARCH_DIR/link.ld" \
    -o "$OUT/kernel64.elf" "$OUT/boot.o" "$OUT/stubs.o" "$OUT/kernel.o" 2>&1 \
    | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
x86_64-elf-objcopy -O elf32-i386 "$OUT/kernel64.elf" "$OUT/kernel.elf"

echo "=== Built: $OUT/kernel.elf ($APP) ==="

if [ "$RUN" = "run" ]; then
    echo "=== QEMU (Ctrl-A X to quit) ==="
    exec qemu-system-x86_64 -kernel "$OUT/kernel.elf" -serial stdio -no-reboot -display none
fi
