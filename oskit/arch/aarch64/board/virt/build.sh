#!/bin/bash
# Build SLIX aarch64 kernel for QEMU virt machine.
# Pipeline: sysl -> LLVM IR -> aarch64 object -> link with boot.s + vectors.s.
#
# Usage:
#   ./build.sh           # build
#   ./build.sh run       # build and run under QEMU

set -e

BOARD_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$BOARD_DIR/../../../../.." && pwd)"
OUT=/tmp/slix-aarch64
mkdir -p "$OUT"

RUN=""
for arg in "$@"; do
    case "$arg" in
        run) RUN=run ;;
    esac
done

ARCH_DIR="$REPO_ROOT/oskit/arch/aarch64"

SYSL_FILES=(
    oskit/config/config.sysl
    oskit/arch/aarch64/cpu.lsysl
    oskit/arch/aarch64/vm.lsysl
    oskit/arch/aarch64/exc.lsysl
    oskit/hal/mem_cpu.lsysl
    oskit/kernel/kernel.lsysl
    oskit/kernel/spinlock.lsysl
    oskit/services/services.lsysl
    oskit/ipc/ipc.lsysl
    oskit/arch/aarch64/board/virt/uart.lsysl
    oskit/arch/aarch64/board/virt/hello.lsysl
)

echo "=== Sysl -> LLVM IR ==="
cd "$REPO_ROOT"
sbt "syslCliJVM/run compile --emit llvm --target=aarch64-elf ${SYSL_FILES[*]} -o $OUT/kernel.ll" > "$OUT/sbt-kernel.log" 2>&1
if ! grep -q "success" "$OUT/sbt-kernel.log"; then
    echo "  Sysl compile failed:" >&2
    tail -10 "$OUT/sbt-kernel.log" >&2
    exit 1
fi

echo "=== LLVM IR -> object ==="
clang -target aarch64-unknown-none-elf -ffreestanding -nostdlib \
    -mcmodel=large -fno-pic -fno-pie -w \
    -c -o "$OUT/kernel.o" "$OUT/kernel.ll"

echo "=== Assemble boot.s ==="
aarch64-elf-as -o "$OUT/boot.o" "$BOARD_DIR/boot.s"

echo "=== Assemble vectors.s ==="
aarch64-elf-as -o "$OUT/vectors.o" "$BOARD_DIR/vectors.s"

echo "=== Assemble cpu_asm.s ==="
aarch64-elf-as -o "$OUT/cpu_asm.o" "$ARCH_DIR/cpu_asm.s"

echo "=== Assemble mmu.s ==="
aarch64-elf-as -o "$OUT/mmu.o" "$ARCH_DIR/mmu.s"

echo "=== Assemble vm_asm.s ==="
aarch64-elf-as -o "$OUT/vm_asm.o" "$ARCH_DIR/vm_asm.s"

echo "=== Assemble irq_asm.s ==="
aarch64-elf-as -o "$OUT/irq_asm.o" "$ARCH_DIR/irq_asm.s"

echo "=== Compile stubs.c ==="
aarch64-elf-gcc -ffreestanding -nostdlib -mcmodel=large \
    -fno-pic -fno-pie -c -o "$OUT/stubs.o" "$BOARD_DIR/stubs.c"

echo "=== Build user program test_putc ==="
bash "$ARCH_DIR/build_prog.sh" test_putc > "$OUT/build-prog.log" 2>&1
if [ ! -f "$OUT/bin/test_putc.bin" ]; then
    echo "  test_putc build failed:" >&2
    tail -10 "$OUT/build-prog.log" >&2
    exit 1
fi

echo "=== Embed test_putc.bin into kernel ==="
# objcopy -I binary produces _binary_<path>_start/_end/_size symbols.
# Run from $OUT/bin so the symbol becomes _binary_test_putc_bin_start.
(cd "$OUT/bin" && aarch64-elf-objcopy \
    -I binary -O elf64-littleaarch64 -B aarch64 \
    --rename-section .data=.rodata,alloc,load,readonly,data,contents \
    test_putc.bin "$OUT/user_blob.o")

echo "=== Link ==="
aarch64-elf-ld -T "$BOARD_DIR/link.ld" \
    -o "$OUT/kernel.elf" \
    "$OUT/boot.o" "$OUT/vectors.o" "$OUT/cpu_asm.o" "$OUT/mmu.o" "$OUT/vm_asm.o" "$OUT/irq_asm.o" "$OUT/stubs.o" "$OUT/kernel.o" "$OUT/user_blob.o" 2>&1 \
    | grep -v "has a LOAD segment with RWX permissions" || true

echo "=== Built: $OUT/kernel.elf ==="

if [ "$RUN" = "run" ]; then
    echo "=== QEMU (Ctrl-A X to quit) ==="
    exec qemu-system-aarch64 \
        -machine virt \
        -cpu cortex-a72 \
        -nographic \
        -no-reboot \
        -kernel "$OUT/kernel.elf"
fi
