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

# USER_PROG selects which test binary gets embedded as user_prog.bin
# into the kernel image. Default: test_putc. Override via first positional
# argument or USER_PROG env var.
USER_PROG="${USER_PROG:-test_putc}"

RUN=""
for arg in "$@"; do
    case "$arg" in
        run) RUN=run ;;
        *)   USER_PROG="$arg" ;;
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

echo "=== Build user program $USER_PROG ==="
bash "$ARCH_DIR/build_prog.sh" "$USER_PROG" > "$OUT/build-prog.log" 2>&1
if [ ! -f "$OUT/bin/$USER_PROG.bin" ]; then
    echo "  $USER_PROG build failed:" >&2
    tail -10 "$OUT/build-prog.log" >&2
    exit 1
fi

echo "=== Build servers (rs, disk, tfs) ==="
bash "$ARCH_DIR/build_servers.sh" rs > "$OUT/build-rs.log" 2>&1
if [ ! -f "$OUT/servers/rs.bin" ]; then
    echo "  RS build failed:" >&2
    tail -30 "$OUT/build-rs.log" >&2
    exit 1
fi
bash "$ARCH_DIR/build_servers.sh" disk > "$OUT/build-disk.log" 2>&1
if [ ! -f "$OUT/servers/disk.bin" ]; then
    echo "  disk server build failed:" >&2
    tail -30 "$OUT/build-disk.log" >&2
    exit 1
fi
bash "$ARCH_DIR/build_servers.sh" tfs > "$OUT/build-tfs.log" 2>&1
if [ ! -f "$OUT/servers/tfs.bin" ]; then
    echo "  tfs server build failed:" >&2
    tail -30 "$OUT/build-tfs.log" >&2
    exit 1
fi

echo "=== Build ramdisk ==="
cd "$REPO_ROOT"
sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeAarch64RamdiskMain" > "$OUT/sbt-ramdisk.log" 2>&1
if [ ! -f "$OUT/ramdisk.img" ]; then
    echo "  ramdisk.img build failed:" >&2
    tail -20 "$OUT/sbt-ramdisk.log" >&2
    exit 1
fi

echo "=== Pack boot info (user=$USER_PROG.bin, rs, disk, tfs) ==="
# Copy the chosen user program to stable name "user.bin" so bi_find("user")
# resolves consistently regardless of which test program was selected.
cp "$OUT/bin/$USER_PROG.bin" "$OUT/bin/user.bin"
cd "$REPO_ROOT"
sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeAarch64BootInfoMain user rs disk tfs" > "$OUT/sbt-bootinfo.log" 2>&1
if [ ! -f "$OUT/bootinfo.img" ]; then
    echo "  bootinfo.img build failed:" >&2
    tail -20 "$OUT/sbt-bootinfo.log" >&2
    exit 1
fi

echo "=== Link ==="
aarch64-elf-ld -T "$BOARD_DIR/link.ld" \
    -o "$OUT/kernel.elf" \
    "$OUT/boot.o" "$OUT/vectors.o" "$OUT/cpu_asm.o" "$OUT/mmu.o" "$OUT/vm_asm.o" "$OUT/irq_asm.o" "$OUT/stubs.o" "$OUT/kernel.o" 2>&1 \
    | grep -v "has a LOAD segment with RWX permissions" || true

echo "=== Built: $OUT/kernel.elf ==="

if [ "$RUN" = "run" ]; then
    echo "=== QEMU (Ctrl-A X to quit) ==="
    exec qemu-system-aarch64 \
        -machine virt \
        -cpu cortex-a72 \
        -m 512M \
        -nographic \
        -no-reboot \
        -kernel "$OUT/kernel.elf" \
        -device loader,file="$OUT/bootinfo.img",addr=0x44000000 \
        -device loader,file="$OUT/ramdisk.img",addr=0x50000000
fi
