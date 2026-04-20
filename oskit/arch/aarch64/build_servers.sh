#!/bin/bash
# Build standalone aarch64 SLIX server boot modules.
#
# Usage:
#   ./build_servers.sh          # builds all supported servers (rs only so far)
#   ./build_servers.sh rs       # builds just RS
#
# Output: /tmp/slix-aarch64/servers/<name>        (ELF64)
#         /tmp/slix-aarch64/servers/<name>.bin    (flat binary for bootinfo.img)
#
# Each server is compiled with:
#   - A generated wrapper main() that reads RS TID from INFO_PAGE_VA
#     (aarch64 = 0x60090000) and calls the server entry point
#   - oskit/ipc/ipc_client.sysl (userspace IPC, not kernel handlers)
#   - oskit/services/services.lsysl (syscall wrappers)
#   - std/alloc/alloc.lsysl (memory allocator)
#   - Server-specific sources

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-aarch64
SRV_OUT="$OUT/servers"
mkdir -p "$SRV_OUT"

# Common source files for all servers
COMMON_SRCS=(
    oskit/ipc/ipc_client.sysl
    oskit/services/services.lsysl
    oskit/arch/aarch64/cpu.lsysl
    std/alloc/alloc.lsysl
)

build_server() {
    local NAME="$1"
    shift
    local ENTRY="$1"
    shift
    local WRAPPER_IMPORTS="$1"
    shift
    # Remaining args are extra source files
    local EXTRA_SRCS=("$@")

    echo "=== Building server: $NAME ==="

    # Generate wrapper main in repo tree so module path matches
    local WRAPPER_DIR="$REPO_ROOT/oskit/arch/aarch64/gen"
    mkdir -p "$WRAPPER_DIR"
    local WRAPPER="$WRAPPER_DIR/srv_${NAME}.sysl"
    cat > "$WRAPPER" <<WRAPPER_EOF
module oskit.arch.aarch64.gen

$WRAPPER_IMPORTS
import oskit.services.{rs_set_tid}
import oskit.arch.{INFO_PAGE_VA}

main()
    val info = *i64(INFO_PAGE_VA)
    rs_set_tid(int(*info))
    $ENTRY()
WRAPPER_EOF

    # Collect all sources
    local SYSL_FILES=("${COMMON_SRCS[@]}" "${EXTRA_SRCS[@]}" "oskit/arch/aarch64/gen/srv_${NAME}.sysl")

    # Compile Sysl → LLVM IR
    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm --target=aarch64-elf ${SYSL_FILES[*]} -o $OUT/srv_${NAME}.ll" > "$OUT/sbt-srv-${NAME}.log" 2>&1
    if ! grep -q "success" "$OUT/sbt-srv-${NAME}.log"; then
        echo "  Sysl compile failed:" >&2
        tail -10 "$OUT/sbt-srv-${NAME}.log" >&2
        return 1
    fi
    echo "  Sysl -> LLVM IR ok"

    # LLVM IR → object
    clang -target aarch64-unknown-none-elf -ffreestanding -nostdlib \
        -fno-pic -fno-pie -w \
        -c -o "$OUT/srv_${NAME}.o" "$OUT/srv_${NAME}.ll"
    echo "  LLVM IR -> object ok"

    # Assemble server-specific startup (calls main(), not sysl_start)
    aarch64-elf-as -o "$OUT/srv_start.o" "$ARCH_DIR/srv_start.s"

    # Compile userspace stubs (shared with programs)
    aarch64-elf-gcc -ffreestanding -nostdlib \
        -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

    # Link → ELF (prog.ld puts everything at 0x60000000)
    aarch64-elf-ld -T "$ARCH_DIR/prog.ld" \
        -o "$SRV_OUT/${NAME}" \
        "$OUT/srv_start.o" "$OUT/prog_stubs.o" "$OUT/srv_${NAME}.o" 2>&1 \
        | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
    echo "  Link -> ELF ok ($(wc -c < "$SRV_OUT/${NAME}" | tr -d ' ') bytes)"

    # Flatten for bootinfo.img (code + rodata + data only; kernel zeroes BSS)
    aarch64-elf-objcopy -O binary \
        -j .text -j .rodata -j .data -j .got -j .got.plt \
        "$SRV_OUT/${NAME}" "$SRV_OUT/${NAME}.bin"
    echo "  objcopy -> raw ok ($(wc -c < "$SRV_OUT/${NAME}.bin" | tr -d ' ') bytes)"
}

build_rs() {
    build_server rs rs_main \
        "import oskit.servers.{rs_main}" \
        oskit/servers/rs.lsysl \
        oskit/hal/mem_cpu.lsysl
}

build_tfs() {
    build_server tfs tfs_server \
        "import oskit.servers.{tfs_server}" \
        oskit/servers/tfs.lsysl \
        oskit/fs/tfs.lsysl \
        oskit/drivers/disk/disk_x86.lsysl \
        oskit/hal/mem_cpu.lsysl
}

build_disk() {
    # Disk server needs a custom wrapper that reads ramdisk base/size
    # from the info page (+8, +16) in addition to the RS TID (+0).
    local WRAPPER_DIR="$REPO_ROOT/oskit/arch/aarch64/gen"
    mkdir -p "$WRAPPER_DIR"
    cat > "$WRAPPER_DIR/srv_disk.sysl" <<'DISKEOF'
module oskit.arch.aarch64.gen

import oskit.drivers.disk.{disk_server, disk_set_ramdisk}
import oskit.services.{rs_set_tid}
import oskit.arch.{INFO_PAGE_VA}

main()
    val info = *i64(INFO_PAGE_VA)
    rs_set_tid(int(*info))
    disk_set_ramdisk(int(info[1]), int(info[2]))
    disk_server()
DISKEOF

    echo "=== Building server: disk ==="

    local SYSL_FILES=(
        "${COMMON_SRCS[@]}"
        oskit/drivers/disk/disk_x86.lsysl
        oskit/hal/mem_cpu.lsysl
        oskit/arch/aarch64/gen/srv_disk.sysl
    )

    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm --target=aarch64-elf ${SYSL_FILES[*]} -o $OUT/srv_disk.ll" > "$OUT/sbt-srv-disk.log" 2>&1
    if ! grep -q "success" "$OUT/sbt-srv-disk.log"; then
        echo "  Sysl compile failed:" >&2
        tail -10 "$OUT/sbt-srv-disk.log" >&2
        return 1
    fi
    echo "  Sysl -> LLVM IR ok"

    clang -target aarch64-unknown-none-elf -ffreestanding -nostdlib \
        -fno-pic -fno-pie -w \
        -c -o "$OUT/srv_disk.o" "$OUT/srv_disk.ll"
    echo "  LLVM IR -> object ok"

    aarch64-elf-as -o "$OUT/srv_start.o" "$ARCH_DIR/srv_start.s"
    aarch64-elf-gcc -ffreestanding -nostdlib \
        -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

    aarch64-elf-ld -T "$ARCH_DIR/prog.ld" \
        -o "$SRV_OUT/disk" \
        "$OUT/srv_start.o" "$OUT/prog_stubs.o" "$OUT/srv_disk.o" 2>&1 \
        | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
    echo "  Link -> ELF ok ($(wc -c < "$SRV_OUT/disk" | tr -d ' ') bytes)"

    aarch64-elf-objcopy -O binary \
        -j .text -j .rodata -j .data -j .got -j .got.plt \
        "$SRV_OUT/disk" "$SRV_OUT/disk.bin"
    echo "  objcopy -> raw ok ($(wc -c < "$SRV_OUT/disk.bin" | tr -d ' ') bytes)"
}

if [ -n "$1" ]; then
    case "$1" in
        rs)   build_rs ;;
        disk) build_disk ;;
        tfs)  build_tfs ;;
        all)  build_rs; build_disk; build_tfs ;;
        *)    echo "Unknown server: $1" >&2; exit 1 ;;
    esac
else
    build_rs
    build_disk
    build_tfs
fi
