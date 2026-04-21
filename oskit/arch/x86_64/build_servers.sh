#!/bin/bash
# Build standalone x86_64 SLIX server ELFs for boot module isolation.
#
# Usage:
#   ./build_servers.sh          # builds all 5 servers
#   ./build_servers.sh disk     # builds just the disk server
#
# Output: /tmp/slix-x86_64/servers/<name> (ELF64)
#
# Each server is compiled with:
#   - A wrapper main() that reads RS TID from 0xBF000
#   - oskit/ipc/ipc_client.sysl (userspace IPC, not kernel handlers)
#   - oskit/services/services.lsysl (syscall wrappers)
#   - std/alloc/alloc.lsysl (memory allocator)
#   - Server-specific sources

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"
OUT=/tmp/slix-x86_64
SRV_OUT="$OUT/servers"
mkdir -p "$SRV_OUT"

# Common source files for all servers
COMMON_SRCS=(
    oskit/ipc/ipc_client.sysl
    oskit/services/services.lsysl
    oskit/arch/x86_64/prog_config.sysl
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
    local WRAPPER_DIR="$REPO_ROOT/oskit/arch/x86_64/gen"
    mkdir -p "$WRAPPER_DIR"
    local WRAPPER="$WRAPPER_DIR/srv_${NAME}.sysl"
    cat > "$WRAPPER" <<WRAPPER_EOF
module oskit.arch.x86_64.gen

$WRAPPER_IMPORTS
import oskit.services.{rs_set_tid}

main()
    val info = *i64(0xBF000)
    rs_set_tid(int(*info))
    $ENTRY()
WRAPPER_EOF

    # Collect all sources
    local SYSL_FILES=("${COMMON_SRCS[@]}" "${EXTRA_SRCS[@]}" "oskit/arch/x86_64/gen/srv_${NAME}.sysl")

    # Compile Sysl → LLVM IR
    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm --target=x86_64-elf ${SYSL_FILES[*]} -o $OUT/srv_${NAME}.ll" > /tmp/sbt-srv-${NAME}.txt 2>&1
    if [ $? -ne 0 ]; then
        echo "  Sysl compile failed:" >&2
        tail -10 /tmp/sbt-srv-${NAME}.txt >&2
        return 1
    fi
    echo "  Sysl → LLVM IR ok"

    # LLVM IR → object
    clang -target x86_64-unknown-none-elf -ffreestanding -nostdlib \
        -mcmodel=kernel -mno-red-zone -fno-pic -fno-pie -w \
        -c -o "$OUT/srv_${NAME}.o" "$OUT/srv_${NAME}.ll"
    echo "  LLVM IR → object ok"

    # Assemble startup
    x86_64-elf-as --64 -o "$OUT/srv_start.o" "$ARCH_DIR/srv_start.s"

    # Compile C stubs
    x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
        -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

    # Link → ELF
    x86_64-elf-ld -T "$ARCH_DIR/server.ld" \
        -o "$SRV_OUT/${NAME}" \
        "$OUT/srv_start.o" "$OUT/prog_stubs.o" "$OUT/srv_${NAME}.o" 2>&1 \
        | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
    echo "  Link ok ($(wc -c < "$SRV_OUT/${NAME}" | tr -d ' ') bytes)"
}

build_disk() {
    # Disk server needs custom wrapper to read ramdisk addr from RS info page
    local WRAPPER_DIR="$REPO_ROOT/oskit/arch/x86_64/gen"
    mkdir -p "$WRAPPER_DIR"
    cat > "$WRAPPER_DIR/srv_disk.sysl" <<'DISKEOF'
module oskit.arch.x86_64.gen

import oskit.drivers.disk.{disk_server, disk_set_ramdisk}
import oskit.services.{rs_set_tid}

main()
    val info = *i64(0xBF000)
    rs_set_tid(int(*info))
    disk_set_ramdisk(int(info[1]), int(info[2]))
    disk_server()
DISKEOF

    echo "=== Building server: disk ==="

    local SYSL_FILES=(
        "${COMMON_SRCS[@]}"
        oskit/drivers/disk/disk_x86.lsysl
        oskit/hal/mem_cpu.lsysl
        oskit/arch/x86_64/gen/srv_disk.sysl
    )

    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm --target=x86_64-elf ${SYSL_FILES[*]} -o $OUT/srv_disk.ll" > /tmp/sbt-srv-disk.txt 2>&1
    if [ $? -ne 0 ]; then
        echo "  Sysl compile failed:" >&2
        tail -10 /tmp/sbt-srv-disk.txt >&2
        return 1
    fi
    echo "  Sysl → LLVM IR ok"

    clang -target x86_64-unknown-none-elf -ffreestanding -nostdlib \
        -mcmodel=kernel -mno-red-zone -fno-pic -fno-pie -w \
        -c -o "$OUT/srv_disk.o" "$OUT/srv_disk.ll"
    echo "  LLVM IR → object ok"

    x86_64-elf-as --64 -o "$OUT/srv_start.o" "$ARCH_DIR/srv_start.s"
    x86_64-elf-gcc -ffreestanding -nostdlib -mcmodel=kernel -mno-red-zone \
        -fno-pic -fno-pie -c -o "$OUT/prog_stubs.o" "$ARCH_DIR/prog_stubs.c"

    x86_64-elf-ld -T "$ARCH_DIR/prog.ld" \
        -o "$SRV_OUT/disk" \
        "$OUT/srv_start.o" "$OUT/prog_stubs.o" "$OUT/srv_disk.o" 2>&1 \
        | grep -v "missing .note.GNU-stack" | grep -v "deprecated" | grep -v "RWX permissions" || true
    echo "  Link ok ($(wc -c < "$SRV_OUT/disk" | tr -d ' ') bytes)"
}

build_tfs() {
    build_server tfs tfs_server \
        "import oskit.servers.{tfs_server}" \
        oskit/servers/tfs.lsysl \
        oskit/fs/tfs.lsysl \
        oskit/drivers/disk/disk_x86.lsysl \
        oskit/hal/mem_cpu.lsysl
}

build_tty() {
    build_server tty tty_server \
        "import oskit.drivers.tty.{tty_server}" \
        oskit/drivers/tty/tty.lsysl
}

build_pm() {
    build_server pm pm_server \
        "import oskit.servers.{pm_server}" \
        oskit/servers/pm.lsysl \
        oskit/config/config.sysl \
        oskit/loader/loader.lsysl \
        oskit/fs/client.lsysl \
        oskit/hal/mem_cpu.lsysl
}

build_vfs() {
    build_server vfs vfs_server \
        "import oskit.servers.{vfs_server}" \
        oskit/servers/vfs.lsysl \
        oskit/config/config.sysl
}

build_ds() {
    build_server ds ds_server \
        "import oskit.servers.{ds_server}" \
        oskit/servers/ds.lsysl
}

build_init() {
    build_server init init \
        "import oskit.apps.init.{init}" \
        oskit/apps/init.lsysl \
        oskit/fs/client.lsysl \
        oskit/servers/pm.lsysl \
        oskit/config/config.sysl \
        oskit/loader/loader.lsysl \
        oskit/hal/mem_cpu.lsysl
}

build_rs() {
    build_server rs rs_main \
        "import oskit.servers.{rs_main}" \
        oskit/servers/rs.lsysl \
        oskit/hal/mem_cpu.lsysl
}

if [ -n "$1" ]; then
    case "$1" in
        rs)   build_rs ;;
        disk) build_disk ;;
        tfs)  build_tfs ;;
        tty)  build_tty ;;
        pm)   build_pm ;;
        vfs)  build_vfs ;;
        ds)   build_ds ;;
        init) build_init ;;
        all)  build_rs; build_disk; build_tfs; build_tty; build_pm; build_vfs; build_ds; build_init ;;
        *)    echo "Unknown server: $1" >&2; exit 1 ;;
    esac
else
    build_rs
    build_disk
    build_tfs
    build_tty
    build_pm
    build_vfs
    build_ds
    build_init
fi
