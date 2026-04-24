#!/bin/bash
# Shared build helpers for SLIX per-arch build scripts.
#
# Each arch script sets a handful of env vars, then sources this file
# and calls the build_* functions. Keeps the arch scripts small and
# ensures x86_64, aarch64, etc. stay in lockstep as the pipeline evolves.
#
# Required env vars (set by caller before sourcing):
#   ARCH_NAME          — short name ("x86_64", "aarch64")
#   OUT                — output dir (e.g. /tmp/slix-x86_64)
#   ARCH_DIR           — absolute path of oskit/arch/<ARCH_NAME>/ (or its board/virt subdir)
#   REPO_ROOT          — absolute path of trisc repo root
#   SYSL_TARGET        — --target= flag for sysl compiler ("x86_64-elf", "aarch64-elf")
#   TOOLCHAIN_PREFIX   — binutils prefix ("x86_64-elf-", "aarch64-elf-")
#   CLANG_TARGET       — clang -target triple ("x86_64-unknown-none-elf", …)
#   CLANG_EXTRA_FLAGS  — e.g. "-mcmodel=kernel -mno-red-zone" on x86_64,
#                        "-mcmodel=large" on aarch64 (kernel builds)
#   GCC_EXTRA_FLAGS    — flags passed to prog_stubs compile (typically same as CLANG_EXTRA_FLAGS minus -w)
#   AS_EXTRA_FLAGS     — e.g. "--64" for x86 gas
#   LD_EXTRA_FLAGS     — e.g. "-z max-page-size=0x1000" for aarch64
#   PROG_CONFIG        — path to arch's prog_config.sysl (relative to REPO_ROOT)
#   NEEDS_FLAT_BIN     — "1" if aarch64-style objcopy to raw .bin is needed, empty otherwise

# Compile sysl sources to LLVM IR. Takes: <log_path> <out_ll> <sources...>
# Returns non-zero if sbt fails or its output doesn't contain "success".
compile_sysl_to_llvm() {
    local LOG="$1"; shift
    local OUT_LL="$1"; shift
    local SOURCES="$*"
    cd "$REPO_ROOT"
    sbt "syslCliJVM/run compile --emit llvm --target=${SYSL_TARGET} ${SOURCES} -o ${OUT_LL}" > "$LOG" 2>&1
    if ! grep -q "success" "$LOG"; then
        echo "  Sysl compile failed:" >&2
        tail -10 "$LOG" >&2
        return 1
    fi
}

# LLVM IR → object. clang reads $CLANG_TARGET + $CLANG_EXTRA_FLAGS.
compile_ll_to_obj() {
    local IN_LL="$1"
    local OUT_O="$2"
    clang -target "$CLANG_TARGET" -ffreestanding -nostdlib \
        $CLANG_EXTRA_FLAGS -fno-pic -fno-pie -w \
        -c -o "$OUT_O" "$IN_LL"
}

# Assemble .s → .o. Uses $TOOLCHAIN_PREFIX-as + $AS_EXTRA_FLAGS.
assemble_s() {
    local IN_S="$1"
    local OUT_O="$2"
    "${TOOLCHAIN_PREFIX}as" $AS_EXTRA_FLAGS -o "$OUT_O" "$IN_S"
}

# Compile C file with freestanding gcc. Uses $TOOLCHAIN_PREFIX-gcc + $GCC_EXTRA_FLAGS.
compile_c() {
    local IN_C="$1"
    local OUT_O="$2"
    "${TOOLCHAIN_PREFIX}gcc" -ffreestanding -nostdlib $GCC_EXTRA_FLAGS \
        -fno-pic -fno-pie -c -o "$OUT_O" "$IN_C"
}

# Link ELF. Takes: <linker_script> <output_elf> <objects...>
link_elf() {
    local LD_SCRIPT="$1"; shift
    local OUT_ELF="$1"; shift
    "${TOOLCHAIN_PREFIX}ld" -T "$LD_SCRIPT" $LD_EXTRA_FLAGS \
        -o "$OUT_ELF" "$@" 2>&1 \
        | grep -v "missing .note.GNU-stack" \
        | grep -v "deprecated" \
        | grep -v "RWX permissions" || true
}

# objcopy ELF → flat binary (only used on aarch64 currently).
objcopy_flat() {
    local IN_ELF="$1"
    local OUT_BIN="$2"
    "${TOOLCHAIN_PREFIX}objcopy" -O binary \
        -j .text -j .rodata -j .data -j .got -j .got.plt \
        "$IN_ELF" "$OUT_BIN"
}

# Full program build: compile <NAME> from oskit/bin/<NAME>.lsysl to $BIN_OUT/<NAME>.
# Assumes $BIN_OUT is set to the per-arch /bin output dir.
build_program() {
    local NAME="$1"
    local SRC="oskit/bin/${NAME}.lsysl"

    if [ ! -f "$REPO_ROOT/$SRC" ]; then
        echo "ERROR: $SRC not found" >&2
        return 1
    fi

    echo "=== Building /bin/$NAME ==="

    local SYSL_FILES=(
        "$SRC"
        "$PROG_CONFIG"
        oskit/ulib/ulib.lsysl
        oskit/ulib/srt0.lsysl
        oskit/ds/client.lsysl
        oskit/net/client.lsysl
        oskit/net/nic_client.lsysl
        oskit/net/dns.lsysl
        std/alloc/alloc.lsysl
        std/net/net.lsysl
        std/net/packet.lsysl
    )
    case "$NAME" in
        login|su)
            SYSL_FILES+=(
                std/crypto/pbkdf2/pbkdf2.lsysl
                std/crypto/hmac/hmac.lsysl
                std/crypto/sha256/sha256.lsysl
                std/encoding/binary/binary.lsysl
                std/mem/mem.lsysl
                std/debug/debug.lsysl
            )
            ;;
    esac

    local LL="$OUT/prog_${NAME}.ll"
    local LOG="$OUT/build-prog-${NAME}.log"
    compile_sysl_to_llvm "$LOG" "$LL" "${SYSL_FILES[*]}" || return 1
    echo "  Sysl -> LLVM IR ok"

    compile_ll_to_obj "$LL" "$OUT/prog_${NAME}.o"
    echo "  LLVM IR -> object ok"

    assemble_s "$ARCH_DIR/prog_start.s" "$OUT/prog_start.o"
    compile_c "$ARCH_DIR/prog_stubs.c" "$OUT/prog_stubs.o"

    link_elf "$ARCH_DIR/prog.ld" "$BIN_OUT/${NAME}" \
        "$OUT/prog_start.o" "$OUT/prog_stubs.o" "$OUT/prog_${NAME}.o"
    echo "  Link -> ELF ok ($(wc -c < "$BIN_OUT/${NAME}" | tr -d ' ') bytes)"

    if [ -n "$NEEDS_FLAT_BIN" ]; then
        objcopy_flat "$BIN_OUT/${NAME}" "$BIN_OUT/${NAME}.bin"
        echo "  objcopy -> raw ok ($(wc -c < "$BIN_OUT/${NAME}.bin" | tr -d ' ') bytes)"
    fi
}

# Convenience: dispatch `./build_prog.sh <name|all>` to build_program.
dispatch_program_arg() {
    if [ "$1" = "all" ]; then
        for src in "$REPO_ROOT"/oskit/bin/*.lsysl; do
            local name
            name=$(basename "$src" .lsysl)
            build_program "$name" || echo "  FAILED: $name" >&2
        done
    elif [ -n "$1" ]; then
        build_program "$1"
    else
        echo "Usage: $0 <program|all>" >&2
        exit 1
    fi
}

# Write a server wrapper .sysl file with the standard entry pattern:
# read RS TID from INFO_PAGE_VA and call the server's entry function.
# Takes: <name> <imports> <entry_call>
# where <entry_call> is e.g. "tfs_server()" or a multi-line body.
write_server_wrapper() {
    local NAME="$1"
    local IMPORTS="$2"
    local BODY="$3"
    local WRAPPER_DIR="$REPO_ROOT/oskit/arch/${ARCH_NAME}/gen"
    mkdir -p "$WRAPPER_DIR"
    local WRAPPER="$WRAPPER_DIR/srv_${NAME}.sysl"
    cat > "$WRAPPER" <<WRAPPER_EOF
module oskit.arch.${ARCH_NAME}.gen

$IMPORTS
import oskit.services.{rs_set_tid}
import oskit.arch.{INFO_PAGE_VA}

main()
    val info = *i64(INFO_PAGE_VA)
    rs_set_tid(int(*info))
    $BODY
WRAPPER_EOF
}

# Compile + link a server whose wrapper has already been written at
# oskit/arch/${ARCH_NAME}/gen/srv_${NAME}.sysl. Takes: <name> <extra_srcs...>.
# Uses SERVER_COMMON_SRCS (set per arch), SERVER_LINKER_SCRIPT (filename
# inside ARCH_DIR), NEEDS_FLAT_BIN, and SRV_OUT from the environment.
build_server_with_wrapper() {
    local NAME="$1"; shift
    local EXTRA_SRCS=("$@")

    local SYSL_FILES=(
        "${SERVER_COMMON_SRCS[@]}"
        "${EXTRA_SRCS[@]}"
        "oskit/arch/${ARCH_NAME}/gen/srv_${NAME}.sysl"
    )

    local LL="$OUT/srv_${NAME}.ll"
    local LOG="$OUT/build-srv-${NAME}.log"
    compile_sysl_to_llvm "$LOG" "$LL" "${SYSL_FILES[*]}" || return 1
    echo "  Sysl -> LLVM IR ok"

    compile_ll_to_obj "$LL" "$OUT/srv_${NAME}.o"
    echo "  LLVM IR -> object ok"

    assemble_s "$ARCH_DIR/srv_start.s" "$OUT/srv_start.o"
    compile_c "$ARCH_DIR/prog_stubs.c" "$OUT/prog_stubs.o"

    link_elf "$ARCH_DIR/${SERVER_LINKER_SCRIPT}" "$SRV_OUT/${NAME}" \
        "$OUT/srv_start.o" "$OUT/prog_stubs.o" "$OUT/srv_${NAME}.o"
    echo "  Link -> ELF ok ($(wc -c < "$SRV_OUT/${NAME}" | tr -d ' ') bytes)"

    if [ -n "$NEEDS_FLAT_BIN" ]; then
        objcopy_flat "$SRV_OUT/${NAME}" "$SRV_OUT/${NAME}.bin"
        echo "  objcopy -> raw ok ($(wc -c < "$SRV_OUT/${NAME}.bin" | tr -d ' ') bytes)"
    fi
}

# Standard server build: writes the standard wrapper and compiles.
# Takes: <name> <entry_fn> <imports> <extra_srcs...>
build_server() {
    local NAME="$1"; shift
    local ENTRY="$1"; shift
    local IMPORTS="$1"; shift
    echo "=== Building server: $NAME ==="
    write_server_wrapper "$NAME" "$IMPORTS" "${ENTRY}()"
    build_server_with_wrapper "$NAME" "$@"
}

# Disk server has a non-standard wrapper — it also reads ramdisk base/size
# from the info page after the RS TID. Same pattern on both arches; only
# the extra srcs differ (disk_x86 stays, mem_cpu stays).
build_disk_server() {
    echo "=== Building server: disk ==="
    write_server_wrapper disk \
        "import oskit.drivers.disk.{disk_server, disk_set_ramdisk}" \
        "disk_set_ramdisk(int(info[1]), int(info[2]))
    disk_server()"
    build_server_with_wrapper disk "$@"
}

# Per-server build recipes. Identical across arches — arch differences
# (toolchain, linker script, flat-binary step) are handled by
# build_server_with_wrapper via the env vars the arch script sets.

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

build_inet() {
    build_server inet inet_server \
        "import oskit.servers.{inet_server}" \
        oskit/servers/inet.lsysl \
        oskit/servers/inet_proto.lsysl \
        std/net/net.lsysl \
        std/net/packet.lsysl
}

build_nic() {
    build_server nic nic_server \
        "import oskit.servers.{nic_server}" \
        oskit/servers/nic.lsysl \
        oskit/arch/${ARCH_NAME}/nic_attach.sysl \
        oskit/drivers/virtio/virtio_transport_mmio.lsysl \
        oskit/drivers/virtio/virtio_net.lsysl \
        oskit/drivers/virtio/virtio_dma_server.lsysl
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

build_disk() {
    build_disk_server \
        oskit/drivers/disk/disk_x86.lsysl \
        oskit/hal/mem_cpu.lsysl
}

# Dispatch `./build_servers.sh <name|all>`.
dispatch_servers_arg() {
    if [ -n "$1" ]; then
        case "$1" in
            rs)   build_rs ;;
            disk) build_disk ;;
            tfs)  build_tfs ;;
            tty)  build_tty ;;
            pm)   build_pm ;;
            vfs)  build_vfs ;;
            ds)   build_ds ;;
            nic)  build_nic ;;
            inet) build_inet ;;
            init) build_init ;;
            all)  build_rs; build_disk; build_tfs; build_tty; build_pm; build_vfs; build_ds; build_nic; build_inet; build_init ;;
            *)    echo "Unknown server: $1" >&2; exit 1 ;;
        esac
    else
        build_rs; build_disk; build_tfs; build_tty; build_pm; build_vfs; build_ds; build_nic; build_inet; build_init
    fi
}
