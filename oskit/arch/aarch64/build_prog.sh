#!/bin/bash
# Build a standalone aarch64 SLIX program as an ELF binary.
#
# Usage:
#   ./build_prog.sh test_putc   # builds oskit/bin/test_putc.lsysl
#   ./build_prog.sh all         # builds all programs in oskit/bin/
#
# Output: /tmp/slix-aarch64/bin/<name>       (ELF64)
#         /tmp/slix-aarch64/bin/<name>.bin   (raw binary, consumed by ramdisk packer)

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"

ARCH_NAME=aarch64
OUT=/tmp/slix-${ARCH_NAME}
BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

SYSL_TARGET=aarch64-elf
TOOLCHAIN_PREFIX=aarch64-elf-
CLANG_TARGET=aarch64-unknown-none-elf
CLANG_EXTRA_FLAGS=""
GCC_EXTRA_FLAGS=""
AS_EXTRA_FLAGS=""
# -z max-page-size=0x1000 drops aarch64-elf-ld's default 64KB page
# alignment, which otherwise leaves ~60KB of padding before .text
# in the output ELF and blows past the loader's LOAD_CROSS_BUF_SIZE.
LD_EXTRA_FLAGS="-z max-page-size=0x1000"
PROG_CONFIG=oskit/arch/aarch64/prog_config.sysl
NEEDS_FLAT_BIN=1

source "$REPO_ROOT/oskit/arch/common.sh"

dispatch_program_arg "$1"
