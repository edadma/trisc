#!/bin/bash
# Build a standalone x86_64 SLIX program as an ELF binary.
#
# Usage:
#   ./build_prog.sh echo   # builds oskit/bin/echo.lsysl
#   ./build_prog.sh all    # builds all programs in oskit/bin/
#
# Output: /tmp/slix-x86_64/bin/<name> (ELF64)

set -e

ARCH_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$ARCH_DIR/../../.." && pwd)"

ARCH_NAME=x86_64
OUT=/tmp/slix-${ARCH_NAME}
BIN_OUT="$OUT/bin"
mkdir -p "$BIN_OUT"

SYSL_TARGET=x86_64-elf
TOOLCHAIN_PREFIX=x86_64-elf-
CLANG_TARGET=x86_64-unknown-none-elf
CLANG_EXTRA_FLAGS="-mcmodel=kernel -mno-red-zone"
GCC_EXTRA_FLAGS="-mcmodel=kernel -mno-red-zone"
AS_EXTRA_FLAGS="--64"
LD_EXTRA_FLAGS=""
PROG_CONFIG=oskit/arch/x86_64/prog_config.sysl
NEEDS_FLAT_BIN=""

source "$REPO_ROOT/oskit/arch/common.sh"

dispatch_program_arg "$1"
