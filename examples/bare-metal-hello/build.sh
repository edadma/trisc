#!/bin/bash
# ============================================================================
# TOS Hello World — build and run
# ============================================================================
#
# Compiles a Sysl hello-world program, assembles the TOS boot stub,
# links them into a single executable, and runs it on the TRISC emulator.
#
# Usage (from repo root):  ./examples/tos-hello/build.sh
#
# ============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

DIR=examples/tos-hello

echo "=== Compiling hello.sysl → hello.asm ==="
sbt -error "syslCliJVM/run compile $DIR/hello.sysl --emit asm -o $DIR/hello.asm"

echo "=== Assembling boot.asm → boot.tof ==="
sbt -error "triscCliJVM/run asm $DIR/boot.asm -o $DIR/boot.tof"

echo "=== Assembling hello.asm → hello.tof ==="
sbt -error "triscCliJVM/run asm $DIR/hello.asm -o $DIR/hello.tof"

echo "=== Linking boot.tof + hello.tof → program.tof ==="
sbt -error "triscCliJVM/run link $DIR/boot.tof $DIR/hello.tof -o $DIR/program.tof"

echo "=== Running ==="
sbt -error "triscCliJVM/run run $DIR/program.tof"

echo "=== Done ==="
