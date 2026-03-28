#!/bin/bash
# ============================================================================
# Bare-metal Hello World — build and run
# ============================================================================
#
# Compiles a Sysl hello-world program to a relocatable TOF, assembles
# the boot stub, links them, and runs it on the TRISC emulator.
#
# Usage (from repo root):  ./examples/bare-metal-hello/build.sh
#
# ============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

DIR=examples/bare-metal-hello

echo "=== Compiling hello.lsysl → hello.tof ==="
sbt -error "syslCliJVM/run compile $DIR/hello.lsysl --emit tof -o $DIR/hello.tof"

echo "=== Assembling boot.asm → boot.tof ==="
sbt -error "triscCliJVM/run asm $DIR/boot.asm -o $DIR/boot.tof"

echo "=== Linking boot.tof + hello.tof → program.tof ==="
sbt -error "triscCliJVM/run link $DIR/boot.tof $DIR/hello.tof -o $DIR/program.tof"

echo "=== Running ==="
sbt -error "triscCliJVM/run run $DIR/program.tof"

echo "=== Done ==="
