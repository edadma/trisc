#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

DIR=examples/circles

echo "=== Compiling circles.lsysl → circles.tof ==="
sbt -error "syslCliJVM/run compile $DIR/circles.lsysl --emit tof -o $DIR/circles.tof"

echo "=== Assembling boot.asm → boot.tof ==="
sbt -error "triscCliJVM/run asm examples/bare-metal-hello/boot.asm -o $DIR/boot.tof"

echo "=== Linking boot.tof + circles.tof → program.tof ==="
sbt -error "triscCliJVM/run link $DIR/boot.tof $DIR/circles.tof -o $DIR/program.tof"

echo "=== Running with GUI ==="
sbt -error "triscCliJVM/run run --gui $DIR/program.tof"
