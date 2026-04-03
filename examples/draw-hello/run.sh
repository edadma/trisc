#!/bin/bash
# Build and run the draw-hello example
# Usage: bash examples/draw-hello/run.sh
set -e

DIR=examples/draw-hello

echo "=== Compiling hello.sysl ==="
sbt -error "syslCliJVM/run compile $DIR/hello.sysl --emit tof -o $DIR/hello.tof"

echo "=== Assembling boot ==="
sbt -error "triscCliJVM/run asm examples/bare-metal-hello/boot.asm -o $DIR/boot.tof"

echo "=== Linking ==="
sbt -error "triscCliJVM/run link $DIR/boot.tof $DIR/hello.tof -o $DIR/program.tof"

echo "=== Running ==="
sbt -error "triscCliJVM/run run --gui $DIR/program.tof"
