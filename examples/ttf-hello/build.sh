#!/bin/bash
# Build the ttf-hello example
# Usage: bash examples/ttf-hello/build.sh
set -e

DIR=examples/ttf-hello

echo "=== Compiling hello.sysl ==="
sbt -error "syslCliJVM/run compile $DIR/hello.sysl --emit tof -o $DIR/hello.tof"

echo "=== Assembling boot ==="
sbt -error "triscCliJVM/run asm examples/bare-metal-hello/boot.asm -o $DIR/boot.tof"

echo "=== Linking ==="
sbt -error "triscCliJVM/run link $DIR/boot.tof $DIR/hello.tof -o $DIR/program.tof"

echo "=== Done ==="
echo "Run: sbt \"triscCliJVM/run run --gui $DIR/program.tof\""
