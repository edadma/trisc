#!/bin/bash
# Build and run the TOS preemptive multitasking demo
set -e
cd "$(git rev-parse --show-toplevel)"

echo "=== Compiling Sysl sources ==="
sbt -error "syslCliJVM/run compile --emit tof -o demo.tof tos/kernel.sysl examples/tos-demo/main.sysl examples/tos-demo/tasks.sysl"

echo "=== Assembling boot.asm ==="
sbt -error "triscCliJVM/run asm boot.asm"

echo "=== Linking ==="
sbt -error "triscCliJVM/run link -o program.tof boot.tof demo.tof"

echo "=== Running TOS demo ==="
sbt -error 'set ThisBuild / run / fork := true' "triscCliJVM/run run --limit 50000 $(pwd)/program.tof"
