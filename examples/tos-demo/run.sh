#!/bin/bash
# Build and run the TOS preemptive multitasking demo
# Task A prints every ~1s, Task B every ~2s. Ctrl-C to stop.
set -e
cd "$(git rev-parse --show-toplevel)"

echo "=== Compiling Sysl sources ==="
sbt -error "syslCliJVM/run compile --emit tof -o demo.tof tos/kernel.sysl examples/tos-demo/main.sysl examples/tos-demo/tasks.sysl"

echo "=== Assembling boot.asm ==="
sbt -error "triscCliJVM/run asm boot.asm"

echo "=== Linking ==="
sbt -error "triscCliJVM/run link -o program.tof boot.tof demo.tof"

echo "=== Running TOS demo (Ctrl-C to stop) ==="
sbt 'set ThisBuild / run / fork := true' "triscCliJVM/run run $(pwd)/program.tof"
