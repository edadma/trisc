#!/bin/bash
# Build and run the TOS preemptive multitasking demo.
# Tasks use sleep() syscalls: A prints every ~1s, B every ~2s.
# Output: A B A A B A A B ...
# Ctrl-C to stop.
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
