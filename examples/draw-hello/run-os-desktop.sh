#!/bin/bash
# Build and run the OS desktop demo (display server + IPC + mouse)
# Usage: bash examples/draw-hello/run-os-desktop.sh
set -e

echo "=== Building OS desktop TOF ==="
sbt -error "triscCliJVM/runMain io.github.edadma.trisc.RegenOskitDemoMain desktop"

echo "=== Running GUI ==="
sbt -error "triscCliJVM/run run --gui /tmp/os-desktop.tof"
