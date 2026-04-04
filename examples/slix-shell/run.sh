#!/bin/bash
# Build and run the SLIX shell demo (microkernel + shell in GUI emulator)
# Usage: bash examples/slix-shell/run.sh
set -e

echo "=== Building SLIX shell TOF ==="
sbt -error "triscCliJVM/testOnly *BuildSLIXShell*"

echo "=== Running GUI ==="
sbt -error "triscCliJVM/run run --gui /tmp/slix-shell.tof"
