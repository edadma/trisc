#!/bin/bash
# Run the login → nsh OS demo in the GUI emulator.
# Usage: bash examples/login/run-os-login.sh          # run only (uses existing TOF)
#        bash examples/login/run-os-login.sh --build   # rebuild TOF first
set -e

cd "$(dirname "$0")/../.."

if [ "$1" = "--build" ]; then
    echo "=== Building OS login TOF ==="
    sbt -error "triscCliJVM/runMain io.github.edadma.trisc.RegenOskitDemoMain login"
fi

echo "=== Running GUI ==="
sbt -error "triscCliJVM/run run --gui /tmp/os-login.tof"
