#!/bin/bash
# Rebuild the login → nsh OS demo and run it in the GUI emulator.
# Usage: bash examples/login/run-os-login.sh
# Headless: after building, run: sbt "triscCliJVM/run run /tmp/os-login.tof"
set -e

cd "$(dirname "$0")/../.."

echo "=== Building OS login TOF ==="
sbt -error "triscCliJVM/runMain io.github.edadma.trisc.RegenOskitDemoMain login"

echo "=== Running GUI ==="
sbt -error "triscCliJVM/run run --gui /tmp/os-login.tof"
