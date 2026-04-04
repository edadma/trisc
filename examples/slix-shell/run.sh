#!/bin/bash
# Boot SLIX and present an interactive shell.
# Uses text terminal mode — keyboard input from stdin.
# Ctrl-C to exit.
set -e
cd "$(git rev-parse --show-toplevel)"

echo "=== Booting SLIX shell ==="
sbt 'set ThisBuild / run / fork := true' 'triscCliJVM/testOnly *RunSLIXShell*'
