#!/bin/bash
# Boot SLIX and present an interactive shell in the GUI emulator.
# Usage: bash examples/slix-shell/run.sh
set -e
cd "$(git rev-parse --show-toplevel)"

echo "=== Compiling Sysl sources ==="
sbt -error "syslCliJVM/run compile --emit tof -o /tmp/slix-shell-sysl.tof \
  oskit/kernel/kernel.lsysl \
  oskit/services/services.lsysl \
  oskit/kernel/timer.lsysl \
  oskit/sync/semaphore.lsysl \
  oskit/sync/mutex.lsysl \
  oskit/ipc/ipc.lsysl \
  oskit/drivers/disk/disk.lsysl \
  oskit/drivers/kbd/keyboard.lsysl \
  oskit/drivers/tty/tty.lsysl \
  oskit/fs/tfs.lsysl \
  oskit/servers/tfs.lsysl \
  oskit/lib/string.lsysl \
  oskit/apps/sh.lsysl \
  oskit/apps/init.lsysl \
  examples/slix-shell/main.lsysl"

echo "=== Assembling boot.asm ==="
sbt -error "triscCliJVM/run asm oskit/boot/boot.asm"

echo "=== Linking ==="
sbt -error "triscCliJVM/run link -s tos/linker.ld -o /tmp/slix-shell.tof oskit/boot/boot.tof /tmp/slix-shell-sysl.tof"

echo "=== Running SLIX shell ==="
sbt "triscCliJVM/run run --gui /tmp/slix-shell.tof"
