#!/bin/bash
# Thin wrapper for the aarch64 musl-hello sanity test.
# The real cross-build logic lives in build-c.sh; this script
# exists for backward-compat with test harnesses and memory
# references that expect `build-hello.sh`.
set -e
exec "$(dirname "$0")/build-c.sh" hello.c mhello
