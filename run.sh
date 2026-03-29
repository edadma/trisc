#!/bin/sh
if [ -z "$1" ]; then
  echo "Usage: ./run.sh <example-name>"
  echo "Example: ./run.sh input_test"
  exit 1
fi

sbt "syslCliJVM/run compile examples/$1.sysl --emit tof -o /tmp/$1.tof" \
    "triscCliJVM/run run --gui /tmp/$1.tof"
