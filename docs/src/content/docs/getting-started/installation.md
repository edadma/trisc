---
title: Installation
description: How to build and run the TRISC toolchain.
---

TRISC is built with [sbt](https://www.scala-sbt.org/) (Scala Build Tool). You need Java 11+ and sbt installed.

## Prerequisites

- **Java 11+** (JDK) — `java -version`
- **sbt 1.x** — `sbt --version`

## Clone and build

```bash
git clone https://github.com/edadma/trisc.git
cd trisc
sbt compile
```

## Run the emulator

### Headless (terminal)

```bash
sbt "triscCliJVM/runMain io.github.edadma.trisc.run run program.tof"
```

### GUI (Swing)

```bash
sbt "triscCliJVM/runMain io.github.edadma.trisc.run run --gui program.tof"
```

### Multi-core

```bash
sbt "triscCliJVM/runMain io.github.edadma.trisc.run run --smp 4 --gui program.tof"
```

## Assemble a program

```bash
sbt "triscCliJVM/runMain io.github.edadma.trisc.run asm hello.asm"
```

This produces `hello.tof` — a relocatable TOF (TRISC Object Format) file.

## Link and run

```bash
sbt "triscCliJVM/runMain io.github.edadma.trisc.run link -o hello.tof hello.tof"
sbt "triscCliJVM/runMain io.github.edadma.trisc.run run hello.tof"
```

The linker combines TOF files, resolves symbols, and prepends the runtime boot module (vector table + I/O stubs).

## Run tests

```bash
sbt test
```

This runs the full test suite (~2400 tests) covering the CPU, assembler, devices, filesystem, and OS.
