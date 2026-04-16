---
title: Device Map
description: Memory-mapped device addresses and sizes.
---

All devices are memory-mapped starting at `0x800000` (8 MB). RAM occupies addresses 0 through 0x7FFFFF.

| Address | Size | Device | Description |
|---------|------|--------|-------------|
| `0x800000` | 4B | Stdout | Console output (write byte/short/int) |
| `0x800004` | 2B | Keyboard | Scancode input with ready bit |
| `0x800008` | 8B | Display Control | Terminal/framebuffer mode switch |
| `0x800010` | -- | Blitter | Block image transfer engine |
| `0x800040` | 48B | [Timer](/devices/timer/) | MCU-style timer with 4 capture/compare channels |
| `0x800080` | 4B | [Interrupt Controller](/devices/intc/) | 8-source priority interrupt controller |
| `0x800090` | -- | Mouse | Mouse position and button state |
| `0x8000A0` | -- | Draw Engine | 2D vector graphics (lines, rects, paths, text) |
| `0x800140` | 16B | [Ramdisk](/devices/disk/) | Sector-based RAM disk with DMA |
| `0x800160` | 80B | SHA-256 | Hardware SHA-256 accelerator |
| `0x8001C0` | 202B | [DMA Controller](/devices/dma/) | 12-channel RP2040-style DMA |
| `0x800300` | 16B/core | [IPI](/devices/smp/) | Inter-processor interrupt (per-core) |
| `0x900000` | 8.3MB | Framebuffer | Pixel data (up to 1920x1080x4) |

## Device register conventions

All device registers are **word-aligned** for struct access from Sysl. Registers wider than one byte use appropriate read/write methods (readInt, readShort, etc.). Multi-byte registers follow big-endian byte order.

## Adding devices

The emulator's `Memory` class composes devices:

```scala
val mem = new Memory("Memory",
  new RAM(0, ramSize),
  new Stdout(0x800000),
  new Timer(0x800040, intc, irq = 0),
  new InterruptController(0x800080),
  // ... more devices
)
```

Devices extend the `Device` trait, implementing `readByte` and `writeByte` at minimum. Multi-byte access methods (`readInt`, `writeInt`, etc.) can be overridden for register-level access.
