---
title: GPIO
description: 8-pin GPIO with edge/level interrupts.
---

The GPIO controller provides 8 digital I/O pins with configurable direction, atomic set/clear/XOR operations, and edge/level interrupt support. Inspired by the RP2040 GPIO.

**Size:** 10 bytes

## Register map

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 1B | DDR | R/W | Data direction (1 = output) |
| +1 | 1B | OUT | R/W | Output latch |
| +2 | 1B | IN | R | Pin state (read-only) |
| +3 | 1B | SET | W | Atomic set (OUT \|= value) |
| +4 | 1B | CLEAR | W | Atomic clear (OUT &= ~value) |
| +5 | 1B | XOR | W | Atomic toggle (OUT ^= value) |
| +6 | 1B | INT_MASK | R/W | Interrupt enable per pin |
| +7 | 1B | INT_STATUS | R/W | Interrupt status (write-1-to-clear) |
| +8 | 1B | INT_MODE | R/W | Interrupt mode (0 = level, 1 = edge) |
| +9 | 1B | INT_POL | R/W | Interrupt polarity (0 = low/falling, 1 = high/rising) |

## Atomic operations

The SET, CLEAR, and XOR registers modify specific bits of OUT without a read-modify-write cycle. This is safe for concurrent access and interrupt handlers.

## Interrupts

Each pin can independently generate interrupts based on mode and polarity:

| Mode | Polarity | Trigger |
|------|----------|---------|
| Level (0) | Low (0) | Pin is low |
| Level (0) | High (1) | Pin is high |
| Edge (0) | Falling (0) | Pin transitions high to low |
| Edge (1) | Rising (1) | Pin transitions low to high |

Interrupts are masked per-pin via INT_MASK. When a pin's interrupt fires, the corresponding bit in INT_STATUS is set. Write 1 to INT_STATUS to clear.
