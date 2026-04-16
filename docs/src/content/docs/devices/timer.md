---
title: Timer
description: MCU-style timer with prescaler, auto-reload, and capture/compare channels.
---

The timer is inspired by STM32 general-purpose timers and RP2040 timer channels. It provides a 32-bit counter with prescaler, auto-reload, and up to 4 independently configurable capture/compare channels.

**Base address:** `0x800040` | **Size:** 48 bytes | **IRQ:** 0

## Register map

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 4B | ARR | R/W | Auto-reload value (period) |
| +4 | 4B | CNT | R/W | Counter value |
| +8 | 2B | PSC | R/W | Prescaler (counter increments every PSC+1 ticks) |
| +10 | 1B | CR | R/W | Control register |
| +11 | 1B | SR | R/W | Status register (write-1-to-clear) |
| +12 | 1B | IER | R/W | Interrupt enable register |

### Control register (CR)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | EN | Enable (1 = counting) |
| 1 | OPM | One-pulse mode (1 = stop at overflow) |
| 2 | DIR | Direction (1 = count down) |

### Status register (SR)

| Bit | Name | Description |
|-----|------|-------------|
| 0 | UIF | Update interrupt flag (overflow) |
| 1-4 | CCxIF | Channel x event flag |

## Channels

4 channels, each 8 bytes, starting at offset +16:

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 4B | CCR | R/W | Capture/compare register |
| +4 | 1B | CCMR | R/W | Channel mode |

### Channel modes (CCMR)

| Value | Mode | Description |
|-------|------|-------------|
| 0 | Off | Channel disabled |
| 1 | Capture rising | Latch CNT on rising edge |
| 2 | Capture falling | Latch CNT on falling edge |
| 3 | Capture both | Latch CNT on any edge |
| 4 | Compare toggle | Toggle output when CNT == CCR |
| 5 | Compare set | Set output when CNT == CCR |
| 6 | Compare clear | Clear output when CNT == CCR |
| 7 | PWM | High while CNT < CCR, low otherwise |

## Usage example

Set up a 1 kHz interrupt with the timer running at 1 MHz (PSC=0, ARR=999):

```asm
movi r1, 0x800040      ; timer base
ldi  r2, 999
stw  r2, r1, r0        ; ARR = 999 (writeInt at +0)
ldi  r2, 1
stb  r2, r1, 12        ; IER = bit 0 (overflow interrupt enabled)
stb  r2, r1, 10        ; CR = EN (start counting)
```
