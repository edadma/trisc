---
title: DMA Controller
description: 12-channel RP2040-style DMA controller.
---

The DMA controller is modeled after the RP2040's DMA engine. It provides 12 independent channels that can transfer data between memory regions or between memory and devices without CPU involvement.

**Base address:** `0x8001C0` | **Size:** 202 bytes | **IRQ:** 4

## Channel registers

Each channel occupies 16 bytes. Channel N starts at base + N * 16.

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 4B | READ_ADDR | R/W | Source address |
| +4 | 4B | WRITE_ADDR | R/W | Destination address |
| +8 | 4B | TRANS_COUNT | R/W | Number of transfers remaining |
| +12 | 4B | CTRL_TRIG | R/W | Control and trigger register |

### CTRL_TRIG bits

| Bits | Name | Description |
|------|------|-------------|
| 1:0 | DATA_SIZE | Transfer width: 0=byte, 1=short, 2=word, 3=long |
| 2 | INCR_READ | Increment read address after each transfer |
| 3 | INCR_WRITE | Increment write address after each transfer |
| 7:4 | CHAIN_TO | Channel to trigger when this one completes (0xF = no chain) |
| 8 | IRQ_QUIET | Suppress IRQ on completion |
| 9 | EN | Enable / trigger (write 1 to start) |
| 10 | BUSY | Transfer in progress (read-only) |
| 11 | PHYS_MODE | Bypass MMU (use physical addresses) |

## Global registers

Starting at base + 192:

| Offset | Size | Name | Description |
|--------|------|------|-------------|
| +192 | 2B | INTS | Interrupt status (one bit per channel) |
| +194 | 2B | INTE | Interrupt enable |
| +196 | 2B | INTF | Interrupt force |
| +198 | 2B | INTC | Interrupt clear (write-1-to-clear) |
| +200 | 2B | ABORT | Abort channels (write-1-to-abort) |

## Channel chaining

When a channel completes and CHAIN_TO is not 0xF, the target channel is automatically triggered. This enables scatter-gather DMA and linked buffer lists without CPU intervention.

## Usage example

Copy 256 bytes from 0x1000 to 0x2000:

```c
// Sysl
val dma = *DmaChannel(0x8001C0)  // channel 0
dma.read_addr = 0x1000
dma.write_addr = 0x2000
dma.trans_count = 256
dma.ctrl_trig = 0x20D  // byte, incr_read, incr_write, no_chain, enable
```
