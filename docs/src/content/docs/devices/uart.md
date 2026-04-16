---
title: UART & PL011
description: Serial communication devices.
---

TRISC includes two UART implementations: a simple custom UART and an ARM PL011-compatible UART for QEMU portability.

## UART

A minimal UART with TX/RX FIFOs and baud rate divider.

**Size:** 8 bytes

| Offset | Size | Name | Access | Description |
|--------|------|------|--------|-------------|
| +0 | 1B | TX_DATA | W | Transmit data (writes to TX FIFO) |
| +1 | 1B | RX_DATA | R | Receive data (reads from RX FIFO) |
| +2 | 1B | STATUS | R | Status flags |
| +3 | 1B | CONTROL | R/W | Control register |
| +4 | 4B | DIVISOR | R/W | Baud rate divisor |

### Status bits

| Bit | Name | Description |
|-----|------|-------------|
| 0 | TX_EMPTY | TX FIFO is empty |
| 1 | RX_READY | RX FIFO has data |
| 2 | TX_FULL | TX FIFO is full |
| 3 | OVERRUN | RX FIFO overflowed |

### Control bits

| Bit | Name | Description |
|-----|------|-------------|
| 0 | TX_IE | TX empty interrupt enable |
| 1 | RX_IE | RX ready interrupt enable |
| 2 | ENABLE | UART enable |

## PL011

An ARM PL011-compatible UART at the standard QEMU address. This allows the same OS UART driver to work on both the TRISC emulator and x86_64 QEMU.

**Base address:** `0x09000000` | **Size:** 4 KB

### Key registers

| Offset | Name | Description |
|--------|------|-------------|
| 0x000 | UARTDR | Data register (read = RX, write = TX) |
| 0x004 | UARTRSR | Receive status / error clear |
| 0x018 | UARTFR | Flag register (busy, FIFO status) |
| 0x024 | IBRD | Integer baud rate divisor |
| 0x028 | FBRD | Fractional baud rate divisor |
| 0x02C | LCR_H | Line control (word length, FIFOs, parity) |
| 0x030 | CR | Control (enable, TX/RX enable, loopback) |
| 0x038 | IMSC | Interrupt mask set/clear |
| 0x03C | RIS | Raw interrupt status |
| 0x040 | MIS | Masked interrupt status |
| 0x044 | ICR | Interrupt clear |

### Flag register (UARTFR)

| Bit | Name | Description |
|-----|------|-------------|
| 4 | RXFE | RX FIFO empty |
| 5 | TXFF | TX FIFO full |
| 6 | RXFF | RX FIFO full |
| 7 | TXFE | TX FIFO empty |

The PL011 implementation supports 16-entry TX and RX FIFOs, configurable FIFO interrupt levels, and all standard PL011 interrupt sources.
