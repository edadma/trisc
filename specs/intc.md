Interrupt Controller (INTC)
===========================

A simple memory-mapped interrupt controller that aggregates multiple device IRQ lines into a single interrupt signal to the CPU. Follows the modern architectural pattern (ARM GIC, RISC-V PLIC) of keeping interrupt routing external to the CPU core.

Overview
--------

The CPU has a single interrupt input. The interrupt controller sits between devices and the CPU:

    Timer ──IRQ 0──┐
    Keyboard ─IRQ 1──┤  Interrupt     ┌─────┐
    Mouse ──IRQ 2──┤  Controller ──IRQ──│ CPU │
    ...           ──┤                   └─────┘
                    └──────────────┘

Devices assert their IRQ line by calling into the controller. The controller checks which IRQs are both pending and enabled, and asserts the CPU's interrupt line if any are active. The CPU takes the interrupt (vector slot 2), and the ISR reads the controller's registers to identify and handle the source.

Registers
---------

Base address: `0x100026` (4 bytes)

Offset | Name    | R/W | Description
------ | ------- | --- | -----------
0      | PENDING | R   | Bitmask of IRQs with pending interrupts
1      | ENABLED | R/W | Bitmask of enabled IRQ lines (default: 0xFF)
2      | CLAIM   | R   | Lowest-numbered pending+enabled IRQ (0xFF if none)
3      | ACK     | W   | Write IRQ number to clear its pending bit

All registers are 1 byte. Bits 0–7 correspond to IRQ sources 0–7.

IRQ Assignments
---------------

IRQ | Device
--- | ------
0   | Timer
1   | Keyboard (planned)
2   | Mouse (planned)
3–7 | Reserved

Priority
--------

Lower IRQ number = higher priority. When multiple IRQs are pending, reading CLAIM returns the lowest-numbered one. This is fixed priority, not configurable.

Software Interface
------------------

### Enabling/disabling sources

Write to ENABLED to mask or unmask individual IRQ lines:

    movi r2, 0x100026
    addi r2, r2, 1       ; ENABLED register
    ldi r1, 0x03         ; enable IRQ 0 (timer) and IRQ 1 (keyboard)
    stb r1, r2, r0

### ISR pattern

    isr:
      ; Read CLAIM to identify source
      movi r2, 0x100026
      addi r2, r2, 2       ; CLAIM register
      ldb r1, r2, r0       ; r1 = IRQ number (0xFF if spurious)

      ; Dispatch based on IRQ number
      beq r1, r0, _handle_timer   ; IRQ 0 = timer
      ldi r3, 1
      beq r1, r3, _handle_kbd     ; IRQ 1 = keyboard
      bra _isr_done                ; unknown, ignore

    _handle_timer:
      ; ... handle timer ...
      bra _isr_ack

    _handle_kbd:
      ; ... handle keyboard ...

    _isr_ack:
      ; Acknowledge the IRQ
      movi r2, 0x100026
      addi r2, r2, 3       ; ACK register
      stb r1, r2, r0       ; clear pending bit for this IRQ
    _isr_done:
      rte

### Polling (no ISR)

Simple systems can poll PENDING without using interrupts:

    movi r2, 0x100026
    ldb r1, r2, r0       ; read PENDING
    ; test bits as needed

Design Notes
------------

- **Default state:** All sources enabled (ENABLED = 0xFF). Systems that need masking write ENABLED explicitly.
- **Edge-triggered semantics:** Pending bits are automatically cleared when the controller delivers the interrupt to the CPU. If the device has another event, it re-raises on the next tick. Reading CLAIM also auto-clears the claimed IRQ's pending bit. Software can also write ACK to clear bits manually.
- **Spurious interrupts:** Reading CLAIM when no IRQ is pending returns 0xFF. ISRs should handle this case.
- **Nested interrupts:** Not directly supported. The CPU disables interrupts on exception entry (Ind flag set). An ISR that wants to allow nesting must re-enable interrupts with `sti` after acknowledging the current IRQ.
- **FPGA mapping:** The controller is a separate bus peripheral, not part of the CPU core. On an FPGA, it would be a Verilog/VHDL module with IRQ input wires from devices and a single IRQ output wire to the CPU.
