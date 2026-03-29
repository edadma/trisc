Mouse Device
============

A memory-mapped mouse input device that reports absolute position and button state. Fires an interrupt on movement or button change.

Registers
---------

Base address: `0x10002A` (6 bytes), IRQ 2

Offset | Name    | R/W | Description
------ | ------- | --- | -----------
0      | STATUS  | R   | Bit 0: event ready (new data since last read)
1      | BUTTONS | R   | Bit 0 = left, 1 = right, 2 = middle. **Reading clears ready, lowers IRQ.**
2      | X_HI    | R   | X position, high byte (big-endian)
3      | X_LO    | R   | X position, low byte
4      | Y_HI    | R   | Y position, high byte (big-endian)
5      | Y_LO    | R   | Y position, low byte

Operation
---------

The mouse device uses **latest-state** semantics (not queued). Each mouse event overwrites the previous position and button state. This avoids flooding the ISR with high-frequency movement events.

Reading BUTTONS:
1. Returns the current button bitmask
2. Clears the ready bit in STATUS
3. Lowers IRQ 2 on the interrupt controller

X and Y are absolute pixel coordinates relative to the framebuffer. They remain valid until the next BUTTONS read.

The device fires IRQ 2 on every movement or button change, regardless of whether interrupts are enabled on the CPU.

The device is read-only. Writes are ignored.

ISR Example
-----------

    mouse_isr:
      movi r2, 0x10002A
      ldb r1, r2, r0         ; read STATUS
      ldi r3, 1
      and r1, r1, r3
      beq r1, r0, _ms_done   ; no event

      addi r2, r2, 1
      ldb r1, r2, r0         ; read BUTTONS (clears ready, lowers IRQ)
      addi r2, r2, 1
      lds r3, r2, r0         ; read X (16-bit, big-endian)
      addi r2, r2, 2
      lds r4, r2, r0         ; read Y (16-bit, big-endian)

      ; ... handle (r1=buttons, r3=x, r4=y) ...

    _ms_done:
      rte

Design Notes
------------

- **Absolute coordinates.** X and Y are pixel positions, not deltas. This is simpler for software and matches modern USB HID absolute pointing devices (touchscreens, tablets). The OS does not need to track cumulative position.
- **No cursor rendering.** The device reports position only. Drawing a mouse cursor on screen is the OS's responsibility (e.g., using the blitter).
- **Coordinate space.** Coordinates are relative to the framebuffer widget in the emulator. When the display is in text mode, mouse events may still arrive but coordinates are relative to the framebuffer panel dimensions.
- **Button state is instantaneous.** BUTTONS reflects the state at the time of the most recent event. If the user clicks and releases between ISR invocations, only the final state (released) is reported.
