Keyboard Device
===============

A memory-mapped keyboard input device that delivers USB HID scancodes. Reports key press and release events with modifier state. Fires an interrupt on each event.

Registers
---------

Base address: `0x100004` (4 bytes), IRQ 1

Offset | Name     | R/W | Description
------ | -------- | --- | -----------
0      | STATUS   | R   | Bit 0: event ready. Bit 1: overflow (events were lost)
1      | SCANCODE | R   | USB HID usage code. **Reading consumes the event, clears ready, lowers IRQ.**
2      | FLAGS    | R   | Bit 0: 1 = press, 0 = release
3      | MODIFIERS| R   | Bit 0 = Shift, 1 = Ctrl, 2 = Alt, 3 = Meta

Operation
---------

Events are queued (up to 64 deep). Each event contains a scancode, press/release flag, and modifier bitmask. FLAGS and MODIFIERS are latched from the same event as SCANCODE — they remain valid until the next SCANCODE read.

Reading SCANCODE:
1. Returns the scancode of the current event
2. Advances the queue to the next event
3. Clears the ready bit in STATUS
4. Lowers IRQ 1 on the interrupt controller

If the queue overflows, bit 1 of STATUS is set. Writing any value to STATUS clears the overflow flag.

The device fires IRQ 1 on every enqueued event, regardless of whether interrupts are enabled on the CPU.

Scancodes
---------

The device uses USB HID Keyboard/Keypad Usage codes (HID Usage Tables, Page 0x07):

Code   | Key
------ | ---
0x04–0x1D | A–Z
0x1E–0x27 | 1–9, 0
0x28   | Enter
0x29   | Escape
0x2A   | Backspace
0x2B   | Tab
0x2C   | Space
0x2D   | Minus (-)
0x2E   | Equals (=)
0x2F   | Left Bracket ([)
0x30   | Right Bracket (])
0x31   | Backslash (\)
0x33   | Semicolon (;)
0x34   | Quote (')
0x35   | Grave Accent (`)
0x36   | Comma (,)
0x37   | Period (.)
0x38   | Slash (/)
0x39   | Caps Lock
0x3A–0x45 | F1–F12
0x49   | Insert
0x4A   | Home
0x4B   | Page Up
0x4C   | Delete
0x4D   | End
0x4E   | Page Down
0x4F   | Right Arrow
0x50   | Left Arrow
0x51   | Down Arrow
0x52   | Up Arrow
0xE0   | Left Control
0xE1   | Left Shift
0xE2   | Left Alt
0xE3   | Left Meta (Cmd/Win)

Modifier keys generate their own scancode events (press/release) AND are reflected in the MODIFIERS register of subsequent events.

ISR Example
-----------

    keyboard_isr:
      movi r2, 0x100004
      ldb r1, r2, r0         ; read STATUS
      ldi r3, 1
      and r1, r1, r3
      beq r1, r0, _kb_done   ; no event ready

      addi r2, r2, 1
      ldb r1, r2, r0         ; read SCANCODE (clears ready, lowers IRQ)
      addi r2, r2, 1
      ldb r3, r2, r0         ; read FLAGS (1=press, 0=release)
      addi r2, r2, 1
      ldb r4, r2, r0         ; read MODIFIERS

      ; ... buffer (r1=scancode, r3=flags, r4=modifiers) for application ...

    _kb_done:
      rte

Design Notes
------------

- **Scancodes, not characters.** The device reports which physical key was pressed, not what character it represents. Scancode-to-character translation (keymaps, shift handling, dead keys) is OS software's responsibility.
- **Modifier keys are both events and state.** Pressing Shift generates a scancode event (0xE1, press) and also sets bit 0 in MODIFIERS for all subsequent events until Shift is released.
- **No key repeat.** The host OS may generate key repeat events, which arrive as additional press events without an intervening release. The TRISC OS can implement its own repeat logic if desired.
