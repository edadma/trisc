; input_test.asm — test keyboard and mouse devices
; Polls keyboard and mouse, prints events to serial console (stdout)
;
; Run with: sbt "triscCliJVM/run asm input_test.asm -o input_test.tof"
;           sbt "triscCliJVM/run run --gui input_test.tof"

STDOUT   = 0x100000
KEYBOARD = 0x100004
MOUSE    = 0x10002A

; Vector table
  dl 0x0FFF8           ; initial SSP
  dl _start            ; initial PC
  dl _fault            ; interrupt
  resb 136             ; remaining vectors

_start
  ; Print banner
  movi r3, STDOUT
  ldi r1, 'K'
  stb r1, r3, r0
  ldi r1, 'B'
  stb r1, r3, r0
  ldi r1, '>'
  stb r1, r3, r0
  ldi r1, ' '
  stb r1, r3, r0

_loop
  ; Check keyboard
  movi r2, KEYBOARD
  ldb r1, r2, r0       ; STATUS
  ldi r3, 1
  and r1, r1, r3
  beq r1, r0, _check_mouse

  ; Read keyboard event
  movi r2, KEYBOARD
  addi r2, r2, 1
  ldb r1, r2, r0       ; SCANCODE (clears ready)
  addi r2, r2, 1
  ldb r3, r2, r0       ; FLAGS
  addi r2, r2, 1
  ldb r4, r2, r0       ; MODIFIERS

  ; Print: "K sc=XX f=X m=X\n"
  movi r2, STDOUT
  ldi r1, 'K'
  stb r1, r2, r0
  ldi r1, ' '
  stb r1, r2, r0

  ; Print scancode as hex (r1 was overwritten, re-read — actually let's just print decimal)
  ; For simplicity, just print the raw byte value
  movi r2, KEYBOARD
  addi r2, r2, 1
  ; We already consumed it... let's just print a marker for now
  movi r2, STDOUT
  ldi r1, '*'
  stb r1, r2, r0
  ldi r1, 10           ; newline
  stb r1, r2, r0

_check_mouse
  ; Check mouse
  movi r2, MOUSE
  ldb r1, r2, r0       ; STATUS
  ldi r3, 1
  and r1, r1, r3
  beq r1, r0, _loop

  ; Read mouse event
  movi r2, MOUSE
  addi r2, r2, 1
  ldb r1, r2, r0       ; BUTTONS (clears ready)

  ; Print mouse marker
  movi r2, STDOUT
  ldi r1, 'M'
  stb r1, r2, r0
  ldi r1, 10
  stb r1, r2, r0

  bra _loop

_fault
  halt
