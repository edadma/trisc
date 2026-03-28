#!/bin/bash
# Generate hello.sysl from font data + code template
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

DIR=examples/ttf-hello

# Generate font data as sysl fragment
sbt -error "fonts/run /System/Library/Fonts/Monaco.ttf 16 $DIR/font_fragment.sysl"

# Build hello.sysl by concatenating code + font data
cat > $DIR/hello.sysl << 'HEADER'
DISPLAY_CTRL = 0xFF06
FB_BASE = 0x10000
FB_WIDTH = 640
FB_HEIGHT = 480

dc_write(offset: int, v: int)
    p: *i8 = DISPLAY_CTRL + offset
    *p = byte(v)

init_fb()
    dc_write(0, 1)
    dc_write(2, FB_WIDTH >> 8)
    dc_write(3, FB_WIDTH & 0xFF)
    dc_write(4, FB_HEIGHT >> 8)
    dc_write(5, FB_HEIGHT & 0xFF)
    dc_write(1, 1)

set_pixel(x: int, y: int, r: int, g: int, b: int, a: int)
    if x >= 0 && x < FB_WIDTH && y >= 0 && y < FB_HEIGHT
        offset = (y * FB_WIDTH + x) * 4
        p: *i8 = FB_BASE + offset
        *p = byte(r * a / 255)
        p = p + 1
        *p = byte(g * a / 255)
        p = p + 1
        *p = byte(b * a / 255)
        p = p + 1
        *p = byte(a)

draw_char(x: int, y: int, ch: int, r: int, g: int, b: int)
    if ch < FONT_FIRST || ch > FONT_LAST then return
    idx = ch - FONT_FIRST
    base = idx * FONT_CELL_W * FONT_CELL_H
    for row = 0; row < FONT_CELL_H; row++
        for col = 0; col < FONT_CELL_W; col++
            a = font_data[base + row * FONT_CELL_W + col]
            if a > 0
                set_pixel(x + col, y + row, r, g, b, a)

draw_text(x: int, y: int, s: *byte, len: int, r: int, g: int, b: int)
    for i = 0; i < len; i++
        draw_char(x + i * FONT_CELL_W, y, s[i], r, g, b)

main() -> int
    init_fb()
    msg: [12]byte = [72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33]
    draw_text(220, 220, msg, 12, 255, 255, 255)
    draw_text(220, 250, msg, 12, 0, 255, 100)
    draw_text(220, 280, msg, 12, 100, 200, 255)
    draw_text(220, 310, msg, 12, 255, 230, 50)
    0

HEADER

# Append font data (strip literate prose header, keep only code lines)
# The font fragment is in literate format; extract indented lines
grep '^ ' $DIR/font_fragment.sysl >> $DIR/hello.sysl

# Compile
echo "=== Compiling hello.sysl ==="
sbt -error "syslCliJVM/run compile $DIR/hello.sysl --emit tof -o $DIR/hello.tof"

echo "=== Assembling boot ==="
sbt -error "triscCliJVM/run asm examples/bare-metal-hello/boot.asm -o $DIR/boot.tof"

echo "=== Linking ==="
sbt -error "triscCliJVM/run link $DIR/boot.tof $DIR/hello.tof -o $DIR/program.tof"

echo "=== Done ==="
echo "Run: sbt \"triscCliJVM/run run --gui $DIR/program.tof\""
