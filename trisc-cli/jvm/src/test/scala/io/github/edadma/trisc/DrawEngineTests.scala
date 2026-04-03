package io.github.edadma.trisc

class DrawEngineTests extends TestHelpers {

  val W = 320
  val H = 240

  def mkDraw(): (DrawEngine, FramebufferImage) =
    val fb = new FramebufferImage(0, (W * H * 4).toLong)
    fb.setResolution(W, H)
    val ram = new RAM(0x100000, 0x10000)
    val mem = new Memory("mem", fb, ram)
    val draw = new DrawEngine(0x200000, mem, fb, () => W, () => H)
    (draw, fb)

  /** Set 16-bit register */
  def set16(d: DrawEngine, off: Int, v: Int): Unit =
    d.writeByte(0x200000L + off, (v >> 8) & 0xFF)
    d.writeByte(0x200000L + off + 1, v & 0xFF)

  /** Set color */
  def setColor(d: DrawEngine, r: Int, g: Int, b: Int, a: Int = 255): Unit =
    d.writeByte(0x20000A, r)
    d.writeByte(0x20000B, g)
    d.writeByte(0x20000C, b)
    d.writeByte(0x20000D, a)

  /** Execute command */
  def exec(d: DrawEngine, cmd: Int): Unit =
    d.writeByte(0x200000, cmd)

  /** Read pixel RGBA from framebuffer */
  def pixel(fb: FramebufferImage, x: Int, y: Int): (Int, Int, Int, Int) =
    val p = fb.pixels(y * W + x)
    ((p >> 16) & 0xFF, (p >> 8) & 0xFF, p & 0xFF, (p >> 24) & 0xFF)

  // ===== Basic =====

  "DrawEngine has correct size" in {
    val (draw, _) = mkDraw()
    draw.size shouldBe 64
  }

  "clear fills framebuffer" in {
    val (draw, fb) = mkDraw()
    setColor(draw, 255, 0, 0)
    exec(draw, 0x01) // CLEAR
    val (r, g, b, a) = pixel(fb, 0, 0)
    r shouldBe 255
    g shouldBe 0
    b shouldBe 0
    a shouldBe 255
  }

  "clear then check center pixel" in {
    val (draw, fb) = mkDraw()
    setColor(draw, 0, 128, 255)
    exec(draw, 0x01)
    val (r, g, b, _) = pixel(fb, W / 2, H / 2)
    r shouldBe 0
    g shouldBe 128
    b shouldBe 255
  }

  // ===== Fill rect =====

  "fill rect draws colored rectangle" in {
    val (draw, fb) = mkDraw()
    // Clear to black
    setColor(draw, 0, 0, 0)
    exec(draw, 0x01)
    // Draw green rect at (10,10) size (50,30)
    setColor(draw, 0, 255, 0)
    set16(draw, 2, 10)  // X1
    set16(draw, 4, 10)  // Y1
    set16(draw, 6, 50)  // X2 (width)
    set16(draw, 8, 30)  // Y2 (height)
    exec(draw, 0x10) // FILL_RECT

    // Inside rect should be green
    val (r1, g1, b1, _) = pixel(fb, 20, 20)
    g1 shouldBe 255
    r1 shouldBe 0

    // Outside rect should be black
    val (r2, g2, b2, _) = pixel(fb, 5, 5)
    r2 shouldBe 0
    g2 shouldBe 0
    b2 shouldBe 0
  }

  // ===== Draw line =====

  "draw line places pixels" in {
    val (draw, fb) = mkDraw()
    setColor(draw, 0, 0, 0)
    exec(draw, 0x01)
    // Draw white horizontal line from (0,50) to (100,50)
    setColor(draw, 255, 255, 255)
    set16(draw, 14, 16) // LINE_W = 1.0 (16 in fixed point)
    set16(draw, 2, 0)   // X1
    set16(draw, 4, 50)  // Y1
    set16(draw, 6, 100) // X2
    set16(draw, 8, 50)  // Y2
    exec(draw, 0x11) // DRAW_LINE

    // Pixel on the line should be white (or near-white with anti-aliasing)
    val (r, g, b, _) = pixel(fb, 50, 50)
    r should be > 200
    g should be > 200
    b should be > 200
  }

  // ===== Path: rect + fill =====

  "path rect then fill" in {
    val (draw, fb) = mkDraw()
    setColor(draw, 0, 0, 0)
    exec(draw, 0x01) // clear

    exec(draw, 0x06) // NEW_PATH
    setColor(draw, 0, 0, 255)
    set16(draw, 2, 20) // X1
    set16(draw, 4, 20) // Y1
    set16(draw, 6, 60) // W
    set16(draw, 8, 40) // H
    exec(draw, 0x07) // RECT
    exec(draw, 0x05) // FILL

    val (r, g, b, _) = pixel(fb, 40, 30)
    b shouldBe 255
    r shouldBe 0
  }

  // ===== Circle =====

  "circle fill" in {
    val (draw, fb) = mkDraw()
    setColor(draw, 0, 0, 0)
    exec(draw, 0x01)

    exec(draw, 0x06) // NEW_PATH
    setColor(draw, 255, 255, 0) // yellow
    set16(draw, 2, 100) // CX
    set16(draw, 4, 100) // CY
    set16(draw, 6, 40)  // radius
    exec(draw, 0x08) // CIRCLE
    exec(draw, 0x05) // FILL

    // Center should be yellow
    val (r, g, b, _) = pixel(fb, 100, 100)
    r shouldBe 255
    g shouldBe 255
    b shouldBe 0

    // Far outside should be black
    val (r2, g2, b2, _) = pixel(fb, 10, 10)
    r2 shouldBe 0
    g2 shouldBe 0
  }

  // ===== Text =====

  "draw text renders characters" in {
    val (draw, fb) = mkDraw()
    setColor(draw, 0, 0, 0)
    exec(draw, 0x01)

    // Write "Hi" to inline text buffer
    draw.writeByte(0x200000L + 32, 'H')
    draw.writeByte(0x200000L + 33, 'i')
    draw.writeByte(0x200000L + 34, 0) // null terminator

    setColor(draw, 255, 255, 255)
    set16(draw, 2, 50)  // X
    set16(draw, 4, 50)  // Y
    set16(draw, 20, 24) // font size
    draw.writeByte(0x200000L + 22, 0) // plain style
    // TEXT_ADDR = 0 means use inline buffer
    draw.writeByte(0x200000L + 16, 0)
    draw.writeByte(0x200000L + 17, 0)
    draw.writeByte(0x200000L + 18, 0)
    draw.writeByte(0x200000L + 19, 0)
    exec(draw, 0x09) // DRAW_TEXT

    // Check that some pixels near the text position are non-black
    var nonBlack = 0
    for x <- 45 until 80 do
      for y <- 30 until 55 do
        val (r, g, b, _) = pixel(fb, x, y)
        if r > 0 || g > 0 || b > 0 then nonBlack += 1
    nonBlack should be > 0
  }

  // ===== Multiple operations =====

  "multiple shapes compose" in {
    val (draw, fb) = mkDraw()
    // White background
    setColor(draw, 255, 255, 255)
    exec(draw, 0x01)

    // Red filled rect
    setColor(draw, 255, 0, 0)
    set16(draw, 2, 0); set16(draw, 4, 0)
    set16(draw, 6, 100); set16(draw, 8, 100)
    exec(draw, 0x10) // FILL_RECT

    // Blue filled rect overlapping
    setColor(draw, 0, 0, 255)
    set16(draw, 2, 50); set16(draw, 4, 50)
    set16(draw, 6, 100); set16(draw, 8, 100)
    exec(draw, 0x10)

    // Top-left corner: red
    val (r1, _, b1, _) = pixel(fb, 10, 10)
    r1 shouldBe 255
    b1 shouldBe 0

    // Bottom-right of overlap: blue
    val (r2, _, b2, _) = pixel(fb, 80, 80)
    r2 shouldBe 0
    b2 shouldBe 255

    // Outside both: white
    val (r3, g3, b3, _) = pixel(fb, 200, 200)
    r3 shouldBe 255
    g3 shouldBe 255
    b3 shouldBe 255
  }
}
