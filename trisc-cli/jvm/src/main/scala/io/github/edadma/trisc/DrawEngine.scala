package io.github.edadma.trisc

import java.awt.{BasicStroke, Color, Font, Graphics2D, RenderingHints}
import java.awt.geom.{AffineTransform, GeneralPath}
import java.awt.image.{BufferedImage, DataBufferByte}
import javax.imageio.ImageIO
import java.io.File

/**
 * 2D vector drawing device backed by Java Graphics2D.
 * Renders into the framebuffer RAM using high-level draw commands.
 *
 * The guest writes commands, coordinates, colors, and text to registers,
 * then writes to the EXEC register to execute. Graphics2D renders into a
 * BufferedImage which is then copied to the framebuffer RAM.
 *
 * Register map (64 bytes):
 *   0:      EXEC     (W)   — write command ID to execute
 *   1:      STATUS   (R)   — 0=idle, 1=busy
 *   2-3:    X1       (R/W) — 16-bit coordinate, big-endian
 *   4-5:    Y1       (R/W) — 16-bit coordinate
 *   6-7:    X2       (R/W) — 16-bit coordinate (for lineTo, rect width, etc.)
 *   8-9:    Y2       (R/W) — 16-bit coordinate (for rect height, etc.)
 *   10:     R        (R/W) — red 0-255
 *   11:     G        (R/W) — green 0-255
 *   12:     B        (R/W) — blue 0-255
 *   13:     A        (R/W) — alpha 0-255
 *   14-15:  LINE_W   (R/W) — line width × 16 (fixed-point, e.g. 32 = 2.0px)
 *   16-19:  TEXT_ADDR (R/W) — RAM address of null-terminated text string
 *   20-21:  FONT_SIZE (R/W) — font size in pixels
 *   22:     FONT_STYLE (R/W) — 0=plain, 1=bold, 2=italic, 3=bold+italic
 *   23:     CORNER_R   (R/W) — corner radius for rounded rectangles
 *   24-31:  reserved
 *   32-63:  TEXT_BUF  (R/W) — 32-byte inline text buffer (alternative to TEXT_ADDR)
 *
 * Commands (write to EXEC):
 *   0x01 CLEAR      — fill entire framebuffer with current color
 *   0x02 MOVE_TO    — set path cursor to (X1, Y1)
 *   0x03 LINE_TO    — draw line from cursor to (X1, Y1), update cursor
 *   0x04 STROKE     — stroke the current path
 *   0x05 FILL       — fill the current path
 *   0x06 NEW_PATH   — clear the current path
 *   0x07 RECT       — add rectangle at (X1, Y1) with size (X2, Y2)
 *   0x08 CIRCLE     — add circle centered at (X1, Y1) with radius X2
 *   0x09 DRAW_TEXT  — draw text at (X1, Y1) from TEXT_ADDR or TEXT_BUF
 *   0x0A FLUSH      — copy rendered image to framebuffer RAM
 *   0x0B SET_CLIP   — set clip rectangle to (X1, Y1, X2, Y2)
 *   0x0C CLEAR_CLIP — remove clipping
 *   0x0D CLOSE_PATH — close the current subpath
 *   0x0E CURVE_TO   — cubic bezier: control1=(X1,Y1), control2=(X2,Y2)
 *                      endpoint loaded from next MOVE_TO or implied
 *   0x0F ARC        — arc centered at (X1,Y1), radius X2, angles in Y2 (packed)
 *   0x10 FILL_RECT  — fill rectangle at (X1,Y1) size (X2,Y2) (no path needed)
 *   0x11 DRAW_LINE  — draw line from (X1,Y1) to (X2,Y2) (no path needed)
 *   0x12 DRAW_IMAGE — load image file (path from TEXT_BUF/TEXT_ADDR), draw at (X1,Y1)
 *                      if X2,Y2 > 0, scale to that size; otherwise draw at native size
 *   0x13 ROUND_RECT — add rounded rect at (X1,Y1) size (X2,Y2) corner radius CORNER_R
 *   0x14 FILL_ROUND_RECT — fill rounded rect (no path needed)
 *   0x15 STROKE_ROUND_RECT — stroke rounded rect (no path needed)
 *
 * @param base     Base address in memory map
 * @param mem      Main memory (for reading text strings)
 * @param fbMemory Framebuffer RAM to render into
 * @param fbWidth  Current framebuffer width
 * @param fbHeight Current framebuffer height
 */
class DrawEngine(
    val base: Long,
    mem: Addressable,
    fbMemory: RAM,
    fbWidth: () => Int,
    fbHeight: () => Int,
) extends Device:
  val name = "DrawEngine"
  val size = 64

  private val regs = new Array[Byte](64)

  // Register offsets
  private val EXEC = 0
  private val STATUS = 1
  private val X1 = 2
  private val Y1 = 4
  private val X2 = 6
  private val Y2 = 8
  private val RED = 10
  private val GREEN = 11
  private val BLUE = 12
  private val ALPHA = 13
  private val LINE_W = 14
  private val TEXT_ADDR = 16
  private val FONT_SIZE = 20
  private val FONT_STYLE = 22
  private val CORNER_R = 23
  private val TEXT_BUF = 32

  // Rendering state
  private var img: BufferedImage = null
  private var g: Graphics2D = null
  private var path = new GeneralPath()
  private var lastW = 0
  private var lastH = 0

  private def reg16(off: Int): Int = ((regs(off) & 0xFF) << 8) | (regs(off + 1) & 0xFF)
  private def reg32(off: Int): Int =
    ((regs(off) & 0xFF) << 24) | ((regs(off + 1) & 0xFF) << 16) |
    ((regs(off + 2) & 0xFF) << 8) | (regs(off + 3) & 0xFF)

  private def ensureImage(): Unit =
    val w = fbWidth()
    val h = fbHeight()
    if img == null || w != lastW || h != lastH then
      if g != null then g.dispose()
      img = new BufferedImage(w, h, BufferedImage.TYPE_4BYTE_ABGR)
      g = img.createGraphics()
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      lastW = w
      lastH = h

  private def currentColor: Color =
    new Color(regs(RED) & 0xFF, regs(GREEN) & 0xFF, regs(BLUE) & 0xFF, regs(ALPHA) & 0xFF)

  private def applyStroke(): Unit =
    val lw = reg16(LINE_W) / 16.0f
    g.setStroke(new BasicStroke(if lw > 0 then lw else 1.0f))

  private def readTextString: String =
    val addr = reg32(TEXT_ADDR)
    if addr != 0 then
      // Read null-terminated string from RAM
      val sb = new StringBuilder
      var i = 0
      var done = false
      while !done && i < 1024 do
        val b = mem.readByte(addr.toLong + i) & 0xFF
        if b == 0 then done = true
        else sb += b.toChar
        i += 1
      sb.toString
    else
      // Read from inline text buffer
      val sb = new StringBuilder
      var i = 0
      while i < 32 && regs(TEXT_BUF + i) != 0 do
        sb += (regs(TEXT_BUF + i) & 0xFF).toChar
        i += 1
      sb.toString

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    if off == STATUS then 0 // always idle (synchronous)
    else if off >= 0 && off < 64 then regs(off) & 0xFF
    else 0

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    if off >= 0 && off < 64 then
      regs(off) = data.toByte
      if off == EXEC then execute(data.toInt & 0xFF)

  private def execute(cmd: Int): Unit =
    ensureImage()
    g.setColor(currentColor)
    applyStroke()

    cmd match
      case 0x01 => // CLEAR
        g.fillRect(0, 0, lastW, lastH)
      case 0x02 => // MOVE_TO
        path.moveTo(reg16(X1).toFloat, reg16(Y1).toFloat)
      case 0x03 => // LINE_TO
        path.lineTo(reg16(X1).toFloat, reg16(Y1).toFloat)
      case 0x04 => // STROKE
        g.draw(path)
      case 0x05 => // FILL
        g.fill(path)
      case 0x06 => // NEW_PATH
        path.reset()
      case 0x07 => // RECT
        path.append(new java.awt.geom.Rectangle2D.Float(
          reg16(X1).toFloat, reg16(Y1).toFloat,
          reg16(X2).toFloat, reg16(Y2).toFloat), false)
      case 0x08 => // CIRCLE
        val cx = reg16(X1)
        val cy = reg16(Y1)
        val r = reg16(X2)
        path.append(new java.awt.geom.Ellipse2D.Float(
          (cx - r).toFloat, (cy - r).toFloat,
          (r * 2).toFloat, (r * 2).toFloat), false)
      case 0x09 => // DRAW_TEXT
        val fontSize = reg16(FONT_SIZE)
        val style = regs(FONT_STYLE) & 0xFF
        g.setFont(new Font(Font.SANS_SERIF, style, if fontSize > 0 then fontSize else 12))
        g.drawString(readTextString, reg16(X1), reg16(Y1))
      case 0x0A => // FLUSH — copy image to framebuffer RAM
        flush()
      case 0x0B => // SET_CLIP
        g.setClip(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x0C => // CLEAR_CLIP
        g.setClip(null)
      case 0x0D => // CLOSE_PATH
        path.closePath()
      case 0x0E => // CURVE_TO (cubic bezier)
        // Control points in X1,Y1 and X2,Y2, endpoint at current cursor
        // For simplicity: quadratic curve with X1,Y1 as control, X2,Y2 as endpoint
        path.quadTo(reg16(X1).toFloat, reg16(Y1).toFloat,
                     reg16(X2).toFloat, reg16(Y2).toFloat)
      case 0x10 => // FILL_RECT (convenience, no path)
        g.fillRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x11 => // DRAW_LINE (convenience, no path)
        g.drawLine(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x12 => // DRAW_IMAGE — load image from file path, draw at (X1, Y1)
        val imgPath = readTextString
        try
          val file = new File(imgPath)
          if file.exists then
            val loaded = ImageIO.read(file)
            if loaded != null then
              val dx = reg16(X1)
              val dy = reg16(Y1)
              val dw = reg16(X2)
              val dh = reg16(Y2)
              g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              if dw > 0 && dh > 0 then
                g.drawImage(loaded, dx, dy, dw, dh, null)
              else
                g.drawImage(loaded, dx, dy, null)
              g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
        catch case _: Exception => ()
      case 0x13 => // ROUND_RECT — add to path
        val cr = (regs(CORNER_R) & 0xFF) * 2
        path.append(new java.awt.geom.RoundRectangle2D.Float(
          reg16(X1).toFloat, reg16(Y1).toFloat,
          reg16(X2).toFloat, reg16(Y2).toFloat,
          cr.toFloat, cr.toFloat), false)
      case 0x14 => // FILL_ROUND_RECT
        val cr = (regs(CORNER_R) & 0xFF) * 2
        g.fillRoundRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2), cr, cr)
      case 0x15 => // STROKE_ROUND_RECT
        val cr = (regs(CORNER_R) & 0xFF) * 2
        g.drawRoundRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2), cr, cr)
      case _ =>

  private def flush(): Unit =
    if img == null then return
    val w = lastW
    val h = lastH
    val fb = fbMemory.bytes
    val raster = img.getRaster
    val pixels = new Array[Int](w * h)
    img.getRGB(0, 0, w, h, pixels, 0, w)
    var i = 0
    while i < pixels.length do
      val argb = pixels(i)
      val off = i * 4
      fb(off) = ((argb >> 16) & 0xFF).toByte     // R
      fb(off + 1) = ((argb >> 8) & 0xFF).toByte  // G
      fb(off + 2) = (argb & 0xFF).toByte          // B
      fb(off + 3) = ((argb >> 24) & 0xFF).toByte  // A
      i += 1
