package io.github.edadma.trisc

import java.awt.{BasicStroke, Color, Font, Graphics2D, RenderingHints}
import java.awt.geom.GeneralPath

/**
 * 2D vector drawing device backed by Java Graphics2D.
 * Draws directly into the shared FramebufferImage — no flush needed.
 *
 * Register map (64 bytes):
 *   0:      EXEC     (W)   — write command ID to execute
 *   1:      STATUS   (R)   — 0=idle (synchronous)
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
 *   23-31:  reserved
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
 *   0x0A FLUSH      — no-op (drawing is immediate)
 *   0x0B SET_CLIP   — set clip rectangle to (X1, Y1, X2, Y2)
 *   0x0C CLEAR_CLIP — remove clipping
 *   0x0D CLOSE_PATH — close the current subpath
 *   0x0E CURVE_TO   — quadratic bezier: control=(X1,Y1), endpoint=(X2,Y2)
 *   0x10 FILL_RECT  — fill rectangle at (X1,Y1) size (X2,Y2) (no path needed)
 *   0x11 DRAW_LINE  — draw line from (X1,Y1) to (X2,Y2) (no path needed)
 */
class DrawEngine(
    val base: Long,
    mem: Addressable,
    fb: FramebufferImage,
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
  private val TEXT_BUF = 32

  // Path state (independent of Graphics2D lifecycle)
  private var path = new GeneralPath()

  private def reg16(off: Int): Int = ((regs(off) & 0xFF) << 8) | (regs(off + 1) & 0xFF)
  private def reg32(off: Int): Int =
    ((regs(off) & 0xFF) << 24) | ((regs(off + 1) & 0xFF) << 16) |
    ((regs(off + 2) & 0xFF) << 8) | (regs(off + 3) & 0xFF)

  private def currentColor: Color =
    new Color(regs(RED) & 0xFF, regs(GREEN) & 0xFF, regs(BLUE) & 0xFF, regs(ALPHA) & 0xFF)

  private def applyStroke(g: Graphics2D): Unit =
    val lw = reg16(LINE_W) / 16.0f
    g.setStroke(new BasicStroke(if lw > 0 then lw else 1.0f))

  private def readTextString: String =
    val addr = reg32(TEXT_ADDR)
    if addr != 0 then
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
      val sb = new StringBuilder
      var i = 0
      while i < 32 && regs(TEXT_BUF + i) != 0 do
        sb += (regs(TEXT_BUF + i) & 0xFF).toChar
        i += 1
      sb.toString

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    if off == STATUS then 0
    else if off >= 0 && off < 64 then regs(off) & 0xFF
    else 0

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    if off >= 0 && off < 64 then
      regs(off) = data.toByte
      if off == EXEC then execute(data.toInt & 0xFF)

  private def execute(cmd: Int): Unit =
    val g = fb.g2d
    g.setColor(currentColor)
    applyStroke(g)

    cmd match
      case 0x01 => // CLEAR
        g.fillRect(0, 0, fbWidth(), fbHeight())
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
      case 0x0A => // FLUSH — no-op, drawing is immediate
        ()
      case 0x0B => // SET_CLIP
        g.setClip(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x0C => // CLEAR_CLIP
        g.setClip(null)
      case 0x0D => // CLOSE_PATH
        path.closePath()
      case 0x0E => // CURVE_TO (quadratic bezier)
        path.quadTo(reg16(X1).toFloat, reg16(Y1).toFloat,
                     reg16(X2).toFloat, reg16(Y2).toFloat)
      case 0x10 => // FILL_RECT
        g.fillRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x11 => // DRAW_LINE
        g.drawLine(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case _ =>
