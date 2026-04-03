package io.github.edadma.trisc

import java.awt.{BasicStroke, Color, Font, Graphics2D, RenderingHints}
import java.awt.geom.{AffineTransform, GeneralPath}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File

/**
 * 2D vector drawing device with surface-based windowing and compositing.
 * Draws via Java Graphics2D. Each surface is a BufferedImage.
 * Surface 0 = screen framebuffer. Surfaces 1+ = window backing stores.
 *
 * Register map (128 bytes):
 *   0:      EXEC       (W)   — write command ID to execute
 *   1:      STATUS     (R)   — 0=idle (synchronous)
 *   2-3:    X1         (R/W) — 16-bit coordinate, big-endian
 *   4-5:    Y1         (R/W) — 16-bit coordinate
 *   6-7:    X2         (R/W) — 16-bit coordinate (width, or second x)
 *   8-9:    Y2         (R/W) — 16-bit coordinate (height, or second y)
 *   10:     R          (R/W) — red 0-255
 *   11:     G          (R/W) — green 0-255
 *   12:     B          (R/W) — blue 0-255
 *   13:     A          (R/W) — alpha 0-255
 *   14-15:  LINE_W     (R/W) — line width × 16 (fixed-point)
 *   16-19:  TEXT_ADDR   (R/W) — RAM address of null-terminated string
 *   20-21:  FONT_SIZE   (R/W) — font size in pixels
 *   22:     FONT_STYLE  (R/W) — 0=plain, 1=bold, 2=italic, 3=bold+italic
 *   23:     CORNER_R    (R/W) — corner radius for rounded rectangles
 *   24:     TARGET      (R/W) — surface ID for draw commands (0=screen)
 *   25:     RESULT      (R)   — result from last command (e.g., new surface/window ID)
 *   26:     WIN_ID      (R/W) — window ID for window commands
 *   27:     WIN_FLAGS   (R/W) — window flags (bit 0: visible, bit 1: decorated)
 *   28-31:  reserved
 *   32-63:  TEXT_BUF    (R/W) — 32-byte inline text buffer
 *   64-95:  TITLE_BUF   (R/W) — 32-byte window title buffer
 *   96-127: reserved
 *
 * Drawing commands (0x01-0x1F) — draw into TARGET surface:
 *   0x01 CLEAR, 0x02 MOVE_TO, 0x03 LINE_TO, 0x04 STROKE, 0x05 FILL,
 *   0x06 NEW_PATH, 0x07 RECT, 0x08 CIRCLE, 0x09 DRAW_TEXT, 0x0A (no-op),
 *   0x0B SET_CLIP, 0x0C CLEAR_CLIP, 0x0D CLOSE_PATH, 0x0E CURVE_TO,
 *   0x10 FILL_RECT, 0x11 DRAW_LINE, 0x12 DRAW_IMAGE,
 *   0x13 ROUND_RECT, 0x14 FILL_ROUND_RECT, 0x15 STROKE_ROUND_RECT
 *
 * Surface commands (0x20-0x2F):
 *   0x20 CREATE_SURFACE — create surface with size (X2, Y2), result in RESULT
 *   0x21 DESTROY_SURFACE — destroy surface TARGET
 *
 * Window commands (0x30-0x3F):
 *   0x30 CREATE_WINDOW — create window: surface from RESULT/TARGET, pos (X1,Y1),
 *                         size (X2,Y2), title from TITLE_BUF, flags from WIN_FLAGS.
 *                         Creates surface automatically. Window ID in RESULT.
 *   0x31 DESTROY_WINDOW — destroy window WIN_ID
 *   0x32 SET_WINDOW_POS — move window WIN_ID to (X1, Y1)
 *   0x33 SET_WINDOW_TITLE — set window WIN_ID title from TITLE_BUF
 *   0x34 SET_WINDOW_FLAGS — set window WIN_ID flags from WIN_FLAGS
 *   0x35 RAISE_WINDOW — bring window WIN_ID to front
 *
 * Compositor commands (0x40-0x4F):
 *   0x40 COMPOSITE — composite all visible windows onto surface 0 (the screen)
 */
class DrawEngine(
    val base: Long,
    mem: Addressable,
    fb: FramebufferImage,
    fbWidth: () => Int,
    fbHeight: () => Int,
) extends Device:
  val name = "DrawEngine"
  val size = 128

  private val regs = new Array[Byte](128)

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
  private val TARGET = 24
  private val RESULT = 25
  private val WIN_ID = 26
  private val WIN_FLAGS = 27
  private val TEXT_BUF = 32
  private val TITLE_BUF = 64

  // Surface table — slot 0 is the screen framebuffer
  private class Surface(val width: Int, val height: Int):
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2d: Graphics2D =
      val g = image.createGraphics()
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g

  private val MaxSurfaces = 64
  private val surfaces = new Array[Surface](MaxSurfaces)
  // Surface 0 is a proxy for the screen framebuffer (handled specially)

  // Window table
  private class Window(
    var surfaceId: Int,
    var x: Int,
    var y: Int,
    var title: String,
    var visible: Boolean,
    var decorated: Boolean,
  )

  private val MaxWindows = 32
  private val windows = new Array[Window](MaxWindows)
  private val windowOrder = scala.collection.mutable.ArrayBuffer[Int]() // z-order, front last

  // Decoration constants
  private val TitleBarHeight = 32
  private val TitleBarColor = new Color(40, 40, 55, 240)
  private val TitleTextColor = new Color(200, 200, 220)
  private val BorderColor = new Color(60, 60, 80)
  private val ShadowColor = new Color(0, 0, 0, 80)
  private val CloseColor = new Color(255, 60, 90)
  private val MinimizeColor = new Color(255, 200, 50)
  private val MaximizeColor = new Color(50, 255, 120)
  private val CornerRadius = 12

  // Path state
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
    else readBuf(TEXT_BUF, 32)

  private def readBuf(offset: Int, maxLen: Int): String =
    val sb = new StringBuilder
    var i = 0
    while i < maxLen && regs(offset + i) != 0 do
      sb += (regs(offset + i) & 0xFF).toChar
      i += 1
    sb.toString

  /** Get the Graphics2D for the current TARGET surface */
  private def targetG2D: Graphics2D =
    val t = regs(TARGET) & 0xFF
    if t == 0 then fb.g2d
    else if t < MaxSurfaces && surfaces(t) != null then surfaces(t).g2d
    else fb.g2d

  /** Get the dimensions of the current TARGET surface */
  private def targetSize: (Int, Int) =
    val t = regs(TARGET) & 0xFF
    if t == 0 then (fbWidth(), fbHeight())
    else if t < MaxSurfaces && surfaces(t) != null then (surfaces(t).width, surfaces(t).height)
    else (fbWidth(), fbHeight())

  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    if off == STATUS then 0
    else if off >= 0 && off < 128 then regs(off) & 0xFF
    else 0

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    if off >= 0 && off < 128 then
      regs(off) = data.toByte
      if off == EXEC then execute(data.toInt & 0xFF)

  private def execute(cmd: Int): Unit =
    cmd match
      // Drawing commands — target the current surface
      case c if c >= 0x01 && c <= 0x1F => executeDraw(c)
      // Surface commands
      case 0x20 => createSurface()
      case 0x21 => destroySurface()
      // Window commands
      case 0x30 => createWindow()
      case 0x31 => destroyWindow()
      case 0x32 => setWindowPos()
      case 0x33 => setWindowTitle()
      case 0x34 => setWindowFlags()
      case 0x35 => raiseWindow()
      // Compositor
      case 0x40 => composite()
      case _ =>

  private def executeDraw(cmd: Int): Unit =
    val g = targetG2D
    g.setColor(currentColor)
    applyStroke(g)

    cmd match
      case 0x01 => // CLEAR
        val (w, h) = targetSize
        g.fillRect(0, 0, w, h)
      case 0x02 => path.moveTo(reg16(X1).toFloat, reg16(Y1).toFloat)
      case 0x03 => path.lineTo(reg16(X1).toFloat, reg16(Y1).toFloat)
      case 0x04 => g.draw(path)
      case 0x05 => g.fill(path)
      case 0x06 => path.reset()
      case 0x07 =>
        path.append(new java.awt.geom.Rectangle2D.Float(
          reg16(X1).toFloat, reg16(Y1).toFloat,
          reg16(X2).toFloat, reg16(Y2).toFloat), false)
      case 0x08 =>
        val cx = reg16(X1); val cy = reg16(Y1); val r = reg16(X2)
        path.append(new java.awt.geom.Ellipse2D.Float(
          (cx - r).toFloat, (cy - r).toFloat, (r * 2).toFloat, (r * 2).toFloat), false)
      case 0x09 =>
        val fontSize = reg16(FONT_SIZE)
        val style = regs(FONT_STYLE) & 0xFF
        g.setFont(new Font(Font.SANS_SERIF, style, if fontSize > 0 then fontSize else 12))
        g.drawString(readTextString, reg16(X1), reg16(Y1))
      case 0x0A => () // no-op
      case 0x0B => g.setClip(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x0C => g.setClip(null)
      case 0x0D => path.closePath()
      case 0x0E =>
        path.quadTo(reg16(X1).toFloat, reg16(Y1).toFloat,
                     reg16(X2).toFloat, reg16(Y2).toFloat)
      case 0x10 => g.fillRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x11 => g.drawLine(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2))
      case 0x12 =>
        val imgPath = readTextString
        try
          val file = new File(imgPath)
          if file.exists then
            val loaded = ImageIO.read(file)
            if loaded != null then
              val dx = reg16(X1); val dy = reg16(Y1)
              val dw = reg16(X2); val dh = reg16(Y2)
              g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
              if dw > 0 && dh > 0 then g.drawImage(loaded, dx, dy, dw, dh, null)
              else g.drawImage(loaded, dx, dy, null)
              g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
        catch case _: Exception => ()
      case 0x13 =>
        val cr = (regs(CORNER_R) & 0xFF) * 2
        path.append(new java.awt.geom.RoundRectangle2D.Float(
          reg16(X1).toFloat, reg16(Y1).toFloat,
          reg16(X2).toFloat, reg16(Y2).toFloat,
          cr.toFloat, cr.toFloat), false)
      case 0x14 =>
        val cr = (regs(CORNER_R) & 0xFF) * 2
        g.fillRoundRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2), cr, cr)
      case 0x15 =>
        val cr = (regs(CORNER_R) & 0xFF) * 2
        g.drawRoundRect(reg16(X1), reg16(Y1), reg16(X2), reg16(Y2), cr, cr)
      case _ =>

  // === Surface commands ===

  private def createSurface(): Unit =
    val w = reg16(X2); val h = reg16(Y2)
    var id = 1
    while id < MaxSurfaces && surfaces(id) != null do id += 1
    if id < MaxSurfaces then
      surfaces(id) = new Surface(w, h)
      regs(RESULT) = id.toByte

  private def destroySurface(): Unit =
    val t = regs(TARGET) & 0xFF
    if t > 0 && t < MaxSurfaces && surfaces(t) != null then
      surfaces(t).g2d.dispose()
      surfaces(t) = null

  // === Window commands ===

  private def createWindow(): Unit =
    val w = reg16(X2); val h = reg16(Y2)
    val x = reg16(X1); val y = reg16(Y1)
    val flags = regs(WIN_FLAGS) & 0xFF
    val title = readBuf(TITLE_BUF, 32)

    // Create backing surface
    var sid = 1
    while sid < MaxSurfaces && surfaces(sid) != null do sid += 1
    if sid >= MaxSurfaces then return
    surfaces(sid) = new Surface(w, h)

    // Create window
    var wid = 1
    while wid < MaxWindows && windows(wid) != null do wid += 1
    if wid >= MaxWindows then
      surfaces(sid).g2d.dispose()
      surfaces(sid) = null
      return

    windows(wid) = new Window(
      surfaceId = sid,
      x = x, y = y,
      title = title,
      visible = (flags & 1) != 0,
      decorated = (flags & 2) != 0,
    )
    windowOrder += wid
    regs(RESULT) = wid.toByte
    // Also set TARGET to the new surface so the caller can draw immediately
    regs(TARGET) = sid.toByte

  private def destroyWindow(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val sid = windows(wid).surfaceId
      if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
        surfaces(sid).g2d.dispose()
        surfaces(sid) = null
      windows(wid) = null
      windowOrder -= wid

  private def setWindowPos(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      windows(wid).x = reg16(X1)
      windows(wid).y = reg16(Y1)

  private def setWindowTitle(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      windows(wid).title = readBuf(TITLE_BUF, 32)

  private def setWindowFlags(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    val flags = regs(WIN_FLAGS) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      windows(wid).visible = (flags & 1) != 0
      windows(wid).decorated = (flags & 2) != 0

  private def raiseWindow(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if windowOrder.contains(wid) then
      windowOrder -= wid
      windowOrder += wid

  // === Compositor ===

  private def composite(): Unit =
    val g = fb.g2d
    val fw = fbWidth()
    val fh = fbHeight()

    // Draw each window in z-order (windowOrder front = last = top)
    for wid <- windowOrder do
      val win = windows(wid)
      if win != null && win.visible then
        val sid = win.surfaceId
        if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
          val surf = surfaces(sid)
          val wx = win.x
          val wy = win.y
          val ww = surf.width
          val wh = surf.height

          if win.decorated then
            // Shadow
            g.setColor(ShadowColor)
            g.fillRoundRect(wx + 4, wy + 4, ww, wh + TitleBarHeight, CornerRadius, CornerRadius)

            // Window frame (title bar + content area)
            g.setColor(TitleBarColor)
            g.fillRoundRect(wx, wy, ww, wh + TitleBarHeight, CornerRadius, CornerRadius)

            // Border
            g.setColor(BorderColor)
            g.setStroke(new BasicStroke(1.0f))
            g.drawRoundRect(wx, wy, ww, wh + TitleBarHeight, CornerRadius, CornerRadius)

            // Title text
            g.setColor(TitleTextColor)
            g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12))
            g.drawString(win.title, wx + 68, wy + 21)

            // Window buttons (close, minimize, maximize — grouped left, macOS style)
            g.setColor(CloseColor)
            g.fillOval(wx + 12, wy + 10, 12, 12)
            g.setColor(MinimizeColor)
            g.fillOval(wx + 30, wy + 10, 12, 12)
            g.setColor(MaximizeColor)
            g.fillOval(wx + 48, wy + 10, 12, 12)

            // Content area — blit the surface below the title bar
            g.drawImage(surf.image, wx, wy + TitleBarHeight, null)
          else
            // Undecorated — just blit
            g.drawImage(surf.image, wx, wy, null)
