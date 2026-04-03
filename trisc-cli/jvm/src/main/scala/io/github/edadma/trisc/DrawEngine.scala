package io.github.edadma.trisc

import java.awt.{AlphaComposite, BasicStroke, Color, Font, GradientPaint, LinearGradientPaint, RadialGradientPaint, Graphics2D, MultipleGradientPaint, RenderingHints}
import java.awt.geom.{AffineTransform, GeneralPath, Point2D}
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
 *   96-127: FONT_BUF   (R/W) — 32-byte font name buffer
 *
 * Drawing commands (0x01-0x1F) — draw into TARGET surface:
 *   0x01 CLEAR, 0x02 MOVE_TO, 0x03 LINE_TO, 0x04 STROKE, 0x05 FILL,
 *   0x06 NEW_PATH, 0x07 RECT, 0x08 CIRCLE, 0x09 DRAW_TEXT, 0x0A (no-op),
 *   0x0B SET_CLIP, 0x0C CLEAR_CLIP, 0x0D CLOSE_PATH, 0x0E CURVE_TO,
 *   0x10 FILL_RECT, 0x11 DRAW_LINE, 0x12 DRAW_IMAGE,
 *   0x13 ROUND_RECT, 0x14 FILL_ROUND_RECT, 0x15 STROKE_ROUND_RECT
 *   0x16 SET_FONT — set current font to name in FONT_BUF (e.g. "Monospaced", "SansSerif")
 *   0x17 LOAD_FONT — load TTF/OTF from RAM at TEXT_ADDR, length (X2<<8|Y2 bytes),
 *                      register under name in FONT_BUF
 *   0x18 SET_LINEAR_GRADIENT — gradient from (X1,Y1) to (X2,Y2); color1=RGBA, color2=TEXT_BUF[0-3]
 *   0x19 SET_RADIAL_GRADIENT — center (X1,Y1), radius=X2; color1=RGBA (center), color2=TEXT_BUF[0-3]
 *   0x1A CLEAR_PAINT — revert to solid color (clears gradient)
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
 *   0x36 RESIZE_WINDOW — resize window WIN_ID to (X2, Y2), preserves content
 *   0x37 HIT_TEST — given screen coords (X1, Y1), put window ID in RESULT (0=desktop)
 *                     also sets WIN_FLAGS bit 2 if hit was on title bar
 *
 *   0x3B MINIMIZE_WINDOW — hide window WIN_ID, remember geometry
 *   0x3C MAXIMIZE_WINDOW — fill screen, remember previous geometry
 *   0x3D RESTORE_WINDOW — return window WIN_ID to normal size/position
 *   0x3E SET_SCROLL — set window WIN_ID scroll offset to (X1, Y1), clamped to valid range
 *   0x3F SET_VIEWPORT — set window WIN_ID visible area to (X2, Y2); 0,0 = use full surface
 *
 * Cursor commands (0x38-0x3F):
 *   0x38 SET_CURSOR_POS — set cursor position to (X1, Y1)
 *   0x39 SET_CURSOR_VISIBLE — show cursor if WIN_FLAGS bit 3 set, hide otherwise
 *   0x3A SET_CURSOR_SURFACE — use TARGET surface as cursor image (0=default arrow)
 *
 * Compositor / input commands (0x40-0x4F):
 *   0x40 COMPOSITE — composite all visible windows onto surface 0, then draw cursor
 *   0x41 PROCESS_MOUSE — feed mouse pos (X1,Y1), button state (WIN_FLAGS bit 4=left).
 *                          Handles title bar drag, click-to-focus. RESULT=window under cursor.
 *   0x42 Z_ORDER_QUERY — write visible window IDs into TEXT_BUF, back-to-front, null-terminated
 *   0x43 SET_WALLPAPER_COLOR — solid color wallpaper from RGBA
 *   0x44 SET_WALLPAPER_GRADIENT — vertical gradient, top=RGBA, bottom=TEXT_BUF[0-3]
 *   0x45 SET_WALLPAPER_IMAGE — image from path (TEXT_ADDR/TEXT_BUF string), scaled to fill
 *   0x46 SET_WINDOW_OPACITY — set window WIN_ID opacity from ALPHA register (0=transparent, 255=opaque)
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
  private val FONT_BUF = 96

  // Surface table — slot 0 is the screen framebuffer
  private class Surface(var width: Int, var height: Int):
    var image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    var g2d: Graphics2D = initG2D()

    private def initG2D(): Graphics2D =
      val g = image.createGraphics()
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
      g

    def resize(newW: Int, newH: Int): Unit =
      val oldImage = image
      g2d.dispose()
      width = newW
      height = newH
      image = new BufferedImage(newW, newH, BufferedImage.TYPE_INT_ARGB)
      g2d = initG2D()
      // Preserve existing content (top-left aligned)
      g2d.drawImage(oldImage, 0, 0, null)

  private val MaxSurfaces = 64
  private val surfaces = new Array[Surface](MaxSurfaces)
  // Surface 0 is a proxy for the screen framebuffer (handled specially)

  // Window state
  private object WinState extends Enumeration:
    val Normal, Minimized, Maximized = Value

  // Window table
  private class Window(
    var surfaceId: Int,
    var x: Int,
    var y: Int,
    var title: String,
    var visible: Boolean,
    var decorated: Boolean,
    var state: WinState.Value = WinState.Normal,
    var normalX: Int = 0,
    var normalY: Int = 0,
    var normalW: Int = 0,
    var normalH: Int = 0,
    var scrollX: Int = 0,
    var scrollY: Int = 0,
    var viewW: Int = 0, // 0 = use full surface width
    var viewH: Int = 0, // 0 = use full surface height
    var opacity: Int = 255, // 0=transparent, 255=opaque
  )

  private val MaxWindows = 32
  private val windows = new Array[Window](MaxWindows)
  private val windowOrder = scala.collection.mutable.ArrayBuffer[Int]() // z-order, front last

  // Decoration constants
  private val TitleBarHeight = 32
  private val TitleBarColorFocused = new Color(50, 50, 70, 245)
  private val TitleBarColorUnfocused = new Color(35, 35, 48, 220)
  private val TitleTextColorFocused = new Color(220, 220, 240)
  private val TitleTextColorUnfocused = new Color(140, 140, 160)
  private val BorderColorFocused = new Color(80, 140, 255, 180)
  private val BorderColorUnfocused = new Color(50, 50, 65)
  private val ShadowColor = new Color(0, 0, 0, 80)
  private val CloseColor = new Color(255, 60, 90)
  private val CloseColorDim = new Color(100, 40, 50)
  private val MinimizeColor = new Color(255, 200, 50)
  private val MinimizeColorDim = new Color(100, 80, 30)
  private val MaximizeColor = new Color(50, 255, 120)
  private val MaximizeColorDim = new Color(30, 100, 50)
  private val CornerRadius = 12

  // Font state
  private var currentFontFamily = Font.SANS_SERIF
  private val loadedFonts = scala.collection.mutable.Map[String, Font]()

  // Cursor state
  private var cursorX = 0
  private var cursorY = 0
  private var cursorVisible = true
  private var cursorSurfaceId = 0 // 0 = default arrow

  // Drag state
  private var dragging = false
  private var dragWindowId = 0
  private var dragOffsetX = 0
  private var dragOffsetY = 0
  private var prevMouseButton = false

  // Gradient paint state (null = use solid color)
  private var gradientPaint: java.awt.Paint = null

  // Wallpaper state
  private var wallpaperMode = 0 // 0=none, 1=solid, 2=gradient, 3=image
  private var wallpaperColor: Color = null
  private var wallpaperColor2: Color = null
  private var wallpaperImage: BufferedImage = null

  // Double buffer for flicker-free compositing
  private var backBuffer: BufferedImage = null
  private var backG2D: Graphics2D = null

  private def ensureBackBuffer(w: Int, h: Int): Graphics2D =
    if backBuffer == null || backBuffer.getWidth != w || backBuffer.getHeight != h then
      if backG2D != null then backG2D.dispose()
      backBuffer = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
      backG2D = backBuffer.createGraphics()
      backG2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      backG2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    backG2D

  // Path state
  private var path = new GeneralPath()

  private def reg16(off: Int): Int = ((regs(off) & 0xFF) << 8) | (regs(off + 1) & 0xFF)
  private def reg32(off: Int): Int =
    ((regs(off) & 0xFF) << 24) | ((regs(off + 1) & 0xFF) << 16) |
    ((regs(off + 2) & 0xFF) << 8) | (regs(off + 3) & 0xFF)

  private def currentColor: Color =
    new Color(regs(RED) & 0xFF, regs(GREEN) & 0xFF, regs(BLUE) & 0xFF, regs(ALPHA) & 0xFF)

  /** Read second color from TEXT_BUF[0-3] as R,G,B,A */
  private def secondColor: Color =
    new Color(regs(TEXT_BUF) & 0xFF, regs(TEXT_BUF + 1) & 0xFF, regs(TEXT_BUF + 2) & 0xFF, regs(TEXT_BUF + 3) & 0xFF)

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
      case 0x36 => resizeWindow()
      case 0x37 => hitTest()
      // Cursor commands
      case 0x38 => cursorX = reg16(X1); cursorY = reg16(Y1)
      case 0x39 => cursorVisible = (regs(WIN_FLAGS) & 8) != 0
      case 0x3B => minimizeWindow()
      case 0x3C => maximizeWindow()
      case 0x3D => restoreWindow()
      case 0x3E => setScroll()
      case 0x3F => setViewport()
      // Window opacity
      case 0x46 =>
        val wid = regs(WIN_ID) & 0xFF
        if wid > 0 && wid < MaxWindows && windows(wid) != null then
          windows(wid).opacity = regs(ALPHA) & 0xFF
      case 0x3A => cursorSurfaceId = regs(TARGET) & 0xFF
      // Compositor
      case 0x40 => composite()
      case 0x41 => processMouse()
      case 0x42 => zOrderQuery()
      case 0x43 => setWallpaperColor()
      case 0x44 => setWallpaperGradient()
      case 0x45 => setWallpaperImage()
      case _ =>

  private def executeDraw(cmd: Int): Unit =
    val g = targetG2D
    g.setColor(currentColor)
    if gradientPaint != null then g.setPaint(gradientPaint)
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
        val baseFont = loadedFonts.get(currentFontFamily) match
          case Some(f) => f.deriveFont(style, fontSize.toFloat)
          case None    => new Font(currentFontFamily, style, if fontSize > 0 then fontSize else 12)
        g.setFont(baseFont)
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
      case 0x16 => // SET_FONT
        currentFontFamily = readBuf(FONT_BUF, 32)
      case 0x17 => // LOAD_FONT
        loadFont()
      case 0x18 => // SET_LINEAR_GRADIENT — from (X1,Y1) to (X2,Y2), color1=RGBA, color2=TEXT_BUF[0-3]
        gradientPaint = new GradientPaint(
          reg16(X1).toFloat, reg16(Y1).toFloat, currentColor,
          reg16(X2).toFloat, reg16(Y2).toFloat, secondColor)
      case 0x19 => // SET_RADIAL_GRADIENT — center (X1,Y1), radius=X2, color1=RGBA (center), color2=TEXT_BUF[0-3] (edge)
        val cx = reg16(X1).toFloat; val cy = reg16(Y1).toFloat
        val radius = reg16(X2).toFloat
        if radius > 0 then
          gradientPaint = new RadialGradientPaint(
            cx, cy, radius,
            Array(0.0f, 1.0f),
            Array(currentColor, secondColor))
      case 0x1A => // CLEAR_PAINT — revert to solid color
        gradientPaint = null
      case _ =>

  // === Font commands ===

  private def loadFont(): Unit =
    val addr = reg32(TEXT_ADDR)
    val len = (reg16(X2) << 16) | reg16(Y2)
    val name = readBuf(FONT_BUF, 32)
    if addr == 0 || len <= 0 || name.isEmpty then return
    try
      val data = new Array[Byte](len)
      var i = 0
      while i < len do
        data(i) = mem.readByte(addr.toLong + i).toByte
        i += 1
      val font = Font.createFont(Font.TRUETYPE_FONT, new java.io.ByteArrayInputStream(data))
      loadedFonts(name) = font
    catch case _: Exception => ()

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
      normalX = x, normalY = y, normalW = w, normalH = h,
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

  private def hitTest(): Unit =
    val (found, onTitleBar) = hitTestAt(reg16(X1), reg16(Y1))
    regs(RESULT) = found.toByte
    val flags = regs(WIN_FLAGS) & 0xFF
    if onTitleBar then regs(WIN_FLAGS) = ((flags | 4) & 0xFF).toByte
    else regs(WIN_FLAGS) = ((flags & ~4) & 0xFF).toByte

  private def resizeWindow(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    val newW = reg16(X2)
    val newH = reg16(Y2)
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val win = windows(wid)
      val sid = win.surfaceId
      if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
        surfaces(sid).resize(newW, newH)
        // Update normal geometry if in normal state
        if win.state == WinState.Normal then
          win.normalW = newW
          win.normalH = newH

  private def minimizeWindow(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val win = windows(wid)
      if win.state == WinState.Normal then
        win.normalX = win.x
        win.normalY = win.y
        win.normalW = surfaces(win.surfaceId).width
        win.normalH = surfaces(win.surfaceId).height
      win.visible = false
      win.state = WinState.Minimized

  private def maximizeWindow(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val win = windows(wid)
      val sid = win.surfaceId
      if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
        // Save normal geometry if coming from normal state
        if win.state == WinState.Normal then
          win.normalX = win.x
          win.normalY = win.y
          win.normalW = surfaces(sid).width
          win.normalH = surfaces(sid).height
        // Fill screen (leave room for taskbar at bottom)
        val fw = fbWidth()
        val fh = fbHeight() - 40 // taskbar height
        val contentH = if win.decorated then fh - TitleBarHeight else fh
        win.x = 0
        win.y = 0
        win.visible = true
        win.state = WinState.Maximized
        win.scrollX = 0
        win.scrollY = 0
        win.viewW = 0 // use full surface
        win.viewH = 0
        surfaces(sid).resize(fw, contentH)

  private def restoreWindow(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val win = windows(wid)
      val sid = win.surfaceId
      if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
        win.x = win.normalX
        win.y = win.normalY
        win.visible = true
        win.state = WinState.Normal
        surfaces(sid).resize(win.normalW, win.normalH)

  // === Scroll/Viewport commands ===

  private def setScroll(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val win = windows(wid)
      val sid = win.surfaceId
      if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
        val surf = surfaces(sid)
        val vw = if win.viewW > 0 then win.viewW else surf.width
        val vh = if win.viewH > 0 then win.viewH else surf.height
        // Clamp scroll to valid range
        win.scrollX = math.max(0, math.min(reg16(X1), surf.width - vw))
        win.scrollY = math.max(0, math.min(reg16(Y1), surf.height - vh))

  private def setViewport(): Unit =
    val wid = regs(WIN_ID) & 0xFF
    if wid > 0 && wid < MaxWindows && windows(wid) != null then
      val win = windows(wid)
      win.viewW = reg16(X2)
      win.viewH = reg16(Y2)
      // Clamp scroll if viewport grew
      val sid = win.surfaceId
      if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
        val surf = surfaces(sid)
        val vw = if win.viewW > 0 then win.viewW else surf.width
        val vh = if win.viewH > 0 then win.viewH else surf.height
        win.scrollX = math.max(0, math.min(win.scrollX, surf.width - vw))
        win.scrollY = math.max(0, math.min(win.scrollY, surf.height - vh))

  // === Mouse input / Drag ===

  private def processMouse(): Unit =
    val mx = reg16(X1)
    val my = reg16(Y1)
    val leftDown = (regs(WIN_FLAGS) & 16) != 0 // bit 4
    val justPressed = leftDown && !prevMouseButton
    val justReleased = !leftDown && prevMouseButton
    prevMouseButton = leftDown

    if justReleased && dragging then
      dragging = false
      dragWindowId = 0

    if dragging && leftDown then
      // Move window by tracking mouse
      val win = windows(dragWindowId)
      if win != null then
        win.x = mx - dragOffsetX
        win.y = my - dragOffsetY

    if justPressed then
      // Hit test to find window under cursor
      val (hitWid, onTitleBar) = hitTestAt(mx, my)
      regs(RESULT) = hitWid.toByte
      if hitWid > 0 then
        // Raise (focus) the clicked window
        if windowOrder.contains(hitWid) then
          windowOrder -= hitWid
          windowOrder += hitWid
        if onTitleBar then
          // Begin drag
          val win = windows(hitWid)
          dragging = true
          dragWindowId = hitWid
          dragOffsetX = mx - win.x
          dragOffsetY = my - win.y
      // Set title bar flag
      val flags = regs(WIN_FLAGS) & 0xFF
      if onTitleBar then regs(WIN_FLAGS) = ((flags | 4) & 0xFF).toByte
      else regs(WIN_FLAGS) = ((flags & ~4) & 0xFF).toByte
    else
      // Just report what's under cursor
      val (hitWid, onTitleBar) = hitTestAt(mx, my)
      regs(RESULT) = hitWid.toByte
      val flags = regs(WIN_FLAGS) & 0xFF
      if onTitleBar then regs(WIN_FLAGS) = ((flags | 4) & 0xFF).toByte
      else regs(WIN_FLAGS) = ((flags & ~4) & 0xFF).toByte

  /** Hit test helper — returns (windowId, onTitleBar) */
  private def hitTestAt(mx: Int, my: Int): (Int, Boolean) =
    var i = windowOrder.length - 1
    while i >= 0 do
      val wid = windowOrder(i)
      val win = windows(wid)
      if win != null && win.visible then
        val sid = win.surfaceId
        if sid > 0 && sid < MaxSurfaces && surfaces(sid) != null then
          val surf = surfaces(sid)
          val wx = win.x
          val wy = win.y
          val vw = if win.viewW > 0 then win.viewW else surf.width
          val vh = if win.viewH > 0 then win.viewH else surf.height
          val totalH = if win.decorated then vh + TitleBarHeight else vh
          if mx >= wx && mx < wx + vw && my >= wy && my < wy + totalH then
            return (wid, win.decorated && my < wy + TitleBarHeight)
      i -= 1
    (0, false)

  // === Z-order query ===

  private def zOrderQuery(): Unit =
    // Write window IDs into TEXT_BUF in back-to-front order, null-terminated
    var i = 0
    for wid <- windowOrder if i < 31 do
      if windows(wid) != null && windows(wid).visible then
        regs(TEXT_BUF + i) = wid.toByte
        i += 1
    regs(TEXT_BUF + i) = 0 // null terminator

  // === Wallpaper commands ===

  private def setWallpaperColor(): Unit =
    wallpaperMode = 1
    wallpaperColor = currentColor
    wallpaperImage = null

  private def setWallpaperGradient(): Unit =
    wallpaperMode = 2
    wallpaperColor = currentColor
    wallpaperColor2 = secondColor
    wallpaperImage = null

  private def setWallpaperImage(): Unit =
    val imgPath = readTextString
    try
      val file = new File(imgPath)
      if file.exists then
        val loaded = ImageIO.read(file)
        if loaded != null then
          wallpaperMode = 3
          wallpaperImage = loaded
    catch case _: Exception => ()

  // === Compositor ===

  private def composite(): Unit =
    val fw = fbWidth()
    val fh = fbHeight()
    val g = ensureBackBuffer(fw, fh)

    // Draw wallpaper
    wallpaperMode match
      case 1 => // Solid color
        g.setColor(wallpaperColor)
        g.fillRect(0, 0, fw, fh)
      case 2 => // Vertical gradient
        g.setPaint(new GradientPaint(0, 0, wallpaperColor, 0, fh.toFloat, wallpaperColor2))
        g.fillRect(0, 0, fw, fh)
        g.setPaint(null)
      case 3 => // Image (scaled to fill)
        if wallpaperImage != null then
          g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
          g.drawImage(wallpaperImage, 0, 0, fw, fh, null)
          g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      case _ => // No wallpaper — clear to black
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, fw, fh)

    // Find the focused window (topmost visible decorated window)
    val focusedWid = windowOrder.reverseIterator.find(wid =>
      windows(wid) != null && windows(wid).visible
    ).getOrElse(0)

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
          val focused = wid == focusedWid

          // Viewport dimensions (0 = use full surface)
          val vw = if win.viewW > 0 then win.viewW else ww
          val vh = if win.viewH > 0 then win.viewH else wh
          val sx = win.scrollX
          val sy = win.scrollY

          // Apply per-window opacity
          if win.opacity < 255 then
            g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, win.opacity / 255.0f))

          if win.decorated then
            // Shadow (larger for focused window)
            g.setColor(ShadowColor)
            if focused then
              g.fillRoundRect(wx + 6, wy + 6, vw, vh + TitleBarHeight, CornerRadius, CornerRadius)
            else
              g.fillRoundRect(wx + 3, wy + 3, vw, vh + TitleBarHeight, CornerRadius, CornerRadius)

            // Window frame
            g.setColor(if focused then TitleBarColorFocused else TitleBarColorUnfocused)
            g.fillRoundRect(wx, wy, vw, vh + TitleBarHeight, CornerRadius, CornerRadius)

            // Border
            g.setColor(if focused then BorderColorFocused else BorderColorUnfocused)
            g.setStroke(new BasicStroke(if focused then 1.5f else 1.0f))
            g.drawRoundRect(wx, wy, vw, vh + TitleBarHeight, CornerRadius, CornerRadius)

            // Title text
            g.setColor(if focused then TitleTextColorFocused else TitleTextColorUnfocused)
            g.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 12))
            g.drawString(win.title, wx + 68, wy + 21)

            // Window buttons — vivid when focused, dimmed when not
            g.setColor(if focused then CloseColor else CloseColorDim)
            g.fillOval(wx + 12, wy + 10, 12, 12)
            g.setColor(if focused then MinimizeColor else MinimizeColorDim)
            g.fillOval(wx + 30, wy + 10, 12, 12)
            g.setColor(if focused then MaximizeColor else MaximizeColorDim)
            g.fillOval(wx + 48, wy + 10, 12, 12)

            // Content area — blit visible viewport from surface
            val cy = wy + TitleBarHeight
            g.drawImage(surf.image, wx, cy, wx + vw, cy + vh,
                        sx, sy, sx + vw, sy + vh, null)
          else
            // Undecorated — blit visible viewport
            g.drawImage(surf.image, wx, wy, wx + vw, wy + vh,
                        sx, sy, sx + vw, sy + vh, null)

          // Reset opacity
          if win.opacity < 255 then
            g.setComposite(AlphaComposite.SrcOver)

    // Draw cursor last (always on top)
    if cursorVisible then drawCursor(g)

    // Blit complete frame to display in one operation
    fb.g2d.drawImage(backBuffer, 0, 0, null)

  private def drawCursor(g: Graphics2D): Unit =
    // Custom cursor surface
    if cursorSurfaceId > 0 && cursorSurfaceId < MaxSurfaces && surfaces(cursorSurfaceId) != null then
      g.drawImage(surfaces(cursorSurfaceId).image, cursorX, cursorY, null)
      return

    // Default arrow pointer
    val x = cursorX
    val y = cursorY
    val xpts = Array(x, x, x + 4, x + 6, x + 9, x + 7, x + 12)
    val ypts = Array(y, y + 16, y + 12, y + 18, y + 17, y + 11, y + 11)
    g.setColor(new Color(0, 0, 0, 120))
    g.fillPolygon(xpts.map(_ + 1), ypts.map(_ + 1), 7)
    g.setColor(Color.WHITE)
    g.fillPolygon(xpts, ypts, 7)
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(1.0f))
    g.drawPolygon(xpts, ypts, 7)
