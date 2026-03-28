package io.github.edadma.trisc

import javax.swing.*
import java.awt.*
import java.awt.event.*
import java.util.concurrent.ConcurrentLinkedQueue

class TerminalWidget(val cols: Int = 80, val rows: Int = 24) extends JComponent with KeyListener:
  private val cells = Array.fill(rows * cols)(' ')
  private val fgColors = Array.fill(rows * cols)(Color.GREEN)
  private val bgColors = Array.fill(rows * cols)(Color.BLACK)
  private val attrs = Array.fill(rows * cols)(0) // bit flags: 1=blink

  var cursorRow: Int = 0
  var cursorCol: Int = 0
  val ATTR_BLINK = 1
  val ATTR_UNDERLINE = 2

  var cursorEnabled: Boolean = true
  private var cursorVisible: Boolean = true
  private var blinkPhase: Boolean = true // true = visible

  private val monoFont = new Font(Font.MONOSPACED, Font.PLAIN, 14)
  private var charWidth: Int = 0
  private var charHeight: Int = 0
  private var ascent: Int = 0

  private val keyQueue = new ConcurrentLinkedQueue[Int]

  setFocusable(true)
  addKeyListener(this)
  setBackground(Color.BLACK)

  private val blinkTimer = new Timer(500, _ => {
    cursorVisible = !cursorVisible
    blinkPhase = !blinkPhase
    repaint()
  })
  blinkTimer.start()

  override def addNotify(): Unit =
    super.addNotify()
    val fm = getFontMetrics(monoFont)
    charWidth = fm.charWidth('M')
    charHeight = fm.getHeight
    ascent = fm.getAscent

  override def getPreferredSize: Dimension =
    if charWidth == 0 then new Dimension(80 * 8, 24 * 16)
    else new Dimension(cols * charWidth, rows * charHeight)

  private def idx(row: Int, col: Int): Int = row * cols + col

  private def repaintCell(row: Int, col: Int): Unit =
    if charWidth > 0 then
      repaint(col * charWidth, row * charHeight, charWidth, charHeight)

  def putChar(row: Int, col: Int, ch: Char, fg: Color, bg: Color, cellAttrs: Int = 0): Unit =
    val i = idx(row, col)
    cells(i) = ch
    fgColors(i) = fg
    bgColors(i) = bg
    attrs(i) = cellAttrs

  def getChar(row: Int, col: Int): Char = cells(idx(row, col))

  def scrollUp(): Unit =
    scrollRegionUp(0, rows - 1, Color.GREEN, Color.BLACK)

  def scrollRegionUp(top: Int, bottom: Int, fg: Color, bg: Color): Unit =
    val srcStart = idx(top + 1, 0)
    val dstStart = idx(top, 0)
    val len = (bottom - top) * cols
    System.arraycopy(cells, srcStart, cells, dstStart, len)
    System.arraycopy(fgColors, srcStart, fgColors, dstStart, len)
    System.arraycopy(bgColors, srcStart, bgColors, dstStart, len)
    System.arraycopy(attrs, srcStart, attrs, dstStart, len)
    val clearStart = idx(bottom, 0)
    for i <- clearStart until clearStart + cols do
      cells(i) = ' '
      fgColors(i) = fg
      bgColors(i) = bg
      attrs(i) = 0
    repaint()

  def scrollRegionDown(top: Int, bottom: Int, fg: Color, bg: Color): Unit =
    val srcStart = idx(top, 0)
    val dstStart = idx(top + 1, 0)
    val len = (bottom - top) * cols
    System.arraycopy(cells, srcStart, cells, dstStart, len)
    System.arraycopy(fgColors, srcStart, fgColors, dstStart, len)
    System.arraycopy(bgColors, srcStart, bgColors, dstStart, len)
    System.arraycopy(attrs, srcStart, attrs, dstStart, len)
    val clearStart = idx(top, 0)
    for i <- clearStart until clearStart + cols do
      cells(i) = ' '
      fgColors(i) = fg
      bgColors(i) = bg
      attrs(i) = 0
    repaint()

  def insertChars(row: Int, col: Int, count: Int): Unit =
    val rowStart = idx(row, 0)
    val src = idx(row, col)
    val dst = idx(row, math.min(col + count, cols))
    val len = math.max(0, cols - col - count)
    if len > 0 then
      System.arraycopy(cells, src, cells, dst, len)
      System.arraycopy(fgColors, src, fgColors, dst, len)
      System.arraycopy(bgColors, src, bgColors, dst, len)
      System.arraycopy(attrs, src, attrs, dst, len)
    for i <- src until math.min(src + count, rowStart + cols) do
      cells(i) = ' '
      fgColors(i) = Color.GREEN
      bgColors(i) = Color.BLACK
      attrs(i) = 0
    repaint()

  def deleteChars(row: Int, col: Int, count: Int): Unit =
    val rowStart = idx(row, 0)
    val rowEnd = rowStart + cols
    val src = idx(row, math.min(col + count, cols))
    val dst = idx(row, col)
    val len = math.max(0, cols - col - count)
    if len > 0 then
      System.arraycopy(cells, src, cells, dst, len)
      System.arraycopy(fgColors, src, fgColors, dst, len)
      System.arraycopy(bgColors, src, bgColors, dst, len)
      System.arraycopy(attrs, src, attrs, dst, len)
    for i <- (dst + len) until rowEnd do
      cells(i) = ' '
      fgColors(i) = Color.GREEN
      bgColors(i) = Color.BLACK
      attrs(i) = 0
    repaint()

  def clear(fg: Color, bg: Color): Unit =
    for i <- cells.indices do
      cells(i) = ' '
      fgColors(i) = fg
      bgColors(i) = bg
      attrs(i) = 0
    cursorRow = 0
    cursorCol = 0
    repaint()

  def clearFrom(row: Int, col: Int, fg: Color, bg: Color): Unit =
    var i = idx(row, col)
    while i < cells.length do
      cells(i) = ' '
      fgColors(i) = fg
      bgColors(i) = bg
      attrs(i) = 0
      i += 1
    repaint()

  def clearTo(row: Int, col: Int, fg: Color, bg: Color): Unit =
    val end = idx(row, col)
    var i = 0
    while i <= end && i < cells.length do
      cells(i) = ' '
      fgColors(i) = fg
      bgColors(i) = bg
      attrs(i) = 0
      i += 1
    repaint()

  def clearLine(row: Int, fromCol: Int, toCol: Int, fg: Color, bg: Color): Unit =
    for c <- fromCol until toCol do
      val i = idx(row, c)
      cells(i) = ' '
      fgColors(i) = fg
      bgColors(i) = bg
      attrs(i) = 0
    repaint()

  def hasKey: Boolean = !keyQueue.isEmpty

  def readKey(): Option[Int] = Option(keyQueue.poll())

  def enqueueKey(byte: Int): Unit = keyQueue.add(byte)

  override def paintComponent(g: Graphics): Unit =
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setFont(monoFont)
    g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)

    g2.setColor(Color.BLACK)
    g2.fillRect(0, 0, getWidth, getHeight)

    if charWidth == 0 then return

    for row <- 0 until rows do
      for col <- 0 until cols do
        val i = idx(row, col)
        val x = col * charWidth
        val y = row * charHeight

        val isBlinking = (attrs(i) & ATTR_BLINK) != 0
        val visible = !isBlinking || blinkPhase

        if bgColors(i) != Color.BLACK then
          g2.setColor(bgColors(i))
          g2.fillRect(x, y, charWidth, charHeight)

        val ch = cells(i)
        if ch != ' ' && visible then
          g2.setColor(fgColors(i))
          g2.drawString(ch.toString, x, y + ascent)

        if visible && (attrs(i) & ATTR_UNDERLINE) != 0 then
          g2.setColor(fgColors(i))
          g2.fillRect(x, y + ascent + 1, charWidth, 1)

    // Cursor
    if cursorEnabled && cursorVisible then
      g2.setColor(Color.GREEN)
      g2.fillRect(cursorCol * charWidth, cursorRow * charHeight + ascent + 1, charWidth, 2)

  // KeyListener
  override def keyTyped(e: KeyEvent): Unit =
    val ch = e.getKeyChar
    if ch != KeyEvent.CHAR_UNDEFINED then keyQueue.add(ch.toInt)

  override def keyPressed(e: KeyEvent): Unit =
    e.getKeyCode match
      case KeyEvent.VK_UP =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('A'.toInt)
      case KeyEvent.VK_DOWN =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('B'.toInt)
      case KeyEvent.VK_RIGHT =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('C'.toInt)
      case KeyEvent.VK_LEFT =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('D'.toInt)
      case KeyEvent.VK_HOME =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('H'.toInt)
      case KeyEvent.VK_END =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('F'.toInt)
      case KeyEvent.VK_INSERT =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('2'.toInt); keyQueue.add('~'.toInt)
      case KeyEvent.VK_DELETE =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('3'.toInt); keyQueue.add('~'.toInt)
      case KeyEvent.VK_PAGE_UP =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('5'.toInt); keyQueue.add('~'.toInt)
      case KeyEvent.VK_PAGE_DOWN =>
        keyQueue.add(0x1b); keyQueue.add('['.toInt); keyQueue.add('6'.toInt); keyQueue.add('~'.toInt)
      case kc if kc >= KeyEvent.VK_F1 && kc <= KeyEvent.VK_F12 =>
        val fn = kc - KeyEvent.VK_F1
        val fnCodes = Array("OP", "OQ", "OR", "OS", "[15~", "[17~", "[18~", "[19~", "[20~", "[21~", "[23~", "[24~")
        keyQueue.add(0x1b)
        for ch <- fnCodes(fn) do keyQueue.add(ch.toInt)
      case _ =>

  override def keyReleased(e: KeyEvent): Unit = ()
