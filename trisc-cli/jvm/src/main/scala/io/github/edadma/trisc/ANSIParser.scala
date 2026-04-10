package io.github.edadma.trisc

import java.awt.Color

class ANSIParser(terminal: TerminalEmulator):
  private enum ParserState:
    case Normal, Escape, CSI, OSC

  private var state: ParserState = ParserState.Normal
  private var params: List[Int] = Nil
  private var currentParam: Int = 0
  private var hasParam: Boolean = false
  private var privateMode: Char = 0 // '?' for DEC private modes

  private var currentFg: Color = Color.GREEN
  private var currentBg: Color = Color.BLACK
  private var bold: Boolean = false
  private var dim: Boolean = false
  private var italic: Boolean = false
  private var underline: Boolean = false
  private var blink: Boolean = false
  private var inverse: Boolean = false
  private var hidden: Boolean = false
  private var strikethrough: Boolean = false

  // Saved cursor state (ESC 7 / ESC 8 or CSI s / CSI u)
  private var savedRow: Int = 0
  private var savedCol: Int = 0
  private var savedFg: Color = Color.GREEN
  private var savedBg: Color = Color.BLACK

  // Scroll region (1-based, inclusive)
  private var scrollTop: Int = 0
  private var scrollBottom: Int = terminal.rows - 1

  private val ansiColors: Array[Color] = Array(
    Color.BLACK,
    new Color(170, 0, 0),     // red
    new Color(0, 170, 0),     // green
    new Color(170, 170, 0),   // yellow
    new Color(0, 0, 170),     // blue
    new Color(170, 0, 170),   // magenta
    new Color(0, 170, 170),   // cyan
    new Color(170, 170, 170), // white
  )

  private val ansiBright: Array[Color] = Array(
    new Color(85, 85, 85),    // bright black
    new Color(255, 85, 85),   // bright red
    new Color(85, 255, 85),   // bright green
    new Color(255, 255, 85),  // bright yellow
    new Color(85, 85, 255),   // bright blue
    new Color(255, 85, 255),  // bright magenta
    new Color(85, 255, 255),  // bright cyan
    Color.WHITE,              // bright white
  )

  def reset(): Unit =
    state = ParserState.Normal
    params = Nil
    currentParam = 0
    hasParam = false
    privateMode = 0
    currentFg = Color.GREEN
    currentBg = Color.BLACK
    bold = false
    dim = false
    italic = false
    underline = false
    blink = false
    inverse = false
    hidden = false
    strikethrough = false
    savedRow = 0
    savedCol = 0
    savedFg = Color.GREEN
    savedBg = Color.BLACK
    scrollTop = 0
    scrollBottom = terminal.rows - 1

  private def effectiveFg: Color = if inverse then currentBg else currentFg
  private def effectiveBg: Color = if inverse then currentFg else currentBg
  private def cellAttrs: Int =
    (if blink then terminal.ATTR_BLINK else 0) |
    (if underline then terminal.ATTR_UNDERLINE else 0)

  def feed(byte: Int): Unit =
    // ESC always interrupts, even mid-sequence
    if byte == 0x1b && state != ParserState.Escape then
      state = ParserState.Escape
      return

    state match
      case ParserState.Normal => feedNormal(byte)
      case ParserState.Escape => feedEscape(byte)
      case ParserState.CSI    => feedCSI(byte)
      case ParserState.OSC    => feedOSC(byte)

  private def feedNormal(byte: Int): Unit =
    byte match
      case 0x1b => state = ParserState.Escape
      case 0x07 => java.awt.Toolkit.getDefaultToolkit.beep()
      case 0x08 => // backspace
        if terminal.cursorCol > 0 then terminal.cursorCol -= 1
      case 0x09 => // tab
        terminal.cursorCol = math.min((terminal.cursorCol + 8) & ~7, terminal.cols - 1)
      case 0x0a | 0x0b | 0x0c => terminal.cursorCol = 0; linefeed() // LF implies CR (standard onlcr mode)
      case 0x0d => terminal.cursorCol = 0
      case 0x0e | 0x0f => () // shift out/in — ignore
      case b if b >= 0x20 =>
        terminal.putChar(terminal.cursorRow, terminal.cursorCol, b.toChar, effectiveFg, effectiveBg, cellAttrs)
        terminal.cursorCol += 1
        if terminal.cursorCol >= terminal.cols then
          terminal.cursorCol = 0
          linefeed()
      case _ => // ignore other control chars

  private def linefeed(): Unit =
    if terminal.cursorRow < scrollBottom then
      terminal.cursorRow += 1
    else
      terminal.scrollRegionUp(scrollTop, scrollBottom, effectiveFg, effectiveBg)

  private def reverseLinefeed(): Unit =
    if terminal.cursorRow > scrollTop then
      terminal.cursorRow -= 1
    else
      terminal.scrollRegionDown(scrollTop, scrollBottom, effectiveFg, effectiveBg)

  private def feedEscape(byte: Int): Unit =
    byte match
      case '[' =>
        state = ParserState.CSI
        params = Nil
        currentParam = 0
        hasParam = false
        privateMode = 0
      case ']' =>
        state = ParserState.OSC
      case '7' => // Save cursor (DECSC)
        saveCursor()
        state = ParserState.Normal
      case '8' => // Restore cursor (DECRC)
        restoreCursor()
        state = ParserState.Normal
      case 'D' => // Index (move down, scroll if at bottom)
        linefeed()
        state = ParserState.Normal
      case 'M' => // Reverse Index (move up, scroll if at top)
        reverseLinefeed()
        state = ParserState.Normal
      case 'E' => // Next Line
        terminal.cursorCol = 0
        linefeed()
        state = ParserState.Normal
      case 'c' => // Reset (RIS)
        reset()
        terminal.clear(Color.GREEN, Color.BLACK)
        state = ParserState.Normal
      case 'H' => // Set tab stop — ignored
        state = ParserState.Normal
      case _ =>
        state = ParserState.Normal

  private def feedCSI(byte: Int): Unit =
    byte match
      case '?' =>
        privateMode = '?'
      case '>' =>
        privateMode = '>'
      case '[' =>
        // '[' is in 0x40..0x7e but is not a meaningful CSI final for normal sequences.
        // Treat as a nested introducer (e.g. malformed ESC [[ …) so the real final
        // (e.g. 'D' for cursor-left) is not fed in Normal state and printed as text.
        params = Nil
        currentParam = 0
        hasParam = false
      case b if b >= '0' && b <= '9' =>
        currentParam = currentParam * 10 + (b - '0')
        hasParam = true
      case ';' =>
        params = params :+ (if hasParam then currentParam else 0)
        currentParam = 0
        hasParam = false
      case b if b >= 0x40 && b <= 0x7e =>
        val finalParams = params :+ (if hasParam then currentParam else 0)
        if privateMode == '?' then
          dispatchDEC(b.toChar, finalParams)
        else
          dispatchCSI(b.toChar, finalParams)
        state = ParserState.Normal
      case _ =>
        state = ParserState.Normal

  private def feedOSC(byte: Int): Unit =
    // Consume until BEL or ST (ESC \)
    if byte == 0x07 then state = ParserState.Normal
    // ESC will be caught by feed() and transition to Escape state

  private def param(ps: List[Int], idx: Int, default: Int): Int =
    if idx < ps.length && ps(idx) != 0 then ps(idx) else default

  private def saveCursor(): Unit =
    savedRow = terminal.cursorRow
    savedCol = terminal.cursorCol
    savedFg = currentFg
    savedBg = currentBg

  private def restoreCursor(): Unit =
    terminal.cursorRow = savedRow
    terminal.cursorCol = savedCol
    currentFg = savedFg
    currentBg = savedBg

  private def dispatchDEC(cmd: Char, ps: List[Int]): Unit =
    cmd match
      case 'h' => // DEC Private Mode Set
        for p <- ps do
          p match
            case 25 => terminal.cursorEnabled = true  // show cursor
            case 7  => () // auto-wrap — already default
            case _  => ()
      case 'l' => // DEC Private Mode Reset
        for p <- ps do
          p match
            case 25 => terminal.cursorEnabled = false // hide cursor
            case 7  => () // disable auto-wrap — ignore
            case _  => ()
      case _ => ()

  private def dispatchCSI(cmd: Char, ps: List[Int]): Unit =
    cmd match
      case 'A' => // Cursor Up
        terminal.cursorRow = math.max(0, terminal.cursorRow - param(ps, 0, 1))
      case 'B' => // Cursor Down
        terminal.cursorRow = math.min(terminal.rows - 1, terminal.cursorRow + param(ps, 0, 1))
      case 'C' => // Cursor Forward
        terminal.cursorCol = math.min(terminal.cols - 1, terminal.cursorCol + param(ps, 0, 1))
      case 'D' => // Cursor Back
        terminal.cursorCol = math.max(0, terminal.cursorCol - param(ps, 0, 1))
      case 'E' => // Cursor Next Line
        terminal.cursorCol = 0
        terminal.cursorRow = math.min(terminal.rows - 1, terminal.cursorRow + param(ps, 0, 1))
      case 'F' => // Cursor Previous Line
        terminal.cursorCol = 0
        terminal.cursorRow = math.max(0, terminal.cursorRow - param(ps, 0, 1))
      case 'G' => // Cursor Horizontal Absolute (1-based)
        terminal.cursorCol = math.max(0, math.min(terminal.cols - 1, param(ps, 0, 1) - 1))
      case 'H' | 'f' => // Cursor Position (1-based)
        terminal.cursorRow = math.max(0, math.min(terminal.rows - 1, param(ps, 0, 1) - 1))
        terminal.cursorCol = math.max(0, math.min(terminal.cols - 1, param(ps, 1, 1) - 1))
      case 'J' => // Erase in Display
        param(ps, 0, 0) match
          case 0 => terminal.clearFrom(terminal.cursorRow, terminal.cursorCol, effectiveFg, effectiveBg)
          case 1 => terminal.clearTo(terminal.cursorRow, terminal.cursorCol, effectiveFg, effectiveBg)
          case 2 | 3 => terminal.clear(effectiveFg, effectiveBg)
          case _ =>
      case 'K' => // Erase in Line
        param(ps, 0, 0) match
          case 0 => terminal.clearLine(terminal.cursorRow, terminal.cursorCol, terminal.cols, effectiveFg, effectiveBg)
          case 1 => terminal.clearLine(terminal.cursorRow, 0, terminal.cursorCol + 1, effectiveFg, effectiveBg)
          case 2 => terminal.clearLine(terminal.cursorRow, 0, terminal.cols, effectiveFg, effectiveBg)
          case _ =>
      case 'S' => // Scroll Up
        for _ <- 0 until param(ps, 0, 1) do
          terminal.scrollRegionUp(scrollTop, scrollBottom, effectiveFg, effectiveBg)
      case 'T' => // Scroll Down
        for _ <- 0 until param(ps, 0, 1) do
          terminal.scrollRegionDown(scrollTop, scrollBottom, effectiveFg, effectiveBg)
      case 'L' => // Insert Lines
        for _ <- 0 until param(ps, 0, 1) do
          terminal.scrollRegionDown(terminal.cursorRow, scrollBottom, effectiveFg, effectiveBg)
      case 'M' => // Delete Lines
        for _ <- 0 until param(ps, 0, 1) do
          terminal.scrollRegionUp(terminal.cursorRow, scrollBottom, effectiveFg, effectiveBg)
      case '@' => // Insert Characters
        terminal.insertChars(terminal.cursorRow, terminal.cursorCol, param(ps, 0, 1))
      case 'P' => // Delete Characters
        terminal.deleteChars(terminal.cursorRow, terminal.cursorCol, param(ps, 0, 1))
      case 'X' => // Erase Characters
        terminal.clearLine(terminal.cursorRow, terminal.cursorCol,
          math.min(terminal.cursorCol + param(ps, 0, 1), terminal.cols), effectiveFg, effectiveBg)
      case 'd' => // Vertical Position Absolute (1-based)
        terminal.cursorRow = math.max(0, math.min(terminal.rows - 1, param(ps, 0, 1) - 1))
      case 'm' => // SGR
        processSGR(ps)
      case 's' => // Save cursor position
        saveCursor()
      case 'u' => // Restore cursor position
        restoreCursor()
      case 'r' => // Set Scrolling Region (DECSTBM, 1-based)
        scrollTop = math.max(0, param(ps, 0, 1) - 1)
        scrollBottom = math.min(terminal.rows - 1, param(ps, 1, terminal.rows) - 1)
        terminal.cursorRow = 0
        terminal.cursorCol = 0
      case 'n' => // Device Status Report — no-op (keyboard decoupled from terminal)
      case 'c' => // Device Attributes — no-op (keyboard decoupled from terminal)
      case _ => // ignore unknown CSI

  private def processSGR(ps: List[Int]): Unit =
    var i = 0
    // SGR with no params means reset
    if ps.isEmpty || (ps.length == 1 && ps.head == 0) then
      resetSGR()
      return

    while i < ps.length do
      ps(i) match
        case 0  => resetSGR()
        case 1  => bold = true
        case 2  => dim = true
        case 3  => italic = true
        case 4  => underline = true
        case 5  => blink = true
        case 7  => inverse = true
        case 8  => hidden = true
        case 9  => strikethrough = true
        case 21 => underline = true // double underline (treat as underline)
        case 22 => bold = false; dim = false
        case 23 => italic = false
        case 24 => underline = false
        case 25 => blink = false
        case 27 => inverse = false
        case 28 => hidden = false
        case 29 => strikethrough = false
        case n if n >= 30 && n <= 37 =>
          currentFg = if bold then ansiBright(n - 30) else ansiColors(n - 30)
        case 38 => // Extended foreground color
          i = parseExtendedColor(ps, i + 1, isFg = true)
        case 39 => currentFg = Color.GREEN
        case n if n >= 40 && n <= 47 =>
          currentBg = ansiColors(n - 40)
        case 48 => // Extended background color
          i = parseExtendedColor(ps, i + 1, isFg = false)
        case 49 => currentBg = Color.BLACK
        case n if n >= 90 && n <= 97 =>
          currentFg = ansiBright(n - 90)
        case n if n >= 100 && n <= 107 =>
          currentBg = ansiBright(n - 100)
        case _ => ()
      i += 1

  private def resetSGR(): Unit =
    bold = false
    dim = false
    italic = false
    underline = false
    blink = false
    inverse = false
    hidden = false
    strikethrough = false
    currentFg = Color.GREEN
    currentBg = Color.BLACK

  // Parse ESC[38;5;n m (256-color) and ESC[38;2;r;g;b m (24-bit color)
  // Returns the new index (will be incremented by the outer loop)
  private def parseExtendedColor(ps: List[Int], idx: Int, isFg: Boolean): Int =
    if idx >= ps.length then return idx
    ps(idx) match
      case 5 => // 256-color: ESC[38;5;n m
        if idx + 1 < ps.length then
          val color = color256(ps(idx + 1))
          if isFg then currentFg = color else currentBg = color
          idx + 1
        else idx
      case 2 => // 24-bit: ESC[38;2;r;g;b m
        if idx + 3 < ps.length then
          val color = new Color(
            math.max(0, math.min(255, ps(idx + 1))),
            math.max(0, math.min(255, ps(idx + 2))),
            math.max(0, math.min(255, ps(idx + 3))),
          )
          if isFg then currentFg = color else currentBg = color
          idx + 3
        else idx
      case _ => idx

  private def color256(n: Int): Color =
    if n < 0 then Color.BLACK
    else if n < 8 then ansiColors(n)
    else if n < 16 then ansiBright(n - 8)
    else if n < 232 then
      // 216-color cube: 6x6x6
      val idx = n - 16
      val r = (idx / 36) * 51
      val g = ((idx / 6) % 6) * 51
      val b = (idx % 6) * 51
      new Color(r, g, b)
    else if n < 256 then
      // Grayscale: 24 shades
      val v = (n - 232) * 10 + 8
      new Color(v, v, v)
    else Color.WHITE
