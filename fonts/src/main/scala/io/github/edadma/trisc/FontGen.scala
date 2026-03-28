package io.github.edadma.trisc

import io.github.edadma.freetype.*
import java.io.{FileWriter, PrintWriter}

object FontGen:
  val FIRST_CHAR = 32  // space
  val LAST_CHAR = 126  // tilde

  def main(args: Array[String]): Unit =
    if args.length < 2 then
      System.err.println("Usage: fonts/run <font.ttf> <pixel-size> [output.lsysl]")
      System.err.println("  Renders printable ASCII (32-126) and generates sysl source with font data.")
      return

    val fontPath = args(0)
    val pixelSize = args(1).toInt
    val outputPath = if args.length > 2 then args(2) else "font_data.lsysl"

    val library = initFreeType match
      case Right(lib) => lib
      case Left(err)  => System.err.println(s"FreeType init failed: $err"); return

    val face = library.newFace(fontPath, 0) match
      case Right(f) => f
      case Left(err) => System.err.println(s"Failed to load font: $err (${errorString(err)})"); return

    face.setPixelSizes(0, pixelSize)

    // First pass: render all glyphs to find max dimensions
    val charCount = LAST_CHAR - FIRST_CHAR + 1
    val bitmaps = new Array[Array[Byte]](charCount)
    val widths = new Array[Int](charCount)
    var maxWidth = 0
    var maxHeight = 0

    for ch <- FIRST_CHAR to LAST_CHAR do
      val idx = ch - FIRST_CHAR
      face.loadChar(ch.toLong, 0)
      face.renderGlyph(RenderMode.NORMAL)
      val bmp = face.bitmap
      val w = bmp.width
      val h = bmp.rows
      val pitch = bmp.pitch

      widths(idx) = w
      if w > maxWidth then maxWidth = w
      if h > maxHeight then maxHeight = h

      val pixels = new Array[Byte](w * h)
      for row <- 0 until h do
        for col <- 0 until w do
          pixels(row * w + col) = bmp.buffer(row * pitch + col).toByte
      bitmaps(idx) = pixels

    // Use fixed cell size for simplicity (monospace-friendly)
    val cellW = maxWidth
    val cellH = maxHeight
    System.err.println(s"Font: ${fontPath.split('/').last}, ${pixelSize}px, cell ${cellW}x${cellH}, ${charCount} chars")

    // Second pass: pack into a single byte array (cellW * cellH bytes per char)
    val totalBytes = charCount * cellW * cellH
    val fontData = new Array[Byte](totalBytes)

    for ch <- FIRST_CHAR to LAST_CHAR do
      val idx = ch - FIRST_CHAR
      val pixels = bitmaps(idx)
      val w = widths(idx)
      val h = pixels.length / (if w > 0 then w else 1)
      val baseOff = idx * cellW * cellH
      // Center glyph in cell
      val offX = (cellW - w) / 2
      val offY = (cellH - h) / 2
      for row <- 0 until h do
        for col <- 0 until w do
          val src = pixels(row * w + col) & 0xff
          if src > 0 then
            val dy = offY + row
            val dx = offX + col
            if dy >= 0 && dy < cellH && dx >= 0 && dx < cellW then
              fontData(baseOff + dy * cellW + dx) = src.toByte

    // Generate sysl source
    val pw = new PrintWriter(new FileWriter(outputPath))
    try
      pw.println(s"Font data generated from ${fontPath.split('/').last} at ${pixelSize}px.")
      pw.println(s"Cell size: ${cellW}x${cellH}, characters 32-126 (${charCount} glyphs).")
      pw.println(s"Each glyph is ${cellW * cellH} bytes (row-major, 1 byte per pixel, grayscale alpha).")
      pw.println()
      pw.println(s"  FONT_CELL_W = $cellW")
      pw.println(s"  FONT_CELL_H = $cellH")
      pw.println(s"  FONT_FIRST = $FIRST_CHAR")
      pw.println(s"  FONT_LAST = $LAST_CHAR")
      pw.println(s"  FONT_COUNT = $charCount")
      pw.println()

      // Emit the font data array
      pw.print(s"  font_data: [${totalBytes}]byte = [")
      for i <- 0 until totalBytes do
        if i > 0 then pw.print(", ")
        if i % 16 == 0 then
          pw.println()
          pw.print("      ")
        pw.print(s"0x${"%02x".format(fontData(i) & 0xff)}")
      pw.println()
      pw.println("  ]")
    finally
      pw.close()

    System.err.println(s"Wrote $outputPath ($totalBytes bytes of font data)")

    face.doneFace
    library.doneFreeType
