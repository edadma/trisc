package io.github.edadma.trisc

import io.github.edadma.freetype.*
import io.github.edadma.freetype.extern.LibFreeType.*
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import java.io.{FileWriter, PrintWriter}

object FontGen:
  val FIRST_CHAR = 32  // space
  val LAST_CHAR = 126  // tilde

  // FT_GlyphSlot field offsets (64-bit, empirically determined)
  private val GLYPH_SLOT_BITMAP_TOP = 196

  private def glyphSlot(face: Face): Ptr[Byte] =
    !(face.faceptr.asInstanceOf[Ptr[Byte]] + 152).asInstanceOf[Ptr[Ptr[Byte]]]

  private def bitmapTop(face: Face): Int =
    !(glyphSlot(face) + GLYPH_SLOT_BITMAP_TOP).asInstanceOf[Ptr[CInt]]

  case class GlyphInfo(pixels: Array[Byte], width: Int, height: Int, bearingX: Int, bearingY: Int)

  def main(args: Array[String]): Unit =
    if args.length < 2 then
      System.err.println("Usage: fonts/run <font.ttf> <pixel-size> [output.sysl]")
      System.err.println("  Renders printable ASCII (32-126) and generates sysl source with font data.")
      return

    val fontPath = args(0)
    val pixelSize = args(1).toInt
    val outputPath = if args.length > 2 then args(2) else "font_data.sysl"

    val library = initFreeType match
      case Right(lib) => lib
      case Left(err)  => System.err.println(s"FreeType init failed: $err"); return

    val face = library.newFace(fontPath, 0) match
      case Right(f) => f
      case Left(err) => System.err.println(s"Failed to load font: $err (${errorString(err)})"); return

    face.setPixelSizes(0, pixelSize)

    // Render all glyphs
    val charCount = LAST_CHAR - FIRST_CHAR + 1
    val glyphs = new Array[GlyphInfo](charCount)
    var maxWidth = 0
    var maxAscent = 0  // max distance above baseline
    var maxDescent = 0 // max distance below baseline

    for ch <- FIRST_CHAR to LAST_CHAR do
      val idx = ch - FIRST_CHAR
      face.loadChar(ch.toLong, 0)
      face.renderGlyph(RenderMode.NORMAL)
      val bmp = face.bitmap
      val w = bmp.width
      val h = bmp.rows
      val pitch = bmp.pitch
      val by = bitmapTop(face)

      if w > maxWidth then maxWidth = w
      val ascent = by
      val descent = h - by
      if ascent > maxAscent then maxAscent = ascent
      if descent > maxDescent then maxDescent = descent

      val pixels = new Array[Byte](w * h)
      for row <- 0 until h do
        for col <- 0 until w do
          pixels(row * w + col) = bmp.buffer(row * pitch + col).toByte
      glyphs(idx) = GlyphInfo(pixels, w, h, 0, by)

    val cellW = maxWidth
    val cellH = maxAscent + maxDescent
    System.err.println(s"Font: ${fontPath.split('/').last}, ${pixelSize}px, cell ${cellW}x${cellH}, ascent=$maxAscent, ${charCount} chars")

    // Pack glyphs aligned to baseline
    val totalBytes = charCount * cellW * cellH
    val fontData = new Array[Byte](totalBytes)

    for ch <- FIRST_CHAR to LAST_CHAR do
      val idx = ch - FIRST_CHAR
      val g = glyphs(idx)
      val baseOff = idx * cellW * cellH
      // Position: baseline is at row maxAscent
      // Glyph top is at row (maxAscent - bearingY)
      val offY = maxAscent - g.bearingY
      val offX = 0 // left-aligned
      for row <- 0 until g.height do
        for col <- 0 until g.width do
          val src = g.pixels(row * g.width + col) & 0xff
          if src > 0 then
            val dy = offY + row
            val dx = offX + col
            if dy >= 0 && dy < cellH && dx >= 0 && dx < cellW then
              fontData(baseOff + dy * cellW + dx) = src.toByte

    // Generate sysl source
    val literate = outputPath.endsWith(".lsysl")
    val indent = if literate then "  " else ""
    val dataIndent = if literate then "      " else "    "
    val pw = new PrintWriter(new FileWriter(outputPath))
    try
      if literate then
        pw.println(s"Font data generated from ${fontPath.split('/').last} at ${pixelSize}px.")
        pw.println(s"Cell size: ${cellW}x${cellH}, characters 32-126 (${charCount} glyphs).")
        pw.println(s"Each glyph is ${cellW * cellH} bytes (row-major, 1 byte per pixel, grayscale alpha).")
        pw.println()
      else
        pw.println(s"// Font data: ${fontPath.split('/').last}, ${pixelSize}px, ${cellW}x${cellH} cells, ${charCount} glyphs")

      pw.println(s"${indent}FONT_CELL_W = $cellW")
      pw.println(s"${indent}FONT_CELL_H = $cellH")
      pw.println(s"${indent}FONT_FIRST = $FIRST_CHAR")
      pw.println(s"${indent}FONT_LAST = $LAST_CHAR")
      pw.println(s"${indent}FONT_COUNT = $charCount")
      pw.println()

      pw.print(s"${indent}font_data: [${totalBytes}]byte = [")
      for i <- 0 until totalBytes do
        if i > 0 then pw.print(", ")
        if i % 16 == 0 then
          pw.println()
          pw.print(dataIndent)
        pw.print(fontData(i) & 0xff)
      pw.println()
      pw.println(s"${indent}]")
    finally
      pw.close()

    System.err.println(s"Wrote $outputPath ($totalBytes bytes of font data)")

    face.doneFace
    library.doneFreeType
