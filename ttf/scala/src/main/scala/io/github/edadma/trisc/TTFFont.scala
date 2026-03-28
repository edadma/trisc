package io.github.edadma.trisc

import java.nio.file.{Files, Path}

class TTFFont private (parser: TTFParser):
  val unitsPerEm: Int = parser.unitsPerEm
  val ascender: Int = parser.ascender
  val descender: Int = parser.descender

  def glyphIndex(char: Char): Int = parser.glyphIndex(char)

  def outline(glyphIndex: Int): GlyphOutline = parser.outline(glyphIndex)

  def metrics(glyphIndex: Int): GlyphMetrics = parser.metrics(glyphIndex)

  def render(char: Char, pixelSize: Double): Bitmap =
    val gid = glyphIndex(char)
    val out = outline(gid)
    val met = metrics(gid)
    Rasterizer.rasterize(out, met, pixelSize, unitsPerEm, ascender)

object TTFFont:
  def fromFile(path: String): TTFFont =
    fromBytes(Files.readAllBytes(Path.of(path)))

  def fromBytes(data: Array[Byte]): TTFFont =
    new TTFFont(new TTFParser(data))
