package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TTFTests extends AnyFreeSpec with Matchers:
  // Use a system font for testing
  val fontPath: String =
    val candidates = Seq(
      "/System/Library/Fonts/Helvetica.ttc", // macOS (TTC — won't work yet)
      "/System/Library/Fonts/Monaco.ttf",    // macOS
      "/System/Library/Fonts/Courier.dfont", // macOS
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", // Linux
    )
    // Find a plain .ttf file
    candidates.find(p => java.nio.file.Files.exists(java.nio.file.Path.of(p)) && p.endsWith(".ttf"))
      .getOrElse(sys.error("no test font found"))

  "parse font" in {
    val font = TTFFont.fromFile(fontPath)
    font.unitsPerEm should be > 0
    font.ascender should be > 0
    info(s"unitsPerEm=${font.unitsPerEm} ascender=${font.ascender} descender=${font.descender}")
  }

  "glyph index lookup" in {
    val font = TTFFont.fromFile(fontPath)
    val gid = font.glyphIndex('A')
    gid should be > 0
    info(s"glyphIndex('A') = $gid")
  }

  "parse glyph outline" in {
    val font = TTFFont.fromFile(fontPath)
    val gid = font.glyphIndex('A')
    val out = font.outline(gid)
    out.contours should not be empty
    info(s"'A' has ${out.contours.length} contours, ${out.contours.map(_.points.length).sum} points")
  }

  "render character to bitmap" in {
    val font = TTFFont.fromFile(fontPath)
    val bm = font.render('A', 48)
    bm.width should be > 0
    bm.height should be > 0
    bm.pixels.exists(_ != 0) shouldBe true
    info(s"bitmap: ${bm.width}x${bm.height}, origin=(${bm.originX},${bm.originY})")

    // Print ASCII art
    val sb = new StringBuilder
    for y <- 0 until bm.height do
      for x <- 0 until bm.width do
        val alpha = bm.pixels(y * bm.width + x) & 0xff
        sb.append(if alpha > 180 then '#' else if alpha > 90 then '+' else if alpha > 30 then '.' else ' ')
      sb.append('\n')
    info("\n" + sb.toString)
  }
