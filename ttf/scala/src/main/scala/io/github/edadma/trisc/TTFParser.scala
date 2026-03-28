package io.github.edadma.trisc

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable.ArrayBuffer

class TTFParser(data: Array[Byte]):
  private val buf = ByteBuffer.wrap(data).order(ByteOrder.BIG_ENDIAN)

  // Table directory
  private val tables: Map[String, (Int, Int)] = {
    buf.position(4) // skip sfVersion
    val numTables = buf.getShort & 0xffff
    buf.position(12) // skip searchRange, entrySelector, rangeShift
    (0 until numTables).map { _ =>
      val tag = new String(Array(buf.get, buf.get, buf.get, buf.get).map(_.toChar))
      buf.getInt // checksum
      val offset = buf.getInt
      val length = buf.getInt
      tag -> (offset, length)
    }.toMap
  }

  private def tableOffset(tag: String): Int =
    tables.getOrElse(tag, sys.error(s"missing table: $tag"))._1

  private def u16(offset: Int): Int = { buf.position(offset); buf.getShort & 0xffff }
  private def s16(offset: Int): Int = { buf.position(offset); buf.getShort.toInt }
  private def u32(offset: Int): Int = { buf.position(offset); buf.getInt }

  // head table
  private val headOffset = tableOffset("head")
  val unitsPerEm: Int = u16(headOffset + 18)
  val indexToLocFormat: Int = s16(headOffset + 50)

  // maxp table
  val numGlyphs: Int = u16(tableOffset("maxp") + 4)

  // hhea table
  private val hheaOffset = tableOffset("hhea")
  val ascender: Int = s16(hheaOffset + 4)
  val descender: Int = s16(hheaOffset + 6)
  val numOfLongHorMetrics: Int = u16(hheaOffset + 34)

  // hmtx table
  private val hmtxOffset = tableOffset("hmtx")

  def metrics(glyphIndex: Int): GlyphMetrics =
    if glyphIndex < numOfLongHorMetrics then
      val off = hmtxOffset + glyphIndex * 4
      GlyphMetrics(u16(off), s16(off + 2))
    else
      val aw = u16(hmtxOffset + (numOfLongHorMetrics - 1) * 4)
      val lsbOff = hmtxOffset + numOfLongHorMetrics * 4 + (glyphIndex - numOfLongHorMetrics) * 2
      GlyphMetrics(aw, s16(lsbOff))

  // loca table
  private val locaOffset = tableOffset("loca")

  private def glyphOffset(glyphIndex: Int): Int =
    val glyfBase = tableOffset("glyf")
    if indexToLocFormat == 0 then
      glyfBase + u16(locaOffset + glyphIndex * 2) * 2
    else
      glyfBase + u32(locaOffset + glyphIndex * 4)

  private def glyphLength(glyphIndex: Int): Int =
    if indexToLocFormat == 0 then
      (u16(locaOffset + (glyphIndex + 1) * 2) - u16(locaOffset + glyphIndex * 2)) * 2
    else
      u32(locaOffset + (glyphIndex + 1) * 4) - u32(locaOffset + glyphIndex * 4)

  // cmap table — find a usable subtable
  private sealed trait CmapStrategy
  private case class CmapFormat4(offset: Int) extends CmapStrategy
  private case class CmapFormat6(offset: Int) extends CmapStrategy
  private case class CmapFormat12(offset: Int) extends CmapStrategy

  private val cmapStrategy: CmapStrategy = {
    val cmapBase = tableOffset("cmap")
    val numSubtables = u16(cmapBase + 2)
    var best: CmapStrategy = null

    for i <- 0 until numSubtables if best == null do
      val recOff = cmapBase + 4 + i * 8
      val platformID = u16(recOff)
      val encodingID = u16(recOff + 2)
      val subtableOff = cmapBase + u32(recOff + 4)
      val format = u16(subtableOff)

      // Prefer format 12 > 4 > 6, from platform 3 (Windows) or 0 (Unicode)
      if (platformID == 3 || platformID == 0) then
        format match
          case 12 => best = CmapFormat12(subtableOff)
          case 4 if best == null || best.isInstanceOf[CmapFormat6] => best = CmapFormat4(subtableOff)
          case 6 if best == null => best = CmapFormat6(subtableOff)
          case _ =>

    if best == null then
      // Fallback: try any platform
      for i <- 0 until numSubtables if best == null do
        val recOff = cmapBase + 4 + i * 8
        val subtableOff = cmapBase + u32(recOff + 4)
        val format = u16(subtableOff)
        format match
          case 4 => best = CmapFormat4(subtableOff)
          case 6 => best = CmapFormat6(subtableOff)
          case 12 => best = CmapFormat12(subtableOff)
          case _ =>

    if best == null then sys.error("no supported cmap subtable found")
    best
  }

  def glyphIndex(char: Char): Int = cmapStrategy match
    case CmapFormat4(off) => glyphIndexFormat4(char, off)
    case CmapFormat6(off) => glyphIndexFormat6(char, off)
    case CmapFormat12(off) => glyphIndexFormat12(char, off)

  private def glyphIndexFormat4(char: Char, off: Int): Int =
    val c = char.toInt
    val segCount = u16(off + 6) / 2
    val endCodeOff = off + 14
    val startCodeOff = endCodeOff + segCount * 2 + 2
    val idDeltaOff = startCodeOff + segCount * 2
    val idRangeOffsetOff = idDeltaOff + segCount * 2

    var i = 0
    while i < segCount do
      val endCode = u16(endCodeOff + i * 2)
      if c <= endCode then
        val startCode = u16(startCodeOff + i * 2)
        if c < startCode then return 0
        val idRangeOffset = u16(idRangeOffsetOff + i * 2)
        if idRangeOffset == 0 then
          return (c + s16(idDeltaOff + i * 2)) & 0xffff
        else
          val glyphAddr = idRangeOffsetOff + i * 2 + idRangeOffset + (c - startCode) * 2
          val gid = u16(glyphAddr)
          return if gid == 0 then 0 else (gid + s16(idDeltaOff + i * 2)) & 0xffff
      i += 1
    0

  private def glyphIndexFormat6(char: Char, off: Int): Int =
    val c = char.toInt
    val firstCode = u16(off + 6)
    val entryCount = u16(off + 8)
    if c >= firstCode && c < firstCode + entryCount then
      u16(off + 10 + (c - firstCode) * 2)
    else 0

  private def glyphIndexFormat12(char: Char, off: Int): Int =
    val c = char.toInt
    val nGroups = u32(off + 12)
    var i = 0
    while i < nGroups do
      val groupOff = off + 16 + i * 12
      val startCharCode = u32(groupOff)
      val endCharCode = u32(groupOff + 4)
      val startGlyphID = u32(groupOff + 8)
      if c >= startCharCode && c <= endCharCode then
        return startGlyphID + (c - startCharCode)
      i += 1
    0

  // glyf table — parse glyph outline
  def outline(glyphIndex: Int): GlyphOutline =
    if glyphLength(glyphIndex) == 0 then
      return GlyphOutline(Vector.empty, 0, 0, 0, 0)

    val off = glyphOffset(glyphIndex)
    val numberOfContours = s16(off)
    val xMin = s16(off + 2)
    val yMin = s16(off + 4)
    val xMax = s16(off + 6)
    val yMax = s16(off + 8)

    if numberOfContours >= 0 then
      parseSimpleGlyph(off, numberOfContours, xMin, yMin, xMax, yMax)
    else
      parseCompoundGlyph(off, xMin, yMin, xMax, yMax)

  private def parseSimpleGlyph(off: Int, numberOfContours: Int, xMin: Int, yMin: Int, xMax: Int, yMax: Int): GlyphOutline =
    buf.position(off + 10)
    val endPts = (0 until numberOfContours).map(_ => buf.getShort & 0xffff).toArray
    val numPoints = if endPts.isEmpty then 0 else endPts.last + 1

    // Skip instructions
    val instrLen = buf.getShort & 0xffff
    buf.position(buf.position + instrLen)

    // Read flags
    val flags = new Array[Int](numPoints)
    var fi = 0
    while fi < numPoints do
      val f = buf.get & 0xff
      flags(fi) = f
      fi += 1
      if (f & 0x08) != 0 then // repeat
        var repeat = buf.get & 0xff
        while repeat > 0 do
          flags(fi) = f
          fi += 1
          repeat -= 1

    // Read X coordinates (delta-encoded)
    val xs = new Array[Int](numPoints)
    var x = 0
    for i <- 0 until numPoints do
      val f = flags(i)
      if (f & 0x02) != 0 then // 1 byte
        val dx = buf.get & 0xff
        x += (if (f & 0x10) != 0 then dx else -dx)
      else if (f & 0x10) == 0 then // 2 bytes signed
        x += buf.getShort.toInt
      // else: same as previous (delta = 0)
      xs(i) = x

    // Read Y coordinates (delta-encoded)
    val ys = new Array[Int](numPoints)
    var y = 0
    for i <- 0 until numPoints do
      val f = flags(i)
      if (f & 0x04) != 0 then // 1 byte
        val dy = buf.get & 0xff
        y += (if (f & 0x20) != 0 then dy else -dy)
      else if (f & 0x20) == 0 then // 2 bytes signed
        y += buf.getShort.toInt
      ys(i) = y

    // Build contours with implied on-curve points
    val contours = ArrayBuffer[Contour]()
    var start = 0
    for c <- 0 until numberOfContours do
      val end = endPts(c)
      val raw = (start to end).map(i => Point(xs(i), ys(i), (flags(i) & 0x01) != 0)).toVector
      contours += Contour(insertImpliedPoints(raw))
      start = end + 1

    GlyphOutline(contours.toVector, xMin, yMin, xMax, yMax)

  private def insertImpliedPoints(points: Vector[Point]): Vector[Point] =
    if points.length < 2 then return points
    val result = ArrayBuffer[Point]()
    val n = points.length
    for i <- 0 until n do
      val curr = points(i)
      val next = points((i + 1) % n)
      result += curr
      if !curr.onCurve && !next.onCurve then
        result += Point((curr.x + next.x) / 2, (curr.y + next.y) / 2, onCurve = true)
    result.toVector

  private def parseCompoundGlyph(off: Int, xMin: Int, yMin: Int, xMax: Int, yMax: Int): GlyphOutline =
    val allContours = ArrayBuffer[Contour]()
    buf.position(off + 10)

    var more = true
    while more do
      val compFlags = buf.getShort & 0xffff
      val glyphIdx = buf.getShort & 0xffff

      // Read translation
      val (dx, dy) = if (compFlags & 0x01) != 0 then // ARG_1_AND_2_ARE_WORDS
        (buf.getShort.toDouble, buf.getShort.toDouble)
      else
        (buf.get.toDouble, buf.get.toDouble)

      // Read scale
      var (a, b, c, d) = (1.0, 0.0, 0.0, 1.0)
      if (compFlags & 0x08) != 0 then // WE_HAVE_A_SCALE
        val s = buf.getShort.toDouble / 16384.0
        a = s; d = s
      else if (compFlags & 0x40) != 0 then // WE_HAVE_AN_X_AND_Y_SCALE
        a = buf.getShort.toDouble / 16384.0
        d = buf.getShort.toDouble / 16384.0
      else if (compFlags & 0x80) != 0 then // WE_HAVE_A_TWO_BY_TWO
        a = buf.getShort.toDouble / 16384.0
        b = buf.getShort.toDouble / 16384.0
        c = buf.getShort.toDouble / 16384.0
        d = buf.getShort.toDouble / 16384.0

      val component = outline(glyphIdx)
      for contour <- component.contours do
        val transformed = contour.points.map { p =>
          Point(a * p.x + b * p.y + dx, c * p.x + d * p.y + dy, p.onCurve)
        }
        allContours += Contour(transformed)

      more = (compFlags & 0x20) != 0 // MORE_COMPONENTS

    GlyphOutline(allContours.toVector, xMin, yMin, xMax, yMax)
