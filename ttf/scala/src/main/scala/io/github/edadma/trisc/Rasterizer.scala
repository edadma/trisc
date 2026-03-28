package io.github.edadma.trisc

import scala.collection.mutable.ArrayBuffer

object Rasterizer:
  private val AA_SAMPLES = 8 // vertical supersampling for anti-aliasing

  case class Edge(x0: Double, y0: Double, x1: Double, y1: Double, dir: Int) // dir: +1 or -1

  def rasterize(outline: GlyphOutline, metrics: GlyphMetrics, pixelSize: Double, unitsPerEm: Int, ascender: Int): Bitmap =
    val scale = pixelSize / unitsPerEm
    val ascent = (ascender * scale).ceil.toInt

    // Glyph bbox in pixels
    val pxMin = (outline.xMin * scale).floor.toInt
    val pyMin = (outline.yMin * scale).floor.toInt
    val pxMax = (outline.xMax * scale).ceil.toInt
    val pyMax = (outline.yMax * scale).ceil.toInt

    val width = math.max(1, pxMax - pxMin + 2)
    val height = math.max(1, pyMax - pyMin + 2)
    val pixels = new Array[Byte](width * height)

    // Build edge list from flattened outlines
    val edges = ArrayBuffer[Edge]()
    for contour <- outline.contours do
      val pts = contour.points
      if pts.length >= 2 then
        val segments = flattenContour(pts, scale, pxMin, pyMin, ascent)
        for i <- segments.indices do
          val (x0, y0) = segments(i)
          val (x1, y1) = segments((i + 1) % segments.length)
          if y0 != y1 then
            val dir = if y0 < y1 then 1 else -1
            edges += Edge(x0, y0, x1, y1, dir)

    // Scanline fill with supersampling
    for row <- 0 until height do
      var totalAlpha = new Array[Int](width)
      for sub <- 0 until AA_SAMPLES do
        val scanY = row.toDouble + (sub.toDouble + 0.5) / AA_SAMPLES
        val intersections = ArrayBuffer[(Double, Int)]()

        for edge <- edges do
          val (ey0, ey1) = if edge.y0 < edge.y1 then (edge.y0, edge.y1) else (edge.y1, edge.y0)
          if scanY >= ey0 && scanY < ey1 then
            val t = (scanY - edge.y0) / (edge.y1 - edge.y0)
            val ix = edge.x0 + t * (edge.x1 - edge.x0)
            intersections += ((ix, edge.dir))

        val sorted = intersections.sortBy(_._1)
        var winding = 0
        var j = 0
        while j < sorted.length do
          val (x, dir) = sorted(j)
          winding += dir
          if winding != 0 && j + 1 < sorted.length then
            // Find where winding returns to zero
            var k = j + 1
            while k < sorted.length && winding != 0 do
              winding += sorted(k)._2
              k += 1
            val xStart = math.max(0, x.floor.toInt)
            val xEnd = math.min(width - 1, sorted(k - 1)._1.ceil.toInt)
            for col <- xStart to xEnd do
              totalAlpha(col) += 1
            j = k
          else
            j += 1

      for col <- 0 until width do
        val alpha = math.min(255, totalAlpha(col) * 255 / AA_SAMPLES)
        pixels(row * width + col) = alpha.toByte

    val originX = -pxMin
    val originY = ascent - pyMin
    Bitmap(width, height, pixels, originX, originY)

  private def flattenContour(points: Vector[Point], scale: Double, offX: Int, offY: Int, ascent: Int): Vector[(Double, Double)] =
    val result = ArrayBuffer[(Double, Double)]()
    val n = points.length
    var i = 0
    while i < n do
      val curr = points(i)
      val next = points((i + 1) % n)
      if curr.onCurve && next.onCurve then
        result += ((curr.x * scale - offX, ascent - curr.y * scale - offY))
        i += 1
      else if curr.onCurve && !next.onCurve then
        val after = points((i + 2) % n)
        val p0 = (curr.x * scale - offX, ascent - curr.y * scale - offY)
        val p1 = (next.x * scale - offX, ascent - next.y * scale - offY)
        val p2 = (after.x * scale - offX, ascent - after.y * scale - offY)
        flattenBezier(p0, p1, p2, result)
        i += 2
      else
        // off-curve start — shouldn't happen after implied point insertion, but handle gracefully
        result += ((curr.x * scale - offX, ascent - curr.y * scale - offY))
        i += 1
    result.toVector

  private def flattenBezier(
      p0: (Double, Double),
      p1: (Double, Double),
      p2: (Double, Double),
      result: ArrayBuffer[(Double, Double)],
      depth: Int = 0,
  ): Unit =
    // Check if curve is flat enough
    val mx = (p0._1 + p2._1) / 2
    val my = (p0._2 + p2._2) / 2
    val dx = p1._1 - mx
    val dy = p1._2 - my
    if (dx * dx + dy * dy < 0.25 || depth > 10) then // 0.5px flatness threshold
      result += p0
    else
      val q0 = ((p0._1 + p1._1) / 2, (p0._2 + p1._2) / 2)
      val q1 = ((p1._1 + p2._1) / 2, (p1._2 + p2._2) / 2)
      val r = ((q0._1 + q1._1) / 2, (q0._2 + q1._2) / 2)
      flattenBezier(p0, q0, r, result, depth + 1)
      flattenBezier(r, q1, p2, result, depth + 1)
