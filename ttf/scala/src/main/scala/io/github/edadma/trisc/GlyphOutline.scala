package io.github.edadma.trisc

case class Point(x: Double, y: Double, onCurve: Boolean)
case class Contour(points: Vector[Point])
case class GlyphOutline(contours: Vector[Contour], xMin: Int, yMin: Int, xMax: Int, yMax: Int)
case class GlyphMetrics(advanceWidth: Int, leftSideBearing: Int)
case class Bitmap(width: Int, height: Int, pixels: Array[Byte], originX: Int, originY: Int)
