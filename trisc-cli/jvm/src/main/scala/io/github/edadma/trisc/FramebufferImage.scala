package io.github.edadma.trisc

import java.awt.{Graphics2D, RenderingHints}
import java.awt.image.{BufferedImage, DataBufferInt}

class FramebufferImage(val base: Long, val size: Long) extends Addressable:
  val name = "Framebuffer"

  private var _width = 320
  private var _height = 200
  private var _image = new BufferedImage(_width, _height, BufferedImage.TYPE_INT_ARGB)
  private var _pixels = _image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
  private var _g2d = initGraphics()

  def width: Int = _width
  def height: Int = _height
  def image: BufferedImage = _image
  def pixels: Array[Int] = _pixels
  def g2d: Graphics2D = _g2d

  def setResolution(w: Int, h: Int): Unit =
    if _g2d != null then _g2d.dispose()
    _width = math.max(1, math.min(w, 1920))
    _height = math.max(1, math.min(h, 1080))
    _image = new BufferedImage(_width, _height, BufferedImage.TYPE_INT_ARGB)
    _pixels = _image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    _g2d = initGraphics()

  private def initGraphics(): Graphics2D =
    val g = _image.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g

  def clear(): Unit = java.util.Arrays.fill(_pixels, 0)

  def loadByte(addr: Long, data: Long): Unit = writeByte(addr, data)

  // CPU read: RGBA byte order from ARGB int storage
  def readByte(addr: Long): Int =
    val off = (addr - base).toInt
    val pixIdx = off / 4
    if pixIdx < 0 || pixIdx >= _pixels.length then return 0
    val p = _pixels(pixIdx)
    (off & 3) match
      case 0 => (p >> 16) & 0xFF // R
      case 1 => (p >> 8) & 0xFF  // G
      case 2 => p & 0xFF         // B
      case 3 => (p >> 24) & 0xFF // A

  // CPU write: RGBA byte order into ARGB int storage
  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    val pixIdx = off / 4
    if pixIdx < 0 || pixIdx >= _pixels.length then return
    val p = _pixels(pixIdx)
    val v = (data & 0xFF).toInt
    _pixels(pixIdx) = (off & 3) match
      case 0 => (p & 0xFF00FFFF) | (v << 16) // R
      case 1 => (p & 0xFFFF00FF) | (v << 8)  // G
      case 2 => (p & 0xFFFFFF00) | v          // B
      case 3 => (p & 0x00FFFFFF) | (v << 24)  // A

  // Optimized: write full RGBA pixel as one int
  override def writeInt(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    if (off & 3) == 0 then
      val pixIdx = off / 4
      if pixIdx >= 0 && pixIdx < _pixels.length then
        val rgba = data.toInt
        _pixels(pixIdx) = ((rgba & 0xFF) << 24) | ((rgba >> 8) & 0xFFFFFF)
        return
    super.writeInt(addr, data)

  // Optimized: read full pixel as RGBA int
  override def readInt(addr: Long): Int =
    val off = (addr - base).toInt
    if (off & 3) == 0 then
      val pixIdx = off / 4
      if pixIdx >= 0 && pixIdx < _pixels.length then
        val p = _pixels(pixIdx)
        return ((p << 8) & 0xFFFFFF00) | ((p >> 24) & 0xFF)
    super.readInt(addr)
