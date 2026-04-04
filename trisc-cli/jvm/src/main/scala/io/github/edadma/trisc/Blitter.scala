package io.github.edadma.trisc

class Blitter(val base: Long, mem: Addressable, fb: FramebufferImage, fbWidth: () => Int, fbHeight: () => Int)
    extends Device:
  val name = "Blitter"
  val size = 18

  // Register offsets
  private val SRC = 0   // 4 bytes: source address in RAM
  private val DST_X = 4 // 2 bytes: destination X
  private val DST_Y = 6 // 2 bytes: destination Y
  private val W = 8     // 2 bytes: width in pixels
  private val H = 10    // 2 bytes: height in pixels
  private val COLOR = 12 // 4 bytes: RGBA color
  private val OP = 16   // 1 byte: operation (0=alpha blit byte, 1=solid fill, 2=alpha blit int)
  private val GO = 17   // 1 byte: write to execute

  // Register storage
  private val regs = new Array[Byte](18)

  private def reg16(off: Int): Int = ((regs(off) & 0xff) << 8) | (regs(off + 1) & 0xff)
  private def reg32(off: Int): Int =
    ((regs(off) & 0xff) << 24) | ((regs(off + 1) & 0xff) << 16) |
    ((regs(off + 2) & 0xff) << 8) | (regs(off + 3) & 0xff)

  def readByte(addr: Long): Int = regs((addr - base).toInt) & 0xff

  def writeByte(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    regs(off) = data.toByte
    if off == GO then execute()

  override def writeInt(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    regs(off) = (data >> 24).toByte
    regs(off + 1) = (data >> 16).toByte
    regs(off + 2) = (data >> 8).toByte
    regs(off + 3) = data.toByte
    if off <= GO && off + 4 > GO then execute()

  override def writeShort(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    regs(off) = (data >> 8).toByte
    regs(off + 1) = data.toByte
    if off <= GO && off + 2 > GO then execute()

  private inline def argb(r: Int, g: Int, b: Int, a: Int): Int = (a << 24) | (r << 16) | (g << 8) | b

  private def execute(): Unit =
    val op = regs(OP) & 0xff
    val srcAddr = reg32(SRC).toLong & 0xffffffffL
    val dstX = reg16(DST_X)
    val dstY = reg16(DST_Y)
    val w = reg16(W)
    val h = reg16(H)
    val color = reg32(COLOR)
    val cr = (color >> 24) & 0xff
    val cg = (color >> 16) & 0xff
    val cb = (color >> 8) & 0xff
    val ca = color & 0xff

    val fw = fbWidth()
    val fh = fbHeight()
    val pixels = fb.pixels

    op match
      case 0 => alphaBlitByte(srcAddr, dstX, dstY, w, h, cr, cg, cb, fw, fh, pixels)
      case 1 => solidFill(dstX, dstY, w, h, cr, cg, cb, ca, fw, fh, pixels)
      case 2 => alphaBlitInt(srcAddr, dstX, dstY, w, h, cr, cg, cb, fw, fh, pixels)
      case _ => ()

  // Op 0: Alpha blit from byte source (font data as bytes)
  private def alphaBlitByte(src: Long, dx: Int, dy: Int, w: Int, h: Int,
      cr: Int, cg: Int, cb: Int, fw: Int, fh: Int, pixels: Array[Int]): Unit =
    for row <- 0 until h do
      val py = dy + row
      if py >= 0 && py < fh then
        for col <- 0 until w do
          val px = dx + col
          if px >= 0 && px < fw then
            val alpha = mem.readByte(src + row * w + col) & 0xff
            if alpha > 0 then
              val idx = py * fw + px
              if alpha >= 255 then
                pixels(idx) = argb(cr, cg, cb, 255)
              else
                val existing = pixels(idx)
                val er = (existing >> 16) & 0xff
                val eg = (existing >> 8) & 0xff
                val eb = existing & 0xff
                val inv = 255 - alpha
                pixels(idx) = argb(
                  (er * inv + cr * alpha) / 255,
                  (eg * inv + cg * alpha) / 255,
                  (eb * inv + cb * alpha) / 255,
                  255,
                )

  // Op 1: Solid fill rectangle
  private def solidFill(dx: Int, dy: Int, w: Int, h: Int,
      cr: Int, cg: Int, cb: Int, ca: Int, fw: Int, fh: Int, pixels: Array[Int]): Unit =
    val color = argb(cr, cg, cb, ca)
    for row <- 0 until h do
      val py = dy + row
      if py >= 0 && py < fh then
        for col <- 0 until w do
          val px = dx + col
          if px >= 0 && px < fw then
            pixels(py * fw + px) = color

  // Op 2: Alpha blit from int source (font data as ints, 4 bytes per alpha value)
  private def alphaBlitInt(src: Long, dx: Int, dy: Int, w: Int, h: Int,
      cr: Int, cg: Int, cb: Int, fw: Int, fh: Int, pixels: Array[Int]): Unit =
    for row <- 0 until h do
      val py = dy + row
      if py >= 0 && py < fh then
        for col <- 0 until w do
          val px = dx + col
          if px >= 0 && px < fw then
            val alpha = mem.readInt(src + (row * w + col) * 4)
            if alpha > 0 then
              val idx = py * fw + px
              if alpha >= 255 then
                pixels(idx) = argb(cr, cg, cb, 255)
              else
                val existing = pixels(idx)
                val er = (existing >> 16) & 0xff
                val eg = (existing >> 8) & 0xff
                val eb = existing & 0xff
                val inv = 255 - alpha
                pixels(idx) = argb(
                  (er * inv + cr * alpha) / 255,
                  (eg * inv + cg * alpha) / 255,
                  (eb * inv + cb * alpha) / 255,
                  255,
                )
