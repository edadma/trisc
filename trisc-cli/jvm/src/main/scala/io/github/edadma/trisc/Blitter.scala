package io.github.edadma.trisc

class Blitter(val base: Long, mem: Addressable, fbMemory: RAM, fbWidth: () => Int, fbHeight: () => Int) extends Device:
  val name = "Blitter"
  val size = 18

  // Register offsets
  private val SRC = 0     // 4 bytes: source address in RAM
  private val DST_X = 4   // 2 bytes: destination X
  private val DST_Y = 6   // 2 bytes: destination Y
  private val W = 8        // 2 bytes: width in pixels
  private val H = 10       // 2 bytes: height in pixels
  private val COLOR = 12   // 4 bytes: RGBA color
  private val OP = 16      // 1 byte: operation (0=alpha blit, 1=solid fill, 2=copy)
  private val GO = 17      // 1 byte: write to execute

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

  // Override writeInt for efficient 4-byte register writes from CPU
  override def writeInt(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    regs(off) = (data >> 24).toByte
    regs(off + 1) = (data >> 16).toByte
    regs(off + 2) = (data >> 8).toByte
    regs(off + 3) = data.toByte
    if off <= GO && off + 4 > GO then execute()

  // Override writeShort for efficient 2-byte register writes
  override def writeShort(addr: Long, data: Long): Unit =
    val off = (addr - base).toInt
    regs(off) = (data >> 8).toByte
    regs(off + 1) = data.toByte
    if off <= GO && off + 2 > GO then execute()

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
    val fb = fbMemory.bytes

    System.err.println(s"[Blitter] op=$op src=$srcAddr dst=($dstX,$dstY) size=${w}x$h color=($cr,$cg,$cb,$ca) fb=${fw}x$fh")

    op match
      case 0 => alphaBlitByte(srcAddr, dstX, dstY, w, h, cr, cg, cb, fw, fh, fb)
      case 1 => solidFill(dstX, dstY, w, h, cr, cg, cb, ca, fw, fh, fb)
      case 2 => alphaBlitInt(srcAddr, dstX, dstY, w, h, cr, cg, cb, fw, fh, fb)
      case _ => ()

  // Op 0: Alpha blit from byte source (font data as bytes)
  private def alphaBlitByte(src: Long, dx: Int, dy: Int, w: Int, h: Int,
      cr: Int, cg: Int, cb: Int, fw: Int, fh: Int, fb: scala.collection.mutable.ArraySeq[Byte]): Unit =
    for row <- 0 until h do
      val py = dy + row
      if py >= 0 && py < fh then
        for col <- 0 until w do
          val px = dx + col
          if px >= 0 && px < fw then
            val alpha = mem.readByte(src + row * w + col) & 0xff
            if alpha > 0 then
              val fbOff = (py * fw + px) * 4
              if alpha >= 255 then
                fb(fbOff) = cr.toByte
                fb(fbOff + 1) = cg.toByte
                fb(fbOff + 2) = cb.toByte
                fb(fbOff + 3) = 0xff.toByte
              else
                // Alpha blend against existing pixel
                val er = fb(fbOff) & 0xff
                val eg = fb(fbOff + 1) & 0xff
                val eb = fb(fbOff + 2) & 0xff
                val inv = 255 - alpha
                fb(fbOff) = ((er * inv + cr * alpha) / 255).toByte
                fb(fbOff + 1) = ((eg * inv + cg * alpha) / 255).toByte
                fb(fbOff + 2) = ((eb * inv + cb * alpha) / 255).toByte
                fb(fbOff + 3) = 0xff.toByte

  // Op 1: Solid fill rectangle
  private def solidFill(dx: Int, dy: Int, w: Int, h: Int,
      cr: Int, cg: Int, cb: Int, ca: Int, fw: Int, fh: Int, fb: scala.collection.mutable.ArraySeq[Byte]): Unit =
    for row <- 0 until h do
      val py = dy + row
      if py >= 0 && py < fh then
        for col <- 0 until w do
          val px = dx + col
          if px >= 0 && px < fw then
            val fbOff = (py * fw + px) * 4
            fb(fbOff) = cr.toByte
            fb(fbOff + 1) = cg.toByte
            fb(fbOff + 2) = cb.toByte
            fb(fbOff + 3) = ca.toByte

  // Op 2: Alpha blit from int source (font data as ints, 4 bytes per alpha value)
  private def alphaBlitInt(src: Long, dx: Int, dy: Int, w: Int, h: Int,
      cr: Int, cg: Int, cb: Int, fw: Int, fh: Int, fb: scala.collection.mutable.ArraySeq[Byte]): Unit =
    for row <- 0 until h do
      val py = dy + row
      if py >= 0 && py < fh then
        for col <- 0 until w do
          val px = dx + col
          if px >= 0 && px < fw then
            val alpha = mem.readInt(src + (row * w + col) * 4)
            if alpha > 0 then
              val fbOff = (py * fw + px) * 4
              if alpha >= 255 then
                fb(fbOff) = cr.toByte
                fb(fbOff + 1) = cg.toByte
                fb(fbOff + 2) = cb.toByte
                fb(fbOff + 3) = 0xff.toByte
              else
                val er = fb(fbOff) & 0xff
                val eg = fb(fbOff + 1) & 0xff
                val eb = fb(fbOff + 2) & 0xff
                val inv = 255 - alpha
                fb(fbOff) = ((er * inv + cr * alpha) / 255).toByte
                fb(fbOff + 1) = ((eg * inv + cg * alpha) / 255).toByte
                fb(fbOff + 2) = ((eb * inv + cb * alpha) / 255).toByte
                fb(fbOff + 3) = 0xff.toByte
