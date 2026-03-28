package io.github.edadma.trisc

import javax.swing.*
import java.awt.*
import java.awt.image.BufferedImage
import scala.collection.mutable

class FramebufferWidget extends JComponent:
  private var fbWidth: Int = 320
  private var fbHeight: Int = 200
  private var image: BufferedImage = new BufferedImage(fbWidth, fbHeight, BufferedImage.TYPE_INT_ARGB)
  private var scale: Int = 2

  var fbData: mutable.ArraySeq[Byte] = null // points to RAM backing array

  private val refreshTimer = new Timer(33, _ => repaint()) // ~30fps
  refreshTimer.start()

  def setResolution(w: Int, h: Int): Unit =
    fbWidth = math.max(1, math.min(w, 1920))
    fbHeight = math.max(1, math.min(h, 1080))
    image = new BufferedImage(fbWidth, fbHeight, BufferedImage.TYPE_INT_ARGB)
    // Scale up small resolutions to fill ~960x720, 1:1 for anything larger
    scale = if fbWidth >= 960 || fbHeight >= 720 then 1
            else math.max(1, math.min(960 / fbWidth, 720 / fbHeight))
    revalidate()

  override def getPreferredSize: Dimension =
    new Dimension(fbWidth * scale, fbHeight * scale)

  override def paintComponent(g: Graphics): Unit =
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setColor(Color.BLACK)
    g2.fillRect(0, 0, getWidth, getHeight)

    if fbData == null then return

    val pixels = math.min(fbWidth * fbHeight, fbData.length / 4)
    var i = 0
    while i < pixels do
      val off = i * 4
      val r = fbData(off) & 0xff
      val ga = fbData(off + 1) & 0xff
      val b = fbData(off + 2) & 0xff
      val a = fbData(off + 3) & 0xff
      image.setRGB(i % fbWidth, i / fbWidth, (a << 24) | (r << 16) | (ga << 8) | b)
      i += 1

    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    g2.drawImage(image, 0, 0, fbWidth * scale, fbHeight * scale, null)
