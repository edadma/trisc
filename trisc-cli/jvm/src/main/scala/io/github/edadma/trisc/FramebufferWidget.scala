package io.github.edadma.trisc

import javax.swing.*
import java.awt.*
import java.awt.image.BufferedImage
import scala.collection.mutable

class FramebufferWidget extends JComponent:
  private var fbWidth: Int = 320
  private var fbHeight: Int = 200
  private var image: BufferedImage = new BufferedImage(fbWidth, fbHeight, BufferedImage.TYPE_INT_ARGB)
  private var displayWidth: Int = 640
  private var displayHeight: Int = 400

  var fbData: mutable.ArraySeq[Byte] = null // points to RAM backing array

  private val refreshTimer = new Timer(33, _ => repaint()) // ~30fps
  refreshTimer.start()

  def setResolution(w: Int, h: Int): Unit =
    fbWidth = math.max(1, math.min(w, 1920))
    fbHeight = math.max(1, math.min(h, 1080))
    image = new BufferedImage(fbWidth, fbHeight, BufferedImage.TYPE_INT_ARGB)
    // Fit to screen: scale up small resolutions, scale down large ones
    val screen = Toolkit.getDefaultToolkit.getScreenSize
    val maxW = (screen.width * 0.85).toInt
    val maxH = (screen.height * 0.80).toInt
    if fbWidth <= maxW && fbHeight <= maxH then
      // Fits on screen — scale up small resolutions
      val s = math.max(1, math.min(maxW / fbWidth, maxH / fbHeight))
      displayWidth = fbWidth * s
      displayHeight = fbHeight * s
    else
      // Too large — scale down to fit, preserving aspect ratio
      val sx = maxW.toDouble / fbWidth
      val sy = maxH.toDouble / fbHeight
      val s = math.min(sx, sy)
      displayWidth = (fbWidth * s).toInt
      displayHeight = (fbHeight * s).toInt
    revalidate()

  override def getPreferredSize: Dimension =
    new Dimension(displayWidth, displayHeight)

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

    val interp = if displayWidth < fbWidth then RenderingHints.VALUE_INTERPOLATION_BILINEAR
                 else RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, interp)
    g2.drawImage(image, 0, 0, displayWidth, displayHeight, null)
