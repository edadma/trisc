package io.github.edadma.trisc

import javax.swing.*
import java.awt.*

class FramebufferWidget extends JComponent:
  private var fb: FramebufferImage = null
  private var displayWidth: Int = 640
  private var displayHeight: Int = 400

  private val refreshTimer = new Timer(33, _ => repaint()) // ~30fps
  refreshTimer.start()

  def setFramebuffer(f: FramebufferImage): Unit = fb = f

  def setResolution(w: Int, h: Int): Unit =
    // Fit to screen: scale up small resolutions, scale down large ones
    val screen = Toolkit.getDefaultToolkit.getScreenSize
    val maxW = (screen.width * 0.85).toInt
    val maxH = (screen.height * 0.80).toInt
    if w <= maxW && h <= maxH then
      // Fits on screen — scale up small resolutions
      val s = math.max(1, math.min(maxW / w, maxH / h))
      displayWidth = w * s
      displayHeight = h * s
    else
      // Too large — scale down to fit, preserving aspect ratio
      val sx = maxW.toDouble / w
      val sy = maxH.toDouble / h
      val s = math.min(sx, sy)
      displayWidth = (w * s).toInt
      displayHeight = (h * s).toInt
    revalidate()

  override def getPreferredSize: Dimension =
    new Dimension(displayWidth, displayHeight)

  override def paintComponent(g: Graphics): Unit =
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setColor(Color.BLACK)
    g2.fillRect(0, 0, getWidth, getHeight)
    if fb == null then return
    val interp = if displayWidth < fb.width then RenderingHints.VALUE_INTERPOLATION_BILINEAR
                 else RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, interp)
    g2.drawImage(fb.image, 0, 0, displayWidth, displayHeight, null)
