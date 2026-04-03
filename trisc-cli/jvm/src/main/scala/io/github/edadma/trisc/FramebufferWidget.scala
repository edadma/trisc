package io.github.edadma.trisc

import javax.swing.*
import java.awt.*

class FramebufferWidget extends JComponent:
  private var fb: FramebufferImage = null
  private var scale: Int = 2

  private val refreshTimer = new Timer(33, _ => repaint()) // ~30fps
  refreshTimer.start()

  def setFramebuffer(f: FramebufferImage): Unit = fb = f

  def setResolution(w: Int, h: Int): Unit =
    scale = if w >= 960 || h >= 720 then 1
            else math.max(1, math.min(960 / w, 720 / h))
    revalidate()

  override def getPreferredSize: Dimension =
    if fb == null then new Dimension(640, 400)
    else new Dimension(fb.width * scale, fb.height * scale)

  override def paintComponent(g: Graphics): Unit =
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setColor(Color.BLACK)
    g2.fillRect(0, 0, getWidth, getHeight)
    if fb == null then return
    g2.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    g2.drawImage(fb.image, 0, 0, fb.width * scale, fb.height * scale, null)
