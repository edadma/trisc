package io.github.edadma.trisc

import javax.swing.*
import java.awt.*

class DisplayController(
    val base: Long,
    terminal: TerminalWidget,
    framebuffer: FramebufferWidget,
    fbMemory: RAM,
    displayPanel: JPanel,
    frame: JFrame,
) extends Device:
  val name = "DisplayController"
  val size = 6

  private val MODE = 0
  private val COMMIT = 1
  private val WIDTH_HI = 2
  private val WIDTH_LO = 3
  private val HEIGHT_HI = 4
  private val HEIGHT_LO = 5

  private var mode: Int = 0 // 0=text, 1=framebuffer
  private var widthHi: Int = 0
  private var widthLo: Int = 80
  private var heightHi: Int = 0
  private var heightLo: Int = 24

  // Wire up framebuffer data on creation
  framebuffer.fbData = fbMemory.bytes

  private def width: Int = (widthHi << 8) | widthLo
  private def height: Int = (heightHi << 8) | heightLo

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case MODE      => mode
      case WIDTH_HI  => widthHi
      case WIDTH_LO  => widthLo
      case HEIGHT_HI => heightHi
      case HEIGHT_LO => heightLo
      case _         => 0

  def writeByte(addr: Long, data: Long): Unit =
    val v = (data & 0xff).toInt
    (addr - base).toInt match
      case MODE      => mode = v & 1
      case COMMIT    => commit()
      case WIDTH_HI  => widthHi = v
      case WIDTH_LO  => widthLo = v
      case HEIGHT_HI => heightHi = v
      case HEIGHT_LO => heightLo = v
      case _         =>

  private def commit(): Unit =
    val w = width
    val h = height
    val update: Runnable = () => {
      val layout = displayPanel.getLayout.asInstanceOf[CardLayout]
      if mode == 0 then
        terminal.setResolution(w, h)
        terminal.clear(java.awt.Color.GREEN, java.awt.Color.BLACK)
        layout.show(displayPanel, "terminal")
      else
        fbMemory.clear()
        framebuffer.setResolution(w, h)
        layout.show(displayPanel, "framebuffer")
      frame.pack()
      frame.setLocationRelativeTo(null)
    }
    if SwingUtilities.isEventDispatchThread then update.run()
    else SwingUtilities.invokeLater(update)

  def currentMode: Int = mode
