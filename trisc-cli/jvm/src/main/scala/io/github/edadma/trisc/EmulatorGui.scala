package io.github.edadma.trisc

import javax.swing.*
import java.awt.*
import java.awt.event.*

object EmulatorGui:
  def launch(cmd: RunCommand, linked: TOF): Unit =
    val latch = new java.util.concurrent.CountDownLatch(1)

    SwingUtilities.invokeLater(() => {
      val frame = new JFrame("TRISC Emulator")
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

      val terminal = new TerminalEmulator()
      val parser = new ANSIParser(terminal)
      val framebufferWidget = new FramebufferWidget()
      val fb = new FramebufferImage(Runtime.framebufferAddress, Runtime.framebufferMaxSize)

      // Display panel with CardLayout for switching terminal/framebuffer
      val displayPanel = new JPanel(new CardLayout())
      displayPanel.add(terminal, "terminal")
      displayPanel.add(framebufferWidget, "framebuffer")
      frame.getContentPane.add(displayPanel, BorderLayout.CENTER)

      val displayCtrl = new DisplayController(
        Runtime.displayCtrlAddress, terminal, framebufferWidget, fb, displayPanel, frame,
      )

      // Status bar
      val statusBar = new JLabel(" Ready")
      statusBar.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
      statusBar.setBorder(BorderFactory.createEtchedBorder())
      frame.getContentPane.add(statusBar, BorderLayout.SOUTH)

      // Toolbar
      // Mouse capture state (before toolbar so handlers can reference)
      var mouseCaptured = false
      val invisibleCursor = Toolkit.getDefaultToolkit.createCustomCursor(
        new java.awt.image.BufferedImage(1, 1, java.awt.image.BufferedImage.TYPE_INT_ARGB),
        new Point(0, 0), "invisible")

      val toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val runBtn = new JButton("Run")
      val stepBtn = new JButton("Step")
      val resetBtn = new JButton("Reset")
      val mouseBtn = new JButton("Show Host Mouse")
      toolbar.add(runBtn)
      toolbar.add(stepBtn)
      toolbar.add(resetBtn)
      toolbar.add(mouseBtn)
      frame.getContentPane.add(toolbar, BorderLayout.NORTH)

      mouseBtn.addActionListener(_ => {
        mouseCaptured = !mouseCaptured
        if mouseCaptured then
          mouseBtn.setText("Hide Host Mouse")
          framebufferWidget.setCursor(invisibleCursor)
        else
          mouseBtn.setText("Show Host Mouse")
          framebufferWidget.setCursor(Cursor.getDefaultCursor)
        frame.requestFocusInWindow()
      })

      // CPU setup — output feeds the ANSI parser on the EDT
      val outputFn: String => Unit = s =>
        val update: Runnable = () => {
          for b <- s.getBytes("UTF-8") do parser.feed(b & 0xff)
          terminal.repaint()
        }
        if SwingUtilities.isEventDispatchThread then update.run()
        else SwingUtilities.invokeAndWait(update)

      // Blitter and DrawEngine need memory access for reading source data — use a proxy
      // that gets wired to the real Memory after setupCpu creates it
      var memRef: Addressable = null
      val memProxy: Addressable = new Addressable {
        val name = "memProxy"; val base = 0L; val size = 0L
        def readByte(addr: Long): Int = memRef.readByte(addr)
        def writeByte(addr: Long, data: Long): Unit = ()
        def loadByte(addr: Long, data: Long): Unit = ()
        override def readInt(addr: Long): Int = memRef.readInt(addr)
      }
      val blitter = new Blitter(
        Runtime.blitterAddress, memProxy, fb,
        () => displayCtrl.currentFBWidth, () => displayCtrl.currentFBHeight,
      )
      val drawEngine = new DrawEngine(
        Runtime.drawEngineAddress, memProxy, fb,
        () => displayCtrl.currentFBWidth, () => displayCtrl.currentFBHeight,
      )

      // Interrupt controller — shared across setupCpu calls (reset recreates timer internally)
      val intc = new InterruptController(Runtime.intcAddress)
      val keyboard = new KeyboardDevice(Runtime.keyboardAddress, intc, irq = 1)
      val mouse = new MouseDevice(Runtime.mouseAddress, intc, irq = 2)

      val guiDevices = Seq(keyboard, mouse, displayCtrl, fb, blitter, drawEngine)

      var cpuState: (CPU, Memory) = TriscCli.setupCpu(linked, outputFn, guiDevices, intc)
      memRef = cpuState._2
      var cpu = cpuState._1
      if cmd.limit > 0 then cpu.limit = cmd.limit

      // Keyboard input — on the frame, independent of display mode
      frame.addKeyListener(new KeyListener {
        override def keyPressed(e: KeyEvent): Unit =
          keyboard.enqueue(e.getKeyCode, press = true, e.isShiftDown, e.isControlDown, e.isAltDown, e.isMetaDown)
        override def keyReleased(e: KeyEvent): Unit =
          keyboard.enqueue(e.getKeyCode, press = false, e.isShiftDown, e.isControlDown, e.isAltDown, e.isMetaDown)
        override def keyTyped(e: KeyEvent): Unit = ()
      })

      // Mouse input — on the framebuffer widget (always forwarded)
      framebufferWidget.addMouseListener(new MouseAdapter {
        override def mousePressed(e: java.awt.event.MouseEvent): Unit = updateMouse(e)
        override def mouseReleased(e: java.awt.event.MouseEvent): Unit = updateMouse(e)
      })
      framebufferWidget.addMouseMotionListener(new MouseMotionAdapter {
        override def mouseMoved(e: java.awt.event.MouseEvent): Unit = updateMouse(e)
        override def mouseDragged(e: java.awt.event.MouseEvent): Unit = updateMouse(e)
      })

      def updateMouse(e: java.awt.event.MouseEvent): Unit =
        // Scale widget coords to framebuffer coords
        val widgetW = framebufferWidget.getWidth
        val widgetH = framebufferWidget.getHeight
        val fbW = displayCtrl.currentFBWidth
        val fbH = displayCtrl.currentFBHeight
        val mx = if widgetW > 0 then e.getX * fbW / widgetW else e.getX
        val my = if widgetH > 0 then e.getY * fbH / widgetH else e.getY
        val buttons = (if SwingUtilities.isLeftMouseButton(e) then 1 else 0) |
          (if SwingUtilities.isRightMouseButton(e) then 2 else 0) |
          (if SwingUtilities.isMiddleMouseButton(e) then 4 else 0)
        mouse.update(mx, my, buttons)

      def updateStatus(): Unit =
        val pc = f"${cpu.pc}%04X"
        val r1 = cpu.r(1).read
        val cpuState = cpu.state
        val mode = if displayCtrl.currentMode == 0 then "Text" else "FB"
        statusBar.setText(s" PC=$pc  R1=$r1  State=$cpuState  Display=$mode")

      // Run in background thread
      runBtn.addActionListener(_ => {
        runBtn.setEnabled(false)
        stepBtn.setEnabled(false)
        frame.requestFocusInWindow()
        new Thread(() => {
          cpu.run()
          SwingUtilities.invokeLater(() => {
            updateStatus()
            runBtn.setEnabled(true)
            stepBtn.setEnabled(true)
          })
        }).start()
      })

      // Step one instruction
      stepBtn.addActionListener(_ => {
        if cpu.state == State.Run || cpu.state.ordinal < State.Halt.ordinal then
          cpu.execute()
          updateStatus()
      })

      // Reset
      resetBtn.addActionListener(_ => {
        cpu.state = State.Halt // stop the old CPU thread's run() loop
        terminal.clear(Color.GREEN, Color.BLACK)
        parser.reset()
        fb.clear() // clear framebuffer so stale content doesn't flash
        drawEngine.reset() // clear all DrawEngine state (windows, surfaces, buffers)
        val layout = displayPanel.getLayout.asInstanceOf[CardLayout]
        layout.show(displayPanel, "terminal")
        cpuState = TriscCli.setupCpu(linked, outputFn, guiDevices, intc)
        cpu = cpuState._1
        memRef = cpuState._2
        if cmd.limit > 0 then cpu.limit = cmd.limit
        updateStatus()
        frame.pack()
      })

      updateStatus()
      frame.addWindowListener(new WindowAdapter {
        override def windowClosed(e: WindowEvent): Unit = latch.countDown()
      })

      frame.pack()
      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
      frame.requestFocusInWindow()
    })

    latch.await()
