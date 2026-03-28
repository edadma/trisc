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

      val terminal = new TerminalWidget()
      val parser = new ANSIParser(terminal)
      val framebuffer = new FramebufferWidget()
      val keyboard = new KeyboardDevice(Runtime.keyboardAddress, terminal)
      val fbMemory = new RAM(Runtime.framebufferAddress, Runtime.framebufferMaxSize)

      // Display panel with CardLayout for switching terminal/framebuffer
      val displayPanel = new JPanel(new CardLayout())
      displayPanel.add(terminal, "terminal")
      displayPanel.add(framebuffer, "framebuffer")
      frame.getContentPane.add(displayPanel, BorderLayout.CENTER)

      val displayCtrl = new DisplayController(
        Runtime.displayCtrlAddress, terminal, framebuffer, fbMemory, displayPanel, frame,
      )

      // Status bar
      val statusBar = new JLabel(" Ready")
      statusBar.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12))
      statusBar.setBorder(BorderFactory.createEtchedBorder())
      frame.getContentPane.add(statusBar, BorderLayout.SOUTH)

      // Toolbar
      val toolbar = new JPanel(new FlowLayout(FlowLayout.LEFT))
      val runBtn = new JButton("Run")
      val stepBtn = new JButton("Step")
      val resetBtn = new JButton("Reset")
      toolbar.add(runBtn)
      toolbar.add(stepBtn)
      toolbar.add(resetBtn)
      frame.getContentPane.add(toolbar, BorderLayout.NORTH)

      // CPU setup — output feeds the ANSI parser on the EDT
      val outputFn: String => Unit = s =>
        val update: Runnable = () => {
          for b <- s.getBytes("UTF-8") do parser.feed(b & 0xff)
          terminal.repaint()
        }
        if SwingUtilities.isEventDispatchThread then update.run()
        else SwingUtilities.invokeAndWait(update)

      val guiDevices = Seq(keyboard, displayCtrl, fbMemory)

      var cpuState: (CPU, Memory) = TriscCli.setupCpu(linked, outputFn, guiDevices)
      var cpu = cpuState._1
      if cmd.limit > 0 then cpu.limit = cmd.limit

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
        terminal.clear(Color.GREEN, Color.BLACK)
        parser.reset()
        val layout = displayPanel.getLayout.asInstanceOf[CardLayout]
        layout.show(displayPanel, "terminal")
        cpuState = TriscCli.setupCpu(linked, outputFn, guiDevices)
        cpu = cpuState._1
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
      terminal.requestFocusInWindow()
    })

    latch.await()
