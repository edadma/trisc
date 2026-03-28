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
      val keyboard = new KeyboardDevice(Runtime.keyboardAddress, terminal)

      frame.getContentPane.add(terminal, BorderLayout.CENTER)

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

      var cpuState: (CPU, Memory) = TriscCli.setupCpu(linked, outputFn, Seq(keyboard))
      var cpu = cpuState._1
      if cmd.limit > 0 then cpu.limit = cmd.limit

      def updateStatus(): Unit =
        val pc = f"${cpu.pc}%04X"
        val r1 = cpu.r(1).read
        val cpuState = cpu.state
        statusBar.setText(s" PC=$pc  R1=$r1  State=$cpuState")

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
        cpuState = TriscCli.setupCpu(linked, outputFn, Seq(keyboard))
        cpu = cpuState._1
        if cmd.limit > 0 then cpu.limit = cmd.limit
        updateStatus()
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
