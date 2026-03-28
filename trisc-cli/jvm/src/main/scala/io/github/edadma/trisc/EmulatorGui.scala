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
      frame.setSize(720, 480)

      val console = new JTextArea()
      console.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 14))
      console.setBackground(Color.BLACK)
      console.setForeground(Color.GREEN)
      console.setCaretColor(Color.GREEN)
      console.setEditable(false)
      console.setLineWrap(true)

      val scrollPane = new JScrollPane(console)
      frame.getContentPane.add(scrollPane, BorderLayout.CENTER)

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

      // CPU setup — output to console on EDT
      val outputFn: String => Unit = s =>
        SwingUtilities.invokeLater(() => {
          console.append(s)
          console.setCaretPosition(console.getDocument.getLength)
        })

      var cpuState: (CPU, Memory) = TriscCli.setupCpu(linked, outputFn)
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
        console.setText("")
        cpuState = TriscCli.setupCpu(linked, outputFn)
        cpu = cpuState._1
        if cmd.limit > 0 then cpu.limit = cmd.limit
        updateStatus()
      })

      updateStatus()
      frame.addWindowListener(new WindowAdapter {
        override def windowClosed(e: WindowEvent): Unit = latch.countDown()
      })

      frame.setLocationRelativeTo(null)
      frame.setVisible(true)
    })

    latch.await()
