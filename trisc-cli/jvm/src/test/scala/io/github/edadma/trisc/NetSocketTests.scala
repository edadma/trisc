package io.github.edadma.trisc

class NetSocketTests extends TestHelpers {

  def mkNet(maxConns: Int = 8): (NetSocket, InterruptController, Memory) =
    val intc = new InterruptController(0x200)
    val ram = new RAM(0, 0x10000)
    val mem = new Memory("mem", ram)
    val transport = new JvmNetTransport()
    val net = new NetSocket(0x100, mem, intc, irq = 5, transport, maxConns)
    (net, intc, mem)

  /** Set ADDR register */
  def setADDR(n: NetSocket, v: Int): Unit =
    n.writeByte(0x104, (v >> 24) & 0xFF)
    n.writeByte(0x105, (v >> 16) & 0xFF)
    n.writeByte(0x106, (v >> 8) & 0xFF)
    n.writeByte(0x107, v & 0xFF)

  /** Set LENGTH register */
  def setLEN(n: NetSocket, v: Int): Unit =
    n.writeByte(0x108, (v >> 8) & 0xFF)
    n.writeByte(0x109, v & 0xFF)

  /** Set PORT register */
  def setPORT(n: NetSocket, v: Int): Unit =
    n.writeByte(0x10C, (v >> 8) & 0xFF)
    n.writeByte(0x10D, v & 0xFF)

  /** Set IP_ADDR register */
  def setIP(n: NetSocket, a: Int, b: Int, c: Int, d: Int): Unit =
    n.writeByte(0x10E, a)
    n.writeByte(0x10F, b)
    n.writeByte(0x110, c)
    n.writeByte(0x111, d)

  /** Read RESULT register */
  def getResult(n: NetSocket): Int =
    ((n.readByte(0x10A) & 0xFF) << 8) | (n.readByte(0x10B) & 0xFF)

  // ===== Basic properties =====

  "NetSocket has correct size" in {
    val (net, _, _) = mkNet()
    net.size shouldBe 32
  }

  // ===== Loopback: connect to own listener =====

  "listen and connect via loopback" in {
    val (net, _, mem) = mkNet()

    // Listen on port 19100, conn slot 0
    net.writeByte(0x102, 0) // CONN_ID = 0
    setPORT(net, 19100)
    net.writeByte(0x100, 0x01) // CMD_LISTEN
    net.readByte(0x113) shouldBe 0 // no error

    // Connect to localhost:19100, conn slot 1
    net.writeByte(0x102, 1) // CONN_ID = 1
    setIP(net, 127, 0, 0, 1)
    setPORT(net, 19100)
    net.writeByte(0x100, 0x03) // CMD_CONNECT
    net.readByte(0x113) shouldBe 0 // no error

    // Accept on listener (slot 0)
    Thread.sleep(50) // let the OS process the connection
    net.writeByte(0x102, 0) // CONN_ID = 0 (listener)
    net.writeByte(0x100, 0x02) // CMD_ACCEPT
    val acceptedSlot = net.readByte(0x112) // NEW_CONN
    acceptedSlot should be >= 0

    // Send data from client (slot 1)
    val msg = "Hello"
    for i <- msg.indices do
      mem.writeByte(0x1000 + i, msg(i).toByte)
    net.writeByte(0x102, 1) // CONN_ID = 1 (client)
    setADDR(net, 0x1000)
    setLEN(net, msg.length)
    net.writeByte(0x100, 0x04) // CMD_SEND
    getResult(net) shouldBe msg.length

    // Receive on accepted connection
    Thread.sleep(50) // let data arrive
    net.writeByte(0x102, acceptedSlot)
    setADDR(net, 0x2000)
    setLEN(net, 256)
    net.writeByte(0x100, 0x05) // CMD_RECV
    val received = getResult(net)
    received shouldBe msg.length

    val buf = new StringBuilder
    for i <- 0 until received do
      buf += (mem.readByte(0x2000 + i) & 0xFF).toChar
    buf.toString shouldBe "Hello"

    // Close all
    net.closeAll()
  }

  // ===== Error handling =====

  "invalid connection ID sets error" in {
    val (net, _, _) = mkNet()
    net.writeByte(0x102, 99) // CONN_ID = 99 (out of range)
    net.writeByte(0x100, 0x04) // CMD_SEND
    net.readByte(0x113) shouldBe 1 // ERR_INVALID_ID
  }

  "send on unconnected slot sets error" in {
    val (net, _, _) = mkNet()
    net.writeByte(0x102, 0)
    net.writeByte(0x100, 0x04) // CMD_SEND on empty slot
    net.readByte(0x113) shouldBe 3 // ERR_NOT_CONNECTED
  }

  "close clears connection" in {
    val (net, _, _) = mkNet()
    // Listen
    net.writeByte(0x102, 0)
    setPORT(net, 19101)
    net.writeByte(0x100, 0x01) // CMD_LISTEN
    // Verify listening
    net.writeByte(0x102, 0)
    (net.readByte(0x103) & 0x02) should not be 0 // CS_LISTENING
    // Close
    net.writeByte(0x100, 0x06) // CMD_CLOSE
    (net.readByte(0x103) & 0x02) shouldBe 0 // no longer listening
  }

  // ===== Bidirectional communication =====

  "bidirectional send and receive" in {
    val (net, _, mem) = mkNet()

    // Listen on 19102
    net.writeByte(0x102, 0)
    setPORT(net, 19102)
    net.writeByte(0x100, 0x01)

    // Connect
    net.writeByte(0x102, 1)
    setIP(net, 127, 0, 0, 1)
    setPORT(net, 19102)
    net.writeByte(0x100, 0x03)

    Thread.sleep(50)

    // Accept
    net.writeByte(0x102, 0)
    net.writeByte(0x100, 0x02)
    val serverSlot = net.readByte(0x112)
    serverSlot should be >= 0

    // Client sends "ping"
    val ping = "ping"
    for i <- ping.indices do mem.writeByte(0x1000 + i, ping(i).toByte)
    net.writeByte(0x102, 1)
    setADDR(net, 0x1000)
    setLEN(net, ping.length)
    net.writeByte(0x100, 0x04)

    Thread.sleep(50)

    // Server receives
    net.writeByte(0x102, serverSlot)
    setADDR(net, 0x2000)
    setLEN(net, 256)
    net.writeByte(0x100, 0x05)
    val n1 = getResult(net)
    n1 shouldBe 4

    // Server sends "pong"
    val pong = "pong"
    for i <- pong.indices do mem.writeByte(0x3000 + i, pong(i).toByte)
    net.writeByte(0x102, serverSlot)
    setADDR(net, 0x3000)
    setLEN(net, pong.length)
    net.writeByte(0x100, 0x04)

    Thread.sleep(50)

    // Client receives
    net.writeByte(0x102, 1)
    setADDR(net, 0x4000)
    setLEN(net, 256)
    net.writeByte(0x100, 0x05)
    val n2 = getResult(net)
    n2 shouldBe 4

    val resp = new StringBuilder
    for i <- 0 until n2 do resp += (mem.readByte(0x4000 + i) & 0xFF).toChar
    resp.toString shouldBe "pong"

    net.closeAll()
  }

  // ===== Interrupt =====

  "accept interrupt fires when connection pending" in {
    val (net, intc, _) = mkNet()
    net.writeByte(0x114, 0x01) // IER: accept interrupt enable

    // Listen
    net.writeByte(0x102, 0)
    setPORT(net, 19103)
    net.writeByte(0x100, 0x01)

    // Connect from slot 1
    net.writeByte(0x102, 1)
    setIP(net, 127, 0, 0, 1)
    setPORT(net, 19103)
    net.writeByte(0x100, 0x03)

    Thread.sleep(50)

    // Tick should detect pending accept and raise interrupt
    net.apply(null)
    (intc.readByte(0x200) & (1 << 5)) should not be 0

    net.closeAll()
  }

  // ===== PORT/IP readback =====

  "PORT register readback" in {
    val (net, _, _) = mkNet()
    setPORT(net, 8080)
    net.readByte(0x10C) shouldBe 0x1F
    net.readByte(0x10D) shouldBe 0x90
  }

  "IP_ADDR register readback" in {
    val (net, _, _) = mkNet()
    setIP(net, 192, 168, 1, 100)
    net.readByte(0x10E) shouldBe 192
    net.readByte(0x10F) shouldBe 168
    net.readByte(0x110) shouldBe 1
    net.readByte(0x111) shouldBe 100
  }
}
