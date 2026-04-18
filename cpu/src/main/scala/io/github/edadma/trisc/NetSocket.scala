package io.github.edadma.trisc

import scala.collection.mutable

/**
 * Network socket device supporting multiple concurrent TCP and UDP connections.
 * The device manages a connection table; the guest OS issues commands to
 * listen, connect, accept, send, receive, close, bind, sendto, and recvfrom.
 *
 * All actual network I/O is delegated to a NetTransport implementation
 * provided by the host environment. The device is platform-agnostic.
 *
 * Register map (32 bytes):
 *   0:     COMMAND     (W)   — write to execute command
 *   1:     STATUS      (R)   — global status bits
 *   2:     CONN_ID     (R/W) — connection ID for commands (0-based)
 *   3:     CONN_STATUS (R)   — status of selected connection
 *   4-7:   ADDR        (R/W) — RAM address for send/recv DMA, big-endian
 *   8-9:   LENGTH      (R/W) — byte count for send/recv, big-endian
 *   10-11: RESULT      (R)   — result of last operation (bytes sent/received), big-endian
 *   12-13: PORT        (R/W) — port number for listen/connect/bind, big-endian
 *   14-17: IP_ADDR     (R/W) — IPv4 address for connect/sendto, big-endian (A.B.C.D)
 *   18:    NEW_CONN    (R)   — connection ID from last accept (-1 if none)
 *   19:    ERROR       (R)   — error code from last operation
 *   20:    IER         (R/W) — interrupt enable: bit 0=accept, bit 1=recv ready, bit 2=disconnect
 *   21:    SOCK_TYPE   (R/W) — socket type: 0=TCP (default), 1=UDP
 *   22-23: REMOTE_PORT (R)   — source port from last recvfrom, big-endian
 *   24-27: REMOTE_IP   (R)   — source IPv4 from last recvfrom, big-endian
 *   28-31: reserved
 *
 * TCP Commands:
 *   0x01 LISTEN  — start listening on PORT; CONN_ID = listener slot
 *   0x02 ACCEPT  — accept pending connection on listener CONN_ID; new ID in NEW_CONN
 *   0x03 CONNECT — connect to IP_ADDR:PORT; CONN_ID = slot to use
 *   0x04 SEND    — send LENGTH bytes from RAM[ADDR] on CONN_ID; RESULT = bytes sent
 *   0x05 RECV    — receive up to LENGTH bytes into RAM[ADDR] on CONN_ID; RESULT = bytes received
 *   0x06 CLOSE   — close CONN_ID
 *   0x07 STATUS  — refresh CONN_STATUS for CONN_ID
 *
 * UDP Commands:
 *   0x08 BIND    — bind a UDP socket to local PORT; CONN_ID = slot to use
 *   0x09 SENDTO  — send LENGTH bytes from RAM[ADDR] to IP_ADDR:PORT; CONN_ID = UDP socket
 *   0x0A RECVFROM — receive up to LENGTH bytes into RAM[ADDR]; source in REMOTE_IP:REMOTE_PORT
 *
 * Connection status bits:
 *   bit 0: connected (TCP) or bound (UDP)
 *   bit 1: listening (TCP only)
 *   bit 2: has pending accept (TCP only)
 *   bit 3: has data available
 *   bit 4: error
 *   bit 5: closed by remote (TCP only)
 *   bit 6: UDP socket
 *
 * Error codes:
 *   0 = no error
 *   1 = invalid connection ID
 *   2 = connection refused/failed
 *   3 = not connected/bound
 *   4 = connection table full
 *   5 = wrong socket type
 *
 * @param base      Base address in memory map
 * @param mem       Main memory for DMA
 * @param intc      Interrupt controller
 * @param irq       IRQ line for socket events
 * @param transport Host-provided network transport
 * @param maxConns  Maximum concurrent connections (default 8)
 */
class NetSocket(
    val base: Long,
    mem: Addressable,
    intc: InterruptController,
    irq: Int,
    transport: NetTransport,
    maxConns: Int = 8,
) extends Device with (Processor => Unit):
  val name = "NetSocket"
  val size = 32

  // Register offsets
  private val COMMAND = 0
  private val STATUS = 1
  private val CONN_ID = 2
  private val CONN_STATUS = 3
  private val ADDR = 4
  private val LENGTH = 8
  private val RESULT = 10
  private val PORT = 12
  private val IP_ADDR = 14
  private val NEW_CONN = 18
  private val ERROR = 19
  private val IER = 20
  private val SOCK_TYPE = 21
  private val REMOTE_PORT = 22
  private val REMOTE_IP = 24

  // Commands
  private val CMD_LISTEN = 0x01
  private val CMD_ACCEPT = 0x02
  private val CMD_CONNECT = 0x03
  private val CMD_SEND = 0x04
  private val CMD_RECV = 0x05
  private val CMD_CLOSE = 0x06
  private val CMD_BIND = 0x08
  private val CMD_SENDTO = 0x09
  private val CMD_RECVFROM = 0x0A

  // Connection status bits
  private val CS_CONNECTED = 0x01
  private val CS_LISTENING = 0x02
  private val CS_PENDING_ACCEPT = 0x04
  private val CS_DATA_AVAILABLE = 0x08
  private val CS_ERROR = 0x10
  private val CS_REMOTE_CLOSED = 0x20
  private val CS_UDP = 0x40

  // Error codes
  private val ERR_NONE = 0
  private val ERR_INVALID_ID = 1
  private val ERR_CONN_FAILED = 2
  private val ERR_NOT_CONNECTED = 3
  private val ERR_TABLE_FULL = 4
  private val ERR_WRONG_TYPE = 5

  // Socket types
  private val TYPE_TCP = 0
  private val TYPE_UDP = 1

  // State
  private var connId: Int = 0
  private var addr: Int = 0
  private var length: Int = 0
  private var result: Int = 0
  private var port: Int = 0
  private var ipAddr: Int = 0 // packed IPv4: (A << 24) | (B << 16) | (C << 8) | D
  private var newConn: Int = -1
  private var error: Int = ERR_NONE
  private var ier: Int = 0
  private var sockType: Int = TYPE_TCP
  private var remotePort: Int = 0
  private var remoteIp: Int = 0

  // Connection table — holds both TCP (NetConnection) and UDP (NetDatagramSocket)
  private val conns = new Array[NetConnection](maxConns)
  private val udpSocks = new Array[NetDatagramSocket](maxConns)

  def readByte(address: Long): Int =
    (address - base).toInt match
      case STATUS =>
        var s = 0
        for i <- 0 until maxConns do
          if conns(i) != null then
            if conns(i).hasPendingAccept then s |= 0x01
            if conns(i).hasData then s |= 0x02
          if udpSocks(i) != null then
            if udpSocks(i).hasData then s |= 0x02
        s
      case CONN_ID => connId
      case CONN_STATUS =>
        if connId < 0 || connId >= maxConns then 0
        else
          val c = conns(connId)
          val u = udpSocks(connId)
          if u != null then
            var s = CS_UDP
            if u.isBound then s |= CS_CONNECTED
            if u.hasData then s |= CS_DATA_AVAILABLE
            if u.hasError then s |= CS_ERROR
            s
          else if c != null then
            var s = 0
            if c.isConnected then s |= CS_CONNECTED
            if c.isListening then s |= CS_LISTENING
            if c.hasPendingAccept then s |= CS_PENDING_ACCEPT
            if c.hasData then s |= CS_DATA_AVAILABLE
            if c.hasError then s |= CS_ERROR
            if c.isRemoteClosed then s |= CS_REMOTE_CLOSED
            s
          else 0
      case 4  => (addr >> 24) & 0xFF
      case 5  => (addr >> 16) & 0xFF
      case 6  => (addr >> 8) & 0xFF
      case 7  => addr & 0xFF
      case 8  => (length >> 8) & 0xFF
      case 9  => length & 0xFF
      case 10 => (result >> 8) & 0xFF
      case 11 => result & 0xFF
      case 12 => (port >> 8) & 0xFF
      case 13 => port & 0xFF
      case 14 => (ipAddr >> 24) & 0xFF
      case 15 => (ipAddr >> 16) & 0xFF
      case 16 => (ipAddr >> 8) & 0xFF
      case 17 => ipAddr & 0xFF
      case NEW_CONN => newConn & 0xFF
      case ERROR => error
      case IER => ier
      case SOCK_TYPE => sockType
      case 22 => (remotePort >> 8) & 0xFF
      case 23 => remotePort & 0xFF
      case 24 => (remoteIp >> 24) & 0xFF
      case 25 => (remoteIp >> 16) & 0xFF
      case 26 => (remoteIp >> 8) & 0xFF
      case 27 => remoteIp & 0xFF
      case _ => 0

  def writeByte(address: Long, data: Long): Unit =
    val d = (data & 0xFF).toInt
    (address - base).toInt match
      case COMMAND => execute(d)
      case CONN_ID => connId = d
      case 4  => addr = (addr & 0x00FFFFFF) | (d << 24)
      case 5  => addr = (addr & 0xFF00FFFF) | (d << 16)
      case 6  => addr = (addr & 0xFFFF00FF) | (d << 8)
      case 7  => addr = (addr & 0xFFFFFF00) | d
      case 8  => length = (length & 0x00FF) | (d << 8)
      case 9  => length = (length & 0xFF00) | d
      case 12 => port = (port & 0x00FF) | (d << 8)
      case 13 => port = (port & 0xFF00) | d
      case 14 => ipAddr = (ipAddr & 0x00FFFFFF) | (d << 24)
      case 15 => ipAddr = (ipAddr & 0xFF00FFFF) | (d << 16)
      case 16 => ipAddr = (ipAddr & 0xFFFF00FF) | (d << 8)
      case 17 => ipAddr = (ipAddr & 0xFFFFFF00) | d
      case IER => ier = d
      case SOCK_TYPE => sockType = d & 1
      case _ =>

  private def execute(cmd: Int): Unit =
    error = ERR_NONE
    cmd match
      // === TCP commands ===
      case CMD_LISTEN =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        if conns(connId) != null then conns(connId).close()
        conns(connId) = transport.listen(port)
      case CMD_ACCEPT =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        val listener = conns(connId)
        if listener == null || !listener.isListening then { error = ERR_NOT_CONNECTED; return }
        val accepted = listener.accept()
        if accepted == null then { newConn = -1; return }
        // Find a free slot
        var slot = -1
        var i = 0
        while i < maxConns && slot < 0 do
          if conns(i) == null && udpSocks(i) == null then slot = i
          i += 1
        if slot < 0 then { error = ERR_TABLE_FULL; accepted.close(); return }
        conns(slot) = accepted
        newConn = slot
      case CMD_CONNECT =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        if conns(connId) != null then conns(connId).close()
        val ip = formatIp(ipAddr)
        val conn = transport.connect(ip, port)
        if conn == null then { error = ERR_CONN_FAILED; return }
        conns(connId) = conn
      case CMD_SEND =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        val c = conns(connId)
        if c == null || !c.isConnected then { error = ERR_NOT_CONNECTED; return }
        val buf = readFromMemory()
        result = c.send(buf)
      case CMD_RECV =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        val c = conns(connId)
        if c == null || !c.isConnected then { error = ERR_NOT_CONNECTED; return }
        val buf = new Array[Byte](length)
        val n = c.recv(buf)
        result = n
        writeToMemory(buf, n)
      case CMD_CLOSE =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        if conns(connId) != null then
          conns(connId).close()
          conns(connId) = null
        if udpSocks(connId) != null then
          udpSocks(connId).close()
          udpSocks(connId) = null

      // === UDP commands ===
      case CMD_BIND =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        // Close any existing socket in this slot
        if conns(connId) != null then { conns(connId).close(); conns(connId) = null }
        if udpSocks(connId) != null then { udpSocks(connId).close(); udpSocks(connId) = null }
        val sock = transport.bindUdp(port)
        if sock == null then { error = ERR_CONN_FAILED; return }
        udpSocks(connId) = sock
      case CMD_SENDTO =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        val u = udpSocks(connId)
        if u == null then { error = ERR_WRONG_TYPE; return }
        val buf = readFromMemory()
        val ip = formatIp(ipAddr)
        result = u.sendTo(buf, ip, port)
      case CMD_RECVFROM =>
        if connId < 0 || connId >= maxConns then { error = ERR_INVALID_ID; return }
        val u = udpSocks(connId)
        if u == null then { error = ERR_WRONG_TYPE; return }
        val buf = new Array[Byte](length)
        val dg = u.recvFrom(buf)
        if dg == null then
          result = 0
        else
          result = dg.length
          writeToMemory(buf, dg.length)
          remoteIp = dg.ip
          remotePort = dg.port

      case _ =>

  private def formatIp(packed: Int): String =
    s"${(packed >> 24) & 0xFF}.${(packed >> 16) & 0xFF}.${(packed >> 8) & 0xFF}.${packed & 0xFF}"

  private def readFromMemory(): Array[Byte] =
    val buf = new Array[Byte](length)
    var i = 0
    while i < length do
      buf(i) = mem.readByte(addr.toLong + i).toByte
      i += 1
    buf

  private def writeToMemory(buf: Array[Byte], n: Int): Unit =
    var i = 0
    while i < n do
      mem.writeByte(addr.toLong + i, buf(i))
      i += 1

  def apply(cpu: Processor): Unit =
    if ier == 0 then return
    var raised = false
    var i = 0
    while i < maxConns && !raised do
      val c = conns(i)
      if c != null then
        if (ier & 0x01) != 0 && c.hasPendingAccept then { intc.raise(irq); raised = true }
        else if (ier & 0x02) != 0 && c.hasData then { intc.raise(irq); raised = true }
        else if (ier & 0x04) != 0 && c.isRemoteClosed then { intc.raise(irq); raised = true }
      val u = udpSocks(i)
      if !raised && u != null then
        if (ier & 0x02) != 0 && u.hasData then { intc.raise(irq); raised = true }
      i += 1

  /** Close all connections. Call on emulator shutdown. */
  def closeAll(): Unit =
    for i <- 0 until maxConns do
      if conns(i) != null then
        conns(i).close()
        conns(i) = null
      if udpSocks(i) != null then
        udpSocks(i).close()
        udpSocks(i) = null

/**
 * Abstract network transport — implemented by the host environment.
 */
trait NetTransport:
  /** Start listening on a TCP port. Returns a listening connection. */
  def listen(port: Int): NetConnection

  /** Connect to a remote host:port via TCP. Returns a connected connection, or null on failure. */
  def connect(host: String, port: Int): NetConnection

  /** Bind a UDP socket to a local port. Returns the socket, or null on failure. */
  def bindUdp(port: Int): NetDatagramSocket

/**
 * Abstract TCP connection — represents a single TCP stream or listener.
 */
trait NetConnection:
  def isConnected: Boolean
  def isListening: Boolean
  def hasPendingAccept: Boolean
  def hasData: Boolean
  def hasError: Boolean
  def isRemoteClosed: Boolean

  /** Accept a pending connection (for listeners). Returns null if none pending. */
  def accept(): NetConnection

  /** Send data. Returns number of bytes sent. */
  def send(data: Array[Byte]): Int

  /** Receive data into buffer. Returns number of bytes received (0 if none available). */
  def recv(buffer: Array[Byte]): Int

  /** Close this connection. */
  def close(): Unit

/**
 * Result of a UDP recvfrom: data length and source address.
 */
case class Datagram(length: Int, ip: Int, port: Int)

/**
 * Abstract UDP socket — bound to a local port, sends/receives datagrams.
 */
trait NetDatagramSocket:
  def isBound: Boolean
  def hasData: Boolean
  def hasError: Boolean

  /** Send a datagram to host:port. Returns number of bytes sent. */
  def sendTo(data: Array[Byte], host: String, port: Int): Int

  /** Receive a datagram into buffer. Returns Datagram with length and source, or null if none available. */
  def recvFrom(buffer: Array[Byte]): Datagram

  /** Close this socket. */
  def close(): Unit
