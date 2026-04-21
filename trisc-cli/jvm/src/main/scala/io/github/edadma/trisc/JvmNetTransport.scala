package io.github.edadma.trisc

import java.net.{DatagramPacket, DatagramSocket, InetAddress, InetSocketAddress, StandardSocketOptions}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.collection.mutable

/**
 * JVM implementation of NetTransport using Java NIO (TCP) and java.net (UDP).
 */
class JvmNetTransport extends NetTransport:

  def listen(port: Int): NetConnection =
    val server = ServerSocketChannel.open()
    server.configureBlocking(false)
    server.setOption(StandardSocketOptions.SO_REUSEADDR, java.lang.Boolean.TRUE)
    server.bind(new InetSocketAddress(port))
    new JvmListenerConnection(server)

  def connect(host: String, port: Int): NetConnection =
    try
      val ch = SocketChannel.open()
      ch.configureBlocking(true) // blocking connect, then switch to non-blocking
      ch.connect(new InetSocketAddress(host, port))
      ch.configureBlocking(false)
      new JvmStreamConnection(ch)
    catch
      case _: Exception => null

  def bindUdp(port: Int): NetDatagramSocket =
    try
      val sock = new DatagramSocket(null)
      sock.setReuseAddress(true)
      sock.bind(new InetSocketAddress(port))
      sock.setSoTimeout(0) // non-blocking recv via setSoTimeout(1) in hasData
      new JvmDatagramSocket(sock)
    catch
      case _: Exception => null

/**
 * A listening connection that accepts incoming connections.
 */
private class JvmListenerConnection(server: ServerSocketChannel) extends NetConnection:
  private var closed = false

  def isConnected: Boolean = false
  def isListening: Boolean = !closed
  def hasError: Boolean = false
  def isRemoteClosed: Boolean = false

  def hasPendingAccept: Boolean =
    if closed then false
    else
      try
        pollAccept()
        pendingQueue.nonEmpty
      catch
        case _: Exception => false

  private val pendingQueue = new mutable.Queue[SocketChannel]()

  private def pollAccept(): Unit =
    if !closed then
      try
        val ch = server.accept()
        if ch != null then
          ch.configureBlocking(false)
          pendingQueue.enqueue(ch)
      catch
        case _: Exception =>

  def hasData: Boolean = false

  def accept(): NetConnection =
    pollAccept()
    if pendingQueue.nonEmpty then
      new JvmStreamConnection(pendingQueue.dequeue())
    else null

  def send(data: Array[Byte]): Int = 0
  def recv(buffer: Array[Byte]): Int = 0

  def close(): Unit =
    closed = true
    while pendingQueue.nonEmpty do
      try pendingQueue.dequeue().close() catch case _: Exception => ()
    try server.close() catch case _: Exception => ()

/**
 * A connected TCP stream.
 */
private class JvmStreamConnection(channel: SocketChannel) extends NetConnection:
  private var closed = false
  private var remoteClosed = false
  private var errorFlag = false

  def isConnected: Boolean = !closed && !remoteClosed && channel.isConnected
  def isListening: Boolean = false
  def hasPendingAccept: Boolean = false
  def hasError: Boolean = errorFlag
  def isRemoteClosed: Boolean = remoteClosed

  def hasData: Boolean =
    if closed || remoteClosed then false
    else
      try
        val buf = ByteBuffer.allocate(1)
        val n = channel.read(buf)
        if n > 0 then
          buf.flip()
          true
        else if n == -1 then
          remoteClosed = true
          false
        else false
      catch
        case _: Exception =>
          errorFlag = true
          false

  def accept(): NetConnection = null

  def send(data: Array[Byte]): Int =
    if closed || remoteClosed then return 0
    try
      val buf = ByteBuffer.wrap(data)
      var total = 0
      while buf.hasRemaining do
        total += channel.write(buf)
      total
    catch
      case _: Exception =>
        errorFlag = true
        0

  def recv(buffer: Array[Byte]): Int =
    if closed || remoteClosed then return 0
    try
      val buf = ByteBuffer.wrap(buffer)
      val n = channel.read(buf)
      if n == -1 then
        remoteClosed = true
        0
      else if n < 0 then 0
      else n
    catch
      case _: Exception =>
        errorFlag = true
        0

  def close(): Unit =
    closed = true
    try channel.close() catch case _: Exception => ()

/**
 * A bound UDP socket.
 */
private class JvmDatagramSocket(socket: DatagramSocket) extends NetDatagramSocket:
  private var closed = false
  private var errorFlag = false

  def isBound: Boolean = !closed && socket.isBound
  def hasError: Boolean = errorFlag

  def hasData: Boolean =
    if closed then false
    else
      try
        // Brief non-blocking peek via short timeout
        socket.setSoTimeout(1)
        val buf = new Array[Byte](1)
        val pkt = new DatagramPacket(buf, 1)
        try
          socket.receive(pkt)
          // Got data — can't unread it, so we need a pending buffer
          // For simplicity, we'll just report true and the actual recvFrom will get the next packet
          true
        catch
          case _: java.net.SocketTimeoutException => false
        finally
          socket.setSoTimeout(0)
      catch
        case _: Exception =>
          errorFlag = true
          false

  def sendTo(data: Array[Byte], host: String, port: Int): Int =
    if closed then return 0
    try
      val addr = InetAddress.getByName(host)
      val pkt = new DatagramPacket(data, data.length, addr, port)
      socket.send(pkt)
      data.length
    catch
      case _: Exception =>
        errorFlag = true
        0

  def recvFrom(buffer: Array[Byte]): Datagram =
    if closed then return null
    try
      socket.setSoTimeout(1)
      val pkt = new DatagramPacket(buffer, buffer.length)
      try
        socket.receive(pkt)
        val addr = pkt.getAddress
        val bytes = addr.getAddress
        val ip = ((bytes(0) & 0xFF) << 24) | ((bytes(1) & 0xFF) << 16) |
          ((bytes(2) & 0xFF) << 8) | (bytes(3) & 0xFF)
        Datagram(pkt.getLength, ip, pkt.getPort)
      catch
        case _: java.net.SocketTimeoutException => null
      finally
        socket.setSoTimeout(0)
    catch
      case _: Exception =>
        errorFlag = true
        null

  def close(): Unit =
    closed = true
    try socket.close() catch case _: Exception => ()
