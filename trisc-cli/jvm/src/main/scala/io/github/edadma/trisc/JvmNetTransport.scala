package io.github.edadma.trisc

import java.net.{InetSocketAddress, StandardSocketOptions}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.collection.mutable

/**
 * JVM implementation of NetTransport using Java NIO non-blocking sockets.
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
        // Peek: try to accept, if we get one, we need to hold it
        // Use a pending queue instead
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
    // Close any pending connections
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
  private val readBuf = ByteBuffer.allocate(4096)

  def isConnected: Boolean = !closed && !remoteClosed && channel.isConnected
  def isListening: Boolean = false
  def hasPendingAccept: Boolean = false
  def hasError: Boolean = errorFlag
  def isRemoteClosed: Boolean = remoteClosed

  def hasData: Boolean =
    if closed || remoteClosed then false
    else
      try
        readBuf.clear()
        val n = channel.read(readBuf)
        if n > 0 then
          readBuf.flip()
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
