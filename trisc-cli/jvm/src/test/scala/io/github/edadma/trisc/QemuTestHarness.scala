package io.github.edadma.trisc

import java.io.{InputStream, OutputStream}
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

/** Spawns QEMU with -serial stdio and provides interactive
  * read/write access to the guest serial console. Output is
  * collected by a background reader thread. The harness waits
  * for patterns (like the shell prompt) before sending input,
  * just like a user watching the screen before typing. */
class QemuTestHarness(
    kernelPath: String = "/tmp/slix-x86_64/kernel.elf",
    ramdiskPath: String = "/tmp/slix-x86_64/ramdisk.img",
    timeoutMs: Long = 10000,
) extends AutoCloseable:

  private var process: Process = null
  private var qemuIn: OutputStream = null
  private val outputBuf = new StringBuilder
  private val outputQueue = new ArrayBlockingQueue[java.lang.Character](65536)
  private var readerThread: Thread = null

  def start(): Unit =
    val cmd = new java.util.ArrayList[String]()
    cmd.add("qemu-system-x86_64")
    cmd.add("-kernel")
    cmd.add(kernelPath)
    cmd.add("-chardev")
    cmd.add("stdio,id=char0,signal=off")
    cmd.add("-serial")
    cmd.add("chardev:char0")
    cmd.add("-no-reboot")
    cmd.add("-display")
    cmd.add("none")
    cmd.add("-monitor")
    cmd.add("none")
    // virtio-net-pci with user-mode networking and a single UDP
    // hostfwd so tests can shoot packets at guest:7777 via host
    // port 17777 (mirrors aarch64's QemuAarch64TestHarness). The
    // nic boot module attaches to this device via the kernel's
    // probe + cached caps.
    cmd.add("-netdev")
    cmd.add("user,id=n0,hostfwd=udp::17777-:7777")
    cmd.add("-device")
    cmd.add("virtio-net-pci,netdev=n0,disable-legacy=on")
    if new java.io.File(ramdiskPath).exists() then
      val bootInfoPath = ramdiskPath.replace("ramdisk.img", "bootinfo.img")
      cmd.add("-initrd")
      if new java.io.File(bootInfoPath).exists() then
        cmd.add(s"$ramdiskPath,$bootInfoPath")
      else
        cmd.add(ramdiskPath)

    val pb = new ProcessBuilder(cmd)
    pb.redirectErrorStream(true)
    process = pb.start()
    qemuIn = process.getOutputStream

    // Background thread reads stdout one byte at a time
    // and pushes characters into a queue for the test thread.
    val stdout = process.getInputStream
    readerThread = new Thread(() => {
      try
        var b = stdout.read()
        while b != -1 do
          outputQueue.put(java.lang.Character.valueOf(b.toChar))
          b = stdout.read()
      catch case _: Exception => ()
    }, "qemu-reader")
    readerThread.setDaemon(true)
    readerThread.start()

  /** Read output until `pattern` appears. Returns all output
    * collected (including text before and after the pattern).
    * Throws if the pattern isn't seen within the timeout. */
  def waitFor(pattern: String): String =
    val buf = new StringBuilder
    val deadline = System.currentTimeMillis() + timeoutMs
    while !buf.toString.contains(pattern) do
      val remaining = deadline - System.currentTimeMillis()
      if remaining <= 0 then
        outputBuf.append(buf)
        throw new RuntimeException(
          s"Timeout waiting for pattern '$pattern'. Output so far:\n${outputBuf.toString + buf.toString}"
        )
      val ch = outputQueue.poll(remaining, TimeUnit.MILLISECONDS)
      if ch != null then buf.append(ch.charValue())
    outputBuf.append(buf)
    buf.toString

  /** Send a string to the guest's serial input. */
  def send(s: String): Unit =
    for ch <- s do
      qemuIn.write(ch.toByte)
      qemuIn.flush()
      // Small delay between characters to avoid UART FIFO overflow
      Thread.sleep(5)

  /** Send a command and wait for the next prompt. Returns
    * the output between sending and the prompt. The prompt
    * pattern defaults to "> " (nsh's "cwd> " format). */
  def command(cmd: String, prompt: String = "> "): String =
    send(cmd + "\n")
    waitFor(prompt)

  /** Get all output collected so far. */
  def allOutput: String = outputBuf.toString

  def close(): Unit =
    if process != null then
      process.destroyForcibly()
      process.waitFor(5, TimeUnit.SECONDS)
      // Drain remaining output
      while outputQueue.peek() != null do
        outputBuf.append(outputQueue.poll())
