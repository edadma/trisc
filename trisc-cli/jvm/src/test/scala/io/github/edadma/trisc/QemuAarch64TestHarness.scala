package io.github.edadma.trisc

import java.io.{InputStream, OutputStream}
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}

/** Spawns qemu-system-aarch64 (machine virt, GICv2, cortex-a72)
  * with the SLIX kernel ELF + bootinfo + ramdisk loader devices,
  * and provides interactive read/write access to the PL011 serial
  * console. Mirrors the x86 [[QemuTestHarness]] but uses the
  * aarch64 boot-module layout: bootinfo at 0x44000000 and ramdisk
  * at 0x50000000 loaded via `-device loader`. */
class QemuAarch64TestHarness(
    kernelPath: String = "/tmp/slix-aarch64/kernel.elf",
    bootinfoPath: String = "/tmp/slix-aarch64/bootinfo.img",
    ramdiskPath: String = "/tmp/slix-aarch64/ramdisk.img",
    timeoutMs: Long = 30000,
) extends AutoCloseable:

  private var process: Process = null
  private var qemuIn: OutputStream = null
  private val outputBuf = new StringBuilder
  private val outputQueue = new ArrayBlockingQueue[java.lang.Character](65536)
  private var readerThread: Thread = null

  def start(): Unit =
    val cmd = new java.util.ArrayList[String]()
    cmd.add("qemu-system-aarch64")
    cmd.add("-machine"); cmd.add("virt,gic-version=2")
    cmd.add("-cpu"); cmd.add("cortex-a72")
    cmd.add("-m"); cmd.add("512M")
    cmd.add("-kernel"); cmd.add(kernelPath)
    cmd.add("-chardev"); cmd.add("stdio,id=char0,signal=off")
    cmd.add("-serial"); cmd.add("chardev:char0")
    cmd.add("-no-reboot")
    cmd.add("-display"); cmd.add("none")
    cmd.add("-monitor"); cmd.add("none")
    cmd.add("-global"); cmd.add("virtio-mmio.force-legacy=false")
    // hostfwd=udp::17777-:7777 forwards host localhost:17777 to guest:7777
    // so async-RX tests (test_udp_echo) can inject unsolicited inbound.
    cmd.add("-netdev"); cmd.add("user,id=n0,hostfwd=udp::17777-:7777")
    cmd.add("-device"); cmd.add("virtio-net-device,netdev=n0,mac=52:54:00:12:34:56")
    if new java.io.File(bootinfoPath).exists() then
      cmd.add("-device")
      cmd.add(s"loader,file=$bootinfoPath,addr=0x44000000")
    if new java.io.File(ramdiskPath).exists() then
      cmd.add("-device")
      cmd.add(s"loader,file=$ramdiskPath,addr=0x50000000")

    val pb = new ProcessBuilder(cmd)
    pb.redirectErrorStream(true)
    process = pb.start()
    qemuIn = process.getOutputStream

    val stdout = process.getInputStream
    readerThread = new Thread(() => {
      try
        var b = stdout.read()
        while b != -1 do
          outputQueue.put(java.lang.Character.valueOf(b.toChar))
          b = stdout.read()
      catch case _: Exception => ()
    }, "qemu-aa64-reader")
    readerThread.setDaemon(true)
    readerThread.start()

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

  def send(s: String): Unit =
    for ch <- s do
      qemuIn.write(ch.toByte)
      qemuIn.flush()
      Thread.sleep(5)

  def command(cmd: String, prompt: String = "> "): String =
    send(cmd + "\n")
    waitFor(prompt)

  def allOutput: String = outputBuf.toString

  def close(): Unit =
    if process != null then
      process.destroyForcibly()
      process.waitFor(5, TimeUnit.SECONDS)
      while outputQueue.peek() != null do
        outputBuf.append(outputQueue.poll())
