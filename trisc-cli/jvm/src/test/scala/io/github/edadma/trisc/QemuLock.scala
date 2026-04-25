package io.github.edadma.trisc

import java.util.concurrent.locks.ReentrantLock

/** JVM-wide mutex serializing the lifetime of [[QemuTestHarness]] and
  * [[QemuAarch64TestHarness]]. Both harnesses share a single fixed set
  * of slirp `hostfwd` ports (tcp::28080, tcp::28082, tcp::28083,
  * udp::17777), so two QEMUs running concurrently fail to bind and
  * abort the whole suite. ScalaTest under sbt's `-P18` runs suites in
  * parallel, which is fine for everything else — only the QEMU ports
  * are an exclusive resource. Acquire on `start()`, release on
  * `close()`. */
object QemuLock:
  private val lock = new ReentrantLock()
  def acquire(): Unit = lock.lock()
  def release(): Unit = lock.unlock()
