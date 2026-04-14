package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

/** x86_64 QEMU integration tests for the nsh shell.
  *
  * Prerequisites: build the kernel and ramdisk before running:
  *   bash oskit/arch/x86_64/build.sh app_nsh
  *   bash oskit/arch/x86_64/build_prog.sh all
  *   sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeX86RamdiskMain"
  *
  * These tests are tagged Slow and excluded from normal `sbt test`. */
class X86NshTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach {

  private var qemu: QemuTestHarness = null

  private def requirePrebuilt(): Unit =
    assume(
      new java.io.File("/tmp/slix-x86_64/kernel.elf").exists(),
      "x86_64 kernel not built — run: bash oskit/arch/x86_64/build.sh app_nsh",
    )

  override def beforeEach(): Unit =
    requirePrebuilt()
    qemu = new QemuTestHarness(timeoutMs = 15000)
    qemu.start()
    // Wait for the first shell prompt (boot complete)
    qemu.waitFor("> ")

  override def afterEach(): Unit =
    if qemu != null then qemu.close()

  "x86 nsh: echo command" taggedAs Slow in {
    val output = qemu.command("echo hello x86")
    output should include("hello x86")
  }

  "x86 nsh: pwd shows root" taggedAs Slow in {
    val output = qemu.command("pwd")
    output should include("/")
  }

  "x86 nsh: help command" taggedAs Slow in {
    val output = qemu.command("help")
    output should include("builtins:")
  }

  "x86 nsh: hello program" taggedAs Slow in {
    val output = qemu.command("hello")
    output should include("Hello")
  }

  "x86 nsh: uptime command" taggedAs Slow in {
    val output = qemu.command("uptime")
    // uptime prints a number
    output.trim should not be empty
  }

  "x86 nsh: ls root" taggedAs Slow in {
    val output = qemu.command("ls")
    output should include("etc")
    output should include("bin")
  }

  "x86 nsh: cat /etc/ttytab" taggedAs Slow in {
    val output = qemu.command("cat /etc/ttytab")
    output should include("tty0 nsh")
  }

  "x86 nsh: whoami" taggedAs Slow in {
    val output = qemu.command("whoami")
    // root user prints "0" or "root"
    output.trim should not be empty
  }

  "x86 nsh: ps lists threads" taggedAs Slow in {
    val output = qemu.command("ps")
    // Should show at least RS, disk, tfs, tty, pm, vfs
    output should include("rs")
  }

  "x86 nsh: multiple commands" taggedAs Slow in {
    qemu.command("echo first")
    val output = qemu.command("echo second")
    output should include("second")
  }
}
