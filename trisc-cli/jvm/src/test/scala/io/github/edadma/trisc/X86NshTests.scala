package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

/** x86_64 QEMU integration tests for the SLIX login + nsh shell.
  *
  * Prerequisites: build the kernel, programs, and ramdisk before running:
  *   bash oskit/arch/x86_64/build.sh app_nsh
  *   bash oskit/arch/x86_64/build_prog.sh all
  *   bash oskit/arch/x86_64/build_servers.sh all
  *   sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeX86RamdiskMain"
  *   sbt "triscCliJVM/runMain io.github.edadma.trisc.MakeX86BootInfoMain"
  *
  * These tests boot through login (root/toor) before testing the shell. */
class X86NshTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach {

  private var qemu: QemuTestHarness = null

  private def requirePrebuilt(): Unit =
    assume(
      new java.io.File("/tmp/slix-x86_64/kernel.elf").exists(),
      "x86_64 kernel not built — run: bash oskit/arch/x86_64/build.sh app_nsh",
    )

  override def beforeEach(): Unit =
    requirePrebuilt()
    qemu = new QemuTestHarness(timeoutMs = 30000)
    qemu.start()
    // Wait for login prompt, then authenticate
    qemu.waitFor("login: ")
    qemu.send("root\n")
    qemu.waitFor("password: ")
    qemu.send("toor\n")
    // Wait for shell prompt (login spawns nsh in /root)
    qemu.waitFor("> ")

  override def afterEach(): Unit =
    if qemu != null then qemu.close()

  "x86 login: echo command" in {
    val output = qemu.command("echo hello x86")
    output should include("hello x86")
  }

  "x86 login: pwd shows home" in {
    val output = qemu.command("pwd")
    output should include("/root")
  }

  "x86 login: help command" in {
    val output = qemu.command("help")
    output should include("builtins:")
  }

  "x86 login: hello program" in {
    val output = qemu.command("hello")
    output should include("Hello")
  }

  "x86 login: uptime command" in {
    val output = qemu.command("uptime")
    // uptime prints a number
    output.trim should not be empty
  }

  "x86 login: ls root" in {
    val output = qemu.command("ls")
    output should include("etc")
    output should include("bin")
  }

  "x86 login: cat /etc/ttytab" in {
    val output = qemu.command("cat /etc/ttytab")
    output should include("tty0 login")
  }

  "x86 login: whoami" in {
    val output = qemu.command("whoami")
    output should include("0")
  }

  "x86 login: ps lists threads" in {
    val output = qemu.command("ps")
    // Should show at least RS, disk, tfs, tty, pm, vfs
    output should include("rs")
  }

  "x86 login: multiple commands" in {
    qemu.command("echo first")
    val output = qemu.command("echo second")
    output should include("second")
  }

  "x86 kill: background process" in {
    // Start count in background
    qemu.send("count &\n")
    qemu.waitFor("> ")
    Thread.sleep(2000)

    // Find count PID from ps
    val psOut = qemu.command("ps")
    val countLine = psOut.split('\n').find(_.contains("count"))
    countLine shouldBe defined
    val countPid = countLine.get.trim.split("\\s+")(1)

    // Kill it
    qemu.command(s"kill $countPid")
    Thread.sleep(1000)

    // Verify count is gone
    val psAfter = qemu.command("ps")
    psAfter should not include ("count")
  }

  "x86 crash recovery: kill tfs and restart" in {
    // Find tfs PID from ps output
    val psOut = qemu.command("ps")
    val tfsLine = psOut.split('\n').find(_.contains("tfs"))
    tfsLine shouldBe defined
    val nums = tfsLine.get.trim.split("\\s+")
    val tfsPid = nums(1) // PID is second column

    // Kill tfs and verify RS detects and restarts it
    qemu.send(s"kill $tfsPid\n")
    val killOutput = qemu.waitFor("restarted ok")
    killOutput should include("RS: restarting tfs")
    killOutput should include("RS: tfs restarted ok")

    // Verify system is fully functional after restart —
    // port transfer means clients keep working transparently
    Thread.sleep(200)
    val psAfter = qemu.command("ps")
    psAfter should include("tfs")

    val after = qemu.command("cat /etc/ttytab")
    after should include("tty0 login")
  }
}
