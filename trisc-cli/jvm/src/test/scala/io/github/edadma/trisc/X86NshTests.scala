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
    // Start count in background — nsh prints "[1] PID"
    qemu.send("count &\n")
    val bgOutput = qemu.waitFor("> ")
    Thread.sleep(2000)

    // Extract PID from nsh's "[N] PID" output
    val pidPattern = """\[\d+\]\s+(\d+)""".r
    val countPid = pidPattern.findFirstMatchIn(bgOutput).map(_.group(1))
    countPid shouldBe defined

    // Kill it
    qemu.command(s"kill ${countPid.get}")
    Thread.sleep(1000)

    // Verify count is gone
    val psAfter = qemu.command("ps")
    psAfter should not include ("count")
  }

  "x86 pipe: echo hello | cat" in {
    val output = qemu.command("echo hello | cat")
    output should include("hello")
  }

  "x86 pipe: echo hello | cat | cat" in {
    val output = qemu.command("echo hello | cat | cat")
    output should include("hello")
  }

  // Redirect tests use "/root> " as prompt to avoid matching ">" in commands
  private val rootPrompt = "/root> "

  "x86 redirect: echo hello > /tmp/out" in {
    qemu.command("echo hello > /tmp/out", rootPrompt)
    val output = qemu.command("cat /tmp/out")
    output should include("hello")
  }

  "x86 redirect: echo append >>" in {
    qemu.command("echo line1 > /tmp/app", rootPrompt)
    qemu.command("echo line2 >> /tmp/app", rootPrompt)
    val output = qemu.command("cat /tmp/app")
    output should include("line1")
    output should include("line2")
  }

  "x86 redirect: cat < /tmp/in" in {
    qemu.command("echo inputdata > /tmp/in", rootPrompt)
    val output = qemu.command("cat < /tmp/in")
    output should include("inputdata")
  }

  "x86 redirect: pipe with output redirect" in {
    qemu.command("echo piped > /tmp/p1", rootPrompt)
    qemu.command("cat /tmp/p1 | cat > /tmp/p2", rootPrompt)
    val output = qemu.command("cat /tmp/p2")
    output should include("piped")
  }

  "x86 head: first 3 lines from pipe" in {
    val output = qemu.command("echo aaa | head -3")
    output should include("aaa")
  }

  "x86 head: first 2 lines of file" in {
    qemu.command("echo line1 > /tmp/hf", rootPrompt)
    qemu.command("echo line2 >> /tmp/hf", rootPrompt)
    qemu.command("echo line3 >> /tmp/hf", rootPrompt)
    val output = qemu.command("head -2 /tmp/hf")
    output should include("line1")
    output should include("line2")
    output should not include "line3"
  }

  "x86 tail: last 2 lines of file" in {
    qemu.command("echo aaa > /tmp/tf", rootPrompt)
    qemu.command("echo bbb >> /tmp/tf", rootPrompt)
    qemu.command("echo ccc >> /tmp/tf", rootPrompt)
    val output = qemu.command("tail -2 /tmp/tf")
    output should not include "aaa"
    output should include("bbb")
    output should include("ccc")
  }

  "x86 tail: pipe from cat" in {
    qemu.command("echo first > /tmp/tp", rootPrompt)
    qemu.command("echo second >> /tmp/tp", rootPrompt)
    qemu.command("echo third >> /tmp/tp", rootPrompt)
    val output = qemu.command("cat /tmp/tp | tail -1")
    output should not include "first"
    output should include("third")
  }

  "x86 signal: ctrl-c kills foreground process" ignore {
    // TODO: Control characters (0x03, 0x1C) don't pass through QEMU serial pipe.
    // Test via TRISC emulator headless test or GUI emulator manually.
    qemu.send("count\n")
    Thread.sleep(2000)
    qemu.send("\u001c")
    qemu.waitFor(rootPrompt)
    val ps = qemu.command("ps")
    ps should not include "count"
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
