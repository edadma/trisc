package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

/** aarch64 QEMU integration tests for the SLIX login + nsh shell.
  *
  * Prerequisites: build the kernel, programs, servers, ramdisk,
  * and bootinfo before running:
  *   bash oskit/arch/aarch64/board/virt/build.sh
  *
  * That single script produces /tmp/slix-aarch64/{kernel.elf,
  * bootinfo.img, ramdisk.img} which the harness consumes. Tests
  * boot through login (root/toor) before exercising the shell. */
class Aarch64NshTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach {

  private var qemu: QemuAarch64TestHarness = null

  private def requirePrebuilt(): Unit =
    assume(
      new java.io.File("/tmp/slix-aarch64/kernel.elf").exists(),
      "aarch64 kernel not built — run: bash oskit/arch/aarch64/board/virt/build.sh",
    )

  override def beforeEach(): Unit =
    requirePrebuilt()
    qemu = new QemuAarch64TestHarness(timeoutMs = 30000)
    qemu.start()
    qemu.waitFor("login: ")
    qemu.send("root\n")
    qemu.waitFor("password: ")
    qemu.send("toor\n")
    qemu.waitFor("> ")

  override def afterEach(): Unit =
    if qemu != null then qemu.close()

  "aarch64 login: echo command" in {
    val output = qemu.command("echo hello aarch64")
    output should include("hello aarch64")
  }

  "aarch64 login: help command" in {
    val output = qemu.command("help")
    output should include("builtins:")
  }

  "aarch64 login: hello program" in {
    val output = qemu.command("hello")
    output should include("Hello")
  }

  "aarch64 login: uptime command" in {
    val output = qemu.command("uptime")
    output.trim should not be empty
  }

  "aarch64 login: ls root" in {
    val output = qemu.command("ls")
    output should include("etc")
    output should include("bin")
  }

  "aarch64 login: cat /etc/ttytab" in {
    val output = qemu.command("cat /etc/ttytab")
    output should include("tty0 login")
  }

  "aarch64 login: whoami" in {
    val output = qemu.command("whoami")
    output should include("0")
  }

  "aarch64 login: ps lists threads" in {
    val output = qemu.command("ps")
    output should include("rs")
  }

  "aarch64 login: multiple commands" in {
    qemu.command("echo first")
    val output = qemu.command("echo second")
    output should include("second")
  }

  private val rootPrompt = "/root> "

  "aarch64 pipe: echo hello | cat" in {
    val output = qemu.command("echo hello | cat")
    output should include("hello")
  }

  "aarch64 redirect: echo hello > /tmp/out" in {
    qemu.command("echo hello > /tmp/out", rootPrompt)
    val output = qemu.command("cat /tmp/out")
    output should include("hello")
  }

  "aarch64 redirect: echo append >>" in {
    qemu.command("echo line1 > /tmp/app", rootPrompt)
    qemu.command("echo line2 >> /tmp/app", rootPrompt)
    val output = qemu.command("cat /tmp/app")
    output should include("line1")
    output should include("line2")
  }

  "aarch64 redirect: cat < /tmp/in" in {
    qemu.command("echo inputdata > /tmp/in", rootPrompt)
    val output = qemu.command("cat < /tmp/in")
    output should include("inputdata")
  }

  "aarch64 redirect: no space after >" in {
    qemu.command("echo spaceless >/tmp/ns", rootPrompt)
    val output = qemu.command("cat /tmp/ns")
    output should include("spaceless")
  }

  "aarch64 pipe: no spaces around |" in {
    val output = qemu.command("echo piped|cat")
    output should include("piped")
  }

  "aarch64 head: first 2 lines of file" in {
    qemu.command("echo line1 > /tmp/hf", rootPrompt)
    qemu.command("echo line2 >> /tmp/hf", rootPrompt)
    qemu.command("echo line3 >> /tmp/hf", rootPrompt)
    val output = qemu.command("head -2 /tmp/hf")
    output should include("line1")
    output should include("line2")
    output should not include "line3"
  }

  "aarch64 tail: last 2 lines of file" in {
    qemu.command("echo aaa > /tmp/tf", rootPrompt)
    qemu.command("echo bbb >> /tmp/tf", rootPrompt)
    qemu.command("echo ccc >> /tmp/tf", rootPrompt)
    val output = qemu.command("tail -2 /tmp/tf")
    output should not include "aaa"
    output should include("bbb")
    output should include("ccc")
  }

  "aarch64 wc: count from file" in {
    qemu.command("echo hello > /tmp/wcf", rootPrompt)
    val output = qemu.command("wc /tmp/wcf")
    output should include("1")
  }

  "aarch64 virtio: probe finds the attached virtio-net device" in {
    // The boot log (captured before the login prompt) should have
    // the virtio probe line confirming QEMU's virtio-net-device is
    // reachable over virtio-mmio.
    val banner = qemu.allOutput
    banner should include("virtio: slot")
    banner should include("(net)")
  }

  "aarch64 inet: UDP loopback via test_net" in {
    // test_net opens a UDP socket on 127.0.0.1:5000, sends "hello"
    // to itself, and prints what recvfrom returned. Phase 2 on
    // aarch64 exercises real-wire sendto without an explicit bind
    // so the `wire sent=5` line also covers inet's auto-bind path.
    val output = qemu.command("test_net")
    output should include("sent=5")
    output should include("recv=5 'hello'")
    output should include("wire sent=5")
  }

  "aarch64 nic: GET_MAC + subscribe + drain via test_nic" in {
    // test_nic exercises the full nic IPC ABI: GET_MAC,
    // SUBSCRIBE_RX, and RECV_PACKET drain loop. QEMU boots
    // virtio-net-device with a fixed MAC. The RX queue gets
    // the ARP/ICMP boot frames enqueued by nic_poller, so
    // the drain count is non-zero by the time test_nic runs
    // under login.
    val output = qemu.command("test_nic")
    output should include("mac=52:54:00:12:34:56")
    output should include("sub=ok")
    output should include("rx=")
  }

  "aarch64 async RX: unsolicited UDP reaches recvfrom via virtio IRQ" in {
    // test_udp_echo binds :7777, blocks in recvfrom. The harness's
    // netdev forwards host localhost:17777 → guest:7777. Sending a
    // datagram from Scala arrives at the guest unsolicited, travels
    // through the virtio-mmio IRQ path (GIC 79 → virtio_handler →
    // nic → inet → deferred recvfrom reply), and test_udp_echo
    // prints the payload. Without the IRQ path, recvfrom would
    // block forever (nothing else pokes nic to drain the ring).
    qemu.send("test_udp_echo\n")
    qemu.waitFor("listening on :7777")

    val sock = new java.net.DatagramSocket()
    try
      val payload = "ping!".getBytes("UTF-8")
      val addr = java.net.InetAddress.getByName("127.0.0.1")
      sock.send(new java.net.DatagramPacket(payload, payload.length, addr, 17777))
    finally sock.close()

    val output = qemu.waitFor("'ping!'")
    output should include("test_udp_echo: got 5 from ")
    output should include("'ping!'")
  }

  "aarch64 tcp: connect, send, receive echo, close" in {
    // Start a tiny host-side TCP echo server on 127.0.0.1:18080.
    // QEMU user-mode networking translates guest dials of
    // 10.0.2.2:18080 into host connections on the matching port,
    // so no hostfwd is needed for outbound. test_tcp sends
    // "ping\n", expects it echoed back, then closes.
    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18080))
    server.setSoTimeout(15000)

    val echoThread = new Thread(() => {
      try
        val client = server.accept()
        try
          val in  = client.getInputStream
          val out = client.getOutputStream
          val buf = new Array[Byte](64)
          val n = in.read(buf)
          if n > 0 then
            out.write(buf, 0, n)
            out.flush()
          // Wait briefly for client to close first (TIME_WAIT on
          // our side avoids FIN collision in the trace).
          Thread.sleep(100)
        finally client.close()
      catch
        case _: Throwable => ()
    }, "tcp-echo-server")
    echoThread.setDaemon(true)
    echoThread.start()

    try
      qemu.send("test_tcp\n")
      val output = qemu.waitFor("test_tcp: closed")
      output should include("test_tcp: connected fd=")
      output should include("test_tcp: sent=5")
      output should include("test_tcp: got 5 'ping")
      output should include("test_tcp: closed")
    finally
      server.close()
      echoThread.join(2000)
  }

  "aarch64 crash recovery: kill tfs and restart" in {
    val psOut = qemu.command("ps")
    val tfsLine = psOut.split('\n').find(_.contains("tfs"))
    tfsLine shouldBe defined
    val nums = tfsLine.get.trim.split("\\s+")
    val tfsPid = nums(1)

    qemu.send(s"kill $tfsPid\n")
    val killOutput = qemu.waitFor("restarted ok")
    killOutput should include("RS: restarting tfs")
    killOutput should include("RS: tfs restarted ok")

    Thread.sleep(200)
    val psAfter = qemu.command("ps")
    psAfter should include("tfs")

    val after = qemu.command("cat /etc/ttytab")
    after should include("tty0 login")
  }
}
