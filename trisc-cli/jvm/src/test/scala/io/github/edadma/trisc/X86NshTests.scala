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
    qemu = new QemuTestHarness(timeoutMs = 60000)
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

  "login: echo command" in {
    val output = qemu.command("echo hello x86")
    output should include("hello x86")
  }

  "login: pwd shows home" in {
    val output = qemu.command("pwd")
    output should include("/root")
  }

  "login: help command" in {
    val output = qemu.command("help")
    output should include("builtins:")
  }

  "login: hello program" in {
    val output = qemu.command("hello")
    output should include("Hello")
  }

  "login: uptime command" in {
    val output = qemu.command("uptime")
    // uptime prints a number
    output.trim should not be empty
  }

  "login: ls root" in {
    val output = qemu.command("ls")
    output should include("etc")
    output should include("bin")
  }

  "login: cat /etc/ttytab" in {
    val output = qemu.command("cat /etc/ttytab")
    output should include("tty0 login")
  }

  "login: whoami" in {
    val output = qemu.command("whoami")
    output should include("0")
  }

  "login: ps lists threads" in {
    val output = qemu.command("ps")
    // Should show at least RS, disk, tfs, tty, pm, vfs
    output should include("rs")
  }

  "login: multiple commands" in {
    qemu.command("echo first")
    val output = qemu.command("echo second")
    output should include("second")
  }

  "kill: background process" in {
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

  "pipe: echo hello | cat" in {
    val output = qemu.command("echo hello | cat")
    output should include("hello")
  }

  "pipe: echo hello | cat | cat" in {
    val output = qemu.command("echo hello | cat | cat")
    output should include("hello")
  }

  // Redirect tests use "/root> " as prompt to avoid matching ">" in commands
  private val rootPrompt = "/root> "

  "redirect: echo hello > /tmp/out" in {
    qemu.command("echo hello > /tmp/out", rootPrompt)
    val output = qemu.command("cat /tmp/out")
    output should include("hello")
  }

  "redirect: echo append >>" in {
    qemu.command("echo line1 > /tmp/app", rootPrompt)
    qemu.command("echo line2 >> /tmp/app", rootPrompt)
    val output = qemu.command("cat /tmp/app")
    output should include("line1")
    output should include("line2")
  }

  "redirect: cat < /tmp/in" in {
    qemu.command("echo inputdata > /tmp/in", rootPrompt)
    val output = qemu.command("cat < /tmp/in")
    output should include("inputdata")
  }

  "redirect: pipe with output redirect" in {
    qemu.command("echo piped > /tmp/p1", rootPrompt)
    qemu.command("cat /tmp/p1 | cat > /tmp/p2", rootPrompt)
    val output = qemu.command("cat /tmp/p2")
    output should include("piped")
  }

  "redirect: no space after >" in {
    qemu.command("echo spaceless >/tmp/ns", rootPrompt)
    val output = qemu.command("cat /tmp/ns")
    output should include("spaceless")
  }

  "pipe: no spaces around |" in {
    val output = qemu.command("echo piped|cat")
    output should include("piped")
  }

  "head: first 3 lines from pipe" in {
    val output = qemu.command("echo aaa | head -3")
    output should include("aaa")
  }

  "head: first 2 lines of file" in {
    qemu.command("echo line1 > /tmp/hf", rootPrompt)
    qemu.command("echo line2 >> /tmp/hf", rootPrompt)
    qemu.command("echo line3 >> /tmp/hf", rootPrompt)
    val output = qemu.command("head -2 /tmp/hf")
    output should include("line1")
    output should include("line2")
    output should not include "line3"
  }

  "tail: last 2 lines of file" in {
    qemu.command("echo aaa > /tmp/tf", rootPrompt)
    qemu.command("echo bbb >> /tmp/tf", rootPrompt)
    qemu.command("echo ccc >> /tmp/tf", rootPrompt)
    val output = qemu.command("tail -2 /tmp/tf")
    output should not include "aaa"
    output should include("bbb")
    output should include("ccc")
  }

  "tail: pipe from cat" in {
    qemu.command("echo first > /tmp/tp", rootPrompt)
    qemu.command("echo second >> /tmp/tp", rootPrompt)
    qemu.command("echo third >> /tmp/tp", rootPrompt)
    val output = qemu.command("cat /tmp/tp | tail -1")
    output should not include "first"
    output should include("third")
  }

  "wc: count from file" in {
    qemu.command("echo hello > /tmp/wcf", rootPrompt)
    val output = qemu.command("wc /tmp/wcf")
    output should include("1")
  }

  "pipe: test_pipe 1 write" in {
    val output = qemu.command("echo x | test_pipe 1")
    output should include("A")
  }

  "pipe: test_pipe 2 writes" in {
    val output = qemu.command("echo x | test_pipe 2")
    output should include("B")
  }

  "pipe: test_pipe 3 writes" in {
    val output = qemu.command("echo x | test_pipe 3")
    output should include("C")
  }

  "pipe: test_pipe 4 writes" in {
    val output = qemu.command("echo x | test_pipe 4")
    output should include("D")
  }

  "wc: echo piped to wc" in {
    val output = qemu.command("echo asdf | wc")
    output should include("1")
  }

  "pipe: echo piped to tail" in {
    val output = qemu.command("echo asdf | tail -1")
    output should include("asdf")
  }

  "signal: ctrl-c kills foreground process" in {
    // Byte 0x03 passes through -chardev stdio,signal=off directly to COM1,
    // since Java's process pipe bypasses the host terminal.
    qemu.send("count\n")
    Thread.sleep(2000)
    qemu.send("\u0003")
    qemu.waitFor(rootPrompt)
    val ps = qemu.command("ps")
    ps should not include "count"
  }

  "ds: publish, retrieve, delete int and string" in {
    // Int round-trip
    qemu.command("ds set answer 42")
    val getAnswer = qemu.command("ds get answer")
    getAnswer should include("42")

    // String round-trip
    qemu.command("ds set greeting hello")
    val getGreeting = qemu.command("ds get greeting")
    getGreeting should include("hello")

    // Delete + retrieve should miss
    qemu.command("ds del answer")
    val afterDel = qemu.command("ds get answer")
    afterDel should include("not found")
  }

  "C program: test_c receives argc and argv" in {
    // test_c is written in C and linked with c_crt0.c (the POSIX->Sysl
    // crt0 bridge). It prints argc and joined argv[1..], returning argc
    // as exit code — exercises the C side of the POSIX argv contract.
    val output = qemu.command("test_c hello world")
    output should include("argc=3")
    output should include("hello world")
  }

  "inet: UDP loopback via test_net" in {
    // test_net opens a UDP socket on 127.0.0.1:5000, sends "hello"
    // to itself, and prints what recvfrom returned. Phase 2 also
    // exercises real-wire sendto without an explicit bind so the
    // `wire sent=5` line covers inet's auto-bind path via the
    // isolated nic server.
    val output = qemu.command("test_net")
    output should include("sent=5")
    output should include("recv=5 'hello'")
    output should include("wire sent=5")
  }

  "async RX: unsolicited UDP reaches recvfrom via virtio IRQ" in {
    // test_udp_echo binds :7777, blocks in recvfrom. The harness's
    // netdev forwards host localhost:17777 → guest:7777. Sending a
    // datagram from Scala arrives at the guest unsolicited, travels
    // through the virtio-pci INTx path (PIC IRQ → virtio_handler →
    // nic → inet → deferred recvfrom reply), and test_udp_echo
    // prints the payload. Without the IRQ path, recvfrom would
    // block forever.
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

  "musl: O_NONBLOCK on stdin returns EAGAIN before key" in {
    // Mirror of the aarch64 mnbstdin test.
    qemu.send("nbstdin\n")
    val pre = qemu.waitFor("mnbstdin: ready_for_input")
    pre should include("mnbstdin: setfl=0")
    pre should include("mnbstdin: empty=-1 errno=11")
    qemu.send("Y")
    val output = qemu.waitFor("mnbstdin: done")
    output should include("mnbstdin: woke=1 byte=89")
    output should include("mnbstdin: done")
  }

  "musl: epoll on stdin (TTY input subscriber)" in {
    // Mirror of the aarch64 estdin test.
    qemu.send("estdin\n")
    val pre = qemu.waitFor("mepoll_stdin: ready_for_input")
    pre should include("mepoll_stdin: idle=0")
    qemu.send("Z")
    val output = qemu.waitFor("mepoll_stdin: done")
    output should include("mepoll_stdin: woke=1 events=1")
    output should include("mepoll_stdin: read=1 byte=90")
    output should include("mepoll_stdin: done")
  }

  "musl: timerfd_create / settime / gettime + epoll" in {
    // Mirror of the aarch64 mtimerfd test.
    qemu.send("timerfd\n")
    val output = qemu.waitFor("mtimerfd: done")
    output should include("mtimerfd: oneshot=1 events=1")
    output should include("mtimerfd: oneshot_read=8 exp=1")
    output should include("mtimerfd: drained=-1 errno=11")
    output should include("mtimerfd: gettime_int_nsec=30000000")
    output should include("mtimerfd: done")
    val periodicLine = output.linesIterator.find(_.contains("mtimerfd: periodic_read")).getOrElse("")
    val expValue = "exp=(\\d+)".r.findFirstMatchIn(periodicLine).map(_.group(1).toInt).getOrElse(0)
    expValue should be >= 1
  }

  "musl: eventfd2 + epoll integration" in {
    // Mirror of the aarch64 meventfd test — exercises slix-musl
    // syscall 156, the new POSIX_FD_EVENTFD shim path, and the
    // EFD_SEMAPHORE counter-decrement mode.
    qemu.send("eventfd\n")
    val output = qemu.waitFor("meventfd: done")
    output should include("meventfd: empty_read=-1 errno=11")
    output should include("meventfd: after_write7=7")
    output should include("meventfd: epoll_after_write=1 events=1")
    output should include("meventfd: epoll_after_drain=0")
    output should include("meventfd: sem1=1")
    output should include("meventfd: sem2=1")
    output should include("meventfd: sem3=1")
    output should include("meventfd: sem4=-1 errno=11")
    output should include("meventfd: done")
  }

  "net: inbound ICMP Port Unreachable surfaces as -ECONNREFUSED" in {
    // test_icmperr binds a UDP socket to 127.0.0.1:7801, asks
    // inet to inject a synthetic ICMP type-3 / code-3 frame whose
    // inner UDP src port is 7801, then non-blocking recvfrom: must
    // return -111 (-ECONNREFUSED) once, then -11 (-EAGAIN) on the
    // follow-up since the error byte is one-shot.
    qemu.send("test_icmperr\n")
    val output = qemu.waitFor("test_icmperr: ok")
    output should include("test_icmperr: ok")
    output should not include "test_icmperr: expected"
    output should not include "test_icmperr: failed"
  }

  "net: NB-connect failure surfaces as SO_ERROR=ECONNREFUSED" in {
    // test_nbconfail issues a non-blocking connect (returns
    // -EINPROGRESS), then synthesises a SYN_SENT failure on the inet
    // slot via INET_CMD_TCP_INJECT_FAIL. The slot's pending_error
    // (111 = ECONNREFUSED) survives close_slot's tear-down and
    // surfaces via getsockopt(SO_ERROR) — closing the async-path side
    // of the SO_ERROR contract. Second getsockopt reads 0 (cleared).
    qemu.send("test_nbconfail\n")
    val output = qemu.waitFor("nbconfail: ok")
    output should include("nbconfail: ok")
    output should not include "nbconfail: expected"
    output should not include "nbconfail: failed"
  }

  "musl: epoll on a pipe (Phase A2 closeout)" in {
    qemu.send("epoll_pipe\n")
    val output = qemu.waitFor("mepoll_pipe: done")
    output should include("mepoll_pipe: empty=0")
    output should include("mepoll_pipe: after_write=1 events=1")
    output should include("mepoll_pipe: after_close=1 events=17")
    output should include("mepoll_pipe: done")
  }

  "timer: subscribe fires expected count in N ticks" in {
    // test_timer subscribes to a period=5 timer and waits for 10
    // notifications. Proves svc_timer_subscribe fires reliably
    // during a quiet channel — required before phase-2 can trust
    // the timer path with real TIME_WAIT parking.
    qemu.send("test_timer\n")
    val output = qemu.waitFor("test_timer: ok")
    output should include("test_timer: subscribed idx=")
    output should include("test_timer: got 10 wakes value=2 delta=")
    output should include("test_timer: ok")
    output should not include "test_timer: FAIL"
  }

  "tcp: connect, send, receive echo, close" in {
    // Host-side TCP echo server on 127.0.0.1:18080. QEMU's
    // user-mode networking routes guest dials of 10.0.2.2:18080
    // to the host's matching port, so no hostfwd is needed for
    // outbound. test_tcp sends "ping\n", expects it echoed
    // back, then closes.
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

  // RST-on-unsolicited-SYN is implemented in inet_proto.lsysl but
  // can't be validated through QEMU's user-mode slirp (see the
  // mirrored note in Aarch64NshTests). Needs tap networking or a
  // guest-side pcap to assert the outbound RST frame.

  "tcp: passive open, accept, echo, close" in {
    // test_tcp_srv listens on :7890. QEMU's hostfwd=tcp::28080-:7890
    // routes host dials of 127.0.0.1:28080 into the guest. Send
    // "ping\n", receive echo, close cleanly.
    qemu.send("test_tcp_srv\n")
    qemu.waitFor("test_tcp_srv: listening fd=")

    val sock = new java.net.Socket()
    sock.setSoTimeout(10000)
    sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 5000)
    try
      val out = sock.getOutputStream
      val in  = sock.getInputStream
      out.write("ping\n".getBytes("UTF-8"))
      out.flush()
      val buf = new Array[Byte](32)
      val n = in.read(buf)
      n should be > 0
      new String(buf, 0, n, "UTF-8") should include("ping")
    finally sock.close()

    val output = qemu.waitFor("test_tcp_srv: closed")
    output should include("test_tcp_srv: accepted cfd=")
    output should include("test_tcp_srv: got ")
    output should include("test_tcp_srv: sent=")
    output should include("test_tcp_srv: closed")
  }

  "tcp: VFS listen bridge accepts optional ',backlog' suffix" in {
    // test_tcp_lsv2 exercises the parser-shape of
    // connect("tcp-listen:PORT,N").  Plain form, comma+backlog,
    // clamp-high, clamp-zero, malformed-trailer all return the
    // expected handle status — closing the VFS-bridge backlog
    // parameter item from the roadmap.
    qemu.send("test_tcp_lsv2\n")
    val output = qemu.waitFor("test_tcp_lsv2: ok")
    output should include("test_tcp_lsv2: ok")
    output should not include "test_tcp_lsv2: failed"
    output should not include "test_tcp_lsv2: unexpectedly opened"
  }

  "tcp: in-guest 127.0.0.1 loopback round trip" in {
    // test_tcp_lpbk drives the loopback fastpath added to
    // inet_tcp_emit / inet_tcp_emit_rst: client and listener live
    // in the same guest and exchange payloads over 127.0.0.1
    // without any NIC/slirp involvement.
    val output = qemu.command("test_tcp_lpbk")
    output should include("lpbk:ok")
    output should not include "lpbk:bad"
  }

  "udp: loopback gate covers 127/8 + own_ip" in {
    // The pre-existing UDP loopback shortcut only matched
    // 127.0.0.1 exactly. inet_handle_sendto now uses
    // inet_is_loopback_ip, so 127.0.0.5 and 10.0.2.15 (our
    // QEMU lease) also short-circuit through the in-memory
    // queue instead of trying ARP and silently failing.
    val output = qemu.command("test_udp_lpbk")
    output should include("udplo: ok")
    output should not include "udplo: bad"
  }

  "icmp: ping 127.0.0.1 returns immediately" in {
    // inet_send_icmp_echo_to short-circuits to inet_ping_deliver
    // when the destination is loopback — `ping 127.0.0.1` sees
    // a synthesized reply on the same tick with rtt=0 instead of
    // the request silently dropping at inet_resolve_mac.
    val output = qemu.command("ping -c 1 127.0.0.1")
    output should include("reply from 127.0.0.1")
    output should include("1 sent, 1 received")
  }

  "tcp: getsockopt(TCP_INFO) on ESTABLISHED loopback fd" in {
    // test_tcp_info opens an in-guest 127.0.0.1 connection and
    // probes getsockopt(IPPROTO_TCP, TCP_INFO). Verifies the
    // 104-byte struct is fully written, tcpi_state maps to 1
    // (TCP_ESTABLISHED), and tcpi_snd_mss decodes as a sane
    // little-endian u32.
    val output = qemu.command("test_tcp_info")
    output should include("tcpinfo: ok")
    output should not include "tcpinfo: bad"
  }

  "procid: getpid/getppid/getuid family + getrandom" in {
    // Process / thread identity syscalls + xorshift-based getrandom.
    // Slix has no multi-threading and boots root, so most return
    // 0 or 1; getrandom is best-effort and just verifies two
    // consecutive calls give different bytes.
    val output = qemu.command("test_proc_id")
    output should include("procid: ok")
    output should not include "procid: bad"
  }

  "time: clock_gettime / gettimeofday / clock_getres / nanosleep" in {
    // POSIX time syscalls fed off uptime() at 100Hz. Verifies
    // clock_getres reports 10ms, clock_gettime + gettimeofday
    // agree within 20ms, and nanosleep(50ms) advances the
    // clock by at least 40ms.
    val output = qemu.command("test_clock")
    output should include("clock: ok")
    output should not include "clock: bad"
  }

  "fs: fsync / fdatasync / sync / syncfs no-op stubs" in {
    // No on-disk persistence yet; these return 0 (or -EBADF for
    // bad fds) so defensive sqlite/log-writer patterns don't
    // crash on -ENOSYS.
    val output = qemu.command("test_fsync")
    output should include("fsync: ok")
    output should not include "fsync: bad"
  }

  "sockopt: SO_TYPE/DOMAIN/PROTOCOL/ACCEPTCONN" in {
    // test_sockinfo verifies the four read-only introspection
    // getsockopts the shim now reports off the fd kind +
    // is_listen flag. Three fds: UDP, TCP pre-listen, TCP
    // post-listen.
    val output = qemu.command("test_sockinfo")
    output should include("sockinfo: ok")
    output should not include "sockinfo: bad"
  }

  "ip: fragmentation reassembly self-test" in {
    // test_ip_reasm triggers inet's IPv4 reassembly self-test via a
    // dedicated IPC op. Three IPv4 fragments of a 32-byte UDP
    // datagram are injected through inet_handle_frame in
    // out-of-order sequence (frag 2, frag 3, frag 1); the reorder
    // bitmap must complete the assembly and route the resulting
    // UDP datagram to a socket bound to port 9100 — this test
    // program. recvfrom then validates the body bytes
    // ('A'x8 + 'B'x8 + 'C'x8).
    val output = qemu.command("test_ip_reasm")
    output should include("test_ip_reasm: ok")
    output should not include "test_ip_reasm: failed"
  }

  "ip: fragmentation RFC corners (overlap + timeout)" in {
    // test_ip_reasm2 covers two RFC corners of the reassembly path:
    //   1. RFC 5722 overlap-fragment drop — a fragment overlapping
    //      a previously received range must poison the slot.
    //   2. ICMP Time Exceeded emit on RFC 791 30-s timeout — the
    //      scan sweep must call inet_send_icmp_time_exceeded for
    //      slots that timed out with have_first=1.
    val output = qemu.command("test_ip_reasm2")
    output should include("test_ip_reasm2: ok")
    output should not include "test_ip_reasm2: failed"
  }

  "ip: egress fragmentation respects PMTU cache" in {
    // First half of Option B (deferred-queue priority #1): RFC 791
    // §3.2 IP fragmentation on the UDP egress path. test_frag seeds
    // a per-destination PMTU cap of 300 for the limited-broadcast
    // address via INET_CMD_PMTU_INJECT_TEST, reads the running
    // fragment-egress counter, sends a 1000-byte sendto to that
    // destination, and verifies the counter advanced by exactly 4
    // (ceil((8 + 1000) / 280)). Broadcast bypasses ARP so the test
    // is independent of slirp ARP timing.
    val output = qemu.command("test_frag")
    output should include("test_frag: ok")
    output should not include "test_frag: failed"
    output should not include "test_frag: expected"
  }

  "ip: ICMP-driven PMTU cache update (RFC 1191)" in {
    // Second half of Option B Session 2: handle_icmp_dest_unreach
    // recognizes ICMP type 3 code 4 ("Fragmentation Needed") and
    // pulls the Next-Hop MTU out of the ICMP body to populate the
    // per-destination PMTU cache. test_pmtuicmp drives the parser
    // via INET_CMD_PMTU_DISCOVER_INJECT (synthesizes a real ICMP
    // type-3 code-4 frame, feeds through the dest-unreach
    // consumer), then reads back inet_pmtu_get to assert the
    // cache learned the value.
    val output = qemu.command("test_pmtuicmp")
    output should include("pmtuicmp: ok")
    output should not include "pmtuicmp: bad"
  }

  "ip: TCP MSS clamps to PMTU cache (RFC 1191 §5)" in {
    // Closing the loop: TCP's drain emits segments sized against
    // inet_pmtu_get(remote_ip), so a Frag-Needed ICMP that says
    // "the path to that peer holds 350 bytes" forces the next
    // segments to MSS = 350 - 20(IP) - 40(TCP worst-case) = 290
    // bytes. test_pmtutcp opens a 127.0.0.1 loopback TCP, injects
    // ICMP code-4 with MTU=350, sends 900 bytes, and asserts the
    // data-segment emit count rose by ≥3 — without the clamp the
    // existing TCP_MAX_SEG=480 ceiling would yield only 2.
    val output = qemu.command("test_pmtutcp")
    output should include("pmtutcp: ok")
    output should not include "pmtutcp: bad"
  }

  "unix: SOCK_DGRAM bind/sendto/recvfrom round-trip" in {
    // Option C Session 1 (AF_UNIX basics): two DGRAM sockets bind
    // to distinct path keys in the unix server's registry; one
    // sendtos a 32-byte deterministic payload, the other recvfroms
    // it. Validates path collision detection (-EADDRINUSE on dup
    // bind), source-path round-trip in recvfrom, and -ECONNREFUSED
    // on sendto to an unbound key.
    val output = qemu.command("test_unixdg")
    output should include("unixdg: ok")
    output should not include "unixdg: bad"
  }

  "unix: SOCK_STREAM listen/accept/send/recv + EOF on close" in {
    // Option C Session 1 (AF_UNIX basics): listener binds + listens
    // on a path; client connects (which queues a server-end slot in
    // the listener's backlog and returns immediately); accept drains
    // the queue. Bidirectional 16/12-byte exchange validates the
    // paired rx_buf rings. Final close on the connector triggers a
    // 0-byte recv on the server-end (clean EOF).
    val output = qemu.command("test_unixstr")
    output should include("unixstr: ok")
    output should not include "unixstr: bad"
  }

  "unix: AF_UNIX via POSIX socket()/bind()/listen()/accept()/write()/read()" in {
    // Option C Session 1.5: AF_UNIX reachable through the standard
    // POSIX syscall surface. sys_socket(AF_UNIX,...) routes via the
    // shim into the unix server with POSIX_FD_UNIX_SOCKET=10 fd
    // kind. Validates DGRAM round-trip (sockaddr_un + recvfrom-out
    // peer path) plus STREAM listen/connect/accept/write/read +
    // half-close → 0-byte read.
    val output = qemu.command("test_punix")
    output should include("posixunix: ok")
    output should not include "posixunix: bad"
  }

  "unix: SCM_RIGHTS fd-passing via sendmsg/recvmsg" in {
    // Option C Session 2: AF_UNIX SCM_RIGHTS fd-passing for DGRAM
    // sockets. sendmsg(... cmsg=SCM_RIGHTS([gamma])) routes through
    // the shim's sys_sendmsg_unix → UNIX_CMD_SENDMSG; recvmsg builds
    // a fresh posix_fd entry per transferred slot and writes a
    // SOL_SOCKET/SCM_RIGHTS cmsg into msg_control. Verifies the
    // received fd is functional by binding it to a fresh path and
    // round-tripping a probe datagram through it.
    val output = qemu.command("test_punixscm")
    output should include("punixscm: ok")
    output should not include "punixscm: bad"
  }

  "unix: non-blocking accept/recvfrom/recv via O_NONBLOCK + MSG_DONTWAIT" in {
    // AF_UNIX non-blocking variants (UNIX_CMD_ACCEPT_NB / RECVFROM_NB /
    // RECV_NB). Validates that empty queues return -EAGAIN instead of
    // parking. Covers SOCK_NONBLOCK at socket-create, fcntl-driven
    // O_NONBLOCK toggle, and per-call MSG_DONTWAIT override on a
    // blocking fd. After data arrives, the same NB fd drains it
    // normally — no spurious EAGAIN once the queue is non-empty.
    val output = qemu.command("test_unixnb")
    output should include("unixnb: ok")
    output should not include "unixnb: bad"
  }

  "unix: accept() returns connector's bound path in peer sockaddr_un" in {
    // Connector's bound_path is now propagated through CONNECT to the
    // server-end slot; ACCEPT reply carries plen + path. Bound
    // connector ("APC1") round-trips with addrlen=7 (family + 4 path
    // bytes + NUL). Unbound connector reports empty path with
    // addrlen=2 — matches Linux's "anonymous client" convention.
    val output = qemu.command("test_unixapth")
    output should include("unixapth: ok")
    output should not include "unixapth: bad"
  }

  "unix: SCM_RIGHTS recvmsg cleans up unix-server slot on EMFILE" in {
    // Fills posix_fd_table via eventfd2 so the cmsg-transferred slot
    // can't get a new posix_fd. The shim now issues UNIX_CMD_CLOSE
    // for the slot — combined with the original sender's close, the
    // unix-server slot is fully released and its bound_path cleared.
    // Verified by re-binding the same path on a fresh socket: would
    // fail with EADDRINUSE if the leak fix were missing.
    val output = qemu.command("test_unixemf")
    output should include("unixemf: ok")
    output should not include "unixemf: bad"
  }

  "unix: recvfrom drain releases SCM_RIGHTS queue-refs" in {
    // sendmsg with cmsg fds enqueues; recvfrom drains the entry
    // (silently dropping ancillary). unix_dg_dequeue_reply now
    // releases each fd's queue-ref so the underlying slot can be
    // orphan-freed when its last owner closes. Verified by
    // re-binding the same path on a fresh socket post-close.
    val output = qemu.command("test_unixsmrf")
    output should include("unixsmrf: ok")
    output should not include "unixsmrf: bad"
  }

  "unix: SCM_RIGHTS duplicate-fd in cmsg yields N aliased posix_fds" in {
    // sendmsg with cmsg=[gamma, gamma]; recvmsg gives the receiver
    // 2 distinct posix_fds, both pointing to gamma's slot. close on
    // one keeps the other valid (sys_close's posix_fd_other_dup_exists
    // suppresses the IPC); close on the last triggers slot cleanup.
    // Regression test for the existing dup-detection mechanism.
    val output = qemu.command("test_unixdup")
    output should include("unixdup: ok")
    output should not include "unixdup: bad"
  }

  "unix: ppoll(2) on AF_UNIX socket fires on data arrival" in {
    // New UNIX_CMD_POLL handler returns the standard EPOLLIN / EPOLLOUT
    // / EPOLLHUP mask for AF_UNIX fds. shim's epoll_query_ready routes
    // through it, so ppoll(2) and select(2) on AF_UNIX fds now fire
    // correctly. EPOLLOUT is always set (slix doesn't enforce send-side
    // backpressure); EPOLLIN follows queue/buffer state.
    val output = qemu.command("test_unixpoll")
    output should include("unixpoll: ok")
    output should not include "unixpoll: bad"
  }

  "unix: epoll_ctl/epoll_wait on AF_UNIX with SUB/UNSUB plumbing" in {
    // UNIX_CMD_EPOLL_SUB/UNSUB/INST_CLOSE handlers + shim
    // epoll_unix_subscribe/unsubscribe/inst_close. epoll_ctl_add on
    // an AF_UNIX fd registers a row in the unix server's subscriber
    // table; sendto fires both the level-triggered POLL path and a
    // notify_send_to to the epoll-owning thread. epoll_pwait returns
    // the POLLIN event; close(epfd) drops the subscription.
    val output = qemu.command("test_unixwk")
    output should include("unixwk: ok")
    output should not include "unixwk: bad"
  }

  "unix: non-blocking recvmsg via O_NONBLOCK + UNIX_CMD_RECVMSG_NB" in {
    // recvmsg used to return -EAGAIN unconditionally on empty queue
    // (no parking). Now it parks by default; the shim selects
    // UNIX_CMD_RECVMSG_NB when the fd is O_NONBLOCK or the call
    // passes MSG_DONTWAIT. After data + cmsg arrive via sendmsg,
    // recvmsg drains correctly with cmsg fds intact.
    val output = qemu.command("test_unixrmnb")
    output should include("unixrmnb: ok")
    output should not include "unixrmnb: bad"
  }

  "unix: SCM_RIGHTS recvmsg EMFILE-mid-loop with duplicate sidx" in {
    // Edge case: cmsg=[gamma, gamma] with posix_fd_table almost
    // full. First alloc succeeds, second fails. The previous
    // implementation called UNIX_CMD_CLOSE on the failed alloc,
    // which stripped the receiver from gamma's owners and
    // dangled the first successful posix_fd. Two-pass fix only
    // closes orphan sidx (no successful alloc in this batch).
    val output = qemu.command("test_unixedf")
    output should include("unixedf: ok")
    output should not include "unixedf: bad"
  }

  "unix: getsockname/getpeername round-trip" in {
    // STREAM bind+getsockname; STREAM connect+accept where
    // getsockname(server_end) returns the listener's path and
    // getpeername(server_end) returns the connector's bound
    // path; getpeername on the client returns the listener's
    // path. DGRAM unconnected → -ENOTCONN. Unbound socket →
    // empty path with addrlen=2.
    val output = qemu.command("test_unixname")
    output should include("unixname: ok")
    output should not include "unixname: bad"
  }

  "unix: socketpair STREAM + DGRAM bidirectional" in {
    // socketpair(AF_UNIX, SOCK_STREAM/DGRAM, 0): two distinct fds
    // with bidirectional payload. STREAM sees EOF on one-side
    // close + SOCK_NONBLOCK propagates (fcntl F_GETFL) and an
    // empty read returns -EAGAIN. DGRAM uses peer_idx as default
    // destination so write/read round-trips both ways. AF_INET
    // socketpair → -EAFNOSUPPORT.
    val output = qemu.command("test_unixpair")
    output should include("unixpair: ok")
    output should not include "unixpair: bad"
  }

  "unix: shutdown SHUT_WR/RD/RDWR + bad-how + DGRAM no-op" in {
    // shutdown(SHUT_WR) drains buffered data then peer reads EOF;
    // subsequent write on the shut-down fd returns -EPIPE.
    // shutdown(SHUT_RD) returns 0 immediately on read even with
    // peer-queued data. SHUT_RDWR is the composite. how>2 →
    // -EINVAL; non-socket fd → -EBADF; DGRAM is a no-op (matches
    // Linux + the existing UDP path).
    val output = qemu.command("test_unixshut")
    output should include("unixshut: ok")
    output should not include "unixshut: bad"
  }

  "unix: abstract namespace bind/connect/getsockname round-trip" in {
    // Linux abstract sockets (sun_path[0] == NUL): the leading
    // NUL plus the body is the key. Verifies STREAM bind+listen+
    // connect+accept on \0abs1, getsockname returns the leading
    // NUL form (no trailing NUL, addrlen = 2 + plen), abstract
    // and filesystem-style namespaces are disjoint, and DGRAM
    // sendto/recvfrom against an abstract address works.
    val output = qemu.command("test_unixabs")
    output should include("unixabs: ok")
    output should not include "unixabs: bad"
  }

  "unix: DGRAM connect() sets default peer" in {
    // connect(2) on a DGRAM AF_UNIX fd records peer_idx so
    // subsequent write(2) routes there without sendto. Re-
    // connect overwrites the default peer; unconnected write →
    // -ENOTCONN; connect to a non-existent or wrong-stype slot
    // → -ECONNREFUSED.
    val output = qemu.command("test_unixdgc")
    output should include("unixdgc: ok")
    output should not include "unixdgc: bad"
  }

  "unix: SO_PEERCRED on STREAM" in {
    // getsockopt(SOL_SOCKET, SO_PEERCRED) returns struct ucred
    // {pid, uid, gid}. Both ends of a connected pair report the
    // creating process's pid (resolved via svc_get_thread_pid
    // from the peer slot's owners[0]). uid/gid are 0 today —
    // slix has no inter-thread uid query primitive yet.
    // Disconnected STREAM → -ENOTCONN; DGRAM → -EINVAL.
    val output = qemu.command("test_unixcred")
    output should include("unixcred: ok")
    output should not include "unixcred: bad"
  }

  "unix: STREAM SCM_RIGHTS fd-passing" in {
    // sendmsg/recvmsg with cmsg=[fd] on a connected SOCK_STREAM
    // pair. The cmsg anchors at the byte position of the
    // sendmsg call (peer.rx_total_drained + peer.rx_count); a
    // recvmsg whose drain crosses the anchor receives the fds
    // and gets a fresh aliased posix_fd. Verifies a
    // close(original)+bind(aliased) round-trip plus a probe
    // sendto/recvfrom to confirm the aliased fd is fully usable.
    val output = qemu.command("test_unixscms")
    output should include("unixscms: ok")
    output should not include "unixscms: bad"
  }

  "unix: STREAM SCM_RIGHTS carries TCP fd (Phase 2B chunk 3)" in {
    // Pass an accepted TCP fd through AF_UNIX SCM_RIGHTS, close
    // the original BEFORE the receiver drains, then read previously
    // queued bytes via the resurrected fd. Exercises the inet
    // queued_refs counter (INET_CMD_QUEUE_REF +1/-1 surrounding
    // SENDMSG/RECVMSG drain) and the receiver-side ADD_OWNER fanout.
    val output = qemu.command("test_unixscma")
    output should include("unixscma: ok")
    output should not include "unixscma: bad"
  }

  "unix: STREAM SCM_RIGHTS carries file fd (Phase 2B chunk 4)" in {
    // Pass a /etc/passwd file fd through AF_UNIX SCM_RIGHTS, close
    // the original BEFORE the receiver drains, then read "root:" via
    // the resurrected fd. Exercises vfs's oft_queued_refs counter
    // (VFS_CMD_OFT_QUEUE_REF_BY_HANDLE +1, REGISTER_FD_BY_OFT, and
    // OFT_QUEUE_REF -1 surrounding the SENDMSG/RECVMSG drain).
    val output = qemu.command("test_unixscmf")
    output should include("unixscmf: ok")
    output should not include "unixscmf: bad"
  }

  "vfs: lseek SEEK_END" in {
    // Validates the new VFS-side SEEK_END (whence=2) handling.
    // Opens /etc/passwd, walks SEEK_END/SEEK_SET, reads boundary
    // bytes, and confirms the negative-result guard returns
    // -EINVAL.
    val output = qemu.command("test_seek_end")
    output should include("seek_end: ok")
    output should not include "seek_end: bad"
  }

  "vfs: O_TRUNC + ftruncate" in {
    // Phase 0a item 1. Validates O_TRUNC plumbing in sys_openat,
    // sys_ftruncate (slix-musl 181), the new TFS_CMD_TRUNCATE +
    // VFS_CMD_TRUNCATE primitives, and tfs_read's hole-as-zero
    // semantics for sparse extension.
    val output = qemu.command("test_truncate")
    output should include("truncate: ok")
    output should not include "truncate: bad"
  }

  "unix: DGRAM recvfrom MSG_PEEK" in {
    // Phase 0a item 2. Validates that MSG_PEEK on AF_UNIX
    // recvfrom reads the head dgram without dequeuing — peek
    // then drain returns the same bytes twice.
    val output = qemu.command("test_upunixpk")
    output should include("upunixpk: ok")
    output should not include "upunixpk: bad"
  }

  "posix: clock_gettime variants + F_GETPIPE_SZ/F_SETPIPE_SZ" in {
    // Phase 0a items 4 + 5. clock_gettime accepts MONOTONIC_RAW,
    // BOOTTIME, TAI etc. (all alias uptime in slix); CPUTIME
    // variants return -EINVAL. F_GETPIPE_SZ returns 512 (slix's
    // fixed PIPE_BUF_SIZE); F_SETPIPE_SZ accepts <=512.
    val output = qemu.command("test_clkpipe")
    output should include("clkpipe: ok")
    output should not include "clkpipe: bad"
  }

  "posix: prlimit64 + getrlimit" in {
    // Phase 0a item 7. prlimit64 / getrlimit report slix's
    // hardcoded capacities (NOFILE=64, NPROC=32, STACK=16384,
    // CORE=0); other resources read RLIM_INFINITY. The
    // new_limit write side is silently accepted (no per-process
    // tracking).
    val output = qemu.command("test_rlimit")
    output should include("rlimit: ok")
    output should not include "rlimit: bad"
  }

  "unix: DGRAM connected-peer recv filter" in {
    // Phase 0a item 3. After connect() on an AF_UNIX DGRAM
    // socket, only datagrams from the connected peer are
    // delivered; non-peer dgrams are silently discarded by
    // the receive path. Sender's sendto() still returns
    // success (POSIX/Linux semantics).
    val output = qemu.command("test_udgflt")
    output should include("udgflt: ok")
    output should not include "udgflt: bad"
  }

  "posix: per-process fd-table exhaustion returns EMFILE" in {
    // Phase 0c chunk 1. Per-process posix-fd cap is 64 (3
    // reserved for stdin/stdout/stderr). Loops eventfd2 until
    // the next call returns -EMFILE; verifies the cap is in
    // the expected range, that the errno is exactly EMFILE,
    // and that closing a fd makes the slot reusable.
    val output = qemu.command("test_emfile")
    output should include("emfile: ok")
    output should not include "emfile: bad"
  }

  "vfs: pipe-pool exhaustion returns EMFILE" in {
    // Phase 0c chunk 2. VFS server's pipe pool is MAX_PIPES = 4.
    // The 5th pipe2() call returns -EMFILE; closing both ends
    // of a pipe frees the slot.
    val output = qemu.command("test_pipemax")
    output should include("pipemax: ok")
    output should not include "pipemax: bad"
  }

  "unix: socket-pool exhaustion returns ENOMEM" in {
    // Phase 0c chunk 3. Unix server's slot pool is
    // UNIX_MAX_SOCKETS = 16. socket(AF_UNIX, DGRAM) past the
    // cap returns -ENOMEM (the unix server's wire status, not
    // EMFILE — the per-process posix-fd cap is 64 so it never
    // bites first). Closing one socket frees the slot.
    val output = qemu.command("test_unixmax")
    output should include("unixmax: ok")
    output should not include "unixmax: bad"
  }

  "vfs: open-file-table exhaustion returns EMFILE" in {
    // Phase 0c chunk 4. VFS_MAX_OPEN_FILES = 32. Loop openat
    // until VFS replies "no slots". Pre-fix, VFS used a single
    // 0xFF status for both ENOENT and EMFILE and the shim mapped
    // both to -ENOENT; the chunk introduces a discriminated
    // reply (1=ENOENT, 2=EMFILE) so the test sees the correct
    // surface. Closing one fd frees the slot.
    val output = qemu.command("test_vfsmax")
    output should include("vfsmax: ok")
    output should not include "vfsmax: bad"
  }

  "inet: UDP per-tid socket cap returns EMFILE" in {
    // Phase 0c chunk 5. inet server enforces a per-tid UDP slot
    // cap (8) inside `inet_handle_socket`; firing before the
    // global INET_MAX_SOCKETS=32. The shim maps the inet
    // server's status!=0 reply to -EMFILE. Closing one fd frees
    // the slot.
    val output = qemu.command("test_udpmax")
    output should include("udpmax: ok")
    output should not include "udpmax: bad"
  }

  "ipc: dispatch fallback fuzz across all servers" in {
    // Phase 0c chunk 7. Sends an unknown command byte (0xFE) to
    // every named server's port (disk, tfs, fs, tty, pm, ds, nic,
    // inet, unix) and verifies each replies with -1 (0xFF) without
    // crashing or hanging. Pins the panic-free else-branch contract
    // each dispatch loop is supposed to honor.
    val output = qemu.command("test_ipcfuzz")
    output should include("ipcfuzz: ok")
    output should not include "ipcfuzz: bad"
  }

  "pm: process-table exhaustion refuses spawn until reap" in {
    // Phase 0c chunk 6. MAX_PROCESSES = 16. The test loops
    // pm_spawn("/bin/test_sleepy", ...) until pm_spawn returns
    // -1, verifies the kernel's slot allocator does refuse new
    // spawns once the table is full, then pm_kill + pm_waitpid
    // one child to reap it and confirms the next pm_spawn
    // succeeds. With ~13 slots already claimed (10 boot-module
    // servers + login + nsh + this binary) the success count is
    // small but the qualitative contract — refuse-then-reap-frees
    // — is what we pin here.
    val output = qemu.command("test_procmax")
    output should include("procmax: ok")
    output should not include "procmax: bad"
  }

  "rs: ds server crash triggers transparent restart" in {
    // Phase 0c chunk 8. The test publishes a key to ds, sends a
    // debug DS_CMD_PANIC_SELF that makes ds reply then exit(),
    // then publishes + retrieves a *different* sentinel value
    // through the same cached port id. RS's reincarnation path
    // (rs_handle_crash → rs_start_from_module → port_transfer →
    // rs_wait_ready) must hand the new ds thread the same port
    // id so the cached client port keeps working transparently.
    // A regression that broke port_transfer, the PM→RS death
    // notify, or the rs_monitor loop would either hang the test
    // (no reply ever returned) or fail the post-restart round
    // trip.
    val output = qemu.command("test_rsrestart")
    output should include("rsrestart: ok")
    output should not include "rsrestart: bad"
  }

  "tfs: 40-char filenames round-trip on TFS-v2" in {
    // Phase 0e. TFS bumped DIR_NAME_LEN 14→60 and made directories
    // span the same direct+indirect block chain files do. The test
    // creates `/tmp/longname_phase0e_abcdefghij_xyz_end` (40 chars,
    // well past v1's 14-char cap), opens it back through the FS
    // server, unlinks it, then re-opens to confirm the unlink
    // landed. A regression that reverted the wire format or kept
    // single-block dir storage would either truncate the name on
    // create or fail the open round-trip.
    val output = qemu.command("test_longname")
    output should include("longname: ok")
    output should not include "longname: bad"
  }

  "tfs: S_IFLNK symlinks round-trip via FS server" in {
    // Phase 0e (symlink piece). Creates `/tmp/lnk_phase0e` →
    // `/etc/passwd` via the new FS_CMD_SYMLINK path, reads the
    // target back through FS_CMD_READLINK, and verifies the
    // type-check on the readlink path (a regular file rejects).
    // Symlinks aren't auto-followed at the TFS or VFS layer — the
    // test only exercises the primitive.
    val output = qemu.command("test_symlink")
    output should include("symlink: ok")
    output should not include "symlink: bad"
  }

  "vma: kernel selftest of red-black VMA module" in {
    // Phase 1 chunk 1. Kernel ships `oskit/kernel/vma.lsysl` with
    // an intrusive red-black tree storing virtual memory areas
    // keyed on `vstart`. The selftest (driven via debug syscall
    // SYS_VMA_SELFTEST=86) exercises insert / point-lookup /
    // split / remove / clear-all on a fresh VMATree, including a
    // 64-VMA fan-out that forces tree rebalancing. No kernel
    // codepath consults the VMA list yet — chunks 2 and 3 wire it
    // into page-fault handling and demand paging.
    val output = qemu.command("test_vma")
    output should include("vma: ok")
    output should not include "vma: bad"
  }

  "vma2: page-fault handler routes through VMA tree" in {
    // Phase 1 chunk 2. Page-fault handler now consults the
    // per-process VMA tree before falling through to the legacy
    // "kill on fault" path. test_vma_fault registers an anonymous
    // VMA at 0x60100000 (a slot inside the user PT but outside the
    // eager-mapped region), then loads from it. The first load
    // page-faults; the handler asks vma_lookup_addr, allocates a
    // fresh zero page, installs the PTE, and returns to retry. The
    // load completes, sees zero, then a write+readback confirms the
    // page is mapped writable.
    val output = qemu.command("test_vma_fault")
    output should include("vma2: ok")
    output should not include "vma2: bad"
  }

  "vma3: demand-paged heap via PM-seeded VMA" in {
    // Phase 1 chunk 3. vm_create_process_pt no longer eager-allocates
    // 144 user pages; PM seeds two anon VMAs in every spawned child
    // (code/data/heap [0x60000000, 0x60080000) and stack
    // [0x60080000, 0x60090000)). test_vma_3 reads from 0x60050000
    // which is past every binary's segments + BSS but inside the
    // seeded heap VMA, so the very first load must fault and route
    // through vma_handle_fault. Confirms the kernel-side spawn-
    // pipeline writes (allocate-on-write vm_copy_to) and the
    // user-runtime fault path agree on what's mapped.
    val output = qemu.command("test_vma_3")
    output should include("vma3: ok")
    output should not include "vma3: bad"
  }

  "vma4: vm_copy_to refuses writes outside VMA list" in {
    // Phase 1 chunk 3.7 (Phase C of the VMA-list invariant): arch
    // vm_copy_to is now a stride+memcpy primitive — no allocate-
    // on-write. The new kernel_vm_copy_to wrapper consults the
    // destination process's VMA tree to decide whether a missing
    // page may be allocate-on-write installed. test_vma_4 drives
    // the path through the SYS_VMA_TRY_COPY (95) debug syscall:
    // an in-VMA destination (0x60050000) succeeds with rc=0 and the
    // payload bytes are readable back; an out-of-VMA destination
    // (0x70000000) is rejected with rc=-1.
    val output = qemu.command("test_vma_4")
    output should include("vma4: ok")
    output should not include "vma4: bad"
  }

  "mmap: anon mmap/munmap/mprotect end-to-end" in {
    // Phase 1 chunk 4. test_mmap calls Linux ARM64 syscalls 222
    // (mmap), 226 (mprotect), and 215 (munmap) via the POSIX shim.
    // It allocates 64KB anon at the per-process arena (starts at
    // 0x60100000), writes a stride pattern that demand-pages every
    // page, reads it back, mprotects to RO, and unmaps. Pass:
    // "mmap: ok".
    val output = qemu.command("test_mmap")
    output should include("mmap: ok")
    output should not include "mmap: bad"
  }

  "cow: anon COW end-to-end" in {
    // Phase 1 chunk 5. test_cow drives the COW infrastructure:
    // (a) the kernel-side refcount-table selftest via debug syscall
    //     SYS_COW_REFCNT_SELFTEST (89);
    // (b) the COW fault path by mmap'ing two pages, sharing one
    //     frame via SYS_COW_SHARE_SELF (90), and writing through
    //     each PTE to verify each side gets its own copy. Stands in
    //     for the chunk-6 fork() path that produces the same shape.
    val output = qemu.command("test_cow")
    output should include("cow: ok")
    output should not include "cow: bad"
  }

  "fork: parent/child anon write isolation" in {
    // Phase 1 chunk 6. test_fork mmaps an anon page, writes 0xAA,
    // forks, and lets the child write 0xBB into the same VA. The
    // parent then waits via PM and verifies its own view of the
    // page is still 0xAA — proves vma_clone_for_fork shared the
    // frame (refcount++), demoted both PTEs to RO, and the child's
    // write COW'd into a private frame so the two views decoupled.
    // Also exercises kernel_reap_user_pages on the child's exit:
    // the child's COW'd frame must drop refcount to 0 and return
    // to the pool.
    val output = qemu.command("test_fork")
    output should include("fork: ok")
    output should not include "fork: bad"
  }

  "elf: kernel selftest" in {
    // Phase 1 chunk 7. test_elf invokes SYS_ELF_SELFTEST (92), which
    // synthesizes a two-LOAD-segment ELF in a kernel buffer and runs
    // every `oskit.lib.elf` accessor against it. Locks down the
    // parser before chunk 8's `exec()` starts feeding it real ELFs.
    val output = qemu.command("test_elf")
    output should include("elf: ok")
    output should not include "elf: bad"
  }

  "execve: replace process image" in {
    // Phase 1 chunk 8. test_execve calls pm_execve("/bin/test_exectgt",
    // ["test_exectgt", "ok"]) — PM drops the caller's old VMAs, installs
    // per-PT_LOAD VMAs from the new ELF, copies the segments via
    // svc_vm_copy_to, builds a SysV argv/auxv init stack, and rewrites
    // the saved RIP/RSP via svc_execve_finalize so the calling thread
    // resumes inside test_exectgt. test_exectgt prints `execve: ok\n`
    // (the literal "ok" passed as argv[1]) — proves the new image is
    // running, the saved frame was rewritten, and argv survived the
    // address-space replacement.
    val output = qemu.command("test_execve")
    output should include("execve: ok")
    output should not include "execve: bad"
  }

  "execve: dynamically-linked hello (PT_INTERP + ld-musl)" in {
    // Phase 1 chunk 9 follow-up. /bin/test_dhello calls
    // pm_execve("/bin/dhello", ["dhello"]); /bin/dhello is a PIE C
    // program built against slix-musl with --enable-shared (slix/test/
    // build-c-dyn.sh). It has a PT_INTERP segment pointing at
    // /lib/ld-musl-x86_64.so.1, which PM detects in pm_handle_execve
    // and loads at INTERP_BASE. The auxv carries AT_BASE / AT_PHDR /
    // AT_ENTRY / AT_RANDOM / etc. so musl's ld-musl bootstrap can find
    // its program headers, run its relocations, then jump to _start in
    // the main exe (Scrt1.o), which calls __libc_start_main → main →
    // write(1, "hello dyn\n", 10).
    val output = qemu.command("test_dhello")
    output should include("hello dyn")
    output should not include "dhello: bad"
  }

  "execve: printf via libc.so under /usr/lib" in {
    // Phase 1 chunk 10. /bin/test_phello calls
    // pm_execve("/bin/phello", ["phello"]); /bin/phello is a PIE C
    // program that calls printf with sqrt(2.0)*1e6 cast to int.
    // Compared to dhello (chunk 9, single write() syscall), this
    // exercises:
    //   - libc.so resolution from /usr/lib (FHS layout) — chunk 10
    //     reorganizes the ramdisk so /lib carries only ld-musl while
    //     libc.so + libm.so live in /usr/lib. ld-musl's default search
    //     path (/lib:/usr/local/lib:/usr/lib) finds libc.so by SONAME.
    //   - Real stdio: printf → vfprintf → __stdio_write → SYS_writev,
    //     plus malloc for the FILE buffer.
    //   - sqrt() resolved from libc.so (musl unifies math into libc).
    //
    // Integer cast is deliberate: musl's vfprintf converts every
    // %a/%e/%f/%g arg from double to long double via __extenddftf2
    // (aarch64) / __extendxftf2 (x86_64), which our compiler_rt
    // stubs (chunk 9) trap on. Real builtins are a known follow-up;
    // %d sidesteps fmt_fp.
    val output = qemu.command("test_phello")
    output should include("hello printf, sqrt(2.0)*1e6 = 1414213")
    output should not include "phello: bad"
  }

  "tcp: SO_REUSEADDR overrides TIME_WAIT bind-block" in {
    val output = qemu.command("test_tcp_reuse")
    output should include("tcpreuse:ok")
    output should not include "tcpreuse:bad"
  }

  "tcp: keepalive idle/intvl fire on the right wall-clock cadence" in {
    // Regression for the keepalive unit bug: the shim used to convert
    // TCP_KEEPIDLE / TCP_KEEPINTVL seconds → 10-ms-ticks (`secs * 100`)
    // before forwarding to inet, but inet stores those fields in
    // `monotonic_ms` milliseconds. So setting KEEPIDLE=2s effectively
    // landed as 200 ms — ~10× too fast a fire.
    //
    // test_tkeep arms the slix-only ACK blackhole on the server-child
    // slot, then sets KEEPIDLE=2s / KEEPINTVL=1s / KEEPCNT=2 on the
    // client. Expected abort time ≈ 4 s. Pre-fix it was ~400 ms.
    val output = qemu.command("test_tkeep")
    output should include("tkeep: ok elapsed=")
    output should not include "tkeep: bad"
  }

  "unix: AF_UNSPEC connect dissolves DGRAM peer" in {
    // Linux's `connect(fd, sin_family=AF_UNSPEC, ...)` clears
    // the DGRAM socket's default peer; subsequent send() (no
    // addr) returns -ENOTCONN while sendto(addr) keeps working.
    // STREAM dissolve attempt → -EINVAL.
    val output = qemu.command("test_unixdiss")
    output should include("unixdiss: ok")
    output should not include "unixdiss: bad"
  }

  "pm: PM_CMD_TRANSPLANT_FD across processes" in {
    // PM-mediated cross-process posix_fd transplant. Parent
    // creates an AF_UNIX socketpair, spawns a child suspended
    // via PM_CMD_SPAWN_SUSP, transplants one end into the
    // child's slot 3, resumes the child, then PINGs through
    // its own end and reads back OK\n. Exercises the new
    // SYS_FD_TRANSPLANT shim helper plus its UNIX_CMD_ADD_OWNER
    // fan-out to the unix server's owners ring.
    val output = qemu.command("test_pmtfd")
    output should include("pmtfd: ok")
    output should not include "pmtfd: bad"
  }

  "pm: TRANSPLANT_FD across processes (TCP fd)" in {
    // Phase 2A of arbitrary-kind SCM_RIGHTS. Parent listens on
    // 127.0.0.1:9200 and accepts a self-loopback connection,
    // writes PING through the client side, then transplants
    // its accepted fd into a suspended child via
    // PM_CMD_TRANSPLANT_FD. The new INET_CMD_ADD_OWNER fan-out
    // adds the child's main_tid to the slot's owners ring;
    // parent then closes its own copy and resumes the child.
    // Child reads PING from fd 3 and writes OK\n back. Verifies
    // multi-owner semantics on the inet TCP path: parent's
    // close decrements but doesn't FIN, recv buffer survives,
    // child can still operate.
    val output = qemu.command("test_pmtfd2")
    output should include("pmtfd2: ok")
    output should not include "pmtfd2: bad"
  }

  "pm: TRANSPLANT_FD across processes (file fd)" in {
    // Phase 2B chunk 1 of arbitrary-kind SCM_RIGHTS. Parent
    // open()s /etc/passwd, reads 5 bytes ("root:") so the OFT
    // pos advances to 5, then transplants the file fd into a
    // suspended child via PM_CMD_TRANSPLANT_FD. The new
    // VFS_CMD_DUP_HANDLE fan-out asks vfs to mint a fresh
    // dst-side handle pointing at the same OFT entry; child's
    // slot's `target` is rewritten to the new handle. Parent
    // closes its own fd (oft_ref drops 2->1, OFT survives),
    // resumes the child. Child reads 1 byte from fd 3 and must
    // get 'x' (offset 5 of /etc/passwd, since OFT pos is
    // shared). Verifies the per-pid handle-table allocation
    // works and that share-the-OFT-entry semantics match
    // POSIX/Linux file-fd-passing.
    val output = qemu.command("test_pmtfd3")
    output should include("pmtfd3: ok")
    output should not include "pmtfd3: bad"
  }

  "udp: 1024-byte datagram via 127.0.0.1 loopback" in {
    // Verifies the bumped UDP datagram cap (512 → 1472). Sends a
    // 1024-byte body with byte i = (i & 0xff), recvfrom-validates
    // the full body comes through. Catches truncation at the old
    // 512 boundary plus any reply-buffer overflow / underflow.
    val output = qemu.command("test_udp_big")
    output should include("udpbig: ok")
    output should not include "udpbig: bad"
  }

  "udp: connect()/send()/recv() with default peer" in {
    // POSIX connect() on UDP saves a default peer; subsequent
    // send() (sendto with NULL addr) targets it. Then dissolve
    // via connect(AF_UNSPEC) and verify send returns -ENOTCONN.
    val output = qemu.command("test_udp_conn")
    output should include("udpcon: ok")
    output should not include "udpcon: bad"
  }

  "udp: connected fd drops non-peer datagrams (recv filter)" in {
    // POSIX/Linux: a UDP fd with a saved peer (via connect())
    // drops datagrams whose source != peer. Slix enforces this
    // at recv time — sys_recvfrom recurses past non-peer
    // datagrams until a matching one arrives or EAGAIN.
    val output = qemu.command("test_udp_filt")
    output should include("udpfilt: ok")
    output should not include "udpfilt: bad"
  }

  "udp: NB recv on empty queue returns EAGAIN" in {
    // Minimal regression check: socket → bind → fcntl(NONBLOCK)
    // → recvfrom → must return -EAGAIN. Catches future
    // sys_recvfrom regressions in the empty-queue path
    // independently of the connect/filter loop.
    val output = qemu.command("test_udp_dbg")
    output should include("udpdbg: ok")
    output should not include "udpdbg: bad"
  }

  "sockopt: SO_RCVTIMEO bounded blocking recv" in {
    // setsockopt SO_RCVTIMEO = 100ms, then blocking recvfrom on
    // an empty UDP queue must return -EAGAIN within the window
    // (Linux semantics) instead of hanging forever.
    val output = qemu.command("test_so_timeo")
    output should include("sotmo: ok")
    output should not include "sotmo: bad"
  }

  "udp: MSG_DONTWAIT per-call non-blocking override" in {
    // MSG_DONTWAIT (0x40) makes a single recvfrom non-blocking
    // even on a blocking fd; libuv uses it to avoid the
    // fcntl(O_NONBLOCK) race when the fd is shared.
    val output = qemu.command("test_msg_dwait")
    output should include("mdwait: ok")
    output should not include "mdwait: bad"
  }

  "udp: getpeername after connect + shutdown no-op" in {
    // POSIX: getpeername on a connected UDP fd returns the saved
    // peer; pre-connect returns -ENOTCONN. shutdown on UDP is
    // accepted as a no-op (Linux compat).
    val output = qemu.command("test_udp_pname")
    output should include("udppeer: ok")
    output should not include "udppeer: bad"
  }

  "tcp: recvfrom with MSG_DONTWAIT" in {
    // sys_recvfrom now accepts TCP fds (previously -EBADF) and
    // honors MSG_DONTWAIT for per-call NB. The pre-data recv
    // returns -EAGAIN; after write, recv returns the bytes.
    val output = qemu.command("test_tcp_dwait")
    output should include("tcpdw: ok")
    output should not include "tcpdw: bad"
  }

  "tcp: sendto / send() round-trip" in {
    // sys_sendto now accepts TCP fds — libc lowers send() to
    // sendto(NULL, 0). Previously rejected with -EBADF.
    val output = qemu.command("test_tcp_send")
    output should include("tcpsnd: ok")
    output should not include "tcpsnd: bad"
  }

  "io: writev / readv vectored I/O" in {
    // POSIX writev / readv. libc stdio buffer flushes use
    // writev (header + body iovs); libuv uses both for TCP.
    // Body walks iovec[] and dispatches per-segment to
    // sys_write / sys_read.
    val output = qemu.command("test_iov")
    output should include("iov: ok")
    output should not include "iov: bad"
  }

  "io: pread64 preserves file position" in {
    // POSIX pread reads at offset without disturbing the fd's
    // current pos. Slix synthesizes via save/seek/read/restore
    // of VFS file pos.
    val output = qemu.command("test_pread")
    output should include("pread: ok")
    output should not include "pread: bad"
  }

  "misc: madvise / sched_yield / prctl / getrusage stubs" in {
    // Defensive syscall stubs. libc startup, jemalloc, glibc
    // compat layers all probe these routinely; -ENOSYS would
    // crash or push them onto slow fallback paths.
    val output = qemu.command("test_stubs")
    output should include("stubs: ok")
    output should not include "stubs: bad"
  }

  "fs: faccessat path-exists probe" in {
    // POSIX faccessat. Slix has no real permission model so
    // F_OK / R_OK / W_OK / X_OK collapse into "VFS opens it".
    // -ENOENT for missing paths.
    val output = qemu.command("test_access")
    output should include("access: ok")
    output should not include "access: bad"
  }

  "net: TCP send-buf parking (Phase 1 quality)" in {
    // Phase 1 of the net-stack quality plan: inet_handle_tcp_send
    // now parks the caller in send_waiter when send_buf is full,
    // and the ACK-handling path wakes the parked caller via
    // inet_tcp_drain_send_waiter once any space frees.  The
    // self-test drives the state machine synthetically (no real
    // TCP connection): force ESTABLISHED + full send_buf, park a
    // fake waiter, drain, verify the helper clears the slot.
    val output = qemu.command("test_tcp_park")
    output should include("tcp_park: ok")
    output should not include "tcp_park: failed"
  }

  "net: ARP-driven TCP retransmit (Phase 1 quality)" in {
    // Phase 1 chunk 2: inet_tcp_emit now records arp_pending=1
    // when the next-hop MAC isn't cached, and inet_arp_drain_pending
    // walks TCP on a fresh ARP entry, clearing the flag and
    // re-emitting via inet_tcp_retransmit.  Saves a full RTO on
    // the first SYN to any off-cache peer.
    val output = qemu.command("test_arpretx")
    output should include("arpretx: ok")
    output should not include "arpretx: failed"
  }

  "net: per-tid socket cap (Phase 2 quality)" in {
    // Phase 2 chunk 1: a single tid can hold at most
    // INET_PER_PID_SOCKET_CAP (8) slots across UDP + TCP pools.
    // The 9th allocation fails with -EMFILE; closing one frees a
    // slot so the next allocation succeeds.
    val output = qemu.command("test_pidcap")
    output should include("pidcap: ok")
    output should not include "pidcap: failed"
  }

  "net: per-tid TCP buffer-memory cap (Phase 2 quality)" in {
    // Phase 2 chunk 2: TCP_PER_TID_BUF_MAX = 65536 bytes caps a
    // single tid's combined send_buf_size + recv_buf_size across
    // every owned TCP slot.  Four full-size (8 KB + 8 KB) listeners
    // exhaust the budget; the fifth listen() must fail.  Closing
    // one frees 16 KB, allowing the fifth retry to succeed.
    val output = qemu.command("test_tcpbufcap")
    output should include("tcpbufcap: ok")
    output should not include "tcpbufcap: failed"
  }

  "net: PID_EXIT cleanup audit (Phase 2 quality)" in {
    // Phase 2 chunk 3: PID_EXIT now also reclaims the auxiliary
    // tables that hold owner_tid — the 4-slot ICMP ping pending
    // table and the 2-slot ARP-pending UDP queue.  The self-test
    // parks one of each under a synthetic owner_tid and verifies
    // the cleanup helpers free them.
    val output = qemu.command("test_pidxclean")
    output should include("pidxclean: ok")
    output should not include "pidxclean: failed"
  }

  "net: GET_STATS introspection (Phase 2 quality)" in {
    // Phase 2 chunk 4: INET_CMD_GET_STATS returns the calling tid's
    // tcp_count / udp_count / tcp_buf_bytes.  The test starts at
    // zero, opens one UDP and one TCP listener, observes the
    // counters move, then closes both and confirms the counters
    // return to zero.  Closes Phase 2 of the netstack-quality plan.
    val output = qemu.command("test_netstats")
    output should include("netstats: ok")
    output should not include "netstats: failed"
  }

  "net: frame buffer pool (Phase 3 chunk 1)" in {
    // Phase 3 chunk 1: replace per-handler `var frame: [1514]byte`
    // with a small reusable pool. INET_CMD_FRAME_POOL_STATS reports
    // (used, high, cap, fails). Test verifies a UDP loopback round-
    // trip pushes high-water above the baseline (proves multiple
    // pool slots got used) and that `used` returns to baseline
    // afterward (no leaks). `fails` must stay zero.
    val output = qemu.command("test_framepool")
    output should include("framepool: ok")
    output should not include "framepool: failed"
  }

  "net: UDP socket lookup hash (Phase 3 chunk 2)" in {
    // Phase 3 chunk 2: replace inet_find_bound + inet_find_bound_port
    // linear scans with a 16-bucket hash chained on (port & 0xf).
    // Test stresses both the spread case (8 sockets in 8 distinct
    // buckets) and the collision case (8 sockets all in bucket 0),
    // then verifies that close() correctly unlinks so a re-bind
    // succeeds without phantom "port in use" rejections.
    val output = qemu.command("test_udphash")
    output should include("udphash: ok")
    output should not include "udphash: failed"
  }

  "net: TCP 4-tuple hash routing (Phase 3 chunk 3)" in {
    // Phase 3 chunk 3: replace inet_tcp_find's linear scan with a
    // hash chained on (local_port ^ remote_port ^ low16(remote_ip)).
    // Test opens a listener and 3 simultaneous loopback connections,
    // then runs a per-connection round-trip with a unique tag byte.
    // If hash routing were broken (wrong slot, missing remove on
    // close), tag mismatch would surface here.
    val output = qemu.command("test_tcphash")
    output should include("tcphash: ok")
    output should not include "tcphash: failed"
  }

  "net: TCP listen hash routing (Phase 3 chunk 4)" in {
    // Phase 3 chunk 4: hash inet_tcp_find_listen on local_port.
    // Test opens 3 listeners at colliding bucket-8 ports
    // (9000/9016/9032), then loops sequential connect-accept-tag
    // round-trips through each. A buggy chain walk that returns
    // first-match-regardless-of-port would route all SYNs to the
    // first listener, hanging the second/third tcp_accept.
    val output = qemu.command("test_tlhash")
    output should include("tlhash: ok")
    output should not include "tlhash: failed"
  }

  "fs: /etc/hosts is pre-populated (Phase 4 chunk 1a)" in {
    // Mirror of the aarch64 chunk 1a check. The ramdisk maker
    // pre-populates /etc/hosts so musl's name_from_hosts() can
    // find localhost without DNS. The actual musl getaddrinfo
    // round-trip is gated on slix-musl malloc support — see
    // the ignored "musl: getaddrinfo via /etc/hosts" entry.
    val output = qemu.command("cat /etc/hosts")
    output should include("127.0.0.1 localhost")
    output should include("::1 localhost")
  }

  "musl: getaddrinfo via /etc/hosts (Phase 4 chunk 1a)" in {
    // Mirror of the aarch64 mgetaddr test. Same path through musl's
    // name_from_hosts and oldmalloc/brk-only allocator.
    qemu.send("mgetaddr\n")
    val output = qemu.waitFor("mgetaddr: ok")
    output should include("mgetaddr: localhost -> 127.0.0.1")
    output should include("mgetaddr: ok")
    output should not include "mgetaddr: failed"
  }

  "crash recovery: kill tfs and restart" in {
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

  "musl: write(1, ...) + read(0, ...) + exit" in {
    // Mirror of the aarch64 mhello test. The C source is shared
    // (slix/test/hello.c) — only the build target differs. Validates
    // the x86 POSIX shim's SYS_WRITE/SYS_READ/SYS_EXIT_GROUP path and
    // the musl __set_thread_area override that lands TLS via WRFSBASE.
    qemu.send("mhello\n")
    qemu.waitFor("hello from musl")
    qemu.send("X")
    val output = qemu.waitFor("read=1")
    output should include("read=1")
  }

  "musl: socket/connect/shutdown/read via libc wrappers" in {
    // Mirror of the aarch64 msocket test. Host peer on
    // 127.0.0.1:18083 reads until EOF and replies with the byte
    // count; guest dials it via slirp's outbound NAT.
    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18083))
    server.setSoTimeout(15000)

    val peerThread = new Thread(() => {
      try
        val client = server.accept()
        try
          val in  = client.getInputStream
          val out = client.getOutputStream
          val buf = new Array[Byte](256)
          var total = 0
          var n = in.read(buf, total, buf.length - total)
          while n > 0 do
            total += n
            n = in.read(buf, total, buf.length - total)
          val reply = s"got $total bytes".getBytes
          out.write(reply)
          out.flush()
          Thread.sleep(100)
        finally client.close()
      catch
        case _: Throwable => ()
    }, "tcp-msocket-peer-x86")
    peerThread.setDaemon(true)
    peerThread.start()

    try
      qemu.send("msocket\n")
      val output = qemu.waitFor("msocket: done")
      output should include("msocket: socket=3")
      output should include("msocket: connect=0")
      output should include("msocket: getsockname=0 family=2")
      output should include("msocket: write=23")
      output should include("msocket: shutdown=0")
      output should include("msocket: read=12 reply='got 23 bytes'")
      output should include("msocket: done")
    finally
      server.close()
      peerThread.join(2000)
  }

  "musl: open/read/lseek/close on /etc/passwd" in {
    qemu.send("mfile\n")
    val output = qemu.waitFor("mfile: done")
    output should include("mfile: open=3")
    output should include("mfile: read=")
    output should not include "mfile: read=0"
    output should not include "mfile: read=-"
    output should include("mfile: lseek=0")
    output should include("mfile: read2=16")
    output should include("mfile: done")
  }

  "musl: stat / lstat / fstat (Phase 4 chunk 2)" in {
    // mstat (slix/test/stat.c) exercises SYS_fstat (178) and
    // SYS_newfstatat (252) through musl's stat/lstat/fstat
    // wrappers. Validates the per-arch kstat layout (x86_64 here)
    // and the shim's TFS-mode → Linux S_IF* translation.
    qemu.send("mstat\n")
    val output = qemu.waitFor("mstat: ok")
    output should include("mstat: stat /etc/passwd size=")
    output should include("reg=1 dir=0")
    output should not include "size=0 reg=1"
    output should include("mstat: lstat /etc/passwd")
    output should include("mstat: stat /etc size=")
    output should include("reg=0 dir=1")
    output should include("mstat: fstat fd=")
    output should include("mstat: stat /no/such missing=1")
  }

  "musl: MSG_PEEK on UDP" in {
    // mpeek (slix/test/mpeek.c) walks the new MSG_PEEK path on UDP:
    // bind, sendto self, recv with MSG_PEEK (queue stays at 1), recv
    // with no flags returns the same bytes (queue drains to 0), and
    // a third non-blocking recv returns -EAGAIN. Tests both the shim
    // wire-format change (peek byte appended to RECVFROM /
    // RECVFROM_TIMEOUT) and inet's split deliver-head path.
    qemu.send("mpeek\n")
    val output = qemu.waitFor("mpeek: done")
    output should include("mpeek: bind=0")
    output should include("mpeek: sendto=7")
    output should include("mpeek: peek=7 data='PEEK-OK'")
    output should include("mpeek: recv=7 data='PEEK-OK'")
    output should include("mpeek: match=1")
    output should include("mpeek: drained=EAGAIN errno=11")
  }

  "musl: SO_SNDTIMEO enforcement on TCP" in {
    // msndto (slix/test/msndto.c) — see Aarch64NshTests for full
    // notes.  Same-process TCP loopback pair, 16 KB payload with
    // 200 ms SO_SNDTIMEO, asserts partial-then-EAGAIN.
    qemu.send("msndto\n")
    val output = qemu.waitFor("msndto: done")
    output should include("msndto: bind=0")
    output should include("msndto: listen=0")
    output should include("msndto: setsockopt_sndtimeo=0")
    output should include("msndto: connect=0")
    output should include("msndto: pass=1")
  }

  "musl: TCP_USER_TIMEOUT enforcement (RFC 5482)" in {
    // musrto (slix/test/musrto.c) — see Aarch64NshTests for full
    // notes.  Single-process loopback with TCP_USER_TIMEOUT=200ms,
    // ACK-blackholed accepted child to make the loopback fastpath
    // simulate a peer gone dark, asserts ETIMEDOUT on the next send.
    qemu.send("musrto\n")
    val output = qemu.waitFor("musrto: done")
    output should include("musrto: bind=0")
    output should include("musrto: listen=0")
    output should include("musrto: setsockopt_userto=0")
    output should include("musrto: getsockopt_userto=0 val=200")
    output should include("musrto: connect=0")
    output should include("musrto: blackhole=0")
    output should include("musrto: pass=1")
  }

  "musl: tar extract end-to-end (Phase 4 chunk 5)" in {
    // muntar (slix/test/untar.c) — same script as the aarch64
    // entry. Bit-identical /test.tar bytes (TestTar.bytes is
    // arch-neutral); the OS-side path here is x86_64's full musl
    // → shim → VFS → TFS chain for mkdir + open(O_CREAT) + write.
    qemu.send("muntar\n")
    val output = qemu.waitFor("muntar: ok")
    output should include("muntar: open /test.tar rc=")
    output should include("muntar: dir /tmp/tx rc=0")
    output should include("muntar: file /tmp/tx/a.txt rc=0 size=6")
    output should include("muntar: file /tmp/tx/b.txt rc=0 size=7")
    output should include("muntar: stat /tmp/tx dir=1")
    output should include("muntar: stat /tmp/tx/a.txt size=6 reg=1 match=1")
    output should include("muntar: stat /tmp/tx/b.txt size=7 reg=1 match=1")
  }

  "musl: open(O_CREAT) + write + readback (Phase 4 chunk 4)" in {
    // mfcreat (slix/test/fcreat.c) — same script as the aarch64
    // entry. The shim's sys_openat is arch-neutral, so this is
    // mostly a parallel-coverage check, but x86's TFS write
    // path independently exercises tfs_write block alloc.
    qemu.send("mfcreat\n")
    val output = qemu.waitFor("mfcreat: ok")
    output should include("mfcreat: write rc=12")
    output should include("mfcreat: stat after create size=12 reg=1")
    output should include("mfcreat: read back rc=12 match=1")
    output should include("mfcreat: O_EXCL on existing rc=-1 errno=17")
    output should include("mfcreat: unlink rc=0 missing=1")
  }

  "musl: mkdir / unlink / rename / chmod (Phase 4 chunk 3)" in {
    // mfsmod (slix/test/fsmod.c) exercises the four chunk-3
    // syscalls — mkdirat (227), fchmodat (167), renameat (281),
    // unlinkat (365) — through musl's libc wrappers. Same script
    // as the aarch64 NshTest entry; the shim is arch-neutral so
    // both arches walk identical code, but x86_64 stresses its
    // own struct stat layout (144 B kstat) on the verification
    // stat()s.
    qemu.send("mfsmod\n")
    val output = qemu.waitFor("mfsmod: ok")
    output should include("mfsmod: mkdir /tmp/ck rc=0 dir=1")
    output should include("mfsmod: chmod /tmp/ck 0700 mode=0700")
    output should include("rc=0 reg=1 size>0=1")
    output should include("mfsmod: stat /etc/hosts after rename missing=1")
    output should include("mfsmod: rename back rc=0")
    output should include("mfsmod: rmdir /tmp/ck rc=0 missing=1")
  }

  "musl: epoll_create1/ctl/wait on a UDP socket" in {
    qemu.send("mepoll\n")
    val output = qemu.waitFor("mepoll: done")
    output should include("mepoll: create=3")
    output should include("mepoll: bind=0")
    output should include("mepoll: ctl_add=0")
    output should include("mepoll: wait_idle=0")
    output should include("mepoll: sendto=13")
    output should include("mepoll: wait_after_send=1 events=0x00000001 data_ok=1")
    output should include("mepoll: recv=13")
    output should include("mepoll: wait_after_drain=0")
    output should include("mepoll: ctl_del=0")
    output should include("mepoll: done")
  }

  "musl: epoll EPOLLET + EPOLLONESHOT (Phase A2)" in {
    qemu.send("mepoll2\n")
    val output = qemu.waitFor("mepoll2: done")
    output should include("mepoll2: et_first=1")
    output should include("mepoll2: et_no_redeliver=0")
    output should include("mepoll2: et_second=1")
    output should include("mepoll2: oneshot_first=1")
    output should include("mepoll2: oneshot_disarmed=0")
    output should include("mepoll2: oneshot_rearmed=1")
    output should include("mepoll2: done")
  }

  "musl: non-blocking accept (Phase B)" in {
    qemu.send("mnbacc\n")
    val pre = qemu.waitFor("mnbacc: ready")
    val client = new java.net.Socket()
    client.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 5000)
    try
      client.getOutputStream.write("ping".getBytes())
      client.getOutputStream.flush()
      val post = qemu.waitFor("mnbacc: done")
      pre should include("mnbacc: empty=-1 errno=11")
      post should include("mnbacc: wait=1")
      post should not include "mnbacc: accept=-1"
      post should include("mnbacc: read=4 data='ping'")
      post should include("mnbacc: done")
    finally client.close()
  }

  "musl: non-blocking connect (Phase B)" in {
    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18080))
    server.setSoTimeout(15000)
    val echoThread = new Thread(() => {
      try
        val client = server.accept()
        try
          val in = client.getInputStream
          val out = client.getOutputStream
          val buf = new Array[Byte](64)
          val n = in.read(buf)
          if n > 0 then
            out.write(buf, 0, n)
            out.flush()
          Thread.sleep(100)
        finally client.close()
      catch case _: Throwable => ()
    }, "tcp-echo-server-mnbcon-x86")
    echoThread.setDaemon(true)
    echoThread.start()
    try
      qemu.send("mnbcon\n")
      val output = qemu.waitFor("mnbcon: done")
      output should include("mnbcon: connect=-1 errno=115")
      output should include("mnbcon: wait=1")
      output should include("mnbcon: sent=10")
      output should include("mnbcon: read=10 data='nbcon-ping'")
      output should include("mnbcon: done")
    finally
      server.close()
      echoThread.join(2000)
  }

  "musl: pipe2 + write + read + EOF" in {
    qemu.send("mpipe\n")
    val output = qemu.waitFor("mpipe: done")
    output should include("mpipe: pipe2=0")
    output should include("mpipe: write=17")
    output should include("mpipe: read=17 data='ping through pipe'")
    output should include("mpipe: read_after_close=0")
    output should include("mpipe: done")
  }

  "musl: sendmsg/recvmsg via libc wrappers" in {
    qemu.send("mmsg\n")
    val output = qemu.waitFor("mmsg: done")
    output should include("mmsg: socket=3")
    output should include("mmsg: bind=0")
    output should include("mmsg: sendmsg=12")
    output should include("mmsg: recvmsg=12 data='hello msghdr'")
    output should include("mmsg: src_port=7790")
    output should include("mmsg: done")
  }

  "musl: listen backlog enforcement (Phase F)" in {
    runBacklogTest("x86")
  }

  "musl: per-fd EPOLLET edge isolation" in {
    // mepoll_multi (slix/test/epoll_multi.c): two UDP sockets share
    // an epoll instance, both EPOLLIN | EPOLLET. Firing one must
    // not re-deliver the other. Before the per-fd fire counter
    // landed, the shim's notify-wake bulk-cleared every entry's
    // `last_reported`, so an unrelated edge re-fired siblings.
    qemu.send("epoll_multi\n")
    val output = qemu.waitFor("mepoll_multi: done")
    output should include("mepoll_multi: after_a=1 data=10")
    output should include("mepoll_multi: idle=0")
    // After the second send only B (data=11) reports — A stays
    // quiet because its last edge was already consumed.
    output should include("mepoll_multi: after_b=1 data0=11")
    output should not include "mepoll_multi: after_b=2"
    output should include("mepoll_multi: done")
  }


  // === ported from Aarch64NshTests for arch parity ===

  "boot: init auto-runs dhclient before opening logins" in {
    // D.3 init integration: init spawns /bin/dhclient, pm_waitpids
    // it, and prints the summary line before reading /etc/ttytab.
    // beforeEach already drove the boot past the "> " prompt, so
    // the full boot trace is in allOutput and we can assert the
    // marker line appears.
    qemu.allOutput should include("network: dhcp ok")
    qemu.allOutput should include("bound 10.0.2.15")
    qemu.allOutput should not include "network: dhcp failed"
    qemu.allOutput should not include "network: dhcp spawn failed"
  }


  "dhcp: dhclient --test parses canned OFFER/ACK" in {
    // dhclient --test runs the in-process parser selftest against
    // canned DHCP packets (known-good OFFER, same-layout ACK, and
    // a malformed magic-cookie negative case). No live DHCP server
    // is involved — this catches byte-layout regressions without
    // needing slirp to run a DHCP server (slirp does, but coupling
    // correctness to a separate service is brittle).
    qemu.send("dhclient --test\n")
    val output = qemu.waitFor("dhclient: selftest ok")
    output should include("dhclient: selftest ok")
    output should not include "dhclient: selftest failed"
  }


  "dhcp: live bind via slirp's DHCP server" in {
    // Exercises the full wire path end-to-end: DISCOVER out via the
    // broadcast UDP TX special-case in inet_send_udp, slirp's
    // built-in DHCP server replies OFFER (also broadcast), dhclient
    // REQUESTs, slirp ACKs, dhclient runs the RFC 5227 probe cycle
    // (clean on slirp), announces, then calls inet_set_ip_config.
    // The expected bound address under slirp's default lease table
    // is 10.0.2.15 mask 255.255.255.0 gw 10.0.2.2.
    qemu.send("dhclient\n")
    val output = qemu.waitFor("lease=")
    output should include("dhclient: bound 10.0.2.15")
    output should include("mask=255.255.255.0")
    output should include("gw=10.0.2.2")
    // Probe line proves the RFC 5227 path executed. Slirp is silent
    // on ARP probes so the window completes cleanly.
    output should include("dhclient: probing 10.0.2.15")
  }


  "dhcp: dhclient writes /etc/resolv.conf (Phase 4 chunk 1b)" in {
    // After a successful bind, dhclient persists DHCP option 6
    // (DNS server list) into /etc/resolv.conf as canonical
    // "nameserver A.B.C.D" lines. Slirp's user-mode DHCP server
    // hands out 10.0.2.3 — its built-in DNS forwarder — so a
    // single nameserver line at that address is the expected
    // result. The file is created fresh via VFS_CMD_CREATE +
    // VFS_CMD_WRITE; this test also validates that runtime file
    // creation under /etc works at all.
    qemu.send("dhclient\n")
    qemu.waitFor("lease=")
    // Drain dhclient's tail output and post-exit prompt before sending
    // the next command — otherwise qemu.command's waitFor("> ") matches
    // dhclient's exit prompt rather than cat's.
    qemu.waitFor("> ")
    val output = qemu.command("cat /etc/resolv.conf")
    output should include("nameserver 10.0.2.3")
  }


  "musl: getaddrinfo over slirp DNS (Phase 4 chunk 1c)" in {
    // dhclient writes /etc/resolv.conf with `options timeout:15
    // attempts:1` so musl's resolver waits up to 15 s for a reply —
    // slirp's stub DNS forwards to the host's resolver, which on a
    // cold cache or slow upstream can take several seconds. The test
    // assumes the host has working DNS (i.e. NOT offline).
    qemu.send("dhclient\n")
    qemu.waitFor("lease=")
    qemu.waitFor("> ")
    qemu.send("mdns\n")
    val output = qemu.waitFor("mdns: ok")
    output should include("mdns: example.com -> ")
    output should not include "mdns: getaddrinfo rc="
    output should not include "mdns: failed"
  }


  "dns: resolve against a mock DNS server" in {
    // Spin up a tiny mock DNS responder on 127.0.0.1:<ephemeral>.
    // The guest sends its query to 10.0.2.2:<that port> — slirp
    // forwards outbound UDP to the host's loopback, so the guest
    // and the mock never need a real network in between. Offline-
    // friendly and deterministic.
    //
    // The mock echoes the query back with QR=1 and one appended
    // A-record pointing at 1.2.3.4 via a compressed name pointer
    // to offset 12 (start of the question's QNAME). That's the
    // minimum a recursive resolver actually emits — recursive
    // resolvers almost always flatten CNAMEs into the same
    // response so a client that only looks at answer records can
    // walk it in one pass.
    val dns = new java.net.DatagramSocket(
      new java.net.InetSocketAddress("127.0.0.1", 0))
    val mockPort = dns.getLocalPort
    val answered = new java.util.concurrent.atomic.AtomicBoolean(false)
    val mockThread = new Thread(() => {
      try
        val qbuf = new Array[Byte](512)
        val inPkt = new java.net.DatagramPacket(qbuf, qbuf.length)
        dns.setSoTimeout(10000)
        dns.receive(inPkt)
        val qlen = inPkt.getLength
        // Build the response in-place: flip QR bit, set ANCOUNT=1,
        // then append the 16-byte answer record.
        val resp = new Array[Byte](qlen + 16)
        System.arraycopy(qbuf, 0, resp, 0, qlen)
        resp(2) = (resp(2) | 0x80.toByte).toByte   // QR=1
        resp(6) = 0                                // ANCOUNT hi
        resp(7) = 1                                // ANCOUNT lo
        var off = qlen
        resp(off)     = 0xc0.toByte                // compressed name
        resp(off + 1) = 0x0c.toByte                // -> offset 12
        resp(off + 2) = 0; resp(off + 3) = 1       // TYPE=A
        resp(off + 4) = 0; resp(off + 5) = 1       // CLASS=IN
        resp(off + 6) = 0; resp(off + 7) = 0
        resp(off + 8) = 0; resp(off + 9) = 60.toByte  // TTL=60
        resp(off + 10) = 0; resp(off + 11) = 4     // RDLENGTH=4
        resp(off + 12) = 1; resp(off + 13) = 2     // RDATA
        resp(off + 14) = 3; resp(off + 15) = 4
        val outPkt = new java.net.DatagramPacket(
          resp, resp.length, inPkt.getAddress, inPkt.getPort)
        dns.send(outPkt)
        answered.set(true)
      catch case _: Throwable => ()
    }, "mock-dns")
    mockThread.setDaemon(true)
    mockThread.start()

    try
      qemu.send(s"test_dns example.com 10.0.2.2 $mockPort\n")
      // Wait for the IP itself so `waitFor` returns with the full
      // "resolved host -> A.B.C.D" line in its output. Matching just
      // the "resolved " prefix would race — waitFor is cumulative up
      // to the matched point, so we'd lose the tail in the assert.
      val output = qemu.waitFor("1.2.3.4")
      output should include("test_dns: resolved example.com -> 1.2.3.4")
      mockThread.join(5000)
      answered.get() shouldBe true
    finally
      dns.close()
  }


  "ifconfig: reports the lease installed at boot" in {
    // init's start_dhcp has already leased 10.0.2.15 from slirp by
    // the time the shell is up, so ifconfig should read it back
    // through inet_get_ip_config.
    val output = qemu.command("ifconfig")
    output should include("ip:      10.0.2.15")
    output should include("mask:    255.255.255.0")
    output should include("gateway: 10.0.2.2")
  }


  "nic: GET_MAC + subscribe + drain via test_nic" in {
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


  "posix: dup / dup3 fd aliasing" in {
    // test_dup exercises dup (151) + dup3 (152). Three checks:
    //   (a) dup(STDOUT) returns a fresh fd >= 3
    //   (b) dup3(udp, 5, 0): closing fd 5 must NOT tear down
    //       the inet UDP record — sendto via the original
    //       still works. Validates the iteration-based refcount
    //       in posix_fd_other_dup_exists.
    //   (c) dup3(fd, fd, 0) reports -EINVAL per Linux.
    qemu.send("test_dup\n")
    val output = qemu.waitFor("dup: done")
    output should include("dup: stdout dup = ")
    output should include("dup: dup3(udp, 5, 0) = 5")
    output should include("dup: sendto via udp = 3")
    output should include("dup: dup3(udp,udp,0) = -22")
    output should include("dup: done")
    output should not include "dup: FAIL"
  }


  "posix: fcntl + O_NONBLOCK + accept4 SOCK_NONBLOCK" in {
    // test_nonblock drives syscall 171 (fcntl) and 132 (accept4).
    // Verifies recvfrom on an empty UDP queue returns -EAGAIN
    // when O_NONBLOCK is set; F_GETFL/F_SETFL round-trip;
    // F_GETFD/F_SETFD accept+ignore; F_DUPFD -EINVAL stub.  Then
    // creates a TCP listener, sets it non-blocking, and asserts
    // accept4(SOCK_NONBLOCK) returns -EAGAIN on an empty queue
    // and accept4 with an unknown flag bit returns -EINVAL.
    qemu.send("test_nonblock\n")
    val output = qemu.waitFor("nonblock: done")
    output should include("nonblock: F_GETFL pre = 0")
    output should include("nonblock: F_SETFL(O_NONBLOCK) = 0")
    output should include("nonblock: F_GETFL post = 2048")
    output should include("nonblock: recvfrom (empty) = -11")
    output should include("nonblock: F_GETFD = 0")
    output should include("nonblock: F_SETFD(FD_CLOEXEC) = 0")
    output should include("nonblock: F_DUPFD = -22")
    output should include("nonblock: accept4 NB empty = -11")
    output should include("nonblock: accept4 bad flag = -22")
    output should include("nonblock: done")
    output should not include "nonblock: FAIL"
  }


  "posix: getsockname / getpeername" in {
    // test_getname drives syscall 203 (getsockname) and 192
    // (getpeername) via the POSIX shim. UDP: bound getsockname
    // round-trips 127.0.0.1:7788, getpeername reports -ENOTCONN.
    // TCP on a fresh socket (no connect/listen yet) reports
    // 0.0.0.0:0 for getsockname and -ENOTCONN for getpeername.
    qemu.send("test_getname\n")
    val output = qemu.waitFor("getname: done")
    output should include("getname: udp-local fam=2 port=7788 ip=127.0.0.1")
    output should include("getname: getpeername UDP = -107")
    output should include("getname: tcp-local-fresh fam=2 port=0 ip=0.0.0.0")
    output should include("getname: getpeername TCP-fresh = -107")
    output should include("getname: done")
    output should not include "getname: FAIL"
  }


  "posix: inet sockets reclaimed on pid exit" in {
    // test_sockleak opens 7 UDP sockets (just under the Phase 2
    // per-tid cap of 8) and exits without close(). Two runs
    // back-to-back exercise PM's PID_EXIT IPC: without cleanup,
    // each run leaks 7 slots and the pool eventually fills.
    // Pass = two `all_opened` markers.
    qemu.send("test_sockleak\n")
    val run1 = qemu.waitFor("sockleak: all_opened")
    run1 should include("sockleak: all_opened")
    run1 should not include "sockleak: failed_at_"

    qemu.send("test_sockleak\n")
    val run2 = qemu.waitFor("sockleak: all_opened")
    run2 should include("sockleak: all_opened")
    run2 should not include "sockleak: failed_at_"
  }


  "posix: setsockopt / getsockopt accept+ignore + bufsize" in {
    // test_sockopt drives the POSIX shim's setsockopt / getsockopt
    // paths.  UDP fd: accept-and-ignore for SO_REUSEADDR /
    // SO_BROADCAST / TCP_NODELAY (always 0); unknown pair returns
    // -ENOPROTOOPT.  TCP fd: SO_SNDBUF / SO_RCVBUF round-trip —
    // setsockopt persists, getsockopt echoes the requested size.
    qemu.send("test_sockopt\n")
    val output = qemu.waitFor("sockopt: done")
    output should include("sockopt: set SO_REUSEADDR = 0")
    output should include("sockopt: set SO_BROADCAST = 0")
    output should include("sockopt: set TCP_NODELAY = 0")
    output should include("sockopt: set unknown = -92")
    // SO_REUSEADDR was set to 1 in the test program; persistence
    // pass echoes the actually-set value.
    output should include("sockopt: get SO_REUSEADDR = 0 olen=4 val=1")
    output should include("sockopt: get unknown = -92")
    output should include("sockopt: set SO_SNDBUF = 0")
    output should include("sockopt: set SO_RCVBUF = 0")
    output should include("sockopt: get SO_SNDBUF = 0 val=2048")
    output should include("sockopt: get SO_RCVBUF = 0 val=4096")
    output should include("sockopt: get TCP_NODELAY = 0 val=1")
    output should include("sockopt: set SO_LINGER = 0")
    output should include("sockopt: get SO_LINGER = 0 olen=8 on=1 secs=0")
    // TCP_KEEPIDLE / TCP_KEEPINTVL / TCP_KEEPCNT round-trip the
    // requested seconds / count via the persisted shim values; a
    // pre-connect TCP fd carries the user value verbatim.
    output should include("sockopt: get TCP_KEEPIDLE = 0 val=60")
    output should include("sockopt: get TCP_KEEPINTVL = 0 val=15")
    output should include("sockopt: get TCP_KEEPCNT = 0 val=7")
    // SO_ERROR on a fresh TCP fd is 0 (no failure recorded).  The
    // read-and-clear semantics are exercised by test_icmperr.
    output should include("sockopt: get SO_ERROR fresh = 0 val=0")
    output should include("sockopt: done")
    output should not include "sockopt: FAIL"
  }


  "posix: shutdown(SHUT_WR) half-close" in {
    // test_shutdown connects to a host peer that reads all bytes
    // until EOF then replies with the consumed byte count. The
    // shim's shutdown(fd, SHUT_WR) must emit FIN (not RST, not
    // nothing) so the peer's read loop terminates and the reply
    // comes back. Without real half-close this test times out.
    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18082))
    server.setSoTimeout(15000)

    val peerThread = new Thread(() => {
      try
        val client = server.accept()
        try
          val in  = client.getInputStream
          val out = client.getOutputStream
          val buf = new Array[Byte](128)
          var total = 0
          var n = in.read(buf, total, buf.length - total)
          while n > 0 do
            total += n
            n = in.read(buf, total, buf.length - total)
          // n == -1 → peer (guest) sent FIN. Reply with count.
          val reply = s"got $total".getBytes
          out.write(reply)
          out.flush()
          Thread.sleep(100)
        finally client.close()
      catch
        case _: Throwable => ()
    }, "tcp-shutdown-peer")
    peerThread.setDaemon(true)
    peerThread.start()

    try
      qemu.send("test_shutdown\n")
      val output = qemu.waitFor("shutdown: done")
      output should include("shutdown: connected")
      output should include("shutdown: wrote 11")
      output should include("shutdown: shutdown SHUT_WR = 0")
      output should include("shutdown: read got 'got 11")
      output should include("shutdown: done")
      output should not include "shutdown: FAIL"
    finally
      server.close()
      peerThread.join(2000)
  }


  "posix: socket/bind/sendto/recvfrom round-trip via shim" in {
    // test_posix_udp drives the POSIX socket syscalls added in the
    // per-process fd-table migration: 337=socket, 135=bind,
    // 307=sendto, 276=recvfrom, 147=close. The shim in
    // hello.lsysl translates sockaddr_in to the inet server's IPC
    // format and returns a POSIX fd backed by an inet socket id.
    // Pass = loopback datagram makes the round trip with the
    // payload and src port intact.
    qemu.send("test_posix_udp\n")
    val output = qemu.waitFor("test_posix_udp: done")
    output should include("test_posix_udp: got 5 from 127.0.0.1:")
    output should include("'posix'")
    output should include("test_posix_udp: done")
    output should not include "test_posix_udp: socket rx failed"
    output should not include "test_posix_udp: socket tx failed"
    output should not include "test_posix_udp: bind failed"
    output should not include "test_posix_udp: sendto"
    output should not include "test_posix_udp: recvfrom failed"
  }


  "posix: TCP connect/write/read/close via shim" in {
    // test_posix_tcp drives the POSIX TCP syscalls added in phase
    // 2: 337=socket(SOCK_STREAM), 148=connect, 128=write,
    // 130=read, 147=close. Uses the same slirp-forwarded
    // host echo pattern as test_tcp (10.0.2.2:18080 → host
    // 127.0.0.1:18080). Pass = full "ping\n" round-trip plus the
    // "closed" line.
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
          Thread.sleep(100)
        finally client.close()
      catch
        case _: Throwable => ()
    }, "tcp-echo-server-posix")
    echoThread.setDaemon(true)
    echoThread.start()

    try
      qemu.send("test_posix_tcp\n")
      val output = qemu.waitFor("test_posix_tcp: closed")
      output should include("test_posix_tcp: connected")
      output should include("test_posix_tcp: sent=5")
      output should include("test_posix_tcp: got 5 'ping")
      output should include("test_posix_tcp: closed")
      output should not include "test_posix_tcp: socket failed"
      output should not include "test_posix_tcp: connect failed"
      output should not include "test_posix_tcp: write unexpected"
      output should not include "test_posix_tcp: read failed"
    finally
      server.close()
      echoThread.join(2000)
  }


  "tcp: 900-byte multi-segment transfer" in {
    // test_tcp_big listens on :7892. Host writes exactly 900 bytes
    // of a known pattern (byte i -> i & 0xff), reads a 6-byte
    // summary (count u16 BE + checksum u32 BE), asserts both match.
    // Exercises the guest's drain-recv loop, multi-segment
    // reassembly at the byte level, and a short reply from the
    // server while the peer is still in ESTABLISHED.
    qemu.send("test_tcp_big\n")
    qemu.waitFor("test_tcp_big: listening fd=")

    val n = 900
    var expectedSum = 0L
    val payload = new Array[Byte](n)
    for i <- 0 until n do
      val b = (i & 0xff).toByte
      payload(i) = b
      expectedSum += (b & 0xff).toLong

    val sock = new java.net.Socket()
    sock.setSoTimeout(15000)
    sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28082), 5000)
    try
      val out = sock.getOutputStream
      val in  = sock.getInputStream
      out.write(payload)
      out.flush()
      val reply = new Array[Byte](6)
      var read = 0
      while read < 6 do
        val r = in.read(reply, read, 6 - read)
        if r <= 0 then throw new RuntimeException(s"short read: got $read")
        read += r
      val gotCount = ((reply(0) & 0xff) << 8) | (reply(1) & 0xff)
      val gotSum =
        ((reply(2) & 0xffL) << 24) |
        ((reply(3) & 0xffL) << 16) |
        ((reply(4) & 0xffL) << 8)  |
        (reply(5) & 0xffL)
      gotCount shouldBe n
      gotSum shouldBe expectedSum
    finally sock.close()

    val output = qemu.waitFor("test_tcp_big: ok")
    output should include("test_tcp_big: drained count=900")
    output should include("test_tcp_big: ok")
  }


  "tcp: accept + send on CLOSE_WAIT child (peer already FIN'd)" in {
    // test_tcp_fcw listens, sleeps 100 ticks, then accepts. While the
    // guest is sleeping the host connects, writes "ping\n", and
    // shutdown(SHUT_WR)s — FIN arrives while the child sits on the
    // accept queue, flipping it ESTABLISHED → CLOSE_WAIT. Exercises
    // three CLOSE_WAIT-path fixes:
    //   - do_accept returns the CLOSE_WAIT child (not dropped)
    //   - do_send accepts a send in CLOSE_WAIT (our write side is
    //     still open — only the peer half-closed)
    //   - retransmit still fires for CLOSE_WAIT if needed (covered
    //     incidentally if the pong reply gets reordered).
    qemu.send("test_tcp_fcw\n")
    qemu.waitFor("test_tcp_fcw: listening h=")

    val sock = new java.net.Socket()
    sock.setSoTimeout(10000)
    sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 5000)
    val replyBytes = new Array[Byte](5)
    try
      val out = sock.getOutputStream
      val in  = sock.getInputStream
      out.write("ping\n".getBytes("UTF-8"))
      out.flush()
      sock.shutdownOutput()  // FIN now, don't wait for guest to accept
      var read = 0
      while read < 5 do
        val r = in.read(replyBytes, read, 5 - read)
        if r <= 0 then throw new RuntimeException(s"short read: got $read")
        read += r
    finally sock.close()

    new String(replyBytes, "UTF-8") shouldBe "pong\n"

    val output = qemu.waitFor("test_tcp_fcw: ok")
    output should include("test_tcp_fcw: accepted ch=")
    output should include("test_tcp_fcw: got 5")
    output should include("test_tcp_fcw: sent=5")
    output should include("test_tcp_fcw: ok")
  }


  "tcp: active-open drain-recv 900 bytes" in {
    // test_tcp_rx dials 10.0.2.2:18081 (slirp routes to host's
    // 18081 via user-mode networking — no hostfwd needed for
    // outbound). We stand up a Scala ServerSocket on 127.0.0.1:18081
    // that writes 900 bytes of pattern (byte i = i & 0xff), reads
    // the guest's 6-byte summary, verifies count + checksum.
    // Mirror of test_tcp_big but from the active-open side.
    val n = 900
    var expectedSum = 0L
    val payload = new Array[Byte](n)
    for i <- 0 until n do
      val b = (i & 0xff).toByte
      payload(i) = b
      expectedSum += (b & 0xff).toLong

    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18081))
    server.setSoTimeout(15000)

    val resultBox = new java.util.concurrent.atomic.AtomicReference[String]("")
    val srvThread = new Thread(() => {
      try
        val client = server.accept()
        try {
          val out = client.getOutputStream
          val in  = client.getInputStream
          out.write(payload)
          out.flush()
          val reply = new Array[Byte](6)
          var read = 0
          while read < 6 do
            val r = in.read(reply, read, 6 - read)
            if r <= 0 then throw new RuntimeException(s"short read: got $read")
            read += r
          val gotCount = ((reply(0) & 0xff) << 8) | (reply(1) & 0xff)
          val gotSum =
            ((reply(2) & 0xffL) << 24) |
            ((reply(3) & 0xffL) << 16) |
            ((reply(4) & 0xffL) << 8)  |
            (reply(5) & 0xffL)
          resultBox.set(s"count=$gotCount sum=$gotSum")
        } finally client.close()
      catch
        case e: Throwable => resultBox.set(s"ERR: ${e.getMessage}")
    }, "tcp-pattern-server")
    srvThread.setDaemon(true)
    srvThread.start()

    try
      qemu.send("test_tcp_rx\n")
      val output = qemu.waitFor("test_tcp_rx: ok")
      output should include("test_tcp_rx: connected fd=")
      output should include("test_tcp_rx: drained count=900")
      output should include("test_tcp_rx: sent=6")
      output should include("test_tcp_rx: ok")

      srvThread.join(5000)
      resultBox.get() shouldBe s"count=$n sum=$expectedSum"
    finally
      server.close()
      srvThread.join(2000)
  }


  "tcp: cwnd grows on ACK (slow start)" in {
    // Phase 1 of TCP congestion control. Exposes snd_cwnd /
    // snd_ssthresh via INET_CMD_TCP_DEBUG, drives a 900-byte
    // pipelined send, and asserts cwnd is observably bigger
    // afterward. The actual growth amount depends on how many
    // cumulative ACKs Linux sends back (typically 1-2 for a
    // small payload), but any growth at all proves the ACK
    // path is feeding into inet_tcp_cwnd_on_ack.
    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18080))
    server.setSoTimeout(15000)
    val bulkEcho = new Thread(() => {
      try
        val client = server.accept()
        try
          val in  = client.getInputStream
          val out = client.getOutputStream
          val buf = new Array[Byte](1024)
          var remaining = 900
          while remaining > 0 do
            val n = in.read(buf, 0, math.min(buf.length, remaining))
            if n <= 0 then remaining = 0
            else
              out.write(buf, 0, n)
              out.flush()
              remaining -= n
          Thread.sleep(100)
        finally client.close()
      catch
        case _: Throwable => ()
    }, "tcp-cwnd-echo")
    bulkEcho.setDaemon(true)
    bulkEcho.start()

    try
      qemu.send("test_tcp_cwnd\n")
      val output = qemu.waitFor("test_tcp_cwnd: ok")
      output should include("test_tcp_cwnd: initial")
      output should include("test_tcp_cwnd: after")
      output should include("test_tcp_cwnd: ok")
      // Pull the two cwnd values out of the output.
      val initRe  = """cwnd=(\d+)\s+ssthresh=(\d+)""".r
      val matches = initRe.findAllMatchIn(output).toList
      matches.length shouldBe 2
      val initialCwnd = matches(0).group(1).toInt
      val initialSs   = matches(0).group(2).toInt
      val afterCwnd   = matches(1).group(1).toInt
      initialCwnd shouldBe 4800
      initialSs   shouldBe 65535
      assert(afterCwnd > initialCwnd,
        s"expected cwnd growth, got initial=$initialCwnd after=$afterCwnd")
    finally
      server.close()
      bulkEcho.join(2000)
  }


  "tcp: fast retransmit + recovery round-trip" in {
    // Phase 2 + Phase 3 of TCP congestion control. The nic's
    // loss-injection knob drops the first TCP data segment
    // post-connect; the host receives segments 2, 3, 4 out of
    // order and emits three duplicate ACKs. Slix fires fast
    // retransmit (Phase 2), enters fast recovery with cwnd
    // inflated (Phase 3 §3.2 ¶5), the retransmit delivers the
    // missing segment, the host ACKs everything cumulatively,
    // slix deflates cwnd back to ssthresh and exits recovery.
    //
    // Asserts:
    //   - ssthresh halved from 65535 (fast retransmit fired)
    //   - fast_recovery == 0 at end (recovery exited cleanly)
    //   - no stuck data (snd_una == snd_nxt at end)
    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18080))
    server.setSoTimeout(15000)
    val bulkEcho = new Thread(() => {
      try
        val client = server.accept()
        try
          val in  = client.getInputStream
          val out = client.getOutputStream
          val buf = new Array[Byte](2048)
          var remaining = 1920
          while remaining > 0 do
            val n = in.read(buf, 0, math.min(buf.length, remaining))
            if n <= 0 then remaining = 0
            else
              out.write(buf, 0, n)
              out.flush()
              remaining -= n
          Thread.sleep(200)
        finally client.close()
      catch
        case _: Throwable => ()
    }, "tcp-fr-echo")
    bulkEcho.setDaemon(true)
    bulkEcho.start()

    try
      qemu.send("test_tcp_fr\n")
      val output = qemu.waitFor("test_tcp_fr: ok")
      output should include("test_tcp_fr: arm drop_next=1")
      output should include("test_tcp_fr: sent=1920")
      val ssRe = """ssthresh=(\d+)\s+cwnd=(\d+)""".r
      val matches = ssRe.findAllMatchIn(output).toList
      matches.length shouldBe 2
      val initialSs = matches(0).group(1).toInt
      val afterSs   = matches(1).group(1).toInt
      initialSs shouldBe 65535
      assert(afterSs < 65535,
        s"expected ssthresh halving from fast retransmit, " +
        s"got initial=$initialSs after=$afterSs " +
        s"(still at initial means fast retransmit didn't fire)")
      // Phase 3 assertions: recovery exits cleanly.
      output should include("test_tcp_fr: fr=0")
      output should include("test_tcp_fr: una==nxt")
    finally
      server.close()
      bulkEcho.join(2000)
  }


  "tcp: minimal HTTP/1.0 interop (httpd)" in {
    // httpd listens on :8080. QEMU's hostfwd=tcp::28083-:8080
    // forwards host dials of 127.0.0.1:28083 into the guest. This
    // is the Phase-5-ish interop milestone — end-to-end proof that
    // the slix TCP stack coexists with real-world HTTP clients,
    // not just our own echo tests.
    qemu.send("httpd\n")
    qemu.waitFor("httpd: listening on :8080 fd=")

    val sock = new java.net.Socket()
    sock.setSoTimeout(10000)
    sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28083), 5000)
    val body = try
      val out = sock.getOutputStream
      val in  = sock.getInputStream
      out.write("GET / HTTP/1.0\r\nHost: slix\r\n\r\n".getBytes("UTF-8"))
      out.flush()
      val buf = new Array[Byte](256)
      var total = 0
      var done  = false
      while !done && total < buf.length do
        val r = in.read(buf, total, buf.length - total)
        if r <= 0 then done = true
        else total += r
      new String(buf, 0, total, "UTF-8")
    finally sock.close()

    body should startWith("HTTP/1.0 200 OK")
    body should include("Content-Type: text/plain")
    body should include("Content-Length: 6")
    body should endWith("hello\n")

    val output = qemu.waitFor("httpd: done")
    output should include("httpd: accept[0] cfd=")
    output should include("httpd: i=0 hdr_end=")
    output should include("httpd: sent=")
    output should include("httpd: done")
  }


  "tcp: multi-client passive open stress" in {
    // test_tcp_mcl listens on :7890 and accepts N clients in sequence.
    // We fire N host-side dials concurrently (all arriving while the
    // server is still processing the first), which forces children
    // into the accept queue. Each reply carries a "#i" tag so we can
    // verify each payload landed. Regression-catching target:
    // accept-queue enqueue/dequeue, SYN_RCVD concurrency, per-child
    // retx arming/disarming across overlapping lifetimes.
    // N must match the test_tcp_mcl binary.
    val n = 3
    qemu.send("test_tcp_mcl\n")
    qemu.waitFor("test_tcp_mcl: listening fd=")

    val replies = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val threads = new Array[Thread](n)
    for i <- 0 until n do
      val runnable: Runnable = () => {
        val sock = new java.net.Socket()
        sock.setSoTimeout(10000)
        try {
          sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 5000)
          val out = sock.getOutputStream
          val in  = sock.getInputStream
          out.write(s"ping$i\n".getBytes("UTF-8"))
          out.flush()
          val buf = new Array[Byte](32)
          val got = in.read(buf)
          if got > 0 then
            replies.add(new String(buf, 0, got, "UTF-8"))
          else
            replies.add(s"EOF-$i")
        } catch {
          case e: Throwable => replies.add(s"ERR-$i: ${e.getMessage}")
        } finally {
          sock.close()
        }
      }
      threads(i) = new Thread(runnable, s"tcp-stress-$i")
    for t <- threads do t.start()
    for t <- threads do t.join(20000)

    val output = qemu.waitFor("test_tcp_mcl: ok")
    for i <- 0 until n do
      output should include(s"test_tcp_mcl: accept[$i] cfd=")
    output should include("test_tcp_mcl: ok")

    import scala.jdk.CollectionConverters.*
    val got = replies.asScala.toSet
    got.size shouldBe n
    // Each reply carries its own payload plus a "#i" tag. Because the
    // parent port is re-used and slot-allocation order isn't
    // guaranteed across parallel SYNs, we don't assert which payload
    // matched which accept slot — just that each payload made it
    // back with SOME tag in the 0..n-1 range.
    for i <- 0 until n do
      got.exists(_.startsWith(s"ping$i#")) shouldBe true
  }


  "tcp: multi-request httpd accept loop" in {
    // Three back-to-back dials to a single httpd process. The
    // server serves 3 requests in a sequential accept loop and
    // exits. Proves the accept loop terminates, closes each child
    // cleanly without affecting the listen socket, and doesn't
    // leak slots across iterations. Companion to test_tcp_mcl —
    // that one proves the queue depth under parallel SYN, this
    // one proves the full request/response cycle works repeatedly.
    val n = 3
    qemu.send(s"httpd $n\n")
    qemu.waitFor("httpd: listening on :8080 fd=")

    for i <- 0 until n do
      val sock = new java.net.Socket()
      sock.setSoTimeout(10000)
      sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28083), 5000)
      val body = try
        val out = sock.getOutputStream
        val in  = sock.getInputStream
        out.write(s"GET /iter/$i HTTP/1.0\r\n\r\n".getBytes("UTF-8"))
        out.flush()
        val buf = new Array[Byte](256)
        var total = 0
        var done  = false
        while !done && total < buf.length do
          val r = in.read(buf, total, buf.length - total)
          if r <= 0 then done = true
          else total += r
        new String(buf, 0, total, "UTF-8")
      finally sock.close()
      body should startWith("HTTP/1.0 200 OK")
      body should endWith("hello\n")

    val output = qemu.waitFor("httpd: done")
    for i <- 0 until n do
      output should include(s"httpd: accept[$i] cfd=")
      output should include(s"httpd: i=$i hdr_end=")
    output should include("httpd: done")
  }


  "tcp: out-of-order reassembly self-test" in {
    // test_tcp_ooo triggers inet's reorder-queue self-test via a
    // dedicated IPC op. The test exercises the stash → drain path
    // without depending on slirp to actually reorder packets, which
    // it doesn't. See inet_tcp_ooo_selftest for the exact sequence:
    // two out-of-order segs at seq 200 and 250 get stashed, then an
    // in-order seg at seq 100 fills the gap and drains both stashed
    // segs in order.
    qemu.send("test_tcp_ooo\n")
    val output = qemu.waitFor("test_tcp_ooo: ok")
    output should include("test_tcp_ooo: ok")
    output should not include "test_tcp_ooo: failed"
  }


  "tcp: VFS bridge (connect/read/write/close)" in {
    // Same shape as the test_tcp_rx test, but the guest program
    // (test_tcp_vfs) calls only connect("tcp:...") + read/write/close —
    // no tcp_* wrappers. Proves VFS routes FS_CMD_READ/WRITE/CLOSE on a
    // tcp-typed handle through INET_CMD_TCP_RECV/SEND/CLOSE.
    val n = 900
    var expectedSum = 0L
    val payload = new Array[Byte](n)
    for i <- 0 until n do
      val b = (i & 0xff).toByte
      payload(i) = b
      expectedSum += (b & 0xff).toLong

    val server = new java.net.ServerSocket()
    server.setReuseAddress(true)
    server.bind(new java.net.InetSocketAddress("127.0.0.1", 18081))
    server.setSoTimeout(15000)

    val resultBox = new java.util.concurrent.atomic.AtomicReference[String]("")
    val srvThread = new Thread(() => {
      try
        val client = server.accept()
        try {
          val out = client.getOutputStream
          val in  = client.getInputStream
          out.write(payload)
          out.flush()
          val reply = new Array[Byte](6)
          var read = 0
          while read < 6 do
            val r = in.read(reply, read, 6 - read)
            if r <= 0 then throw new RuntimeException(s"short read: got $read")
            read += r
          val gotCount = ((reply(0) & 0xff) << 8) | (reply(1) & 0xff)
          val gotSum =
            ((reply(2) & 0xffL) << 24) |
            ((reply(3) & 0xffL) << 16) |
            ((reply(4) & 0xffL) << 8)  |
            (reply(5) & 0xffL)
          resultBox.set(s"count=$gotCount sum=$gotSum")
        } finally client.close()
      catch
        case e: Throwable => resultBox.set(s"ERR: ${e.getMessage}")
    }, "tcp-pattern-server-vfs")
    srvThread.setDaemon(true)
    srvThread.start()

    try
      qemu.send("test_tcp_vfs\n")
      val output = qemu.waitFor("test_tcp_vfs: ok")
      output should include("test_tcp_vfs: connected h=")
      output should include("test_tcp_vfs: drained count=900")
      output should include("test_tcp_vfs: sent=6")
      output should include("test_tcp_vfs: ok")

      srvThread.join(5000)
      resultBox.get() shouldBe s"count=$n sum=$expectedSum"
    finally
      server.close()
      srvThread.join(2000)
  }


  "tcp: VFS listen bridge (connect/accept/read/write/close)" in {
    // test_tcp_lsv listens via connect("tcp-listen:7890") + accept()
    // + read/write/close — no tcp_listen/tcp_accept wrappers. Proves
    // that OFT_TYPE_TCP_LISTEN + FS_CMD_ACCEPT let a passive TCP
    // server run entirely through VFS. QEMU's hostfwd=tcp::28080-:7890
    // forwards host dials of 127.0.0.1:28080 into the guest.
    qemu.send("test_tcp_lsv\n")
    qemu.waitFor("test_tcp_lsv: listening h=")

    val sock = new java.net.Socket()
    sock.setSoTimeout(10000)
    sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 5000)
    try
      val out = sock.getOutputStream
      val in  = sock.getInputStream
      out.write("ping\n".getBytes("UTF-8"))
      out.flush()
      val buf = new Array[Byte](32)
      val n = in.read(buf)
      n should be > 0
      new String(buf, 0, n, "UTF-8") should include("ping")
    finally sock.close()

    val output = qemu.waitFor("test_tcp_lsv: closed")
    output should include("test_tcp_lsv: accepted ch=")
    output should include("test_tcp_lsv: got ")
    output should include("test_tcp_lsv: sent=")
    output should include("test_tcp_lsv: closed")
  }


  "udp: recvfrom_timeout fires after ~1s with no sender" in {
    // test_udp_tmo binds 0.0.0.0:7788 and calls
    // recvfrom_timeout(..., 1000 ms) with nothing sending to it.
    // Verifies that:
    //   (a) the call returns 0 (timed out, not blocked, not errored),
    //   (b) the elapsed uptime is in the expected range,
    //   (c) the server main loop's inet_udp_scan_timeouts
    //       delivered the deferred reply.
    // Without the scan, the thread would park in
    // inet_handle_recvfrom_timeout forever and the harness
    // would time out rather than the program.
    // (Binary name shortened from test_udp_timeout to fit TFS's
    //  DIR_NAME_LEN=14 limit — "test_udp_timeout" was silently
    //  truncated to "test_udp_timeo" in an earlier iteration.)
    qemu.send("test_udp_tmo\n")
    val output = qemu.waitFor("test_udp_tmo: ok")
    output should include("test_udp_tmo: timeout elapsed=")
    output should include("test_udp_tmo: ok")
    output should not include "test_udp_tmo: recvfrom_timeout error"
    output should not include "test_udp_tmo: too early"
    output should not include "test_udp_tmo: too late"
    output should not include "test_udp_tmo: unexpected data"
  }


  "virtio: probe finds the attached virtio-net device" in {
    // The boot log (captured before the login prompt) should
    // confirm a working virtio-net probe. The banner format is
    // arch-specific — aarch64 uses virtio-mmio and prints
    // `virtio: slot N ... (net)`; x86 uses PCI and prints
    // `virtio-net: probe ok ...`. The test accepts either, so the
    // same description holds across arches.
    val banner = qemu.allOutput
    val hasMmio = banner.contains("virtio: slot") && banner.contains("(net)")
    val hasPci  = banner.contains("virtio-net: probe ok")
    (hasMmio || hasPci) shouldBe true
    banner should not include "virtio: probe failed"
  }


  "wget: resolve + fetch via mock DNS and mock HTTP" in {
    // End-to-end "real internet" proof: guest parses URL, resolves
    // name via our mock DNS (which answers any query with an A
    // record = 10.0.2.2), opens TCP to 10.0.2.2:<httpPort> which
    // slirp forwards to the host's mock HTTP server on
    // 127.0.0.1:<httpPort>. Mock HTTP replies with "hello wget\n"
    // and closes. Guest reads until EOF, prints body to stdout,
    // emits `wget: done`.
    val http = new java.net.ServerSocket()
    http.setReuseAddress(true)
    http.bind(new java.net.InetSocketAddress("127.0.0.1", 0))
    http.setSoTimeout(10000)
    val httpPort = http.getLocalPort

    val dns = new java.net.DatagramSocket(
      new java.net.InetSocketAddress("127.0.0.1", 0))
    val dnsPort = dns.getLocalPort

    val httpAnswered = new java.util.concurrent.atomic.AtomicBoolean(false)
    val dnsAnswered  = new java.util.concurrent.atomic.AtomicBoolean(false)

    val dnsThread = new Thread(() => {
      try
        val qbuf = new Array[Byte](512)
        val inPkt = new java.net.DatagramPacket(qbuf, qbuf.length)
        dns.setSoTimeout(10000)
        dns.receive(inPkt)
        val qlen = inPkt.getLength
        val resp = new Array[Byte](qlen + 16)
        System.arraycopy(qbuf, 0, resp, 0, qlen)
        resp(2) = (resp(2) | 0x80.toByte).toByte
        resp(6) = 0
        resp(7) = 1
        var off = qlen
        resp(off)      = 0xc0.toByte
        resp(off + 1)  = 0x0c.toByte
        resp(off + 2)  = 0; resp(off + 3)  = 1
        resp(off + 4)  = 0; resp(off + 5)  = 1
        resp(off + 6)  = 0; resp(off + 7)  = 0
        resp(off + 8)  = 0; resp(off + 9)  = 60.toByte
        resp(off + 10) = 0; resp(off + 11) = 4
        resp(off + 12) = 10
        resp(off + 13) = 0
        resp(off + 14) = 2
        resp(off + 15) = 2
        dns.send(new java.net.DatagramPacket(
          resp, resp.length, inPkt.getAddress, inPkt.getPort))
        dnsAnswered.set(true)
      catch case _: Throwable => ()
    }, "mock-dns-wget")
    dnsThread.setDaemon(true)
    dnsThread.start()

    val httpThread = new Thread(() => {
      try
        val client = http.accept()
        try
          val in  = client.getInputStream
          val out = client.getOutputStream
          val rbuf = new Array[Byte](2048)
          var total = 0
          var done  = false
          while !done && total < rbuf.length do
            val r = in.read(rbuf, total, rbuf.length - total)
            if r <= 0 then done = true
            else
              total += r
              val s = new String(rbuf, 0, total, "UTF-8")
              if s.contains("\r\n\r\n") then done = true
          val body = "hello wget\n"
          val respStr = s"HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\nContent-Length: ${body.length}\r\nConnection: close\r\n\r\n$body"
          out.write(respStr.getBytes("UTF-8"))
          out.flush()
          httpAnswered.set(true)
        finally client.close()
      catch case _: Throwable => ()
    }, "mock-http-wget")
    httpThread.setDaemon(true)
    httpThread.start()

    try
      qemu.send(s"wget http://example.local:$httpPort/hello 10.0.2.2 $dnsPort\n")
      val output = qemu.waitFor("wget: done")
      output should include("wget: resolved example.local -> 10.0.2.2")
      output should include("wget: status=200")
      output should include("hello wget")
      output should include("wget: done")
      dnsThread.join(5000)
      httpThread.join(5000)
      dnsAnswered.get()  shouldBe true
      httpAnswered.get() shouldBe true
    finally
      dns.close()
      http.close()
  }



  /** Shared body for the Phase F listen-backlog test (also used by
    * Aarch64NshTests). Listens with backlog=2; fires four parallel
    * host connects so two SYNs are dropped on first arrival and only
    * succeed after the peer's automatic retransmit (which fires once
    * the guest accept loop drains the queue). */
  private def runBacklogTest(label: String): Unit =
    qemu.send("mlbacklog\n")
    qemu.waitFor("mlbacklog: ready")

    val tags = "abcd".toList
    val replies = new java.util.concurrent.ConcurrentHashMap[Char, String]()
    val errors = new java.util.concurrent.ConcurrentLinkedQueue[Throwable]()
    val threads: List[Thread] = tags.map { tag =>
      val runnable: Runnable = () => {
        try
          val sock = new java.net.Socket()
          sock.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 8000)
          try
            sock.getOutputStream.write(Array(tag.toByte))
            sock.getOutputStream.flush()
            val in = sock.getInputStream
            val buf = new Array[Byte](16)
            val n = in.read(buf)
            replies.put(tag, if n > 0 then new String(buf, 0, n) else "")
          finally sock.close()
        catch case e: Throwable => errors.add(e)
        ()
      }
      val t = new Thread(runnable, s"$label-mlbacklog-client-$tag")
      t.setDaemon(true)
      t.start()
      t
    }

    qemu.waitFor("mlbacklog: done")
    threads.foreach(t => t.join(8000))

    val output = qemu.allOutput
    output should include("mlbacklog: ready")
    for i <- 0 until 4 do
      output should include(s"mlbacklog: child[$i]")
    output should include("mlbacklog: done")

    errors.size shouldBe 0
    replies.size shouldBe 4
    tags.foreach { tag =>
      replies.get(tag) should include("ack")
    }
}
