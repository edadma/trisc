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

  "x86 wc: count from file" in {
    qemu.command("echo hello > /tmp/wcf", rootPrompt)
    val output = qemu.command("wc /tmp/wcf")
    output should include("1")
  }

  "x86 pipe: test_pipe 1 write" in {
    val output = qemu.command("echo x | test_pipe 1")
    output should include("A")
  }

  "x86 pipe: test_pipe 2 writes" in {
    val output = qemu.command("echo x | test_pipe 2")
    output should include("B")
  }

  "x86 pipe: test_pipe 3 writes" in {
    val output = qemu.command("echo x | test_pipe 3")
    output should include("C")
  }

  "x86 pipe: test_pipe 4 writes" in {
    val output = qemu.command("echo x | test_pipe 4")
    output should include("D")
  }

  "x86 wc: echo piped to wc" in {
    val output = qemu.command("echo asdf | wc")
    output should include("1")
  }

  "x86 pipe: echo piped to tail" in {
    val output = qemu.command("echo asdf | tail -1")
    output should include("asdf")
  }

  "x86 signal: ctrl-c kills foreground process" in {
    // Byte 0x03 passes through -chardev stdio,signal=off directly to COM1,
    // since Java's process pipe bypasses the host terminal.
    qemu.send("count\n")
    Thread.sleep(2000)
    qemu.send("\u0003")
    qemu.waitFor(rootPrompt)
    val ps = qemu.command("ps")
    ps should not include "count"
  }

  "x86 ds: publish, retrieve, delete int and string" in {
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

  "x86 C program: test_c receives argc and argv" in {
    // test_c is written in C and linked with c_crt0.c (the POSIX->Sysl
    // crt0 bridge). It prints argc and joined argv[1..], returning argc
    // as exit code — exercises the C side of the POSIX argv contract.
    val output = qemu.command("test_c hello world")
    output should include("argc=3")
    output should include("hello world")
  }

  "x86 inet: UDP loopback via test_net" in {
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

  "x86 async RX: unsolicited UDP reaches recvfrom via virtio IRQ" in {
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

  "x86 musl: O_NONBLOCK on stdin returns EAGAIN before key" in {
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

  "x86 musl: epoll on stdin (TTY input subscriber)" in {
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

  "x86 musl: timerfd_create / settime / gettime + epoll" in {
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

  "x86 musl: eventfd2 + epoll integration" in {
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

  "x86 net: inbound ICMP Port Unreachable surfaces as -ECONNREFUSED" in {
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

  "x86 net: NB-connect failure surfaces as SO_ERROR=ECONNREFUSED" in {
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

  "x86 musl: epoll on a pipe (Phase A2 closeout)" in {
    qemu.send("epoll_pipe\n")
    val output = qemu.waitFor("mepoll_pipe: done")
    output should include("mepoll_pipe: empty=0")
    output should include("mepoll_pipe: after_write=1 events=1")
    output should include("mepoll_pipe: after_close=1 events=17")
    output should include("mepoll_pipe: done")
  }

  "x86 timer: subscribe fires expected count in N ticks" in {
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

  "x86 tcp: connect, send, receive echo, close" in {
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

  "x86 tcp: passive open, accept, echo, close" in {
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

  "x86 tcp: VFS listen bridge accepts optional ',backlog' suffix" in {
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

  "x86 tcp: in-guest 127.0.0.1 loopback round trip" in {
    // test_tcp_lpbk drives the loopback fastpath added to
    // inet_tcp_emit / inet_tcp_emit_rst: client and listener live
    // in the same guest and exchange payloads over 127.0.0.1
    // without any NIC/slirp involvement.
    val output = qemu.command("test_tcp_lpbk")
    output should include("lpbk:ok")
    output should not include "lpbk:bad"
  }

  "x86 udp: loopback gate covers 127/8 + own_ip" in {
    // The pre-existing UDP loopback shortcut only matched
    // 127.0.0.1 exactly. inet_handle_sendto now uses
    // inet_is_loopback_ip, so 127.0.0.5 and 10.0.2.15 (our
    // QEMU lease) also short-circuit through the in-memory
    // queue instead of trying ARP and silently failing.
    val output = qemu.command("test_udp_lpbk")
    output should include("udplo: ok")
    output should not include "udplo: bad"
  }

  "x86 icmp: ping 127.0.0.1 returns immediately" in {
    // inet_send_icmp_echo_to short-circuits to inet_ping_deliver
    // when the destination is loopback — `ping 127.0.0.1` sees
    // a synthesized reply on the same tick with rtt=0 instead of
    // the request silently dropping at inet_resolve_mac.
    val output = qemu.command("ping -c 1 127.0.0.1")
    output should include("reply from 127.0.0.1")
    output should include("1 sent, 1 received")
  }

  "x86 tcp: getsockopt(TCP_INFO) on ESTABLISHED loopback fd" in {
    // test_tcp_info opens an in-guest 127.0.0.1 connection and
    // probes getsockopt(IPPROTO_TCP, TCP_INFO). Verifies the
    // 104-byte struct is fully written, tcpi_state maps to 1
    // (TCP_ESTABLISHED), and tcpi_snd_mss decodes as a sane
    // little-endian u32.
    val output = qemu.command("test_tcp_info")
    output should include("tcpinfo: ok")
    output should not include "tcpinfo: bad"
  }

  "x86 procid: getpid/getppid/getuid family + getrandom" in {
    // Process / thread identity syscalls + xorshift-based getrandom.
    // Slix has no multi-threading and boots root, so most return
    // 0 or 1; getrandom is best-effort and just verifies two
    // consecutive calls give different bytes.
    val output = qemu.command("test_proc_id")
    output should include("procid: ok")
    output should not include "procid: bad"
  }

  "x86 time: clock_gettime / gettimeofday / clock_getres / nanosleep" in {
    // POSIX time syscalls fed off uptime() at 100Hz. Verifies
    // clock_getres reports 10ms, clock_gettime + gettimeofday
    // agree within 20ms, and nanosleep(50ms) advances the
    // clock by at least 40ms.
    val output = qemu.command("test_clock")
    output should include("clock: ok")
    output should not include "clock: bad"
  }

  "x86 fs: fsync / fdatasync / sync / syncfs no-op stubs" in {
    // No on-disk persistence yet; these return 0 (or -EBADF for
    // bad fds) so defensive sqlite/log-writer patterns don't
    // crash on -ENOSYS.
    val output = qemu.command("test_fsync")
    output should include("fsync: ok")
    output should not include "fsync: bad"
  }

  "x86 sockopt: SO_TYPE/DOMAIN/PROTOCOL/ACCEPTCONN" in {
    // test_sockinfo verifies the four read-only introspection
    // getsockopts the shim now reports off the fd kind +
    // is_listen flag. Three fds: UDP, TCP pre-listen, TCP
    // post-listen.
    val output = qemu.command("test_sockinfo")
    output should include("sockinfo: ok")
    output should not include "sockinfo: bad"
  }

  "x86 ip: fragmentation reassembly self-test" in {
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

  "x86 ip: fragmentation RFC corners (overlap + timeout)" in {
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

  "x86 udp: 1024-byte datagram via 127.0.0.1 loopback" in {
    // Verifies the bumped UDP datagram cap (512 → 1472). Sends a
    // 1024-byte body with byte i = (i & 0xff), recvfrom-validates
    // the full body comes through. Catches truncation at the old
    // 512 boundary plus any reply-buffer overflow / underflow.
    val output = qemu.command("test_udp_big")
    output should include("udpbig: ok")
    output should not include "udpbig: bad"
  }

  "x86 udp: connect()/send()/recv() with default peer" in {
    // POSIX connect() on UDP saves a default peer; subsequent
    // send() (sendto with NULL addr) targets it. Then dissolve
    // via connect(AF_UNSPEC) and verify send returns -ENOTCONN.
    val output = qemu.command("test_udp_conn")
    output should include("udpcon: ok")
    output should not include "udpcon: bad"
  }

  "x86 udp: connected fd drops non-peer datagrams (recv filter)" in {
    // POSIX/Linux: a UDP fd with a saved peer (via connect())
    // drops datagrams whose source != peer. Slix enforces this
    // at recv time — sys_recvfrom recurses past non-peer
    // datagrams until a matching one arrives or EAGAIN.
    val output = qemu.command("test_udp_filt")
    output should include("udpfilt: ok")
    output should not include "udpfilt: bad"
  }

  "x86 udp: NB recv on empty queue returns EAGAIN" in {
    // Minimal regression check: socket → bind → fcntl(NONBLOCK)
    // → recvfrom → must return -EAGAIN. Catches future
    // sys_recvfrom regressions in the empty-queue path
    // independently of the connect/filter loop.
    val output = qemu.command("test_udp_dbg")
    output should include("udpdbg: ok")
    output should not include "udpdbg: bad"
  }

  "x86 sockopt: SO_RCVTIMEO bounded blocking recv" in {
    // setsockopt SO_RCVTIMEO = 100ms, then blocking recvfrom on
    // an empty UDP queue must return -EAGAIN within the window
    // (Linux semantics) instead of hanging forever.
    val output = qemu.command("test_so_timeo")
    output should include("sotmo: ok")
    output should not include "sotmo: bad"
  }

  "x86 udp: MSG_DONTWAIT per-call non-blocking override" in {
    // MSG_DONTWAIT (0x40) makes a single recvfrom non-blocking
    // even on a blocking fd; libuv uses it to avoid the
    // fcntl(O_NONBLOCK) race when the fd is shared.
    val output = qemu.command("test_msg_dwait")
    output should include("mdwait: ok")
    output should not include "mdwait: bad"
  }

  "x86 udp: getpeername after connect + shutdown no-op" in {
    // POSIX: getpeername on a connected UDP fd returns the saved
    // peer; pre-connect returns -ENOTCONN. shutdown on UDP is
    // accepted as a no-op (Linux compat).
    val output = qemu.command("test_udp_pname")
    output should include("udppeer: ok")
    output should not include "udppeer: bad"
  }

  "x86 tcp: recvfrom with MSG_DONTWAIT" in {
    // sys_recvfrom now accepts TCP fds (previously -EBADF) and
    // honors MSG_DONTWAIT for per-call NB. The pre-data recv
    // returns -EAGAIN; after write, recv returns the bytes.
    val output = qemu.command("test_tcp_dwait")
    output should include("tcpdw: ok")
    output should not include "tcpdw: bad"
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

  "x86 musl: write(1, ...) + read(0, ...) + exit" in {
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

  "x86 musl: socket/connect/shutdown/read via libc wrappers" in {
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

  "x86 musl: open/read/lseek/close on /etc/passwd" in {
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

  "x86 musl: epoll_create1/ctl/wait on a UDP socket" in {
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

  "x86 musl: epoll EPOLLET + EPOLLONESHOT (Phase A2)" in {
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

  "x86 musl: non-blocking accept (Phase B)" in {
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

  "x86 musl: non-blocking connect (Phase B)" in {
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

  "x86 musl: pipe2 + write + read + EOF" in {
    qemu.send("mpipe\n")
    val output = qemu.waitFor("mpipe: done")
    output should include("mpipe: pipe2=0")
    output should include("mpipe: write=17")
    output should include("mpipe: read=17 data='ping through pipe'")
    output should include("mpipe: read_after_close=0")
    output should include("mpipe: done")
  }

  "x86 musl: sendmsg/recvmsg via libc wrappers" in {
    qemu.send("mmsg\n")
    val output = qemu.waitFor("mmsg: done")
    output should include("mmsg: socket=3")
    output should include("mmsg: bind=0")
    output should include("mmsg: sendmsg=12")
    output should include("mmsg: recvmsg=12 data='hello msghdr'")
    output should include("mmsg: src_port=7790")
    output should include("mmsg: done")
  }

  "x86 musl: listen backlog enforcement (Phase F)" in {
    runBacklogTest("x86")
  }

  "x86 musl: per-fd EPOLLET edge isolation" in {
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
