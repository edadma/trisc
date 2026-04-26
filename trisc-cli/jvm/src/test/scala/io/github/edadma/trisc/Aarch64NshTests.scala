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

  "aarch64 musl: write(1, ...) + read(0, ...) + exit" in {
    // mhello is a C program cross-compiled against slix's musl fork
    // (slix/test/hello.c, built by slix/test/build-hello.sh). It
    // exercises SYS_WRITE=128, SYS_READ=130 and SYS_EXIT_GROUP=129
    // through the POSIX shim granted by pm_handle_spawn's
    // svc_grant_posix_range call.
    //
    // SYS_READ on stdin now routes through TTY (per the TTY input
    // subscriber chunk). To unblock the read we send one keystroke
    // after the program has written its banner; mhello then reads
    // 1 byte and prints "read=1".
    //
    // If this test reports "mhello: not found", rebuild the musl
    // libc and the mhello binary:
    //   bash slix/build-musl.sh          (one-shot; slow on first run)
    //   bash slix/test/build-hello.sh    (rebuilds /tmp/slix-aarch64/bin/mhello)
    //   bash oskit/arch/aarch64/board/virt/build.sh  (repacks ramdisk)
    qemu.send("mhello\n")
    qemu.waitFor("hello from musl")
    qemu.send("X")
    val output = qemu.waitFor("read=1")
    output should include("read=1")
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

  "aarch64 boot: init auto-runs dhclient before opening logins" in {
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

  "aarch64 ifconfig: reports the lease installed at boot" in {
    // init's start_dhcp has already leased 10.0.2.15 from slirp by
    // the time the shell is up, so ifconfig should read it back
    // through inet_get_ip_config.
    val output = qemu.command("ifconfig")
    output should include("ip:      10.0.2.15")
    output should include("mask:    255.255.255.0")
    output should include("gateway: 10.0.2.2")
  }

  "aarch64 udp: recvfrom_timeout fires after ~1s with no sender" in {
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

  "aarch64 posix: socket/bind/sendto/recvfrom round-trip via shim" in {
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

  "aarch64 posix: TCP connect/write/read/close via shim" in {
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

  "aarch64 posix: inet sockets reclaimed on pid exit" in {
    // test_sockleak opens 30 UDP sockets and exits without
    // close(). The inet server has 32 slots, so a second run can
    // only succeed if PM's PID_EXIT IPC caused inet to reclaim
    // the dying process's sockets. Without the cleanup, run 2
    // would hit -1 on the third socket() and print
    // `sockleak: failed_at_2`. Pass = two `all_opened` markers.
    qemu.send("test_sockleak\n")
    val run1 = qemu.waitFor("sockleak: all_opened")
    run1 should include("sockleak: all_opened")
    run1 should not include "sockleak: failed_at_"

    qemu.send("test_sockleak\n")
    val run2 = qemu.waitFor("sockleak: all_opened")
    run2 should include("sockleak: all_opened")
    run2 should not include "sockleak: failed_at_"
  }

  "aarch64 posix: setsockopt / getsockopt accept+ignore + bufsize" in {
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

  "aarch64 posix: getsockname / getpeername" in {
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

  "aarch64 posix: fcntl + O_NONBLOCK + accept4 SOCK_NONBLOCK" in {
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

  "aarch64 posix: shutdown(SHUT_WR) half-close" in {
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

  "aarch64 posix: dup / dup3 fd aliasing" in {
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

  "aarch64 musl: O_NONBLOCK on stdin returns EAGAIN before key" in {
    // Pre-check via TTY_CMD_POLL: with O_NONBLOCK set, sys_read on
    // fd 0 must return -EAGAIN when the tty input ring is empty;
    // after a key arrives, the same read returns 1.
    qemu.send("nbstdin\n")
    val pre = qemu.waitFor("mnbstdin: ready_for_input")
    pre should include("mnbstdin: setfl=0")
    pre should include("mnbstdin: empty=-1 errno=11")
    qemu.send("Y")
    val output = qemu.waitFor("mnbstdin: done")
    output should include("mnbstdin: woke=1 byte=89") // 'Y' = 0x59 = 89
    output should include("mnbstdin: done")
  }

  "aarch64 musl: epoll on stdin (TTY input subscriber)" in {
    // estdin adds fd 0 to an epoll instance and waits. Pre-injection
    // poll reports 0 events. After the harness sends a keystroke the
    // tty server bumps the per-console fire_seq and notifies the
    // subscribed shim tid; epoll_wait wakes with EPOLLIN and read(0)
    // returns the byte.
    qemu.send("estdin\n")
    val pre = qemu.waitFor("mepoll_stdin: ready_for_input")
    pre should include("mepoll_stdin: idle=0")
    qemu.send("Z")
    val output = qemu.waitFor("mepoll_stdin: done")
    output should include("mepoll_stdin: woke=1 events=1")
    output should include("mepoll_stdin: read=1 byte=90")  // 'Z' = 0x5A = 90
    output should include("mepoll_stdin: done")
  }

  "aarch64 musl: timerfd_create / settime / gettime + epoll" in {
    // mtimerfd exercises slix-musl 356/357/358 (timerfd
    // create/gettime/settime). One-shot at 50ms fires once and
    // stops; periodic 30ms fires N≥1 times in ~100ms;
    // gettime preserves the interval.
    qemu.send("timerfd\n")
    val output = qemu.waitFor("mtimerfd: done")
    output should include("mtimerfd: oneshot=1 events=1")
    output should include("mtimerfd: oneshot_read=8 exp=1")
    output should include("mtimerfd: drained=-1 errno=11")
    output should include("mtimerfd: gettime_int_nsec=30000000")
    output should include("mtimerfd: done")
    // Periodic count is non-deterministic but must be >= 1.
    val periodicLine = output.linesIterator.find(_.contains("mtimerfd: periodic_read")).getOrElse("")
    val expValue = "exp=(\\d+)".r.findFirstMatchIn(periodicLine).map(_.group(1).toInt).getOrElse(0)
    expValue should be >= 1
  }

  "aarch64 musl: eventfd2 + epoll integration" in {
    // meventfd binds the eventfd2 syscall (slix-musl 156). Tests
    // empty-NB read returns EAGAIN, write 7 / read 7 round-trip,
    // epoll EPOLLIN fires when count > 0 and quiesces after drain,
    // and EFD_SEMAPHORE mode hands back 1 per read.
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

  "aarch64 net: inbound ICMP Port Unreachable surfaces as -ECONNREFUSED" in {
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

  "aarch64 net: NB-connect failure surfaces as SO_ERROR=ECONNREFUSED" in {
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

  "aarch64 musl: socket/connect/shutdown/read via libc wrappers" in {
    // msocket is a C program linked against slix's musl fork; uses
    // socket(), connect(), write(), shutdown(), read(), close(),
    // getsockname() through real libc wrappers. Validates that the
    // aarch64-slix syscall numbers (337/148/128/334/130/147/203)
    // match what musl's arch/aarch64-slix/bits/syscall.h.in says.
    // If this fails with -ENOSYS inside a wrapper, the shim and the
    // musl header are out of sync.
    //
    // Host peer on 127.0.0.1:18083 reads until EOF, replies with
    // the byte count. See build-c.sh for the build recipe (C source
    // at slix/test/socket.c).
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
    }, "tcp-msocket-peer")
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

  "aarch64 musl: open/read/lseek/close on /etc/passwd" in {
    // mfile (slix/test/file.c) opens /etc/passwd through musl's
    // open(2), reads ~256 bytes, lseeks back to 0, reads 16 more.
    // Validates that the shim's POSIX_FD_FILE kind, sys_openat,
    // sys_lseek, and the VFS read/write grant plumbing all line
    // up. The first read should return >0 bytes; the lseek should
    // return 0 (new absolute position); the second read should
    // return 16 starting from the same offset.
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

  "aarch64 musl: epoll_create1/ctl/wait on a UDP socket" in {
    // mepoll (slix/test/epoll.c) walks the level-triggered epoll
    // path: empty wait times out, sendto-self makes the fd
    // readable, drain returns to idle. Validates the shim's
    // POSIX_FD_EPOLL kind, the entry table, and inet's
    // INET_CMD_POLL non-consuming readiness query.
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

  "aarch64 musl: epoll EPOLLET + EPOLLONESHOT (Phase A2)" in {
    // mepoll2 (slix/test/epoll2.c) validates the Phase A2
    // edge-trigger and one-shot semantics layered on the
    // inet→shim notify path. ET fires on rising edges only;
    // ONESHOT disarms after first wake and re-arms via MOD.
    qemu.send("mepoll2\n")
    val output = qemu.waitFor("mepoll2: done")
    output should include("mepoll2: et_first=1")
    // Edge-triggered: a second wait without new data must NOT
    // re-report. Level-triggered would say 1 here.
    output should include("mepoll2: et_no_redeliver=0")
    output should include("mepoll2: et_second=1")
    output should include("mepoll2: oneshot_first=1")
    output should include("mepoll2: oneshot_disarmed=0")
    output should include("mepoll2: oneshot_rearmed=1")
    output should include("mepoll2: done")
  }

  "aarch64 musl: epoll on a pipe (Phase A2 closeout)" in {
    // epoll_pipe (slix/test/epoll_pipe.c): exercise VFS_CMD_POLL
    // + the VFS epoll subscriber list landed in this chunk. Empty
    // pipe → 0 events; write end fires EPOLLIN via vfs_epoll_fire
    // → notify_send_to → sleep_or_notify wake; close-write-end
    // surfaces EPOLLHUP+EPOLLIN (Linux semantics: EOF reads as
    // readable). Without this work the EPOLLIN edge would never
    // surface — POSIX_FD_FILE used to report want unconditionally.
    qemu.send("epoll_pipe\n")
    val output = qemu.waitFor("mepoll_pipe: done")
    output should include("mepoll_pipe: empty=0")
    output should include("mepoll_pipe: after_write=1 events=1")
    // EPOLLHUP=0x10, EPOLLIN=0x01 → 0x11 = 17.
    output should include("mepoll_pipe: after_close=1 events=17")
    output should include("mepoll_pipe: done")
  }

  "aarch64 musl: non-blocking accept (Phase B)" in {
    // mnbacc (slix/test/nbacc.c): non-blocking listen fd that
    // first accepts on an empty queue (-EAGAIN), then waits via
    // epoll_wait for EPOLLIN, then accepts the queued child.
    // Host side connects to localhost:28080 (slirp forwards to
    // guest port 7890) once the guest prints "mnbacc: ready".
    qemu.send("mnbacc\n")
    val pre = qemu.waitFor("mnbacc: ready")
    val client = new java.net.Socket()
    client.connect(new java.net.InetSocketAddress("127.0.0.1", 28080), 5000)
    try
      client.getOutputStream.write("ping".getBytes())
      client.getOutputStream.flush()
      val post = qemu.waitFor("mnbacc: done")
      pre should include("mnbacc: empty=-1 errno=11")    // EAGAIN
      post should include("mnbacc: wait=1")
      post should not include "mnbacc: accept=-1"
      post should include("mnbacc: read=4 data='ping'")
      post should include("mnbacc: done")
    finally client.close()
  }

  "aarch64 musl: non-blocking connect (Phase B)" in {
    // mnbcon (slix/test/nbcon.c): O_NONBLOCK connect to
    // 10.0.2.2:18080 (slirp routes to host 127.0.0.1:18080)
    // surfaces -EINPROGRESS, EPOLLOUT fires once SYN-ACK lands,
    // and the round-trip works end-to-end.
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
    }, "tcp-echo-server-mnbcon")
    echoThread.setDaemon(true)
    echoThread.start()
    try
      qemu.send("mnbcon\n")
      val output = qemu.waitFor("mnbcon: done")
      output should include("mnbcon: connect=-1 errno=115") // EINPROGRESS
      output should include("mnbcon: wait=1")
      output should include("mnbcon: sent=10")
      output should include("mnbcon: read=10 data='nbcon-ping'")
      output should include("mnbcon: done")
    finally
      server.close()
      echoThread.join(2000)
  }

  "aarch64 musl: pipe2 + write + read + EOF" in {
    // mpipe (slix/test/pipe.c) creates a pipe, writes a string,
    // reads it back, closes the write end, then reads again
    // expecting EOF. Exercises sys_pipe2 → VFS_CMD_PIPE → two
    // POSIX_FD_FILE handles, plus the VFS pipe close-end path.
    qemu.send("mpipe\n")
    val output = qemu.waitFor("mpipe: done")
    output should include("mpipe: pipe2=0")
    output should include("mpipe: write=17")
    output should include("mpipe: read=17 data='ping through pipe'")
    output should include("mpipe: read_after_close=0")
    output should include("mpipe: done")
  }

  "aarch64 musl: sendmsg/recvmsg via libc wrappers" in {
    // mmsg validates the shim's msghdr offset parsing by going
    // through musl's real sendmsg(3) / recvmsg(3) — those build
    // the struct with the layout from
    // slix/musl/include/sys/socket.h. UDP loopback round-trip
    // with a 2-iov gather and a single-iov scatter; payload is
    // "hello msghdr" and src_port mirrors the bound port (7790).
    qemu.send("mmsg\n")
    val output = qemu.waitFor("mmsg: done")
    output should include("mmsg: socket=3")
    output should include("mmsg: bind=0")
    output should include("mmsg: sendmsg=12")
    output should include("mmsg: recvmsg=12 data='hello msghdr'")
    output should include("mmsg: src_port=7790")
    output should include("mmsg: done")
  }

  "aarch64 timer: subscribe fires expected count in N ticks" in {
    // test_timer subscribes to a period=5 timer and waits for 10
    // notifications via notify_wait/notify_read_self. Proves the
    // kernel's svc_timer_subscribe path is load-bearing-reliable
    // during a quiet channel (no incoming frames, no other wakes).
    // If the kernel timer wake is dropped silently, notify_wait
    // blocks forever and the harness times out — a detectable fail.
    qemu.send("test_timer\n")
    val output = qemu.waitFor("test_timer: ok")
    output should include("test_timer: subscribed idx=")
    output should include("test_timer: got 10 wakes value=2 delta=")
    output should include("test_timer: ok")
    output should not include "test_timer: FAIL"
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

  "aarch64 tcp: cwnd grows on ACK (slow start)" in {
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

  "aarch64 tcp: fast retransmit + recovery round-trip" in {
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

  // RST-on-unsolicited-SYN is implemented in inet_proto.lsysl
  // (inet_tcp_emit_rst + handle_segment listen-miss dispatch) but
  // can't be validated through QEMU's user-mode slirp: hostfwd
  // completes the host-side three-way handshake locally regardless
  // of what the guest does, so a stateless RST from the guest
  // doesn't surface to the host dial as ConnectException. Direct
  // validation needs tap networking or a guest-side pcap. The
  // positive path is exercised by the passive-open test below.

  "aarch64 tcp: passive open, accept, echo, close" in {
    // test_tcp_srv listens on :7890. QEMU's hostfwd=tcp::28080-:7890
    // forwards host dials of 127.0.0.1:28080 into the guest. We
    // send "ping\n", expect the same bytes back, close cleanly.
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

  "aarch64 tcp: multi-client passive open stress" in {
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

  "aarch64 tcp: minimal HTTP/1.0 interop (httpd)" in {
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

  "aarch64 dns: resolve against a mock DNS server" in {
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

  "aarch64 wget: resolve + fetch via mock DNS and mock HTTP" in {
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

  "aarch64 tcp: multi-request httpd accept loop" in {
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

  "aarch64 tcp: 900-byte multi-segment transfer" in {
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

  "aarch64 tcp: active-open drain-recv 900 bytes" in {
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

  "aarch64 tcp: VFS bridge (connect/read/write/close)" in {
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

  "aarch64 tcp: accept + send on CLOSE_WAIT child (peer already FIN'd)" in {
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

  "aarch64 tcp: VFS listen bridge (connect/accept/read/write/close)" in {
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

  "aarch64 tcp: VFS listen bridge accepts optional ',backlog' suffix" in {
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

  "aarch64 tcp: in-guest 127.0.0.1 loopback round trip" in {
    // test_tcp_lpbk drives the loopback fastpath added to
    // inet_tcp_emit / inet_tcp_emit_rst: client and listener live
    // in the same guest and exchange payloads over 127.0.0.1
    // without any NIC/slirp involvement.
    val output = qemu.command("test_tcp_lpbk")
    output should include("lpbk:ok")
    output should not include "lpbk:bad"
  }

  "aarch64 udp: loopback gate covers 127/8 + own_ip" in {
    // The pre-existing UDP loopback shortcut only matched
    // 127.0.0.1 exactly. inet_handle_sendto now uses
    // inet_is_loopback_ip, so 127.0.0.5 and 10.0.2.15 (our
    // QEMU lease) also short-circuit through the in-memory
    // queue instead of trying ARP and silently failing.
    val output = qemu.command("test_udp_lpbk")
    output should include("udplo: ok")
    output should not include "udplo: bad"
  }

  "aarch64 icmp: ping 127.0.0.1 returns immediately" in {
    // inet_send_icmp_echo_to short-circuits to inet_ping_deliver
    // when the destination is loopback — `ping 127.0.0.1` sees
    // a synthesized reply on the same tick with rtt=0 instead of
    // the request silently dropping at inet_resolve_mac.
    val output = qemu.command("ping -c 1 127.0.0.1")
    output should include("reply from 127.0.0.1")
    output should include("1 sent, 1 received")
  }

  "aarch64 tcp: getsockopt(TCP_INFO) on ESTABLISHED loopback fd" in {
    // test_tcp_info opens an in-guest 127.0.0.1 connection and
    // probes getsockopt(IPPROTO_TCP, TCP_INFO). Verifies the
    // 104-byte struct is fully written, tcpi_state maps to 1
    // (TCP_ESTABLISHED), and tcpi_snd_mss decodes as a sane
    // little-endian u32.
    val output = qemu.command("test_tcp_info")
    output should include("tcpinfo: ok")
    output should not include "tcpinfo: bad"
  }

  "aarch64 procid: getpid/getppid/getuid family + getrandom" in {
    // Process / thread identity syscalls + xorshift-based getrandom.
    // Slix has no multi-threading and boots root, so most return
    // 0 or 1; getrandom is best-effort and just verifies two
    // consecutive calls give different bytes.
    val output = qemu.command("test_proc_id")
    output should include("procid: ok")
    output should not include "procid: bad"
  }

  "aarch64 time: clock_gettime / gettimeofday / clock_getres / nanosleep" in {
    // POSIX time syscalls fed off uptime() at 100Hz. Verifies
    // clock_getres reports 10ms, clock_gettime + gettimeofday
    // agree within 20ms, and nanosleep(50ms) advances the
    // clock by at least 40ms.
    val output = qemu.command("test_clock")
    output should include("clock: ok")
    output should not include "clock: bad"
  }

  "aarch64 fs: fsync / fdatasync / sync / syncfs no-op stubs" in {
    // No on-disk persistence yet; these return 0 (or -EBADF for
    // bad fds) so defensive sqlite/log-writer patterns don't
    // crash on -ENOSYS.
    val output = qemu.command("test_fsync")
    output should include("fsync: ok")
    output should not include "fsync: bad"
  }

  "aarch64 sockopt: SO_TYPE/DOMAIN/PROTOCOL/ACCEPTCONN" in {
    // test_sockinfo verifies the four read-only introspection
    // getsockopts the shim now reports off the fd kind +
    // is_listen flag. Three fds: UDP, TCP pre-listen, TCP
    // post-listen.
    val output = qemu.command("test_sockinfo")
    output should include("sockinfo: ok")
    output should not include "sockinfo: bad"
  }

  "aarch64 tcp: out-of-order reassembly self-test" in {
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

  "aarch64 ip: fragmentation reassembly self-test" in {
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

  "aarch64 ip: fragmentation RFC corners (overlap + timeout)" in {
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

  "aarch64 udp: 1024-byte datagram via 127.0.0.1 loopback" in {
    // Verifies the bumped UDP datagram cap (512 → 1472). Sends a
    // 1024-byte body with byte i = (i & 0xff), recvfrom-validates
    // the full body comes through. Catches truncation at the old
    // 512 boundary plus any reply-buffer overflow / underflow.
    val output = qemu.command("test_udp_big")
    output should include("udpbig: ok")
    output should not include "udpbig: bad"
  }

  "aarch64 udp: connect()/send()/recv() with default peer" in {
    // POSIX connect() on UDP saves a default peer; subsequent
    // send() (sendto with NULL addr) targets it. Then dissolve
    // via connect(AF_UNSPEC) and verify send returns -ENOTCONN.
    val output = qemu.command("test_udp_conn")
    output should include("udpcon: ok")
    output should not include "udpcon: bad"
  }

  "aarch64 dhcp: dhclient --test parses canned OFFER/ACK" in {
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

  "aarch64 dhcp: live bind via slirp's DHCP server" in {
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

  "aarch64 musl: per-fd EPOLLET edge isolation" in {
    // mepoll_multi (slix/test/epoll_multi.c): two UDP sockets share
    // an epoll instance, both EPOLLIN | EPOLLET. Firing one must
    // not re-deliver the other. Before the per-fd fire counter
    // landed, the shim's notify-wake bulk-cleared every entry's
    // `last_reported`, so an unrelated edge re-fired siblings.
    qemu.send("epoll_multi\n")
    val output = qemu.waitFor("mepoll_multi: done")
    output should include("mepoll_multi: after_a=1 data=10")
    output should include("mepoll_multi: idle=0")
    output should include("mepoll_multi: after_b=1 data0=11")
    output should not include "mepoll_multi: after_b=2"
    output should include("mepoll_multi: done")
  }

  "aarch64 musl: listen backlog enforcement (Phase F)" in {
    // mlbacklog (slix/test/lbacklog.c): listen() with backlog=2,
    // four parallel host connects. Two of the four SYNs land in
    // the queue immediately; the other two are dropped at SYN
    // time and only succeed after the peer's automatic retransmit
    // — proves both that the cap is enforced AND that a peer can
    // recover via its standard retry path. All four eventually
    // get served once the accept loop drains the queue.
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
      val t = new Thread(runnable, s"aarch64-mlbacklog-client-$tag")
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
}
