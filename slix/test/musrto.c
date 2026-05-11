/* musrto: validate TCP_USER_TIMEOUT (RFC 5482) enforcement.
 *
 * Single-process loopback test that simulates "peer went dark
 * mid-conversation".  Slix's loopback fastpath ACKs every
 * segment synchronously, so a normal in-process server would
 * never let the user-timeout fire — the client's snd_una would
 * track snd_nxt forever.  We use a slix-only test injector to
 * arm an ACK blackhole on the *server child* slot: once armed,
 * inet_tcp_emit silently drops pure-ACK segments from the child,
 * while SYN/FIN/RST handshake bytes still flow.  The client then
 * sends a payload, the data segment is received but never ACK'd,
 * earliest_unack_ms stays anchored, and inet_tcp_scan_timers
 * aborts the client slot once now - earliest_unack_ms exceeds
 * user_timeout_ms.  The aborted slot records pending_error =
 * ETIMEDOUT (110); the next syscall on the fd surfaces it.
 *
 * Wire-up:
 *   - 127.0.0.1:7797
 *   - TCP_USER_TIMEOUT = 200 ms on the *client* fd
 *   - SOL_SLIX_TEST (0x534c) optname=1 arms the blackhole on the
 *     accepted child fd; the shim's setsockopt routes that to
 *     INET_CMD_TCP_BLACKHOLE_ACKS_TEST (cmd 53)
 *   - Wait ~250 ms, second send() should return -1/ETIMEDOUT
 *
 * Pass criteria:
 *   - setsockopt(TCP_USER_TIMEOUT) returns 0
 *   - blackhole arm returns 0
 *   - first send returns positive (data buffered, segment emitted)
 *   - after ~250 ms, second send/recv returns -1 with
 *     errno == ETIMEDOUT (110), proving the abort fired
 */
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <time.h>

#ifndef TCP_USER_TIMEOUT
#define TCP_USER_TIMEOUT 18
#endif

/* slix-only test channel — see oskit/posix/shim.lsysl::sys_setsockopt
 * level == 0x534c branch. */
#define SOL_SLIX_TEST 0x534c
#define SLIX_TEST_BLACKHOLE_ACKS 1

static void wstr(const char *s) {
    size_t n = 0;
    while (s[n]) n++;
    write(1, s, n);
}

static void wint(long v) {
    char buf[24];
    int i = 0, neg = 0;
    if (v < 0) { neg = 1; v = -v; }
    if (v == 0) buf[i++] = '0';
    while (v > 0) { buf[i++] = '0' + (v % 10); v /= 10; }
    if (neg) buf[i++] = '-';
    char out[24];
    for (int j = 0; j < i; j++) out[j] = buf[i - 1 - j];
    write(1, out, i);
}

int main(void) {
    int ls = socket(AF_INET, SOCK_STREAM, 0);
    if (ls < 0) return 1;

    struct sockaddr_in baddr = {0};
    baddr.sin_family = AF_INET;
    baddr.sin_port = htons(7797);
    baddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int br = bind(ls, (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("musrto: bind=");
    wint(br);
    wstr("\n");
    if (br < 0) return 2;

    int lr = listen(ls, 4);
    wstr("musrto: listen=");
    wint(lr);
    wstr("\n");
    if (lr < 0) return 3;

    int cl = socket(AF_INET, SOCK_STREAM, 0);
    if (cl < 0) return 4;

    int uto = 200;  /* milliseconds */
    int ur = setsockopt(cl, IPPROTO_TCP, TCP_USER_TIMEOUT,
                        &uto, sizeof(uto));
    wstr("musrto: setsockopt_userto=");
    wint(ur);
    wstr("\n");
    if (ur < 0) return 5;

    /* Verify round-trip: getsockopt should echo 200. */
    int got = 0;
    socklen_t glen = sizeof(got);
    int gr = getsockopt(cl, IPPROTO_TCP, TCP_USER_TIMEOUT,
                       &got, &glen);
    wstr("musrto: getsockopt_userto=");
    wint(gr);
    wstr(" val=");
    wint(got);
    wstr("\n");

    struct sockaddr_in caddr = {0};
    caddr.sin_family = AF_INET;
    caddr.sin_port = htons(7797);
    caddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int cr = connect(cl, (struct sockaddr *)&caddr, sizeof(caddr));
    wstr("musrto: connect=");
    wint(cr);
    wstr("\n");
    if (cr < 0) return 6;

    int ch = accept(ls, NULL, NULL);
    wstr("musrto: accept=");
    wint(ch);
    wstr("\n");
    if (ch < 0) return 7;

    /* Arm the ACK blackhole on the accepted child so the server
     * stops emitting acknowledgements while SYN/FIN/RST still
     * flow.  The blackhole is the *only* way to make a slix
     * loopback peer "go dark" — without it, the synchronous
     * fastpath would ACK every segment immediately. */
    int on = 1;
    int bh = setsockopt(ch, SOL_SLIX_TEST,
                       SLIX_TEST_BLACKHOLE_ACKS,
                       &on, sizeof(on));
    wstr("musrto: blackhole=");
    wint(bh);
    wstr("\n");
    if (bh < 0) return 8;

    /* Send a payload: the segment is emitted, the server's child
     * receives it but its ACK is silently dropped. */
    static char payload[256];
    memset(payload, 'X', sizeof(payload));

    long s1 = send(cl, payload, sizeof(payload), 0);
    wstr("musrto: send1=");
    wint(s1);
    wstr("\n");
    if (s1 <= 0) return 9;

    /* Wait past the 200 ms TCP_USER_TIMEOUT — kernel scan_timers
     * fires every tick (10 ms) so 250 ms is comfortably past the
     * deadline. */
    struct timespec ts;
    ts.tv_sec = 0;
    ts.tv_nsec = 250 * 1000 * 1000;
    nanosleep(&ts, NULL);

    /* Next syscall on the aborted slot must surface ETIMEDOUT
     * via PosixFd.last_error (set by close_slot's pending_error
     * route through the shim's wake-poll). */
    long s2 = send(cl, payload, sizeof(payload), 0);
    wstr("musrto: send2=");
    if (s2 < 0) {
        wstr("ETIMEDOUT errno=");
        wint(errno);
    } else {
        wint(s2);
    }
    wstr("\n");

    int ok = (s2 < 0 && errno == ETIMEDOUT) ? 1 : 0;
    wstr("musrto: pass=");
    wint(ok);
    wstr("\n");

    close(ch);
    close(cl);
    close(ls);
    wstr("musrto: done\n");
    return 0;
}
