/* mepoll_multi: validate per-fd EPOLLET edge isolation (Phase A2
 * closeout). Two sockets share an epoll instance, both registered
 * with EPOLLIN | EPOLLET. We fire them one at a time and confirm
 * that an unrelated edge on socket B does NOT re-deliver socket A.
 *
 * Before this fix, every notify wake bulk-cleared every entry's
 * `last_reported` in the shim, so a fresh edge on B would also
 * re-fire A even though A had already been delivered (no rising
 * bit) — a Node.js style loop with thousands of fds would see most
 * of them re-reported on every wake and waste a lot of work.
 *
 * Sequence:
 *  1. Bind A on 127.0.0.1:7795, B on 127.0.0.1:7796.
 *  2. ADD both with EPOLLIN | EPOLLET, distinct data tags.
 *  3. sendto(A) → epoll_wait → 1 event (A only).
 *  4. epoll_wait(timeout=20ms) → 0 events (ET filtered).
 *  5. sendto(B) → epoll_wait → 1 event (B only, not A).
 *  6. recvfrom both, close all.
 */
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <string.h>

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

static int bind_udp(int port) {
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) return -1;
    struct sockaddr_in sa = {0};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    sa.sin_addr.s_addr = inet_addr("127.0.0.1");
    if (bind(fd, (struct sockaddr *)&sa, sizeof(sa)) < 0) {
        close(fd);
        return -1;
    }
    return fd;
}

static int send_self(int fd, int port, const char *msg) {
    struct sockaddr_in sa = {0};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    sa.sin_addr.s_addr = inet_addr("127.0.0.1");
    return (int)sendto(fd, msg, (int)strlen(msg), 0,
                       (struct sockaddr *)&sa, sizeof(sa));
}

int main(void) {
    int ep = epoll_create1(0);
    int a = bind_udp(7795);
    int b = bind_udp(7796);
    if (ep < 0 || a < 0 || b < 0) {
        wstr("mepoll_multi: setup_fail\n");
        return 1;
    }
    wstr("mepoll_multi: a="); wint(a);
    wstr(" b="); wint(b); wstr("\n");

    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLET;
    ev.data.u64 = 0xa;
    epoll_ctl(ep, EPOLL_CTL_ADD, a, &ev);
    ev.data.u64 = 0xb;
    epoll_ctl(ep, EPOLL_CTL_ADD, b, &ev);

    /* Step 3: poke A only. ET should report A once. */
    send_self(a, 7795, "ping-a");
    struct epoll_event out[4];
    int n = epoll_wait(ep, out, 4, 1000);
    wstr("mepoll_multi: after_a=");
    wint(n);
    if (n == 1) {
        wstr(" data=");
        wint((long)out[0].data.u64);
    }
    wstr("\n");

    /* Step 4: ET filter — no new edge, no event. */
    n = epoll_wait(ep, out, 4, 20);
    wstr("mepoll_multi: idle=");
    wint(n);
    wstr("\n");

    /* Step 5: poke B. Only B should fire — A's bit is still ready
     * level-wise but its edge memory is intact, so EPOLLET filter
     * suppresses it.
     */
    send_self(b, 7796, "ping-b");
    n = epoll_wait(ep, out, 4, 1000);
    wstr("mepoll_multi: after_b=");
    wint(n);
    if (n >= 1) {
        wstr(" data0=");
        wint((long)out[0].data.u64);
    }
    if (n >= 2) {
        wstr(" data1=");
        wint((long)out[1].data.u64);
    }
    wstr("\n");

    char rbuf[32];
    recvfrom(a, rbuf, sizeof(rbuf), 0, NULL, NULL);
    recvfrom(b, rbuf, sizeof(rbuf), 0, NULL, NULL);
    close(a);
    close(b);
    close(ep);
    wstr("mepoll_multi: done\n");
    return 0;
}
