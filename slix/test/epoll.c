/* mepoll: validate the shim's epoll implementation through musl
 * libc — epoll_create1, epoll_ctl(ADD/MOD/DEL), epoll_wait. The
 * test creates a UDP socket bound to 127.0.0.1:7791, sends a
 * datagram to itself, then asks epoll_wait to surface the
 * readiness. After draining via recvfrom, epoll_wait with
 * timeout=0 should return 0 ready events.
 *
 * If the shim's epoll plumbing or INET_CMD_POLL response drifts,
 * either the first wait returns 0 (timeout) or the second
 * returns >0 (false-positive readable). Both surface as obvious
 * wrong output here.
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

int main(void) {
    int ep = epoll_create1(0);
    wstr("mepoll: create=");
    wint(ep);
    wstr("\n");
    if (ep < 0) return 1;

    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    wstr("mepoll: sock=");
    wint(fd);
    wstr("\n");
    if (fd < 0) return 2;

    struct sockaddr_in baddr = {0};
    baddr.sin_family = AF_INET;
    baddr.sin_port = htons(7791);
    baddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int br = bind(fd, (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("mepoll: bind=");
    wint(br);
    wstr("\n");
    if (br < 0) return 3;

    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0xdeadbeef;
    int er = epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev);
    wstr("mepoll: ctl_add=");
    wint(er);
    wstr("\n");

    /* Nothing waiting yet — short timeout should expire. */
    struct epoll_event out[4];
    int n = epoll_wait(ep, out, 4, 30);
    wstr("mepoll: wait_idle=");
    wint(n);
    wstr("\n");

    /* Send a datagram to ourselves. */
    const char *msg = "epoll-payload";
    int mlen = (int)strlen(msg);
    int sr = (int)sendto(fd, msg, mlen, 0,
                         (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("mepoll: sendto=");
    wint(sr);
    wstr("\n");

    /* Now epoll_wait should report fd as readable. */
    n = epoll_wait(ep, out, 4, 1000);
    wstr("mepoll: wait_after_send=");
    wint(n);
    if (n > 0) {
        wstr(" events=0x");
        unsigned int e = out[0].events;
        char hex[9];
        int hi = 0;
        for (int s = 28; s >= 0; s -= 4) {
            int d = (e >> s) & 0xf;
            hex[hi++] = (char)(d < 10 ? '0' + d : 'a' + d - 10);
        }
        write(1, hex, hi);
        wstr(" data_ok=");
        wint(out[0].data.u64 == 0xdeadbeef ? 1 : 0);
    }
    wstr("\n");

    /* Drain the socket. */
    char rbuf[64];
    int rd = (int)recvfrom(fd, rbuf, sizeof(rbuf), 0, NULL, NULL);
    wstr("mepoll: recv=");
    wint(rd);
    wstr("\n");

    /* Now timeout=0 should return 0 ready events. */
    n = epoll_wait(ep, out, 4, 0);
    wstr("mepoll: wait_after_drain=");
    wint(n);
    wstr("\n");

    /* DEL the entry — subsequent ops on the deleted fd should
     * not be tracked. */
    int dr = epoll_ctl(ep, EPOLL_CTL_DEL, fd, NULL);
    wstr("mepoll: ctl_del=");
    wint(dr);
    wstr("\n");

    close(fd);
    close(ep);
    wstr("mepoll: done\n");
    return 0;
}
