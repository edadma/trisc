/* mepoll2: validate Phase A2 epoll semantics — EPOLLET (edge-
 * triggered) only fires on rising edges, and EPOLLONESHOT
 * disarms after the first wake (re-armed via EPOLL_CTL_MOD).
 * Both are layered on the inet→shim notify path; the level-
 * triggered behaviour is already covered by mepoll.
 *
 * Sequence:
 *   1. Bind a UDP socket on 127.0.0.1:7792, watch with EPOLLIN
 *      | EPOLLET. Send-self → epoll_wait reports 1 event with
 *      EPOLLIN. A second epoll_wait without further sends sees
 *      no event (level-triggered would re-report; ET filters).
 *   2. Drain via recvfrom, then send another datagram. The
 *      rising edge fires again — confirms ET re-arms each edge.
 *   3. EPOLL_CTL_MOD to EPOLLIN | EPOLLONESHOT. Send-self →
 *      epoll_wait reports 1 event. Second send-self → second
 *      epoll_wait reports 0 events (one-shot disarmed).
 *   4. EPOLL_CTL_MOD same mask → re-arms. Third event surfaces.
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
    if (ep < 0) { wstr("mepoll2: create<0\n"); return 1; }

    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) { wstr("mepoll2: sock<0\n"); return 2; }

    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(7792);
    addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        wstr("mepoll2: bind<0\n"); return 3;
    }

    /* ---- ET (edge-triggered) ---- */
    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLET;
    ev.data.u64 = 0xa5a5a5a5;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, fd, &ev) < 0) {
        wstr("mepoll2: ctl_add<0\n"); return 4;
    }

    const char *m = "et-ping";
    sendto(fd, m, (int)strlen(m), 0, (struct sockaddr *)&addr, sizeof(addr));

    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 1000);
    wstr("mepoll2: et_first=");
    wint(n);
    wstr("\n");

    /* No new sends — ET should NOT report again, even though
     * the socket is still readable level-wise. */
    n = epoll_wait(ep, out, 2, 30);
    wstr("mepoll2: et_no_redeliver=");
    wint(n);
    wstr("\n");

    /* Drain and send again — new rising edge fires. */
    char rbuf[64];
    recvfrom(fd, rbuf, sizeof(rbuf), 0, NULL, NULL);
    sendto(fd, m, (int)strlen(m), 0, (struct sockaddr *)&addr, sizeof(addr));

    n = epoll_wait(ep, out, 2, 1000);
    wstr("mepoll2: et_second=");
    wint(n);
    wstr("\n");
    recvfrom(fd, rbuf, sizeof(rbuf), 0, NULL, NULL);

    /* ---- EPOLLONESHOT ---- */
    ev.events = EPOLLIN | EPOLLONESHOT;
    ev.data.u64 = 0xc0ffee;
    if (epoll_ctl(ep, EPOLL_CTL_MOD, fd, &ev) < 0) {
        wstr("mepoll2: ctl_mod<0\n"); return 5;
    }

    sendto(fd, m, (int)strlen(m), 0, (struct sockaddr *)&addr, sizeof(addr));
    n = epoll_wait(ep, out, 2, 1000);
    wstr("mepoll2: oneshot_first=");
    wint(n);
    wstr("\n");
    recvfrom(fd, rbuf, sizeof(rbuf), 0, NULL, NULL);

    sendto(fd, m, (int)strlen(m), 0, (struct sockaddr *)&addr, sizeof(addr));
    /* Disarmed — should not fire even though data is readable. */
    n = epoll_wait(ep, out, 2, 30);
    wstr("mepoll2: oneshot_disarmed=");
    wint(n);
    wstr("\n");

    /* Re-arm via MOD with same mask. */
    if (epoll_ctl(ep, EPOLL_CTL_MOD, fd, &ev) < 0) {
        wstr("mepoll2: rearm<0\n"); return 6;
    }
    n = epoll_wait(ep, out, 2, 1000);
    wstr("mepoll2: oneshot_rearmed=");
    wint(n);
    wstr("\n");

    close(fd);
    close(ep);
    wstr("mepoll2: done\n");
    return 0;
}
