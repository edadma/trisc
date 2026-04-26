/* meventfd: validate eventfd2 + epoll integration.
 *
 * Sequence:
 *   1. eventfd(0, EFD_NONBLOCK) — fresh counter at 0.
 *   2. read should return -EAGAIN (counter is 0, NB).
 *   3. write 7 — counter becomes 7.
 *   4. read returns 7, counter resets to 0.
 *   5. write 5; epoll_wait(EPOLLIN) returns the fd ready.
 *   6. read drains; epoll_wait(EPOLLIN) with timeout 0 reports 0 events.
 *   7. Semaphore mode: eventfd(3, EFD_SEMAPHORE | EFD_NONBLOCK);
 *      three reads each return 1, fourth returns -EAGAIN.
 */
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

static void wstr(const char *s) {
    size_t n = 0;
    while (s[n]) n++;
    write(1, s, n);
}

static void wlong(long v) {
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
    int efd = eventfd(0, EFD_NONBLOCK);
    if (efd < 0) { wstr("meventfd: efd<0\n"); return 1; }

    eventfd_t v;
    int r1 = read(efd, &v, sizeof(v));
    wstr("meventfd: empty_read=");
    wlong(r1);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    if (eventfd_write(efd, 7) < 0) { wstr("meventfd: write7<0\n"); return 2; }
    if (eventfd_read(efd, &v) < 0) { wstr("meventfd: read<0\n"); return 3; }
    wstr("meventfd: after_write7=");
    wlong((long)v);
    wstr("\n");

    int ep = epoll_create1(0);
    if (ep < 0) { wstr("meventfd: epoll<0\n"); return 4; }
    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0xc0ffee;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, efd, &ev) < 0) {
        wstr("meventfd: ctl_add<0\n"); return 5;
    }

    eventfd_write(efd, 5);
    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 100);
    wstr("meventfd: epoll_after_write=");
    wlong(n);
    if (n > 0) { wstr(" events="); wlong(out[0].events); }
    wstr("\n");

    eventfd_read(efd, &v);
    n = epoll_wait(ep, out, 2, 0);
    wstr("meventfd: epoll_after_drain=");
    wlong(n);
    wstr("\n");

    close(efd);
    close(ep);

    int sfd = eventfd(3, EFD_SEMAPHORE | EFD_NONBLOCK);
    if (sfd < 0) { wstr("meventfd: sfd<0\n"); return 6; }
    eventfd_read(sfd, &v); wstr("meventfd: sem1="); wlong((long)v); wstr("\n");
    eventfd_read(sfd, &v); wstr("meventfd: sem2="); wlong((long)v); wstr("\n");
    eventfd_read(sfd, &v); wstr("meventfd: sem3="); wlong((long)v); wstr("\n");
    int r4 = read(sfd, &v, sizeof(v));
    wstr("meventfd: sem4=");
    wlong(r4);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    close(sfd);
    wstr("meventfd: done\n");
    return 0;
}
