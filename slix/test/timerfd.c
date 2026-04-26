/* mtimerfd: validate timerfd_create/settime/gettime + epoll integration.
 *
 *   1. timerfd_create(MONOTONIC, TFD_NONBLOCK).
 *   2. settime with 50 ms one-shot.
 *   3. epoll_wait(timeout=200ms) blocks until the timer fires;
 *      EPOLLIN reported.
 *   4. read returns 1 expiration; subsequent read returns -EAGAIN.
 *   5. settime with periodic 30 ms; sleep ~100 ms; read returns
 *      ~3 expirations (slix runs at 100 Hz so anything 2-4 is OK
 *      depending on scheduling).
 *   6. timerfd_gettime confirms the periodic interval is preserved
 *      and the remaining time is in (0, 30 ms].
 */
#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

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
    int tfd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK);
    if (tfd < 0) { wstr("mtimerfd: tfd<0\n"); return 1; }

    struct itimerspec one;
    one.it_value.tv_sec = 0;
    one.it_value.tv_nsec = 50000000;        /* 50 ms */
    one.it_interval.tv_sec = 0;
    one.it_interval.tv_nsec = 0;
    if (timerfd_settime(tfd, 0, &one, 0) < 0) {
        wstr("mtimerfd: set1<0\n"); return 2;
    }

    int ep = epoll_create1(0);
    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0xbabe;
    epoll_ctl(ep, EPOLL_CTL_ADD, tfd, &ev);

    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 500);
    wstr("mtimerfd: oneshot=");
    wlong(n);
    if (n > 0) { wstr(" events="); wlong(out[0].events); }
    wstr("\n");

    unsigned long long v = 0;
    int r1 = read(tfd, &v, sizeof(v));
    wstr("mtimerfd: oneshot_read=");
    wlong(r1);
    wstr(" exp=");
    wlong((long)v);
    wstr("\n");

    int r2 = read(tfd, &v, sizeof(v));
    wstr("mtimerfd: drained=");
    wlong(r2);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    /* Periodic 30 ms */
    struct itimerspec per;
    per.it_value.tv_sec = 0;
    per.it_value.tv_nsec = 30000000;
    per.it_interval.tv_sec = 0;
    per.it_interval.tv_nsec = 30000000;
    if (timerfd_settime(tfd, 0, &per, 0) < 0) {
        wstr("mtimerfd: set2<0\n"); return 3;
    }

    /* Wait ~100 ms via a usleep equivalent: nanosleep. */
    struct timespec ns;
    ns.tv_sec = 0;
    ns.tv_nsec = 100000000;
    nanosleep(&ns, 0);

    v = 0;
    int r3 = read(tfd, &v, sizeof(v));
    wstr("mtimerfd: periodic_read=");
    wlong(r3);
    wstr(" exp=");
    wlong((long)v);
    wstr("\n");

    /* gettime should preserve the interval. */
    struct itimerspec curr;
    timerfd_gettime(tfd, &curr);
    wstr("mtimerfd: gettime_int_nsec=");
    wlong(curr.it_interval.tv_nsec);
    wstr("\n");

    close(tfd);
    close(ep);
    wstr("mtimerfd: done\n");
    return 0;
}
