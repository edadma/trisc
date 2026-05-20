/* msignalfd: validate signalfd4 end-to-end.
 *
 * Sequence:
 *   1. sigprocmask(SIG_BLOCK, {SIGUSR1, SIGUSR2}) so the kernel
 *      walker doesn't deliver them via a handler.
 *   2. signalfd4(-1, &mask, _NSIG/8, SFD_NONBLOCK) — fresh fd.
 *   3. read with nothing pending — expect -EAGAIN.
 *   4. raise(SIGUSR1); read returns 128, ssi_signo == SIGUSR1.
 *   5. raise(SIGUSR2); epoll_wait reports EPOLLIN on the sfd.
 *   6. read after epoll drains the queue; epoll_wait timeout
 *      reports 0.
 *   7. Two-at-once: raise(SIGUSR1) + raise(SIGUSR2), read with a
 *      256-byte buffer returns 256 and contains both signo values.
 */
#include <sys/signalfd.h>
#include <sys/epoll.h>
#include <signal.h>
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
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGUSR1);
    sigaddset(&mask, SIGUSR2);
    if (sigprocmask(SIG_BLOCK, &mask, NULL) != 0) {
        wstr("msignalfd: block<0 errno="); wlong(errno); wstr("\n"); return 1;
    }

    int sfd = signalfd(-1, &mask, SFD_NONBLOCK);
    if (sfd < 0) { wstr("msignalfd: sfd<0 errno="); wlong(errno); wstr("\n"); return 2; }
    wstr("msignalfd: step2 sfd_ok\n");

    struct signalfd_siginfo si;
    int r = read(sfd, &si, sizeof(si));
    wstr("msignalfd: step3 empty_read=");
    wlong(r);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    if (raise(SIGUSR1) != 0) {
        wstr("msignalfd: raise1<0 errno="); wlong(errno); wstr("\n"); return 4;
    }
    memset(&si, 0, sizeof(si));
    r = read(sfd, &si, sizeof(si));
    wstr("msignalfd: step4 read=");
    wlong(r);
    wstr(" signo=");
    wlong(si.ssi_signo);
    wstr("\n");

    int ep = epoll_create1(0);
    if (ep < 0) { wstr("msignalfd: epoll<0\n"); return 5; }
    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0xc0ffee;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, sfd, &ev) < 0) {
        wstr("msignalfd: ctl_add<0\n"); return 6;
    }

    if (raise(SIGUSR2) != 0) {
        wstr("msignalfd: raise2<0 errno="); wlong(errno); wstr("\n"); return 7;
    }
    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 100);
    wstr("msignalfd: step5 epoll_n=");
    wlong(n);
    if (n > 0) { wstr(" events="); wlong(out[0].events); }
    wstr("\n");

    memset(&si, 0, sizeof(si));
    r = read(sfd, &si, sizeof(si));
    wstr("msignalfd: step6 read=");
    wlong(r);
    wstr(" signo=");
    wlong(si.ssi_signo);
    wstr("\n");
    n = epoll_wait(ep, out, 2, 0);
    wstr("msignalfd: step6 epoll_drained_n=");
    wlong(n);
    wstr("\n");

    raise(SIGUSR1);
    raise(SIGUSR2);
    struct signalfd_siginfo two[2];
    memset(two, 0, sizeof(two));
    r = read(sfd, two, sizeof(two));
    wstr("msignalfd: step7 read=");
    wlong(r);
    wstr(" signo0=");
    wlong(two[0].ssi_signo);
    wstr(" signo1=");
    wlong(two[1].ssi_signo);
    wstr("\n");

    close(sfd);
    close(ep);
    wstr("msignalfd: done\n");
    return 0;
}
