/* mepoll_stdin: validate epoll on stdin (POSIX_FD_STDIN routed
 * through TTY_CMD_POLL + TTY input subscriber list).
 *
 * Sequence:
 *   1. epoll_create + add stdin (fd 0) with EPOLLIN.
 *   2. epoll_wait(timeout=0): expect 0 events (no input yet).
 *   3. Print a marker so the test harness knows to inject a
 *      keystroke into the slix console.
 *   4. epoll_wait(timeout=2000ms): blocks until the harness
 *      sends a keystroke; expect 1 event with EPOLLIN.
 *   5. read(fd 0, buf, 1) returns the byte the harness sent.
 */
#include <sys/epoll.h>
#include <unistd.h>
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
    int ep = epoll_create1(0);
    if (ep < 0) { wstr("mepoll_stdin: epoll<0\n"); return 1; }

    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0xfeed;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, 0, &ev) < 0) {
        wstr("mepoll_stdin: ctl_add<0\n"); return 2;
    }

    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 0);
    wstr("mepoll_stdin: idle=");
    wlong(n);
    wstr("\n");

    /* Marker for the harness — once it sees this it injects a key. */
    wstr("mepoll_stdin: ready_for_input\n");

    n = epoll_wait(ep, out, 2, 5000);
    wstr("mepoll_stdin: woke=");
    wlong(n);
    if (n > 0) { wstr(" events="); wlong(out[0].events); }
    wstr("\n");

    if (n > 0) {
        char rbuf[4];
        int r = read(0, rbuf, 1);
        wstr("mepoll_stdin: read=");
        wlong(r);
        if (r > 0) {
            wstr(" byte=");
            wlong((unsigned char)rbuf[0]);
        }
        wstr("\n");
    }

    close(ep);
    wstr("mepoll_stdin: done\n");
    return 0;
}
