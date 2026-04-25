/* mepoll_pipe: validate Phase A2 closeout — VFS_CMD_POLL +
 * VFS epoll subscriber list make pipes wake epoll_wait without
 * busy-polling, and the kernel's `sleep_or_notify` lets a
 * finite-timeout wait take the same fast path.
 *
 * Sequence:
 *   1. Create a pipe + epoll instance, register the read end
 *      with EPOLLIN.
 *   2. Drain readiness with a short poll (timeout=0) before any
 *      data — should report 0 events.
 *   3. Write to the pipe, immediately call epoll_wait with a
 *      finite timeout. Expect 1 event with EPOLLIN delivered
 *      well under the timeout (the wake_on_edge path fires
 *      from vfs_pipe_write → notify_send_to → sleep_or_notify
 *      returns immediately).
 *   4. Read the data so the pipe is empty again.
 *   5. Close the write end. epoll_wait should report EPOLLHUP
 *      (and EPOLLIN, per Linux semantics: empty pipe with closed
 *      writer reads as EOF, which counts as readable).
 */
#include <sys/epoll.h>
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
    if (ep < 0) { wstr("mepoll_pipe: create<0\n"); return 1; }

    int pfd[2];
    if (pipe(pfd) < 0) { wstr("mepoll_pipe: pipe<0\n"); return 2; }

    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0x900dface;
    if (epoll_ctl(ep, EPOLL_CTL_ADD, pfd[0], &ev) < 0) {
        wstr("mepoll_pipe: ctl_add<0\n"); return 3;
    }

    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 0);
    wstr("mepoll_pipe: empty=");
    wint(n);
    wstr("\n");

    const char *m = "ping";
    write(pfd[1], m, 4);

    n = epoll_wait(ep, out, 2, 1000);
    wstr("mepoll_pipe: after_write=");
    wint(n);
    if (n > 0) {
        wstr(" events=");
        wint(out[0].events);
    }
    wstr("\n");

    char rbuf[16];
    read(pfd[0], rbuf, sizeof(rbuf));

    close(pfd[1]);
    n = epoll_wait(ep, out, 2, 1000);
    wstr("mepoll_pipe: after_close=");
    wint(n);
    if (n > 0) {
        wstr(" events=");
        wint(out[0].events);
    }
    wstr("\n");

    close(pfd[0]);
    close(ep);
    wstr("mepoll_pipe: done\n");
    return 0;
}
