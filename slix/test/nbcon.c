/* mnbcon: validate Phase B non-blocking connect() with
 * EINPROGRESS + EPOLLOUT on completion. Uses the existing
 * test_tcp pattern (guest connects to 10.0.2.2:18080, host
 * runs an echo server on 127.0.0.1:18080 — slirp transparently
 * loops 10.0.2.2 connections back to the host).
 *
 *  1. Open a fresh TCP socket, set O_NONBLOCK BEFORE connect.
 *  2. connect(10.0.2.2:18080) returns -EINPROGRESS.
 *  3. epoll_wait for EPOLLOUT; the inet→shim notify path
 *     (Phase A2) fires when SYN_SENT → ESTABLISHED.
 *  4. Send "nbcon-ping", read echo, confirm round-trip.
 */
#include <sys/epoll.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
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
    int cs = socket(AF_INET, SOCK_STREAM, 0);
    fcntl(cs, F_SETFL, O_NONBLOCK);

    struct sockaddr_in sa = {0};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(18080);
    sa.sin_addr.s_addr = inet_addr("10.0.2.2");

    int rc = connect(cs, (struct sockaddr *)&sa, sizeof(sa));
    wstr("mnbcon: connect=");
    wint(rc);
    wstr(" errno=");
    wint(errno);
    wstr("\n");

    int ep = epoll_create1(0);
    struct epoll_event ev;
    ev.events = EPOLLOUT;
    ev.data.u64 = 0x1234;
    epoll_ctl(ep, EPOLL_CTL_ADD, cs, &ev);

    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 5000);
    wstr("mnbcon: wait=");
    wint(n);
    wstr("\n");

    if (n > 0) {
        const char *m = "nbcon-ping";
        int sent = (int)write(cs, m, 10);
        wstr("mnbcon: sent=");
        wint(sent);
        wstr("\n");
        /* Read back the echo. */
        char rb[16] = {0};
        int rb_n = (int)read(cs, rb, sizeof(rb) - 1);
        wstr("mnbcon: read=");
        wint(rb_n);
        wstr(" data='");
        write(1, rb, rb_n > 0 ? rb_n : 0);
        wstr("'\n");
    }

    close(cs);
    close(ep);
    wstr("mnbcon: done\n");
    return 0;
}
