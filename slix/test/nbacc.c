/* mnbacc: validate Phase B non-blocking accept(). Uses the
 * existing host-into-guest pattern (slirp hostfwd 28080 → 7890).
 *
 *  1. Guest listens on 0.0.0.0:7890 with O_NONBLOCK on the
 *     listen fd.
 *  2. accept() with empty queue → -EAGAIN.
 *  3. epoll_ctl(ADD) the listen fd watching EPOLLIN.
 *  4. Host (Scala test side) connects, sends "ping".
 *  5. epoll_wait fires; accept() now succeeds.
 *  6. Read the host's payload to confirm the connection is real.
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
    int ls = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in sa = {0};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(7890);
    sa.sin_addr.s_addr = htonl(INADDR_ANY);
    bind(ls, (struct sockaddr *)&sa, sizeof(sa));
    listen(ls, 4);
    fcntl(ls, F_SETFL, O_NONBLOCK);

    /* Empty queue → EAGAIN. */
    int r = accept(ls, NULL, NULL);
    wstr("mnbacc: empty=");
    wint(r);
    wstr(" errno=");
    wint(errno);
    wstr("\n");

    int ep = epoll_create1(0);
    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.u64 = 0xfeedface;
    epoll_ctl(ep, EPOLL_CTL_ADD, ls, &ev);

    /* Tell the host we're ready to accept. */
    wstr("mnbacc: ready\n");

    /* Host (test harness) initiates connect now. */
    struct epoll_event out[2];
    int n = epoll_wait(ep, out, 2, 5000);
    wstr("mnbacc: wait=");
    wint(n);
    wstr("\n");

    int child = accept(ls, NULL, NULL);
    wstr("mnbacc: accept=");
    wint(child);
    wstr("\n");

    if (child >= 0) {
        char rb[16] = {0};
        int rb_n = (int)read(child, rb, sizeof(rb) - 1);
        wstr("mnbacc: read=");
        wint(rb_n);
        wstr(" data='");
        write(1, rb, rb_n > 0 ? rb_n : 0);
        wstr("'\n");
        close(child);
    }
    close(ls);
    close(ep);
    wstr("mnbacc: done\n");
    return 0;
}
