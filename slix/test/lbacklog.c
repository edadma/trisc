/* mlbacklog: validate Phase F listen() backlog enforcement.
 *
 * Listens on :7890 with backlog=2. The host harness fires four
 * connects in parallel; with a queue depth of 2, two of them are
 * accepted via the first SYN, the other two are dropped at SYN
 * time and only succeed after the peer's automatic SYN retransmit
 * — which fires once the queue drains as accept()s complete.
 *
 * The test passes if all four host connections are eventually
 * served (proves the listener didn't lose sockets) AND the guest
 * prints `mlbacklog: child[N]` for every N in 0..3 (proves the
 * accept loop dequeued each one rather than getting an orphan).
 *
 * Each child receives a single tag byte from the host and replies
 * with "ack\n", so the host can match its connection by tag.
 */
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
    int ls = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in sa = {0};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(7890);
    sa.sin_addr.s_addr = htonl(INADDR_ANY);
    bind(ls, (struct sockaddr *)&sa, sizeof(sa));
    listen(ls, 2);

    wstr("mlbacklog: ready\n");

    for (int i = 0; i < 4; i++) {
        int cfd = accept(ls, NULL, NULL);
        wstr("mlbacklog: child[");
        wint(i);
        wstr("] cfd=");
        wint(cfd);
        if (cfd < 0) {
            wstr(" FAIL\n");
            return 1;
        }
        char tag = '?';
        int n = (int)read(cfd, &tag, 1);
        wstr(" tag=");
        if (n == 1) {
            char b[1] = { tag };
            write(1, b, 1);
        } else {
            wstr("?");
        }
        wstr("\n");
        write(cfd, "ack\n", 4);
        close(cfd);
    }

    close(ls);
    wstr("mlbacklog: done\n");
    return 0;
}
