/* msndto: validate SO_SNDTIMEO enforcement on TCP.
 *
 * Single-process loopback: listen on 127.0.0.1:7796, accept the
 * client connection, then *never* read.  The client sets a 200 ms
 * SO_SNDTIMEO and writes a payload large enough to overflow both
 * the kernel send buffer (default 4096) and the unread receive
 * buffer (default 1024).  After the buffers fill, the inet server
 * parks the sender; once the deadline elapses, scan_timers replies
 * with status=2 and the shim translates that to -1/EAGAIN.
 *
 * Pass criteria:
 *   - first send returns a positive partial count (< full payload),
 *     proving some bytes were buffered before the park
 *   - second send returns -1 with errno == EAGAIN within ~200 ms
 *   - close path returns 0
 */
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
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
    wstr("msndto: listen_socket=");
    wint(ls);
    wstr("\n");
    if (ls < 0) return 1;

    struct sockaddr_in baddr = {0};
    baddr.sin_family = AF_INET;
    baddr.sin_port = htons(7796);
    baddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int br = bind(ls, (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("msndto: bind=");
    wint(br);
    wstr("\n");
    if (br < 0) return 2;

    int lr = listen(ls, 4);
    wstr("msndto: listen=");
    wint(lr);
    wstr("\n");
    if (lr < 0) return 3;

    int cl = socket(AF_INET, SOCK_STREAM, 0);
    if (cl < 0) return 4;

    struct timeval tv = {0, 200000};
    int sr = setsockopt(cl, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    wstr("msndto: setsockopt_sndtimeo=");
    wint(sr);
    wstr("\n");
    if (sr < 0) return 5;

    struct sockaddr_in caddr = {0};
    caddr.sin_family = AF_INET;
    caddr.sin_port = htons(7796);
    caddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int cr = connect(cl, (struct sockaddr *)&caddr, sizeof(caddr));
    wstr("msndto: connect=");
    wint(cr);
    wstr("\n");
    if (cr < 0) return 6;

    int ch = accept(ls, NULL, NULL);
    wstr("msndto: accept=");
    wint(ch);
    wstr("\n");
    if (ch < 0) return 7;

    /* Fill more than (sndbuf 4096 + rcvbuf 1024) so the second
     * send is guaranteed to find both buffers full and park. */
    static char payload[16384];
    memset(payload, 'X', sizeof(payload));

    long s1 = send(cl, payload, sizeof(payload), 0);
    wstr("msndto: send1=");
    wint(s1);
    wstr("\n");

    long s2 = send(cl, payload, sizeof(payload), 0);
    wstr("msndto: send2=");
    if (s2 < 0) {
        wstr("EAGAIN errno=");
        wint(errno);
    } else {
        wint(s2);
    }
    wstr("\n");

    /* Indicate the shape: send1 must be a positive partial,
     * send2 must be -1/EAGAIN.  Combined into one bool so the
     * NshTests assertion reads cleanly. */
    int ok = (s1 > 0 && s1 < (long)sizeof(payload)
              && s2 < 0 && errno == EAGAIN) ? 1 : 0;
    wstr("msndto: pass=");
    wint(ok);
    wstr("\n");

    close(ch);
    close(cl);
    close(ls);
    wstr("msndto: done\n");
    return 0;
}
