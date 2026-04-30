/* mpeek: validate MSG_PEEK on UDP — read without dequeuing.
 *
 * Loopback round-trip: bind a port, sendto self, recv with MSG_PEEK,
 * recv again without flags. The bytes returned by both calls must be
 * identical and equal to what was sent. The second recv with no flags
 * must dequeue, so a third non-blocking recv must return -EAGAIN.
 */
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>
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
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    wstr("mpeek: socket=");
    wint(fd);
    wstr("\n");
    if (fd < 0) return 1;

    struct sockaddr_in baddr = {0};
    baddr.sin_family = AF_INET;
    baddr.sin_port = htons(7795);
    baddr.sin_addr.s_addr = inet_addr("127.0.0.1");
    int br = bind(fd, (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("mpeek: bind=");
    wint(br);
    wstr("\n");
    if (br < 0) return 2;

    const char payload[] = "PEEK-OK";
    long sent = sendto(fd, payload, sizeof(payload) - 1, 0,
                      (struct sockaddr *)&baddr, sizeof(baddr));
    wstr("mpeek: sendto=");
    wint(sent);
    wstr("\n");

    char b1[16];
    memset(b1, 0, sizeof(b1));
    long g1 = recvfrom(fd, b1, sizeof(b1), MSG_PEEK, NULL, NULL);
    wstr("mpeek: peek=");
    wint(g1);
    wstr(" data='");
    if (g1 > 0) write(1, b1, g1);
    wstr("'\n");
    char b2[16];
    memset(b2, 0, sizeof(b2));
    long g2 = recvfrom(fd, b2, sizeof(b2), 0, NULL, NULL);
    wstr("mpeek: recv=");
    wint(g2);
    wstr(" data='");
    if (g2 > 0) write(1, b2, g2);
    wstr("'\n");

    int match = (g1 == g2 && g1 == (long)(sizeof(payload) - 1)
                 && memcmp(b1, b2, g1) == 0
                 && memcmp(b1, payload, g1) == 0) ? 1 : 0;
    wstr("mpeek: match=");
    wint(match);
    wstr("\n");

    int flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    char b3[16];
    long g3 = recvfrom(fd, b3, sizeof(b3), 0, NULL, NULL);
    wstr("mpeek: drained=");
    if (g3 < 0) {
        wstr("EAGAIN errno=");
        wint(errno);
    } else {
        wint(g3);
    }
    wstr("\n");

    close(fd);
    wstr("mpeek: done\n");
    return 0;
}
