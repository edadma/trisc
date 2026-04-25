/* msocket: exercise the POSIX sockets stack through musl's libc
 * wrappers (not raw syscall6).  This validates that the aarch64-
 * slix syscall numbers match what musl's socket/connect/write/
 * shutdown/read/close/getsockname all call.
 *
 * Host-side peer (Aarch64NshTests "socket: end-to-end via musl"):
 *   listens on 127.0.0.1:18083, reads until EOF, then writes
 *   "got N bytes" back.  Slirp forwards guest 10.0.2.2:18083 to
 *   that port.
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
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    wstr("msocket: socket=");
    wint(fd);
    wstr("\n");
    if (fd < 0) return 1;

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(18083);
    addr.sin_addr.s_addr = inet_addr("10.0.2.2");

    int r = connect(fd, (struct sockaddr *)&addr, sizeof(addr));
    wstr("msocket: connect=");
    wint(r);
    wstr("\n");
    if (r < 0) return 2;

    struct sockaddr_in local;
    socklen_t llen = sizeof(local);
    r = getsockname(fd, (struct sockaddr *)&local, &llen);
    wstr("msocket: getsockname=");
    wint(r);
    wstr(" family=");
    wint(local.sin_family);
    wstr(" port=");
    wint(ntohs(local.sin_port));
    wstr("\n");

    const char *msg = "hello from musl sockets";
    int mlen = (int)strlen(msg);
    int w = (int)write(fd, msg, mlen);
    wstr("msocket: write=");
    wint(w);
    wstr("\n");

    r = shutdown(fd, SHUT_WR);
    wstr("msocket: shutdown=");
    wint(r);
    wstr("\n");

    char buf[128];
    int total = 0;
    while (total < (int)sizeof(buf)) {
        int n = (int)read(fd, buf + total, sizeof(buf) - total);
        if (n <= 0) break;
        total += n;
    }
    wstr("msocket: read=");
    wint(total);
    wstr(" reply='");
    write(1, buf, total);
    wstr("'\n");

    close(fd);
    wstr("msocket: done\n");
    return 0;
}
