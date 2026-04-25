/* mpipe: validate pipe2() + write/read/close from a single process.
 * Writes a known string to the write end, reads it back from the read
 * end, then closes both. After close-of-write the next read should
 * return 0 (EOF). Single-process so no fork/exec dependency.
 */
#include <unistd.h>
#include <fcntl.h>
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
    int fds[2];
    int r = pipe2(fds, 0);
    wstr("mpipe: pipe2=");
    wint(r);
    wstr(" rfd=");
    wint(fds[0]);
    wstr(" wfd=");
    wint(fds[1]);
    wstr("\n");
    if (r < 0) return 1;

    const char *msg = "ping through pipe";
    int mlen = (int)strlen(msg);
    int w = (int)write(fds[1], msg, mlen);
    wstr("mpipe: write=");
    wint(w);
    wstr("\n");

    char buf[64];
    int n = (int)read(fds[0], buf, sizeof(buf));
    wstr("mpipe: read=");
    wint(n);
    wstr(" data='");
    if (n > 0) write(1, buf, n);
    wstr("'\n");

    close(fds[1]);
    int n2 = (int)read(fds[0], buf, sizeof(buf));
    wstr("mpipe: read_after_close=");
    wint(n2);
    wstr("\n");

    close(fds[0]);
    wstr("mpipe: done\n");
    return 0;
}
