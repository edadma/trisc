/* mnbstdin: validate non-blocking stdin (O_NONBLOCK on fd 0).
 *
 *   1. fcntl(F_SETFL, O_NONBLOCK) on fd 0.
 *   2. read(0, ..., 1) returns -1 / errno=EAGAIN when the tty
 *      input buffer is empty.
 *   3. After harness injects a key, read returns 1 and the byte.
 */
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

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
    int rc = fcntl(0, F_SETFL, O_NONBLOCK);
    wstr("mnbstdin: setfl=");
    wlong(rc);
    wstr("\n");

    char rbuf[4];
    int r1 = read(0, rbuf, 1);
    wstr("mnbstdin: empty=");
    wlong(r1);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    /* Marker — harness injects a key, then we re-read. We loop a
     * few times since the read might race the kernel's notification. */
    wstr("mnbstdin: ready_for_input\n");
    int r2 = -1;
    for (int i = 0; i < 200; i++) {
        r2 = read(0, rbuf, 1);
        if (r2 > 0) break;
        /* No blocking sleep available without timerfd inline; just
         * spin through nanosleep(50ms) to let the harness deliver. */
        struct timespec ts = { .tv_sec = 0, .tv_nsec = 10000000 };
        nanosleep(&ts, 0);
    }
    wstr("mnbstdin: woke=");
    wlong(r2);
    if (r2 > 0) {
        wstr(" byte=");
        wlong((unsigned char)rbuf[0]);
    }
    wstr("\n");

    wstr("mnbstdin: done\n");
    return 0;
}
