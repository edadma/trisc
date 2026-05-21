/* mtls: exercise app-defined __thread storage end-to-end.
 *
 * Until pthread_create lands there's only one thread, so the
 * goal here isn't isolation — it's proving the TLS layout is
 * wired correctly:
 *
 *   1. ld-musl reads the main exe's PT_TLS from AT_PHDR/AT_PHNUM.
 *   2. __copy_tls allocates the per-thread block and copies the
 *      .tdata image at the right offset from TP.
 *   3. The compiler's initial-exec relocations land on a tpoff
 *      that, added to TPIDR_EL0 / %fs:0, hits each variable's
 *      slot.
 *
 * The test prints initial values (.tdata image was copied
 * correctly), zero-init values (.tbss was zero-filled), and
 * values after a write (the TLS block is writable, not just a
 * read of the image). A struct __thread checks alignment.
 */
#include <unistd.h>

static void wstr(const char *s) {
    long n = 0;
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

static __thread int counter_zero;
static __thread int counter_init = 42;
static __thread long big_init = 0x0123456789abcdefL;
static __thread char greeting[16] = "hello tls";

struct point {
    int x;
    int y;
    long tag;
};
static __thread struct point pt = { 7, 11, 0xfeedfaceL };

int main(void) {
    wstr("mtls: zero="); wlong(counter_zero); wstr("\n");
    wstr("mtls: init="); wlong(counter_init); wstr("\n");
    wstr("mtls: big=");  wlong(big_init);     wstr("\n");

    wstr("mtls: greet=");
    long gn = 0;
    while (greeting[gn]) gn++;
    write(1, greeting, gn);
    wstr("\n");

    wstr("mtls: pt=");
    wlong(pt.x); wstr(","); wlong(pt.y); wstr(",");
    wlong(pt.tag);
    wstr("\n");

    counter_zero = 1234;
    counter_init = -1;
    big_init = 0x55aa55aa55aa55aaL;
    greeting[0] = 'H';
    pt.x = -7;
    pt.tag = 0x1122334455667788L;

    wstr("mtls: zero2="); wlong(counter_zero); wstr("\n");
    wstr("mtls: init2="); wlong(counter_init); wstr("\n");
    wstr("mtls: big2=");  wlong(big_init);     wstr("\n");

    wstr("mtls: greet2=");
    gn = 0;
    while (greeting[gn]) gn++;
    write(1, greeting, gn);
    wstr("\n");

    wstr("mtls: pt2=");
    wlong(pt.x); wstr(","); wlong(pt.y); wstr(",");
    wlong(pt.tag);
    wstr("\n");

    wstr("mtls: done\n");
    return 0;
}
