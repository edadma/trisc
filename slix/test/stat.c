/* mstat: validate POSIX stat / fstat / lstat through musl.
 *
 * Phase 4 chunk 2 of the netstack-quality plan: musl's stat
 * group routes through SYS_fstat (178) for the fd-based call
 * and SYS_newfstatat (252) for the path-based ones, both via
 * the VFS server's STAT/FSTAT commands.
 *
 * Ramdisk content the test relies on:
 *   /etc/passwd — populated by Make{Aarch64,X86}RamdiskMain;
 *                 size > 0 and S_ISREG.
 *   /etc      — directory; S_ISDIR.
 *
 * Output (success):
 *   mstat: stat /etc/passwd size=<N> reg=1 dir=0
 *   mstat: lstat /etc/passwd size=<N> reg=1
 *   mstat: stat /etc size=<M> reg=0 dir=1
 *   mstat: fstat fd=<F> size=<N> reg=1
 *   mstat: stat /no/such missing=1
 *   mstat: ok
 */
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

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
    struct stat st;

    if (stat("/etc/passwd", &st) != 0) {
        wstr("mstat: stat /etc/passwd failed errno=");
        wint(errno);
        wstr("\n");
        return 1;
    }
    wstr("mstat: stat /etc/passwd size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr(" dir=");
    wint(S_ISDIR(st.st_mode) ? 1 : 0);
    wstr("\n");

    if (lstat("/etc/passwd", &st) != 0) {
        wstr("mstat: lstat /etc/passwd failed errno=");
        wint(errno);
        wstr("\n");
        return 2;
    }
    wstr("mstat: lstat /etc/passwd size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr("\n");

    if (stat("/etc", &st) != 0) {
        wstr("mstat: stat /etc failed errno=");
        wint(errno);
        wstr("\n");
        return 3;
    }
    wstr("mstat: stat /etc size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr(" dir=");
    wint(S_ISDIR(st.st_mode) ? 1 : 0);
    wstr("\n");

    int fd = open("/etc/passwd", O_RDONLY);
    if (fd < 0) {
        wstr("mstat: open /etc/passwd failed\n");
        return 4;
    }
    if (fstat(fd, &st) != 0) {
        wstr("mstat: fstat failed errno=");
        wint(errno);
        wstr("\n");
        close(fd);
        return 5;
    }
    wstr("mstat: fstat fd=");
    wint(fd);
    wstr(" size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr("\n");
    close(fd);

    if (stat("/no/such", &st) == 0) {
        wstr("mstat: stat /no/such unexpectedly succeeded\n");
        return 6;
    }
    wstr("mstat: stat /no/such missing=1\n");

    wstr("mstat: ok\n");
    return 0;
}
