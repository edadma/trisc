/* mfile: validate POSIX file I/O through musl's libc — open/read/lseek/
 * close. Reads the first ~256 bytes of /etc/passwd from the SLIX
 * ramdisk, prints them, then re-reads from offset 0 to confirm lseek.
 *
 * The shim's POSIX_FD_FILE kind forwards open/read/lseek/close to
 * the VFS server. If the syscall numbers, message layout, or grant
 * plumbing drifts, the read returns either -1 (BADF/IO) or zero
 * bytes — both surface here as obvious wrong output.
 */
#include <fcntl.h>
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
    int fd = open("/etc/passwd", O_RDONLY);
    wstr("mfile: open=");
    wint(fd);
    wstr("\n");
    if (fd < 0) return 1;

    char buf[256];
    int n = (int)read(fd, buf, sizeof(buf) - 1);
    wstr("mfile: read=");
    wint(n);
    wstr("\n");
    if (n > 0) {
        buf[n] = 0;
        wstr("mfile: data='");
        write(1, buf, n);
        wstr("'\n");
    }

    off_t pos = lseek(fd, 0, SEEK_SET);
    wstr("mfile: lseek=");
    wint((long)pos);
    wstr("\n");

    int n2 = (int)read(fd, buf, 16);
    wstr("mfile: read2=");
    wint(n2);
    wstr("\n");

    close(fd);
    wstr("mfile: done\n");
    return 0;
}
