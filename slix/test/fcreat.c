/* mfcreat: validate open(O_CREAT) end-to-end through musl + VFS.
 *
 * Phase 4 chunk 4 of the netstack-quality plan: extends sys_openat
 * with the create-then-retry path so a fresh slix program can do
 *   fd = open("/tmp/x", O_CREAT|O_WRONLY, 0644);
 *   write(fd, ...); close(fd);
 *   open + read it back, fstat it.
 *
 * Ramdisk content the test relies on:
 *   /tmp        — directory (so /tmp/newfile has a parent inode).
 *
 * Output (success):
 *   mfcreat: open O_CREAT|O_WRONLY rc=3
 *   mfcreat: write rc=12
 *   mfcreat: close rc=0
 *   mfcreat: stat after create size=12 reg=1
 *   mfcreat: read back rc=12 match=1
 *   mfcreat: O_EXCL on existing rc=-1 errno=17
 *   mfcreat: unlink rc=0 missing=1
 *   mfcreat: ok
 */
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

static const char PAYLOAD[] = "hello world\n";  /* 12 bytes */
#define PAYLOAD_LEN 12

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
    int fd = open("/tmp/newfile", O_CREAT | O_WRONLY, 0644);
    if (fd < 0) {
        wstr("mfcreat: open O_CREAT|O_WRONLY failed errno=");
        wint(errno);
        wstr("\n");
        return 1;
    }
    wstr("mfcreat: open O_CREAT|O_WRONLY rc=");
    wint(fd);
    wstr("\n");

    long w = write(fd, PAYLOAD, PAYLOAD_LEN);
    wstr("mfcreat: write rc=");
    wint(w);
    wstr("\n");
    if (w != PAYLOAD_LEN) {
        wstr("mfcreat: write short or failed\n");
        return 2;
    }

    int rc = close(fd);
    wstr("mfcreat: close rc=");
    wint(rc);
    wstr("\n");

    struct stat st;
    if (stat("/tmp/newfile", &st) != 0) {
        wstr("mfcreat: stat after create failed\n");
        return 3;
    }
    wstr("mfcreat: stat after create size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr("\n");

    int rfd = open("/tmp/newfile", O_RDONLY);
    if (rfd < 0) {
        wstr("mfcreat: re-open RDONLY failed\n");
        return 4;
    }
    char rbuf[32];
    long r = read(rfd, rbuf, sizeof(rbuf));
    int match = (r == PAYLOAD_LEN) && (memcmp(rbuf, PAYLOAD, PAYLOAD_LEN) == 0);
    wstr("mfcreat: read back rc=");
    wint(r);
    wstr(" match=");
    wint(match ? 1 : 0);
    wstr("\n");
    close(rfd);

    int xfd = open("/tmp/newfile", O_CREAT | O_EXCL | O_WRONLY, 0644);
    int xerr = errno;
    wstr("mfcreat: O_EXCL on existing rc=");
    wint(xfd);
    wstr(" errno=");
    wint(xerr);
    wstr("\n");
    if (xfd >= 0) {
        close(xfd);
        wstr("mfcreat: O_EXCL unexpectedly succeeded!\n");
        return 5;
    }

    if (unlink("/tmp/newfile") != 0) {
        wstr("mfcreat: unlink failed errno=");
        wint(errno);
        wstr("\n");
        return 6;
    }
    if (stat("/tmp/newfile", &st) == 0) {
        wstr("mfcreat: /tmp/newfile still present after unlink!\n");
        return 7;
    }
    wstr("mfcreat: unlink rc=0 missing=1\n");

    wstr("mfcreat: ok\n");
    return 0;
}
