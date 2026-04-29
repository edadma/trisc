/* muntar: end-to-end test of Phase 4 chunks 3+4 — a slix-musl
 * program that opens a pre-baked tar at /test.tar and extracts
 * it under /tmp/ using mkdir + open(O_CREAT) + write + chmod,
 * then stat-verifies the result.
 *
 * The tar is synthesized by trisc-cli/jvm/.../TestTar.scala so the
 * extracted file/dir names and contents are pinned bytes:
 *
 *   tx/                      (mode 0755, dir)
 *   tx/a.txt   "hello\n"     (6 bytes,  mode 0644)
 *   tx/b.txt   "world!\n"    (7 bytes,  mode 0644)
 *
 * The output prefix /tmp/ is hard-coded — /tmp is in the prefilled
 * ramdisk, so the parent always exists. Trailing slashes on dir
 * entries are stripped before mkdir.
 *
 * Output (success):
 *   muntar: open /test.tar rc=3
 *   muntar: dir /tmp/tx rc=0
 *   muntar: file /tmp/tx/a.txt rc=0 size=6
 *   muntar: file /tmp/tx/b.txt rc=0 size=7
 *   muntar: stat /tmp/tx dir=1
 *   muntar: stat /tmp/tx/a.txt size=6 reg=1 match=1
 *   muntar: stat /tmp/tx/b.txt size=7 reg=1 match=1
 *   muntar: ok
 */
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
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

/* Parse a fixed-width octal field. Stops at NUL or space. */
static long parse_octal(const char *p, int len) {
    long v = 0;
    for (int i = 0; i < len; i++) {
        char c = p[i];
        if (c == 0 || c == ' ') break;
        if (c < '0' || c > '7') break;
        v = (v << 3) | (c - '0');
    }
    return v;
}

static int read_full(int fd, void *buf, size_t n) {
    char *p = buf;
    size_t got = 0;
    while (got < n) {
        long r = read(fd, p + got, n - got);
        if (r <= 0) return (int)got;
        got += (size_t)r;
    }
    return (int)got;
}

#define BLOCK 512

int main(void) {
    int fd = open("/test.tar", O_RDONLY);
    if (fd < 0) {
        wstr("muntar: open /test.tar failed errno=");
        wint(errno);
        wstr("\n");
        return 1;
    }
    wstr("muntar: open /test.tar rc=");
    wint(fd);
    wstr("\n");

    char hdr[BLOCK];
    char data[BLOCK];
    char outpath[160];

    while (1) {
        int n = read_full(fd, hdr, BLOCK);
        if (n != BLOCK) {
            wstr("muntar: short header read\n");
            return 2;
        }
        if (hdr[0] == 0) break;     /* end-of-archive zero block */

        /* name[100] is NUL-terminated when shorter than 100; copy at most 99 */
        char name[100];
        int nl = 0;
        while (nl < 99 && hdr[nl] != 0) { name[nl] = hdr[nl]; nl++; }
        name[nl] = 0;

        long size = parse_octal(hdr + 124, 12);
        long mode = parse_octal(hdr + 100, 8);
        char typeflag = hdr[156];
        if (typeflag == 0) typeflag = '0';

        /* prefix /tmp/ */
        int op = 0;
        outpath[op++] = '/'; outpath[op++] = 't'; outpath[op++] = 'm'; outpath[op++] = 'p'; outpath[op++] = '/';
        for (int i = 0; i < nl && op < (int)sizeof(outpath) - 1; i++)
            outpath[op++] = name[i];
        /* strip trailing slash on dirs */
        if (op > 0 && outpath[op - 1] == '/') op--;
        outpath[op] = 0;

        if (typeflag == '5') {
            int rc = mkdir(outpath, (mode_t)(mode & 0777));
            wstr("muntar: dir ");
            wstr(outpath);
            wstr(" rc=");
            wint(rc);
            wstr("\n");
            if (rc != 0) return 3;
        } else if (typeflag == '0') {
            int wfd = open(outpath, O_CREAT | O_WRONLY, (mode_t)(mode & 0777));
            if (wfd < 0) {
                wstr("muntar: open-create ");
                wstr(outpath);
                wstr(" failed errno=");
                wint(errno);
                wstr("\n");
                return 4;
            }
            long remaining = size;
            while (remaining > 0) {
                int chunk = read_full(fd, data, BLOCK);
                if (chunk != BLOCK) {
                    wstr("muntar: short data read\n");
                    return 5;
                }
                long want = remaining < BLOCK ? remaining : BLOCK;
                long w = write(wfd, data, (size_t)want);
                if (w != want) {
                    wstr("muntar: short write rc=");
                    wint(w);
                    wstr("\n");
                    return 6;
                }
                remaining -= want;
            }
            close(wfd);
            wstr("muntar: file ");
            wstr(outpath);
            wstr(" rc=0 size=");
            wint(size);
            wstr("\n");
        } else {
            wstr("muntar: unsupported typeflag\n");
            return 7;
        }
    }

    close(fd);

    /* Stat-verify directory + each file */
    struct stat st;
    if (stat("/tmp/tx", &st) != 0) {
        wstr("muntar: stat /tmp/tx failed\n");
        return 8;
    }
    wstr("muntar: stat /tmp/tx dir=");
    wint(S_ISDIR(st.st_mode) ? 1 : 0);
    wstr("\n");

    static const char EXPECT_A[] = "hello\n";
    static const char EXPECT_B[] = "world!\n";

    if (stat("/tmp/tx/a.txt", &st) != 0) {
        wstr("muntar: stat a.txt failed\n");
        return 9;
    }
    int rfd = open("/tmp/tx/a.txt", O_RDONLY);
    char rbuf[32];
    long r = read(rfd, rbuf, sizeof(rbuf));
    int match_a = (r == 6) && (memcmp(rbuf, EXPECT_A, 6) == 0);
    close(rfd);
    wstr("muntar: stat /tmp/tx/a.txt size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr(" match=");
    wint(match_a ? 1 : 0);
    wstr("\n");

    if (stat("/tmp/tx/b.txt", &st) != 0) {
        wstr("muntar: stat b.txt failed\n");
        return 10;
    }
    rfd = open("/tmp/tx/b.txt", O_RDONLY);
    r = read(rfd, rbuf, sizeof(rbuf));
    int match_b = (r == 7) && (memcmp(rbuf, EXPECT_B, 7) == 0);
    close(rfd);
    wstr("muntar: stat /tmp/tx/b.txt size=");
    wint((long)st.st_size);
    wstr(" reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr(" match=");
    wint(match_b ? 1 : 0);
    wstr("\n");

    if (!match_a || !match_b) {
        wstr("muntar: content mismatch\n");
        return 11;
    }

    wstr("muntar: ok\n");
    return 0;
}
