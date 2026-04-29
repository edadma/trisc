/* mfsmod: validate POSIX mkdir / unlink / rename / chmod via VFS.
 *
 * Phase 4 chunk 3 of the netstack-quality plan: musl on slix
 * routes the legacy syscalls through the at-suffixed ones —
 *   mkdir(p, m)        -> mkdirat(AT_FDCWD, p, m)
 *   chmod(p, m)        -> fchmodat(AT_FDCWD, p, m, 0)
 *   rename(o, n)       -> renameat(AT_FDCWD, o, AT_FDCWD, n)
 *   rmdir(p)           -> unlinkat(AT_FDCWD, p, AT_REMOVEDIR)
 *   unlink(p)          -> unlinkat(AT_FDCWD, p, 0)
 * — so exercising the four syscall numbers (167, 227, 281, 365)
 * covers all the legacy entry points too.
 *
 * Ramdisk content the test relies on:
 *   /tmp        — directory; writable, used to host /tmp/ck.
 *   /etc/hosts  — regular file, non-zero size; renamed in place
 *                 then renamed back so the ramdisk is clean for
 *                 any subsequent test that opens /etc/hosts.
 *
 * Output (success):
 *   mfsmod: mkdir /tmp/ck rc=0 dir=1
 *   mfsmod: chmod /tmp/ck 0700 mode=0700
 *   mfsmod: rename /etc/hosts -> /etc/hostsx rc=0 reg=1 size>0=1
 *   mfsmod: stat /etc/hosts after rename missing=1
 *   mfsmod: rename back rc=0
 *   mfsmod: rmdir /tmp/ck rc=0 missing=1
 *   mfsmod: ok
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

static void woct(unsigned v) {
    char buf[12];
    int i = 0;
    if (v == 0) buf[i++] = '0';
    while (v > 0) { buf[i++] = '0' + (v & 7); v >>= 3; }
    char out[12];
    for (int j = 0; j < i; j++) out[j] = buf[i - 1 - j];
    write(1, out, i);
}

int main(void) {
    struct stat st;

    if (mkdir("/tmp/ck", 0755) != 0) {
        wstr("mfsmod: mkdir /tmp/ck failed errno=");
        wint(errno);
        wstr("\n");
        return 1;
    }
    if (stat("/tmp/ck", &st) != 0) {
        wstr("mfsmod: stat /tmp/ck after mkdir failed\n");
        return 2;
    }
    wstr("mfsmod: mkdir /tmp/ck rc=0 dir=");
    wint(S_ISDIR(st.st_mode) ? 1 : 0);
    wstr("\n");

    if (chmod("/tmp/ck", 0700) != 0) {
        wstr("mfsmod: chmod /tmp/ck failed errno=");
        wint(errno);
        wstr("\n");
        return 3;
    }
    if (stat("/tmp/ck", &st) != 0) {
        wstr("mfsmod: stat /tmp/ck after chmod failed\n");
        return 4;
    }
    wstr("mfsmod: chmod /tmp/ck 0700 mode=0");
    woct(st.st_mode & 0777);
    wstr("\n");

    if (rename("/etc/hosts", "/etc/hostsx") != 0) {
        wstr("mfsmod: rename /etc/hosts -> /etc/hostsx failed errno=");
        wint(errno);
        wstr("\n");
        return 5;
    }
    if (stat("/etc/hostsx", &st) != 0) {
        wstr("mfsmod: stat /etc/hostsx failed\n");
        return 6;
    }
    wstr("mfsmod: rename /etc/hosts -> /etc/hostsx rc=0 reg=");
    wint(S_ISREG(st.st_mode) ? 1 : 0);
    wstr(" size>0=");
    wint(st.st_size > 0 ? 1 : 0);
    wstr("\n");

    if (stat("/etc/hosts", &st) == 0) {
        wstr("mfsmod: /etc/hosts still present after rename!\n");
        return 7;
    }
    wstr("mfsmod: stat /etc/hosts after rename missing=1\n");

    if (rename("/etc/hostsx", "/etc/hosts") != 0) {
        wstr("mfsmod: rename back failed errno=");
        wint(errno);
        wstr("\n");
        return 8;
    }
    wstr("mfsmod: rename back rc=0\n");

    if (rmdir("/tmp/ck") != 0) {
        wstr("mfsmod: rmdir /tmp/ck failed errno=");
        wint(errno);
        wstr("\n");
        return 9;
    }
    if (stat("/tmp/ck", &st) == 0) {
        wstr("mfsmod: /tmp/ck still present after rmdir!\n");
        return 10;
    }
    wstr("mfsmod: rmdir /tmp/ck rc=0 missing=1\n");

    wstr("mfsmod: ok\n");
    return 0;
}
