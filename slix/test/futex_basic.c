/* mfx: smoke test for SYS_futex (FUTEX_WAIT / FUTEX_WAKE / EAGAIN /
 * ETIMEDOUT).
 *
 * Single-process coverage, plus one fork+wake round-trip to prove the
 * physical-address hashing actually links waiters and wakers across
 * page tables. We can't pthread_create yet (chunk 9), so the wake
 * test relies on COW: parent and child see the same physical page for
 * the futex word until either of them writes, and our wake protocol
 * never writes, so the address translates the same on both sides.
 *
 * Five blocks:
 *   1. FUTEX_WAKE with no waiters returns 0.
 *   2. FUTEX_WAIT with *uaddr != expected returns -1/EAGAIN.
 *   3. FUTEX_WAIT with a short relative timeout returns -1/ETIMEDOUT.
 *   4. FUTEX_WAKE+FUTEX_WAIT round-trip across fork(): child wakes
 *      the parent's park.
 *   5. FUTEX_WAIT_BITSET with a short absolute timeout returns
 *      -1/ETIMEDOUT.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#ifndef FUTEX_WAIT
#define FUTEX_WAIT          0
#define FUTEX_WAKE          1
#define FUTEX_WAIT_BITSET   9
#endif

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

static long fwait(volatile int *uaddr, int expected, struct timespec *to) {
    return syscall(SYS_futex, uaddr, FUTEX_WAIT, expected, to, 0, 0);
}

static long fwait_bitset(volatile int *uaddr, int expected, struct timespec *to, unsigned int bits) {
    return syscall(SYS_futex, uaddr, FUTEX_WAIT_BITSET, expected, to, 0, bits);
}

static long fwake(volatile int *uaddr, int n) {
    return syscall(SYS_futex, uaddr, FUTEX_WAKE, n, 0, 0, 0);
}

int main(void) {
    static volatile int word = 0;

    long r = fwake(&word, 1);
    wstr("mfx: wake0 r="); wlong(r); wstr("\n");

    r = fwait(&word, 99, NULL);
    int e = errno;
    wstr("mfx: wait_bad r="); wlong(r); wstr(" errno="); wlong(e); wstr("\n");

    struct timespec to;
    to.tv_sec = 0;
    to.tv_nsec = 50000000;
    r = fwait(&word, 0, &to);
    e = errno;
    wstr("mfx: wait_to r="); wlong(r); wstr(" errno="); wlong(e); wstr("\n");

    pid_t pid = fork();
    if (pid == 0) {
        /* Child: pause briefly, then wake the parent and exit
         * silently. The parent only checks its own fwait() return,
         * which already proves the wake reached across page tables;
         * the child doesn't touch STDOUT (no slix `wait4` shim yet)
         * and never writes `word` (COW would split the page). */
        struct timespec child_sleep;
        child_sleep.tv_sec = 0;
        child_sleep.tv_nsec = 100000000;
        nanosleep(&child_sleep, NULL);
        (void)fwake(&word, 1);
        _exit(0);
    }
    struct timespec parent_to;
    parent_to.tv_sec = 3;
    parent_to.tv_nsec = 0;
    errno = 0;
    r = fwait(&word, 0, &parent_to);
    e = errno;
    wstr("mfx: parent r="); wlong(r); wstr(" errno="); wlong(e); wstr("\n");

    struct timeval now;
    gettimeofday(&now, NULL);
    struct timespec abs_to;
    abs_to.tv_sec = now.tv_sec;
    abs_to.tv_nsec = (now.tv_usec + 50000) * 1000;
    if (abs_to.tv_nsec >= 1000000000) {
        abs_to.tv_sec += 1;
        abs_to.tv_nsec -= 1000000000;
    }
    r = fwait_bitset(&word, 0, &abs_to, 0xFFFFFFFFu);
    e = errno;
    wstr("mfx: bitset_to r="); wlong(r); wstr(" errno="); wlong(e); wstr("\n");

    wstr("mfx: done\n");
    return 0;
}
