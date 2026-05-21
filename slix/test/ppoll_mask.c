/* mppollmask: validate sigmask atomicity in ppoll(2) and pselect(2).
 *
 * Each system call should:
 *   - swap in the caller-supplied sigmask while it waits,
 *   - return -1/EINTR if a signal handler runs during the wait, and
 *   - restore the original sigmask before returning.
 *
 * Sequence:
 *   1. install a SIGUSR1 handler that bumps a counter.
 *   2. sigprocmask(SIG_BLOCK, SIGUSR1) — outside-wait mask.
 *   3. raise(SIGUSR1) — pending under the blocked outer mask.
 *   4. ppoll(NULL, 0, {3s}, &empty_mask) — empty mask unblocks
 *      SIGUSR1 atomically inside the wait. Handler must run, ppoll
 *      must return -1/EINTR well before the 3-second timeout, and
 *      the post-wait sigprocmask must report SIGUSR1 blocked again.
 *   5. raise(SIGUSR1) — re-arm pending.
 *   6. pselect(0, NULL, NULL, NULL, {3s}, &empty_mask) — same shape.
 *      Handler must run a second time, return -1/EINTR, mask
 *      restored.
 *
 * Dynamic linking required: the kernel sigframe is restored via
 * musl's libc __restore_rt trampoline, same as sigact.
 */
#define _GNU_SOURCE
#include <sys/select.h>
#include <poll.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
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

static volatile int handler_count = 0;
static volatile int handler_signo = 0;
static void on_usr1(int signo) {
    handler_signo = signo;
    handler_count++;
}

int main(void) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = on_usr1;
    if (sigaction(SIGUSR1, &sa, NULL) != 0) {
        wstr("mppm: sigaction<0 errno="); wlong(errno); wstr("\n"); return 1;
    }

    sigset_t block_u1, oldmask, empty;
    sigemptyset(&block_u1);
    sigaddset(&block_u1, SIGUSR1);
    sigemptyset(&empty);
    if (sigprocmask(SIG_BLOCK, &block_u1, &oldmask) != 0) {
        wstr("mppm: block<0 errno="); wlong(errno); wstr("\n"); return 2;
    }

    if (raise(SIGUSR1) != 0) {
        wstr("mppm: raise1<0 errno="); wlong(errno); wstr("\n"); return 3;
    }

    struct timespec ts;
    ts.tv_sec = 3;
    ts.tv_nsec = 0;
    int r = ppoll(NULL, 0, &ts, &empty);
    int saved_errno = errno;
    sigset_t after;
    sigprocmask(0, NULL, &after);
    int still_blocked = sigismember(&after, SIGUSR1);
    wstr("mppm: ppoll r=");
    wlong(r);
    wstr(" errno=");
    wlong(saved_errno);
    wstr(" hcount=");
    wlong(handler_count);
    wstr(" hsigno=");
    wlong(handler_signo);
    wstr(" blocked=");
    wlong(still_blocked);
    wstr("\n");

    if (raise(SIGUSR1) != 0) {
        wstr("mppm: raise2<0 errno="); wlong(errno); wstr("\n"); return 4;
    }
    ts.tv_sec = 3;
    ts.tv_nsec = 0;
    r = pselect(0, NULL, NULL, NULL, &ts, &empty);
    saved_errno = errno;
    sigprocmask(0, NULL, &after);
    still_blocked = sigismember(&after, SIGUSR1);
    wstr("mppm: pselect r=");
    wlong(r);
    wstr(" errno=");
    wlong(saved_errno);
    wstr(" hcount=");
    wlong(handler_count);
    wstr(" hsigno=");
    wlong(handler_signo);
    wstr(" blocked=");
    wlong(still_blocked);
    wstr("\n");

    wstr("mppm: done\n");
    return 0;
}
