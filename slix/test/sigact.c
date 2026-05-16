/* sigact: validate Phase 2 chunks 1 and 2 — sigaction /
 * sigprocmask / sigpending state plumbing (chunk 1) plus the
 * signal-generation syscalls kill / tkill / raise (chunk 2). No
 * signal is ever *delivered* in either chunk; the test verifies
 * only that state is stored, queried back, and (for chunk 2) that
 * the pending bitmap reflects raised signals.
 *
 * Steps:
 *   1. sigaction(SIGTERM, NULL, &old) — verify default disposition
 *      is SIG_DFL (sa_handler == NULL).
 *   2. sigaction(SIGTERM, &new, NULL) where new.sa_handler is a
 *      function pointer.
 *   3. sigaction(SIGTERM, NULL, &old) — verify old.sa_handler is
 *      the pointer installed in step 2.
 *   4. sigaction(SIGKILL, &new, NULL) — must fail with EINVAL.
 *   5. sigprocmask: block SIGUSR1, query mask, unblock, verify.
 *   6. sigpending with no pending signal — expect bit clear.
 *   7. raise(SIGUSR1) while blocked — expect pending bit set.
 *      (Pre-arms SIGUSR1 with SIG_IGN so chunk 3+ delivery will
 *      silently discard, preventing this test from killing itself
 *      on the chunk-4 default-TERM action.)
 *   8. kill(getpid(), 0) — POSIX existence probe; must succeed.
 *   9. kill(getpid(), 999) — bad signo; must return EINVAL.
 */
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

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

static void my_handler(int signo) {
    (void)signo;
    /* never actually called in chunk 1 */
}

int main(void) {
    struct sigaction sa, old;

    /* Step 1 — default disposition is SIG_DFL. */
    memset(&old, 0xff, sizeof(old));
    if (sigaction(SIGTERM, NULL, &old) != 0) {
        wstr("sigact: q1_rc<0 errno="); wlong(errno); wstr("\n"); return 1;
    }
    wstr("sigact: step1 handler=");
    wlong((long)(void *)old.sa_handler);
    wstr("\n");

    /* Step 2 — install handler. */
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = my_handler;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGTERM, &sa, NULL) != 0) {
        wstr("sigact: install_rc<0 errno="); wlong(errno); wstr("\n"); return 2;
    }
    wstr("sigact: step2 installed\n");

    /* Step 3 — read back. */
    memset(&old, 0, sizeof(old));
    if (sigaction(SIGTERM, NULL, &old) != 0) {
        wstr("sigact: q2_rc<0 errno="); wlong(errno); wstr("\n"); return 3;
    }
    wstr("sigact: step3 readback_matches=");
    wlong(old.sa_handler == my_handler ? 1 : 0);
    wstr("\n");

    /* Step 4 — SIGKILL refused. */
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = my_handler;
    int kill_rc = sigaction(SIGKILL, &sa, NULL);
    wstr("sigact: step4 kill_rc=");
    wlong(kill_rc);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    /* Step 5 — block + query SIGUSR1. */
    sigset_t blk, oldset, cur;
    sigemptyset(&blk);
    sigaddset(&blk, SIGUSR1);
    if (sigprocmask(SIG_BLOCK, &blk, &oldset) != 0) {
        wstr("sigact: block_rc<0 errno="); wlong(errno); wstr("\n"); return 5;
    }
    if (sigprocmask(SIG_BLOCK, NULL, &cur) != 0) {
        wstr("sigact: q_rc<0 errno="); wlong(errno); wstr("\n"); return 6;
    }
    wstr("sigact: step5 USR1_blocked=");
    wlong(sigismember(&cur, SIGUSR1));
    wstr("\n");

    /* Step 6 — sigpending starts empty. */
    sigset_t pending;
    sigemptyset(&pending);
    if (sigpending(&pending) != 0) {
        wstr("sigact: pend_rc<0 errno="); wlong(errno); wstr("\n"); return 7;
    }
    wstr("sigact: step6 pending_USR1=");
    wlong(sigismember(&pending, SIGUSR1));
    wstr("\n");

    /* Step 7 — pre-arm SIGUSR1 with SIG_IGN, then raise() while
     * SIGUSR1 is still blocked, then re-query sigpending. POSIX:
     * a blocked, pending signal stays pending until unblocked; the
     * bit must show up. We install SIG_IGN ahead of time so that
     * when chunk 3 delivery + chunk 4 default-action land, this
     * test stays self-terminating-safe (default action for SIGUSR1
     * is TERM). */
    struct sigaction ign;
    memset(&ign, 0, sizeof(ign));
    ign.sa_handler = SIG_IGN;
    sigemptyset(&ign.sa_mask);
    if (sigaction(SIGUSR1, &ign, NULL) != 0) {
        wstr("sigact: ign_rc<0 errno="); wlong(errno); wstr("\n"); return 8;
    }
    if (raise(SIGUSR1) != 0) {
        wstr("sigact: raise_rc<0 errno="); wlong(errno); wstr("\n"); return 9;
    }
    sigemptyset(&pending);
    if (sigpending(&pending) != 0) {
        wstr("sigact: pend2_rc<0 errno="); wlong(errno); wstr("\n"); return 10;
    }
    wstr("sigact: step7 raised_pending_USR1=");
    wlong(sigismember(&pending, SIGUSR1));
    wstr("\n");

    /* Step 8 — kill(getpid(), 0) is the POSIX "does this pid
     * exist?" probe. Must return 0 without modifying any state. */
    int probe_rc = kill(getpid(), 0);
    wstr("sigact: step8 kill0_rc=");
    wlong(probe_rc);
    wstr("\n");

    /* Step 9 — kill(getpid(), 999) must reject with EINVAL. */
    int bad_rc = kill(getpid(), 999);
    wstr("sigact: step9 bad_rc=");
    wlong(bad_rc);
    wstr(" errno=");
    wlong(errno);
    wstr("\n");

    /* Restore mask (after step 9 so SIGUSR1 stays blocked through
     * step 7's pending probe). The pending bit will linger past
     * the unblock until chunk-3 delivery lands; in chunk 2 with no
     * delivery, the SIG_IGN-armed bit just stays set until exit. */
    sigprocmask(SIG_SETMASK, &oldset, NULL);

    wstr("sigact: done\n");
    return 0;
}
