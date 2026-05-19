/* sigact: exercise the POSIX signal-handling surface end-to-end.
 *
 * Steps 1-9 cover state plumbing (sigaction / sigprocmask /
 * sigpending) and signal generation (kill / tkill / raise) without
 * any signal actually being delivered. Step 10 installs a real
 * SIGUSR2 handler, calls raise(SIGUSR2), and checks that the
 * handler ran — that's the end-to-end proof that the kernel built
 * a sigframe, ERET'd into userspace at the handler, and that the
 * libc trampoline's SYS_RT_SIGRETURN restored execution back to
 * the caller.
 *
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
 *      (Pre-arms SIGUSR1 with SIG_IGN so delivery silently discards
 *      it once unblocked, preventing the test from killing itself
 *      on the default-TERM action.)
 *   8. kill(getpid(), 0) — POSIX existence probe; must succeed.
 *   9. kill(getpid(), 999) — bad signo; must return EINVAL.
 *  10. Install a real SIGUSR2 handler that bumps a counter, then
 *      raise(SIGUSR2), then read the counter back — handler must
 *      have run exactly once and control must have returned past
 *      the raise() call line.
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
    /* installed via sigaction but never delivered — step 2 only
     * verifies storage, not invocation. */
}

static volatile int handler_counter = 0;
static volatile int handler_signo = 0;
static void counter_handler(int signo) {
    handler_signo = signo;
    handler_counter++;
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
     * step 7's pending probe). With delivery wired up the SIG_IGN
     * disposition causes the kernel to silently clear the pending
     * SIGUSR1 bit on the unblock — no handler runs. */
    sigprocmask(SIG_SETMASK, &oldset, NULL);

    /* Step 10 — install a real handler, raise the signal, verify
     * the handler ran and that we returned past raise(). */
    struct sigaction usr2;
    memset(&usr2, 0, sizeof(usr2));
    usr2.sa_handler = counter_handler;
    sigemptyset(&usr2.sa_mask);
    usr2.sa_flags = 0;
    if (sigaction(SIGUSR2, &usr2, NULL) != 0) {
        wstr("sigact: usr2_install_rc<0 errno="); wlong(errno); wstr("\n"); return 11;
    }
    if (raise(SIGUSR2) != 0) {
        wstr("sigact: raise2_rc<0 errno="); wlong(errno); wstr("\n"); return 12;
    }
    wstr("sigact: step10 counter=");
    wlong(handler_counter);
    wstr(" signo=");
    wlong(handler_signo);
    wstr("\n");

    wstr("sigact: done\n");
    return 0;
}
