/* mclone: minimal SYS_clone end-to-end exerciser.
 *
 * Calls __clone() directly (bypassing pthread_create) with the flag
 * set that musl's pthread_create would use for a "real" thread, minus
 * CLONE_SETTLS. The child function is restricted to writes against
 * globals — TLS is not set up for the new thread, so any libc call
 * that touches errno or other __thread storage would fault on a NULL
 * TPIDR_EL0 / FS_BASE. raw-syscall write(2) is fine because slix-musl's
 * __syscall path doesn't go through TLS.
 *
 * Verifies:
 *   1. The kernel honours SYS_clone (146) — `rc` returns a positive
 *      tid in the parent.
 *   2. CLONE_PARENT_SETTID writes the new tid into *ptid before the
 *      caller observes the return value.
 *   3. The child wakes on its new user stack, runs user code, and
 *      can write a shared global (proves CLONE_VM is in effect and
 *      `arch_setup_clone_frame` left the post-syscall PC + register
 *      state consistent with parent's at SVC/int-$0x80 entry).
 *   4. The child can SYS_exit without taking the rest of the process
 *      with it — the thread-exit-vs-process-exit distinction in
 *      `terminate_current` keeps the parent alive.
 */
#include <unistd.h>
#include <stdint.h>

#define STACK_SIZE 8192
#define CLONE_VM             0x00000100
#define CLONE_FS             0x00000200
#define CLONE_FILES          0x00000400
#define CLONE_SIGHAND        0x00000800
#define CLONE_THREAD         0x00010000
#define CLONE_PARENT_SETTID  0x00100000

typedef int pid_t;

extern int __clone(int (*fn)(void *), void *stack, int flags, void *arg,
                   pid_t *ptid, void *tls, pid_t *ctid);

__attribute__((aligned(16))) static char child_stack[STACK_SIZE];
static volatile int child_ran;

static int child_fn(void *arg)
{
    child_ran = (int)(uintptr_t)arg | 1;
    return 0;
}

static void wstr(const char *s)
{
    long n = 0;
    while (s[n]) n++;
    write(1, s, n);
}

static void wlong(long v)
{
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

int main(void)
{
    pid_t new_tid = -1;
    int flags = CLONE_VM | CLONE_FS | CLONE_FILES | CLONE_SIGHAND
              | CLONE_THREAD | CLONE_PARENT_SETTID;

    long rc = __clone(child_fn, child_stack + STACK_SIZE, flags,
                      (void *)(uintptr_t)0xCA11ED,
                      &new_tid, 0, 0);
    wstr("mclone: rc="); wlong(rc); wstr("\n");
    wstr("mclone: ptid="); wlong(new_tid); wstr("\n");

    long spins = 0;
    while (!child_ran && spins < 50000000L) spins++;

    wstr("mclone: child_ran="); wlong(child_ran); wstr("\n");
    wstr("mclone: done\n");
    return 0;
}
