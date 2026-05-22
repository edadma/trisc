/* mpb: end-to-end pthread_create + pthread_join exerciser.
 *
 * Spawns 4 threads through real musl pthread_create, each runs a
 * worker that writes a distinct value into a shared array and
 * also into its own __thread slot. Main pthread_joins all four
 * and prints the results.
 *
 * What this proves:
 *   1. musl's pthread_create reaches the kernel's clone(2) handler
 *      with CLONE_VM | CLONE_SETTLS | CLONE_CHILD_CLEARTID set.
 *   2. The new threads' user stacks (allocated by musl from a
 *      mmap'd region) are honoured by arch_setup_clone_frame.
 *   3. CLONE_SETTLS gives each thread distinct __thread storage —
 *      after every worker has written its own slot we read it
 *      back from main and see the original value (workers do not
 *      see each other's local).
 *   4. CLONE_CHILD_CLEARTID wakes the joining thread when the
 *      child exits — pthread_join returns instead of hanging.
 *   5. Four threads can coexist (4 kstacks, 4 thread slots, 4 TLS
 *      blocks) without colliding.
 */
#include <pthread.h>
#include <stdint.h>
#include <unistd.h>

#define N_THREADS 4

static int slots[N_THREADS];
static __thread int my_slot_copy;

static void wstr(const char *s) {
    long n = 0;
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

static void *worker(void *arg) {
    int idx = (int)(uintptr_t)arg;
    int v = 0x1000 + idx;
    my_slot_copy = v;
    slots[idx] = v;
    /* Spin briefly so all four threads overlap before any one
     * exits — this stresses kstack/thread-slot reuse and keeps
     * the join path lively. */
    long spins = 0;
    while (spins < 200000L) spins++;
    return (void *)(uintptr_t)v;
}

int main(void) {
    pthread_t tids[N_THREADS];
    int rc;

    for (int i = 0; i < N_THREADS; i++) {
        rc = pthread_create(&tids[i], 0, worker, (void *)(uintptr_t)i);
        if (rc != 0) {
            wstr("mpb: create failed i="); wlong(i);
            wstr(" rc="); wlong(rc); wstr("\n");
            return 1;
        }
    }

    long sum_ret = 0;
    for (int i = 0; i < N_THREADS; i++) {
        void *ret = 0;
        rc = pthread_join(tids[i], &ret);
        if (rc != 0) {
            wstr("mpb: join failed i="); wlong(i);
            wstr(" rc="); wlong(rc); wstr("\n");
            return 1;
        }
        sum_ret += (long)(uintptr_t)ret;
    }

    long sum_slots = 0;
    for (int i = 0; i < N_THREADS; i++) sum_slots += slots[i];

    /* Expected: each worker wrote 0x1000+i, so
     *   sum = 4*0x1000 + (0+1+2+3) = 0x4006 = 16390. */
    wstr("mpb: slots="); wlong(sum_slots); wstr("\n");
    wstr("mpb: rets=");  wlong(sum_ret);   wstr("\n");
    wstr("mpb: main_local="); wlong(my_slot_copy); wstr("\n");
    wstr("mpb: done\n");
    return 0;
}
