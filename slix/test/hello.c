#include <unistd.h>

/* mhello: exercises SYS_WRITE=128, SYS_READ=130, SYS_EXIT_GROUP=129.
 * Expected output on the current Phase-2 stopgap (sys_read returns
 * EOF for fd=0) is:
 *     hello from musl
 *     read=0
 * The `read=0` line confirms that SYS_READ is dispatched and returns
 * a sensible value; real fd routing is deferred per the
 * posix_fd_bridge memory. */
int main(void) {
    const char msg[] = "hello from musl\n";
    write(1, msg, sizeof(msg) - 1);

    char buf[16];
    int n = read(0, buf, sizeof buf);
    char out[8] = { 'r', 'e', 'a', 'd', '=', '0', '\n', 0 };
    if (n < 0) out[5] = 'E';
    else out[5] = (char)('0' + (n & 0xF));
    write(1, out, 7);
    return 0;
}
