/* SBI (Supervisor Binary Interface) wrappers.
 *
 * We use two SBI calls:
 *   - Legacy CONSOLE_PUTCHAR (extension 0x01) — write one byte to the SBI
 *     console. Universally available; predates the modern DBCN extension.
 *   - SystemReset (SRST, extension 0x53525354) function 0 — shutdown. Used
 *     by abort()/exit() and at the end of _start once main returns.
 *
 * SBI ABI: ecall with a7 = extension ID, a6 = function ID (for new-style
 * extensions), args in a0..a5. Return: a0 = error code (sbiret.error),
 * a1 = value (sbiret.value). Legacy extensions return their result in a0.
 */

#include <stdint.h>

/* Legacy console_putchar — extension 0x01, function 0. */
void sbi_console_putchar(int c) {
    register long a0 asm("a0") = (long)(c & 0xff);
    register long a7 asm("a7") = 0x01;
    asm volatile ("ecall"
                  : "+r"(a0)
                  : "r"(a7)
                  : "memory");
}

/* SystemReset (SRST) — extension 0x53525354, function 0.
 *   type:   0 = shutdown, 1 = cold reboot, 2 = warm reboot.
 *   reason: 0 = no reason, 1 = system failure, others reserved.
 */
__attribute__((noreturn))
void sbi_system_reset(uint32_t type, uint32_t reason) {
    register long a0 asm("a0") = (long)type;
    register long a1 asm("a1") = (long)reason;
    register long a6 asm("a6") = 0;
    register long a7 asm("a7") = 0x53525354L;
    asm volatile ("ecall"
                  :
                  : "r"(a0), "r"(a1), "r"(a6), "r"(a7)
                  : "memory");
    /* SystemReset is documented as not returning, but if the call fails (e.g.
     * the SRST extension isn't installed) ecall does return. Spin so we don't
     * fall off the end. */
    for (;;) asm volatile ("wfi");
}

/* _sysl_rv_exit — called by _start once main() returns. main()'s return value
 * is an i32 status code; SBI SystemReset only carries "no reason"/"failure",
 * so we map nonzero → failure. The exact code can't survive across SBI but
 * qemu's exit code reflects the SRST reason, so 0 vs nonzero is preserved. */
__attribute__((noreturn))
void _sysl_rv_exit(int code) {
    sbi_system_reset(0, code == 0 ? 0 : 1);
}
