#include "libc.h"

/* SLIX aarch64 variant: no HWCAP munging (SLIX doesn't expose HWCAP
 * aux entries), just set TPIDR_EL0 and return 0. Upstream aarch64's
 * SME/HWCAP filtering is irrelevant here and pulls in auxv, which
 * the minimal SLIX init stack doesn't populate. */
int __set_thread_area(void *p)
{
	__asm__ __volatile__ ("msr tpidr_el0,%0" : : "r"(p) : "memory");
	return 0;
}
