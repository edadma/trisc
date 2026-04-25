/* SLIX x86_64 syscall ABI.
 *
 * Uses `int $0x80` (not `syscall`) because the SLIX kernel installs
 * its syscall_entry on IDT vector 0x80 — there is no SYSCALL/SYSRET
 * fast-path on slix yet. Vector 0x80 has DPL=3 in the IDT, so user
 * processes can issue it directly.
 *
 * Register convention mirrors the kernel's syscall_entry in
 * `oskit/arch/x86_64/boot.s` and the SLIX-local syscall numbers
 * defined in `bits/syscall.h.in`:
 *
 *   rdi  = syscall number
 *   rsi  = a0
 *   rdx  = a1
 *   rcx  = a2
 *   r8   = a3
 *   r9   = a4
 *   r10  = a5
 *   rax  = return value
 *
 * The aarch64-slix port uses x8/x0..x5; this is the x86_64
 * counterpart. Both use the same SLIX-local numbering scheme so
 * the kernel's posix_dispatch is fully arch-neutral.
 */

#define __SYSCALL_LL_E(x) (x)
#define __SYSCALL_LL_O(x) (x)

static __inline long __syscall0(long n)
{
	unsigned long ret;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n)
		: "rcx", "r11", "memory");
	return ret;
}

static __inline long __syscall1(long n, long a1)
{
	unsigned long ret;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n), "S"(a1)
		: "rcx", "r11", "memory");
	return ret;
}

static __inline long __syscall2(long n, long a1, long a2)
{
	unsigned long ret;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n), "S"(a1), "d"(a2)
		: "rcx", "r11", "memory");
	return ret;
}

static __inline long __syscall3(long n, long a1, long a2, long a3)
{
	unsigned long ret;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n), "S"(a1), "d"(a2), "c"(a3)
		: "r11", "memory");
	return ret;
}

static __inline long __syscall4(long n, long a1, long a2, long a3, long a4)
{
	unsigned long ret;
	register long r8 __asm__("r8") = a4;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n), "S"(a1), "d"(a2), "c"(a3), "r"(r8)
		: "r11", "memory");
	return ret;
}

static __inline long __syscall5(long n, long a1, long a2, long a3, long a4, long a5)
{
	unsigned long ret;
	register long r8 __asm__("r8") = a4;
	register long r9 __asm__("r9") = a5;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n), "S"(a1), "d"(a2), "c"(a3), "r"(r8), "r"(r9)
		: "r11", "memory");
	return ret;
}

static __inline long __syscall6(long n, long a1, long a2, long a3,
                                long a4, long a5, long a6)
{
	unsigned long ret;
	register long r8 __asm__("r8") = a4;
	register long r9 __asm__("r9") = a5;
	register long r10 __asm__("r10") = a6;
	__asm__ __volatile__ ("int $0x80"
		: "=a"(ret) : "D"(n), "S"(a1), "d"(a2), "c"(a3),
		             "r"(r8), "r"(r9), "r"(r10)
		: "r11", "memory");
	return ret;
}

#define VDSO_USEFUL
#define VDSO_CGT_SYM "__vdso_clock_gettime"
#define VDSO_CGT_VER "LINUX_2.6"
#define VDSO_GETCPU_SYM "__vdso_getcpu"
#define VDSO_GETCPU_VER "LINUX_2.6"

#define IPC_64 0
