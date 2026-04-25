/* SLIX x86_64 variant: write FS base directly via WRFSBASE.
 * The kernel enables CR4.FSGSBASE in boot.s, so ring 3 can set
 * FS base without an arch_prctl(SET_FS) syscall (which SLIX
 * doesn't implement). Returns 0 unconditionally — failure modes
 * upstream cared about (kernel-side syscall errors) can't apply
 * when the instruction is local to the user thread. */
.text
.global __set_thread_area
.hidden __set_thread_area
.type __set_thread_area,@function
__set_thread_area:
	wrfsbase %rdi
	xorl %eax, %eax
	ret
