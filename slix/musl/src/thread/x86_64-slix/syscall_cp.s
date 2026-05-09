/* SLIX x86_64 cancellation-point syscall stub.
 *
 * The C ABI passes args (cancel-flag-ptr, nr, a, b, c, d, e, f) in
 * (rdi, rsi, rdx, rcx, r8, r9, [rsp+8], [rsp+16]) per SysV. The SLIX
 * syscall ABI expects (nr, a, b, c, d, e, f) in (rdi, rsi, rdx, rcx,
 * r8, r9, r10) and uses `int $0x80` rather than `syscall`. See the
 * register convention in arch/x86_64-slix/syscall_arch.h.
 *
 * Upstream's src/thread/x86_64/syscall_cp.s targets Linux's `syscall`
 * insn with rax = nr; that opcode is not enabled on the SLIX kernel
 * (no IA32_EFER.SCE), so it would fault with #UD if used here. The
 * shadow copy under aarch64-slix/ is byte-identical to upstream because
 * the aarch64 syscall ABI matches between Linux and SLIX; the x86_64
 * port has to diverge.
 */

.text
.global __cp_begin
.hidden __cp_begin
.global __cp_end
.hidden __cp_end
.global __cp_cancel
.hidden __cp_cancel
.hidden __cancel
.global __syscall_cp_asm
.hidden __syscall_cp_asm
.type   __syscall_cp_asm,@function
__syscall_cp_asm:

__cp_begin:
	mov (%rdi),%eax        # load cancel flag (rdi still holds the ptr)
	test %eax,%eax
	jnz __cp_cancel

	# Shuffle SysV C-ABI inputs into the SLIX syscall convention.
	# Forward order is safe: each step's source has already been
	# read by (or copied to) its eventual destination.
	mov %rsi,%rdi          # nr -> rdi
	mov %rdx,%rsi          # a -> rsi
	mov %rcx,%rdx          # b -> rdx
	mov %r8,%rcx           # c -> rcx
	mov %r9,%r8            # d -> r8
	mov 8(%rsp),%r9        # e -> r9
	mov 16(%rsp),%r10      # f -> r10

	int $0x80
__cp_end:
	ret
__cp_cancel:
	jmp __cancel
