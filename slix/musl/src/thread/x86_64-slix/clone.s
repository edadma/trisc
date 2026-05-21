.text
.global __clone
.hidden __clone
.type   __clone,@function
__clone:
	/* SLIX x86_64 __clone — int $0x80 ABI.
	 *
	 * C args (sysv): rdi=func, rsi=stack, rdx=flags, rcx=arg,
	 *                r8=ptid,  r9=tls,    8(%rsp)=ctid
	 *
	 * SLIX clone(146) regs: rdi=146, rsi=flags, rdx=stack,
	 *                       rcx=ptid, r8=ctid,  r9=tls. */

	/* Fetch ctid from caller's stack before touching %rsp. */
	mov  8(%rsp),%r11

	/* New stack: align then push func, arg. */
	and  $-16,%rsi
	sub  $16,%rsi
	mov  %rdi,0(%rsi)
	mov  %rcx,8(%rsi)

	/* Register shuffle. r9 (tls) is already in place. */
	xchg %rdx,%rsi             /* rsi=flags, rdx=stack */
	mov  %r8,%rcx              /* rcx=ptid */
	mov  %r11,%r8              /* r8 =ctid */

	mov  $146,%edi             /* SYS_clone (slix) */
	int  $0x80

	test %eax,%eax
	jz   2f
	ret                        /* parent */

2:	xor  %ebp,%ebp             /* child */
	pop  %rax                  /* func */
	pop  %rdi                  /* arg */
	call *%rax
	mov  %eax,%esi             /* exit code -> a0 */
	mov  $129,%edi             /* SYS_exit (slix) */
	int  $0x80
	hlt
