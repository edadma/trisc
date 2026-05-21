.global vfork
.type vfork,@function
vfork:
	/* SLIX has no SYS_vfork; emulate via clone(146) with
	 *   flags = CLONE_VM | CLONE_VFORK | SIGCHLD = 0x4111
	 *   stack = 0 (share parent's stack)
	 * The kernel suspends the parent until the child execs or
	 * exits, so it's safe for both to share the same RSP. */
	mov  $0x4111,%esi          /* a0 = flags */
	xor  %edx,%edx             /* a1 = stack (0) */
	mov  $146,%edi             /* SYS_clone (slix) */
	int  $0x80
	.hidden __syscall_ret
	jmp  __syscall_ret
