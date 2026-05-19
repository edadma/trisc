	nop
.global __restore_rt
.hidden __restore_rt
.type __restore_rt,@function
__restore_rt:
	mov $287, %edi
	int $0x80
.size __restore_rt,.-__restore_rt
