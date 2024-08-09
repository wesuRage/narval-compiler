format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	__for_pointer_0 dq 0, 0
	__for_0_number dq 2, 0

segment readable executable
main:
__flow.for_0:
	mov rax, 0
	mov rcx, rax
	mov rax, 9
	inc rax
__flow.for_0_loop:
	cmp rcx, rax
	je __flow.for_0_end
	push rcx
	push rax
	mov qword [__for_pointer_0+8], rcx

	mov rdi, __for_pointer_0
	call totxt

	mov [__for_0_number+8], rax
	mov rdi, __for_0_number
	call write


	pop rax
	pop rcx
	inc rcx
	jmp __flow.for_0_loop
__flow.for_0_end:
	mov rdi, 0
	call exit
