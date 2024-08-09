format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	__for_pointer_0 dq 0, 0
	__STR_0_PTR db "oh great goddess"
	__STR_0 dq 2, __STR_0_PTR

segment readable executable
main:
__flow.for_0:
	mov rax, 0
	mov rcx, rax
	mov rax, 5
	inc rax
__flow.for_0_loop:
	cmp rcx, rax
	je __flow.for_0_end
	push rcx
	push rax
	mov qword [__for_pointer_0+8], rcx

	mov rax, [__for_pointer_0+8]
	mov rbx, rax
	mov rax, 2
	cmp rax, rbx
	jne __flow.if_alternate_0
	jmp __flow.for_0_end
	jmp __flow.if_end_0
__flow.if_alternate_0:
__flow.if_end_0:
	mov rdi, __STR_0
	call write


	pop rax
	pop rcx
	inc rcx
	jmp __flow.for_0_loop
__flow.for_0_end:
	mov rdi, 0
	call exit
