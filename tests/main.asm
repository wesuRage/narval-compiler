format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	slashe_targe dq 0
	__TEMP_RETURN dq 0
	num dq 0x0
	string dq 0x0

segment readable executable
main:
	push 7
	call slashe

	mov [num], rax
	push qword [num]
	call totxt

	mov [string], rax
	push qword [string]
	call write

	push 0
	call exit

slashe:
	push rbp
	mov rbp, rsp

	mov rdi, [rbp+16]
	mov rax, 2
	mov rbx, rax
	mov rax, 2
	mov rbx, rax
	mov rax, rdi
	xchg rax, rbx
	cqo
	idiv rbx
	mov rax, rdx
	mov [slashe_targe], rax
	add rax, rbx
	mov [slashe_targe], rax
	mov rax, [slashe_targe]
	mov [__TEMP_RETURN], rax
	mov rax, [__TEMP_RETURN]

	mov rsp, rbp
	pop rbp
	ret

