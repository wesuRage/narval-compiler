format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	slashe_targe dq 0
	slashe_niggatron dq 0
	__TEMP_RETURN_0 dq 0
	resto_rest dq 0
	__TEMP_RETURN_1 dq 0
	num dq 0x0
	string dq 0x0

segment readable executable
main:
	push 9
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
	mov rax, rdi
	add rax, rbx
	mov [slashe_targe], rax
	mov rax, [slashe_targe]
	mov rbx, rax
	mov rax, 3
	push rax
	push rbx
	call __pow

	mov [slashe_niggatron], rax
	push qword [slashe_niggatron]
	call resto

	mov [__TEMP_RETURN_0], rax
	mov rax, [__TEMP_RETURN_0]

	mov rsp, rbp
	pop rbp
	ret

resto:
	push rbp
	mov rbp, rsp

	mov rdi, [rbp+16]
	mov rax, rdi
	mov rbx, rax
	mov rax, 4
	xchg rax, rbx
	cqo
	idiv rbx
	mov rax, rdx
	mov [resto_rest], rax
	mov rax, [resto_rest]
	mov [__TEMP_RETURN_1], rax
	mov rax, [__TEMP_RETURN_1]

	mov rsp, rbp
	pop rbp
	ret

