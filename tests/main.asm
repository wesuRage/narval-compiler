format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	__INT_0_PTR db 3
	num dq 0, __INT_0_PTR
	numero dq 0, 0
	hashtag dq 2, 0
	__STR_0_PTR db "#", 0
	__STR_0 dq 2, __STR_0_PTR
	total dq 2, 0
	__STR_1_PTR db "total:"
	__STR_1 dq 2, __STR_1_PTR

segment readable executable
main:
	mov rax, [num+8]
	mov rax, [rax]
	add rax, rax
	mov [numero+8], rax
	mov rbx, rax
	mov rax, 1
	add rax, rax
	mov [numero+8], rax
	mov rdi, rbx
	mov rsi, rax
	call __pow
	mov [numero+8], rax
	mov rbx, rax
	mov rax, 2
	add rax, rbx
	mov [numero+8], rax
	mov rbx, rax
	mov rax, 2
	xchg rax, rbx
	cqo
	idiv rbx
	mov [numero+8], rax
	mov rax, [numero+8]
	mov rdi, rax
	mov rsi, [__STR_0+8]
	call __txt_repeater
	mov [hashtag+8], rax
	mov rdi, numero
	call totxt

	mov [total+8], rax
	mov rdi, hashtag
	call write

	mov rdi, __STR_1
	call write

	mov rdi, total
	call write

	mov rdi, 0
	call exit

