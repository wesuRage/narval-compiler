format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	pinto dq 0, 0

segment readable executable
main:
	mov rax, 2
	add rax, rax
	mov [pinto+8], rax
	push r15
	mov r15, 1
	mov rax, [pinto+8]
	mov rbx, rax
	mov rax, 1
	sub rbx, rax
	mov [pinto+8], rax
	mov rdi, pinto
	call totxt

	pop r15
	mov rdi, rax
	call write

	mov rdi, 0
	call exit
