format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	abc dq 0, 0

segment readable executable
main:
	mov rax, 2
	mov rbx, rax
	mov rax, 3
	add rax, rbx
	mov [abc+8], rax
	push r15
	mov r15, 1
	mov rax, [abc+8]
	mov rbx, rax
	mov rax, 1
	sub rbx, rax
	mov rax, rbx
	mov qword [__TEMP_INTEGER_BUFFER], 0
	mov qword [__TEMP_INTEGER_BUFFER+8], rax
	mov rdi, __TEMP_INTEGER_BUFFER
	call totxt

	pop r15
	mov rdi, rax
	call write

	mov rdi, 0
	call exit
