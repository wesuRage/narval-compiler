format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	__STR_0 db "1", 0x0
	pinto dq 0x0
	rola dq 0x0

segment readable executable
main:
	mov rdi, __STR_0
	call toint

	mov [pinto], rax
	mov rdi, [pinto]
	call totxt

	mov [rola], rax
	mov rdi, [rola]
	call write

	mov rdi, 0
	call exit

