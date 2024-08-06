format ELF64 executable
entry main

include "/var/lib/narval/libs/x86_64/standard.s"

segment readable writeable
	__INT_0_PTR db 3
	pinto db 0, __INT_0_PTR
	__STR_0_PTR db "oi"
	__STR_0 dq 2, __STR_0_PTR
	__STR_1_PTR db "chora paulin"
	__STR_1 dq 2, __STR_1_PTR

segment readable executable
main:
__if_0:
	mov rax, 1
	mov rbx, rax
	mov rax, 1
	cmp rax, rbx
	jne __if_0_consequent_0
	mov rdi, __STR_0
	call write

__if_alternate_0:
__if_0:
	mov rax, 2
	mov rbx, rax
	mov rax, 3
	cmp rax, rbx
	jne __if_0_consequent_1
	mov rdi, __STR_1
	call write

__end_if_0:
__end_if_1:
	mov rdi, 0
	call exit

