format ELF64 executable
entry main

include "/root/rust/narval/libs/standard.s"

segment readable writeable
	expr dq 0
	str_ dq 0x0

segment readable executable
main:
	mov rax, 9
	mov rbx, rax
	mov rax, 2
	xchg rax, rbx
	cqo
	idiv rbx
	mov [expr], rax
	push qword [expr]
	call int_to_str

	mov [str_], rax
	push qword [str_]
	call write

	push 0
	call exit

