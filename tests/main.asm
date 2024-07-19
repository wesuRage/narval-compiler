format ELF64 executable
entry main

include "/root/rust/narval/libs/standard.s"

segment readable writeable
	expr dq "sexo", 0x0
	txt dq 0x0

segment readable executable
main:
	push expr
	call totxt

	mov [txt], rax
	push qword [txt]
	call write

	push 0
	call exit

