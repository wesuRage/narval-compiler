format ELF64 executable
entry main

include "/root/rust/narval/libs/x86_64/standard.s"

segment readable writeable

segment readable executable
main:
	push 10
	call totxt

	push rax
	call write

	push 0
	call exit

