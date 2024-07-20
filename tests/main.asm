format ELF64 executable
entry main

include "/root/rust/narval/libs/standard.s"

segment readable writeable
	__STR_0 db "#", 0x0

segment readable executable
main:
	push __STR_0
	push 5
	call __txt_repeater

	push rax
	call write

	push 0
	call exit

